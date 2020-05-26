// Copyright 2019-2020 VMware, all rights reserved

#include "thin_replica_client.hpp"

#include <jaegertracing/Tracer.h>
#include <opentracing/propagation.h>
#include <opentracing/span.h>
#include <opentracing/tracer.h>
#include <future>
#include <memory>
#include <numeric>
#include <sstream>

#include "trc_hash.hpp"

using com::vmware::concord::thin_replica::BlockId;
using com::vmware::concord::thin_replica::Data;
using com::vmware::concord::thin_replica::Hash;
using com::vmware::concord::thin_replica::KVPair;
using com::vmware::concord::thin_replica::ReadStateHashRequest;
using com::vmware::concord::thin_replica::ReadStateRequest;
using com::vmware::concord::thin_replica::SubscriptionRequest;
using grpc::ClientContext;
using grpc::ClientReaderInterface;
using grpc::Status;
using std::condition_variable;
using std::exception;
using std::future_status;
using std::hash;
using std::list;
using std::lock_guard;
using std::make_pair;
using std::map;
using std::mutex;
using std::pair;
using std::runtime_error;
using std::shared_ptr;
using std::string;
using std::stringstream;
using std::thread;
using std::to_string;
using std::unique_lock;
using std::unique_ptr;
using std::unordered_set;
using std::vector;
using thin_replica_client::BasicUpdateQueue;
using thin_replica_client::ThinReplicaClient;
using thin_replica_client::Update;

ThinReplicaClient::SpanPtr ThinReplicaClient::GetSpan(
    const Data& data, const std::string& child_name) {
  if (data.span_context().empty()) {
    LOG4CPLUS_DEBUG(logger_,
                    "Span for block: '" << data.block_id() << "' is empty");
    return nullptr;
  }
  std::istringstream context_stream;
  auto parent_span_context =
      opentracing::Tracer::Global()->Extract(context_stream);
  if (parent_span_context) {
    return opentracing::Tracer::Global()->StartSpan(
        child_name, {opentracing::SetTag{"cid", data.correlation_id()}});
  }

  LOG4CPLUS_DEBUG(logger_, "Failed to extract span for block: '"
                               << data.block_id()
                               << "', error:" << parent_span_context.error());
  return nullptr;
}
void ThinReplicaClient::InjectSpan(const ThinReplicaClient::SpanPtr& span,
                                   Update& update) {
  if (!span) return;
  std::ostringstream context;
  span->tracer().Inject(span->context(), context);
  update.span_context = context.str();
}
// Directs Jaeger log messages to Log4cpp
class JaegerLogger : public jaegertracing::logging::Logger {
 private:
  log4cplus::Logger logger =
      log4cplus::Logger::getInstance("thin_replica_client.jaeger");

 public:
  void error(const std::string& message) override {
    LOG4CPLUS_ERROR(logger, message);
  }

  void info(const std::string& message) override {
    LOG4CPLUS_INFO(logger, message);
  }
};

BasicUpdateQueue::BasicUpdateQueue()
    : queue_data_(), mutex_(), condition_(), release_consumers_(false) {}

BasicUpdateQueue::~BasicUpdateQueue() {}

void BasicUpdateQueue::ReleaseConsumers() {
  {
    lock_guard<mutex> lock(mutex_);
    release_consumers_ = true;
  }
  condition_.notify_all();
}

void BasicUpdateQueue::Clear() {
  lock_guard<mutex> lock(mutex_);
  queue_data_.clear();
}

void BasicUpdateQueue::Push(unique_ptr<Update> update) {
  {
    lock_guard<mutex> lock(mutex_);
    queue_data_.push_back(move(update));
  }
  condition_.notify_one();
}

unique_ptr<Update> BasicUpdateQueue::Pop() {
  unique_lock<mutex> lock(mutex_);
  while (!(release_consumers_ || (queue_data_.size() > 0))) {
    condition_.wait(lock);
  }
  if (release_consumers_) {
    return unique_ptr<Update>(nullptr);
  }
  assert(queue_data_.size() > 0);
  unique_ptr<Update> ret = move(queue_data_.front());
  queue_data_.pop_front();
  return ret;
}

unique_ptr<Update> BasicUpdateQueue::TryPop() {
  lock_guard<mutex> lock(mutex_);
  if (queue_data_.size() > 0) {
    unique_ptr<Update> ret = move(queue_data_.front());
    queue_data_.pop_front();
    return ret;
  } else {
    return unique_ptr<Update>(nullptr);
  }
}
uint64_t thin_replica_client::BasicUpdateQueue::Size() {
  return queue_data_.size();
}

void ThinReplicaClient::RecordCollectedHash(
    size_t update_source, uint64_t block_id,
    ThinReplicaClient::UpdateHashType update_hash,
    map<pair<uint64_t, ThinReplicaClient::UpdateHashType>,
        unordered_set<size_t>>& server_indexes_by_reported_update,
    size_t& maximal_agreeing_subset_size,
    pair<uint64_t, ThinReplicaClient::UpdateHashType>&
        maximally_agreed_on_update) {
  pair<uint64_t, UpdateHashType> update = make_pair(block_id, update_hash);
  if (server_indexes_by_reported_update.count(update) < 1) {
    server_indexes_by_reported_update.emplace(update, unordered_set<size_t>());
  }
  server_indexes_by_reported_update[update].emplace(update_source);
  size_t update_agreement = server_indexes_by_reported_update[update].size();
  if (update_agreement > maximal_agreeing_subset_size) {
    maximal_agreeing_subset_size = update_agreement;
    maximally_agreed_on_update = update;
  }
}

void ThinReplicaClient::ReadUpdateHashFromStream(
    size_t server_index,
    map<pair<uint64_t, ThinReplicaClient::UpdateHashType>,
        unordered_set<size_t>>& server_indexes_by_reported_update,
    size_t& maximal_agreeing_subset_size,
    pair<uint64_t, ThinReplicaClient::UpdateHashType>&
        maximally_agreed_on_update) {
  Hash hash;
  LOG4CPLUS_DEBUG(logger_, "Read hash from " << server_index);

  auto reader = std::async(std::launch::async, [this, server_index, &hash] {
    return subscription_hash_streams_[server_index]->Read(&hash);
  });
  auto status = reader.wait_for(timeout_read_hash_stream_);
  if (status == future_status::timeout || status == future_status::deferred) {
    CloseStream(subscription_hash_streams_[server_index],
                sub_hash_contexts_[server_index]);
    LOG4CPLUS_WARN(logger_, "Hash stream " << server_index << " timed out.");
    return;
  }
  assert(status == future_status::ready);

  if (!reader.get()) {
    LOG4CPLUS_WARN(logger_, "Hash stream " << server_index << " read failed.");
    return;
  }

  if (hash.block_id() < latest_verified_block_id_) {
    LOG4CPLUS_WARN(logger_,
                   "Hash stream "
                       << server_index
                       << " gave an update with decreasing block number: "
                       << hash.block_id());
    return;
  }

  if (hash.hash().length() > sizeof(UpdateHashType)) {
    LOG4CPLUS_WARN(logger_,
                   "Hash stream "
                       << server_index
                       << " gave a hash update with an unexpectedly long hash: "
                       << hash.hash().length());
    return;
  }

  LOG4CPLUS_DEBUG(logger_, "Record hash for block " << hash.block_id());
  string hash_string = hash.hash();
  assert(hash_string.length() <= sizeof(UpdateHashType));
  hash_string.resize(sizeof(UpdateHashType), '\0');

  UpdateHashType hash_value =
      *(reinterpret_cast<const UpdateHashType*>(hash.hash().data()));
  RecordCollectedHash(server_index, hash.block_id(), hash_value,
                      server_indexes_by_reported_update,
                      maximal_agreeing_subset_size, maximally_agreed_on_update);
}

template <class ReaderType>
void ThinReplicaClient::CloseStream(unique_ptr<ReaderType>& stream,
                                    unique_ptr<ClientContext>& context) {
  assert(stream);
  assert(context);

  context->TryCancel();
  stream->Finish();
  stream.reset();
  context.reset();
}

std::pair<bool, ThinReplicaClient::SpanPtr> ThinReplicaClient::ReadBlock(
    Data& update_in, AgreeingSubsetMembers& agreeing_subset_members,
    size_t& most_agreeing, BlockIdHashPair& most_agreed_block) {
  auto reader = std::async(std::launch::async, [this, &update_in] {
    return subscription_data_stream_->Read(&update_in);
  });
  auto status = reader.wait_for(timeout_read_data_stream_);
  if (status == future_status::timeout || status == future_status::deferred) {
    LOG4CPLUS_WARN(logger_,
                   "Data stream " << current_data_source_ << " timed out");
    return {false, nullptr};
  }
  assert(status == future_status::ready);

  if (!reader.get()) {
    LOG4CPLUS_WARN(logger_,
                   "Data stream " << current_data_source_ << " read failed");
    return {false, nullptr};
  }

  auto span = GetSpan(update_in, "trc_read_block");
  if (update_in.block_id() < latest_verified_block_id_) {
    LOG4CPLUS_WARN(logger_,
                   "Data subscription stream (to server index "
                       << current_data_source_
                       << ") gave a data update with decreasing block number.");
    return {false, nullptr};
  }

  UpdateHashType update_data_hash = HashUpdate(update_in);
  RecordCollectedHash(current_data_source_, update_in.block_id(),
                      update_data_hash, agreeing_subset_members, most_agreeing,
                      most_agreed_block);
  return {true, std::move(span)};
}

void ThinReplicaClient::StartHashStreamWith(size_t server_index) {
  assert(server_index != current_data_source_);
  if (subscription_hash_streams_[server_index]) {
    CloseStream(subscription_hash_streams_[server_index],
                sub_hash_contexts_[server_index]);
  }
  sub_hash_contexts_[server_index].reset(new ClientContext());
  sub_hash_contexts_[server_index]->AddMetadata("client_id", client_id_);
  SubscriptionRequest sub_request;
  sub_request.set_block_id(latest_verified_block_id_ + 1);
  sub_request.set_key_prefix(key_prefix_);
  subscription_hash_streams_[server_index] =
      server_stubs_[server_index]->SubscribeToUpdateHashes(
          sub_hash_contexts_[server_index].get(), sub_request);
}

void ThinReplicaClient::FindBlockHashAgreement(
    std::vector<bool>& servers_tried,
    AgreeingSubsetMembers& agreeing_subset_members, size_t& most_agreeing,
    BlockIdHashPair& most_agreed_block, SpanPtr& parent_span) {
  SpanPtr span = nullptr;
  if (parent_span) {
    span = opentracing::Tracer::Global()->StartSpan(
        "trclient_verify_hash_against_additional_servers",
        {opentracing::ChildOf(&parent_span->context())});
  }

  // Create a list of server indexes so that we start iterating over the ones
  // that have an open stream already. If we cannot find agreement amongst them
  // then we keep going and try the other servers too.
  std::vector<size_t> sorted_servers(server_stubs_.size());
  std::iota(sorted_servers.begin(), sorted_servers.end(), 0);
  std::stable_sort(
      sorted_servers.begin(), sorted_servers.end(), [this](auto a, auto b) {
        return subscription_hash_streams_[a] > subscription_hash_streams_[b];
      });

  for (auto server_index : sorted_servers) {
    assert(server_stubs_[server_index]);
    if (servers_tried[server_index]) {
      continue;
    }
    if (stop_subscription_thread_) {
      return;
    }

    if (!subscription_hash_streams_[server_index]) {
      LOG4CPLUS_DEBUG(logger_, "Additionally asking " << server_index);
      StartHashStreamWith(server_index);
    }

    ReadUpdateHashFromStream(server_index, agreeing_subset_members,
                             most_agreeing, most_agreed_block);
    servers_tried[server_index] = true;

    if (most_agreeing >= (max_faulty_ + 1)) {
      return;
    }
  }
}

void ThinReplicaClient::ResetDataStreamTo(size_t server_index) {
  assert(server_stubs_[server_index]);
  if (subscription_data_stream_) {
    CloseStream(subscription_data_stream_, sub_data_context_);
  }
  if (subscription_hash_streams_[server_index]) {
    CloseStream(subscription_hash_streams_[server_index],
                sub_hash_contexts_[server_index]);
  }
  SubscriptionRequest sub_request;
  sub_data_context_.reset(new ClientContext());
  sub_data_context_->AddMetadata("client_id", client_id_);
  sub_request.set_block_id(latest_verified_block_id_ + 1);
  sub_request.set_key_prefix(key_prefix_);
  subscription_data_stream_ = server_stubs_[server_index]->SubscribeToUpdates(
      sub_data_context_.get(), sub_request);
  current_data_source_ = server_index;
}

void ThinReplicaClient::CloseAllHashStreams() {
  for (size_t i = 0; i < server_stubs_.size(); ++i) {
    if (subscription_hash_streams_[i]) {
      CloseStream(subscription_hash_streams_[i], sub_hash_contexts_[i]);
    }
  }
}

bool ThinReplicaClient::RotateDataStreamAndVerify(
    Data& update_in, AgreeingSubsetMembers& agreeing_subset_members,
    BlockIdHashPair& most_agreed_block, SpanPtr& parent_span) {
  SpanPtr span = nullptr;
  if (parent_span) {
    span = opentracing::Tracer::Global()->StartSpan(
        "trclient_rotate_server_and_verify_hash",
        {opentracing::ChildOf(&parent_span->context())});
  }

  for (const auto server_index : agreeing_subset_members[most_agreed_block]) {
    assert(server_index < server_stubs_.size());
    if (stop_subscription_thread_) {
      return false;
    }
    if (subscription_hash_streams_[server_index]) {
      CloseStream(subscription_hash_streams_[server_index],
                  sub_hash_contexts_[server_index]);
    }

    ResetDataStreamTo(server_index);

    auto reader = std::async(std::launch::async, [this, &update_in] {
      return subscription_data_stream_->Read(&update_in);
    });
    auto status = reader.wait_for(timeout_read_hash_stream_);
    if (status == future_status::timeout || status == future_status::deferred) {
      LOG4CPLUS_WARN(
          logger_,
          "Read timed out on a data subscription stream (to server index "
              << server_index << ").");
      continue;
    }
    assert(status == future_status::ready);

    if (!reader.get()) {
      LOG4CPLUS_WARN(
          logger_, "Read failed on a data subscription stream (to server index "
                       << server_index << ").");
      continue;
    }

    if (update_in.block_id() != most_agreed_block.first) {
      LOG4CPLUS_WARN(logger_,
                     "Data subscription stream (to server index "
                         << server_index
                         << ") gave a data update with a block number in "
                            "disagreement with the consensus and "
                            "contradicting its own hash update.");
      continue;
    }

    UpdateHashType update_data_hash = HashUpdate(update_in);
    if (update_data_hash != most_agreed_block.second) {
      LOG4CPLUS_WARN(logger_,
                     "Data subscription stream (to server index "
                         << server_index
                         << ") gave a data update hashing to a value "
                            "in disagreement with the consensus on the "
                            "hash for this block and contradicting the "
                            "server's own hash update.");
      continue;
    }

    return true;
  }
  return false;
}

void ThinReplicaClient::ReceiveUpdates() {
  assert(!subscription_data_stream_);
  assert(subscription_hash_streams_.size() == 0);
  assert(server_stubs_.size() > 0);

  if (stop_subscription_thread_) {
    LOG4CPLUS_WARN(logger_, "Need to stop receiving updates");
    return;
  }

  // We should have stubs for all servers
  for (auto server = 0; server < server_stubs_.size(); ++server) {
    assert(server_stubs_[server]);
  }

  subscription_hash_streams_ =
      vector<unique_ptr<ClientReaderInterface<Hash>>>(server_stubs_.size());
  sub_hash_contexts_ = vector<unique_ptr<ClientContext>>(server_stubs_.size());

  // Set initial data stream
  ResetDataStreamTo(0);

  // Main subscription-driving loop; one iteration of this outer loop
  // corresponds to receiving, validating, and returning one update.
  // We break out of this loop only if the application sets the flag.
  while (!stop_subscription_thread_) {
    // For each loop of the outer iteration, we need to find at least
    // (max_faulty_ + 1) responsive agreeing servers (we count the server that
    // gave us the actual data for this update as one of the agreeing, so we
    // need it plus max_faulty_ servers giving agreeing hashes) in order to
    // validate and return an update.

    Data update_in;
    SpanPtr span = nullptr;
    vector<bool> servers_tried(server_stubs_.size(), false);

    AgreeingSubsetMembers agreeing_subset_members;
    BlockIdHashPair most_agreed_block;
    size_t most_agreeing = 0;
    bool has_data = false;
    bool has_verified_data = false;

    assert(subscription_data_stream_);

    // First, we collect updates from all subscriptions streams we have which
    // are already open, starting with the data stream and followed by any hash
    // streams.
    LOG4CPLUS_DEBUG(logger_, "Read from data stream " << current_data_source_);
    std::tie(has_data, span) = ReadBlock(update_in, agreeing_subset_members,
                                         most_agreeing, most_agreed_block);
    servers_tried[current_data_source_] = true;

    LOG4CPLUS_DEBUG(logger_, "Find agreement amongst all servers for block "
                                 << update_in.block_id());
    FindBlockHashAgreement(servers_tried, agreeing_subset_members,
                           most_agreeing, most_agreed_block, span);
    if (stop_subscription_thread_) {
      break;
    }

    // At this point we need to have agreeing servers.
    if (most_agreeing < (max_faulty_ + 1)) {
      LOG4CPLUS_WARN(logger_,
                     "Couldn't find agreement amongst all servers. Try again.");
      // We need to force re-subscription on at least one of the f+1 open
      // streams otherwise we might skip an update. By closing all streams here
      // we do exactly what the algorithm would do in the next iteration of this
      // loop anyways.
      CloseAllHashStreams();
      ResetDataStreamTo((current_data_source_ + 1) % server_stubs_.size());
      continue;
    }

    // If we have data, check whether its hash is the agreement.
    if (has_data && agreeing_subset_members[most_agreed_block].count(
                        current_data_source_) > 0) {
      has_verified_data = true;
    }

    // We have enough agreeing servers but, if the existing data stream is not
    // among them then let's rotate the data stream to one of the servers
    // within the agreeing set.
    if (!has_verified_data) {
      has_verified_data = RotateDataStreamAndVerify(
          update_in, agreeing_subset_members, most_agreed_block, span);
      if (!has_verified_data) {
        LOG4CPLUS_WARN(logger_,
                       "Couldn't get data from agreeing servers. Try again.");
        // We need to force re-subscription on at least one of the f+1 open
        // streams otherwise we might skip an update. By closing all streams
        // here we do exactly what the algorithm would do in the next iteration
        // of this loop anyways.
        CloseAllHashStreams();
        ResetDataStreamTo((current_data_source_ + 1) % server_stubs_.size());
        continue;
      }
    }

    assert(has_verified_data);
    LOG4CPLUS_DEBUG(
        logger_, "Read and verified data for block " << update_in.block_id());

    assert(update_queue_);

    unique_ptr<Update> update(new Update());
    update->block_id = update_in.block_id();
    update->correlation_id_ = update_in.correlation_id();
    for (const auto& kvp_in : update_in.data()) {
      update->kv_pairs.push_back(make_pair(kvp_in.key(), kvp_in.value()));
    }
    InjectSpan(span, *update);

    // Update metrics
    trc_updates_counter_.Increment();
    trc_queue_size_.Set(update_queue_->Size());

    update_queue_->Push(move(update));
    latest_verified_block_id_ = update_in.block_id();

    // Cleanup before the next update

    // The main subscription loop should not be leaving any more than
    // (max_faulty_ + 1) subscription streams open before ending each iteration;
    // the fact it shouldn't may or not be used as a simplifying assumption in
    // the loop's implementation. Note that we always close data streams before
    // opening new ones when rotating data streams, so it is only necessary to
    // close
    for (size_t server_index = 0; server_index < server_stubs_.size();
         ++server_index) {
      if (!subscription_hash_streams_[server_index]) {
        continue;
      }
      if (agreeing_subset_members[most_agreed_block].count(server_index) < 1) {
        LOG4CPLUS_DEBUG(logger_, "Close hash stream " << server_index
                                                      << " after block "
                                                      << update_in.block_id());
        CloseStream(subscription_hash_streams_[server_index],
                    sub_hash_contexts_[server_index]);
      }
    }
  }

  // Cleanup logic to close subscription streams and such if the subscription
  // fails or is stopped by the application.

  if (subscription_data_stream_) {
    CloseStream(subscription_data_stream_, sub_data_context_);
  }
  assert(subscription_hash_streams_.size() == sub_hash_contexts_.size());
  CloseAllHashStreams();
  subscription_hash_streams_.clear();
  sub_hash_contexts_.clear();

  stop_subscription_thread_ = true;

  // TODO (Alex): Complete, revise, and clean up error handling for this
  //              function.
  // TODO (Alex): Add logging to document any observed disagreements among
  //              servers.
}

ThinReplicaClient::~ThinReplicaClient() {
  stop_subscription_thread_ = true;
  if (subscription_thread_) {
    assert(subscription_thread_->joinable());
    subscription_thread_->join();
  }
}

void ThinReplicaClient::Subscribe(const string& key_prefix_bytes) {
  assert(server_stubs_.size() > 0);
  // XXX: The following implementation does not achieve Subscribe's specified
  //      interface and behavior (see the comments with Subscribe's declaration
  //      in the Thin Replica Client Library header file for documentation of
  //      that interface); this implementation is intended to establish minimal
  //      end-to-end connectivity with a non-faulty Thin Replica Server in order
  //      to preserve the general behavior of the example Thin Replica Client
  //      application (which at this time just connects to a server and checks
  //      the status returned for a Block read from data streamStateRequest).

  // Stop any existing subscription before trying to start a new one.
  stop_subscription_thread_ = true;
  if (subscription_thread_) {
    assert(subscription_thread_->joinable());
    subscription_thread_->join();
    subscription_thread_.reset();
  }

  bool has_verified_state = false;
  size_t data_server_index = 0;
  list<unique_ptr<Update>> state;
  uint64_t block_id = 0;

  while (!has_verified_state && (data_server_index < server_stubs_.size())) {
    state.clear();
    block_id = 0;
    StateHashType expected_hash = 0;
    bool received_state_invalid = false;

    ReadStateRequest request;
    request.set_key_prefix(key_prefix_bytes);
    ClientContext read_context;
    read_context.AddMetadata("client_id", client_id_);
    LOG4CPLUS_DEBUG(logger_, "Read state from " << data_server_index);
    unique_ptr<ClientReaderInterface<Data>> stream =
        server_stubs_[data_server_index]->ReadState(&read_context, request);

    Data response;
    while (!received_state_invalid && stream->Read(&response)) {
      if ((state.size() > 0) && (response.block_id() < block_id)) {
        LOG4CPLUS_WARN(logger_,
                       "While trying to fetch initial state for a "
                       "subscription, ThinReplicaClient received an update "
                       "with a decreasing Block ID from a server (server index "
                           << data_server_index << ").");
        received_state_invalid = true;
      } else {
        block_id = response.block_id();
        unique_ptr<Update> update(new Update());
        update->block_id = block_id;
        update->correlation_id_ = response.correlation_id();
        for (int i = 0; i < response.data_size(); ++i) {
          KVPair kvp = response.data(i);
          update->kv_pairs.push_back(make_pair(kvp.key(), kvp.value()));
          size_t update_tail_index = update->kv_pairs.size() - 1;
        }
        expected_hash ^= HashUpdate(*update);
        state.push_back(move(update));
      }
    }
    Status status = stream->Finish();
    if (!status.ok()) {
      LOG4CPLUS_WARN(logger_,
                     "While trying to fetch initial state for a subscription, "
                     "ThinRepliThinReplicaClient received response to "
                     "ReadStateRequest from ThinReplicaServer (server index: "
                         << data_server_index
                         << ") giving a failure status (error code: "
                         << status.error_code() << ", error message: \""
                         << status.error_message() << "\").");
      received_state_invalid = true;
    }

    LOG4CPLUS_DEBUG(logger_, "Got initial state from " << data_server_index);

    // We count the server we got the initial state data from as the first of
    // (max_faulty + 1) servers we need to find agreeing upon this state in
    // order to accept it.
    size_t agreeing_servers = 1;
    size_t hash_server_index = 0;
    while (!received_state_invalid &&
           (hash_server_index < server_stubs_.size()) &&
           (agreeing_servers <= (size_t)max_faulty_)) {
      if (hash_server_index == data_server_index) {
        ++hash_server_index;
        continue;
      }
      ReadStateHashRequest hash_request;
      hash_request.set_block_id(block_id);
      hash_request.set_key_prefix(key_prefix_bytes);
      Hash hash_response;
      ClientContext hash_context;
      hash_context.AddMetadata("client_id", client_id_);
      LOG4CPLUS_DEBUG(logger_, "Read state hash from " << hash_server_index);
      status = server_stubs_[hash_server_index]->ReadStateHash(
          &hash_context, hash_request, &hash_response);
      ++hash_server_index;

      // Check whether the hash came back with an ok status, matches the Block
      // ID we requested, and matches the hash we computed locally of the data,
      // and only count it as agreeing if we complete all this verification.
      if (!status.ok()) {
        LOG4CPLUS_WARN(
            logger_,
            "Server "
                << hash_server_index
                << " gave error response to ReadStateHash (requested Block ID: "
                << block_id << ").");
        continue;
      }
      if (hash_response.block_id() != block_id) {
        LOG4CPLUS_WARN(logger_,
                       "Server "
                           << hash_server_index
                           << " gave response to ReadStateHash disagreeing "
                              "with requested Block ID (requested Block ID: "
                           << block_id << ", response contained Block ID: "
                           << hash_response.block_id() << ").");
        continue;
      }
      if (hash_response.hash() !=
          string(reinterpret_cast<char*>(&expected_hash),
                 sizeof(StateHashType))) {
        LOG4CPLUS_WARN(
            logger_, "Server "
                         << hash_server_index
                         << " gave response to ReadStateHash in disagreement "
                            "with the expected hash value (requested Block ID: "
                         << block_id << ").");
        continue;
      }

      ++agreeing_servers;
    }
    if (!received_state_invalid && (agreeing_servers > max_faulty_)) {
      has_verified_state = true;
    }

    ++data_server_index;
  }
  if (!has_verified_state) {
    throw runtime_error(
        "Could not start ThinReplicaClient subscription: Failed to find a set "
        "of at least " +
        to_string((uint32_t)max_faulty_ + 1) +
        " responding Thin Replica Client Servers in agreement about what the "
        "initial state should be.");
  }

  LOG4CPLUS_DEBUG(logger_, "Got verified initial state for block " << block_id);

  update_queue_->Clear();
  while (state.size() > 0) {
    update_queue_->Push(move(state.front()));
    state.pop_front();
  }
  key_prefix_ = key_prefix_bytes;
  latest_verified_block_id_ = block_id;
  // Create and launch thread to stream updates from the servers and push them
  // into the queue.
  stop_subscription_thread_ = false;
  subscription_thread_.reset(
      new thread(&ThinReplicaClient::ReceiveUpdates, this));

  // TODO (Alex): Complete, revise, and clean up error handling for this
  //              function.
  LOG4CPLUS_WARN(
      logger_,
      "thin_replica_client::ThinReplicaClient::Subscribe is incomplete in its "
      "error handling and recovery; the worker thread Subscribe creates is "
      "also incomple in its error handling and recovery.");
}

void ThinReplicaClient::Subscribe(const string& key_prefix_bytes,
                                  uint64_t block_id) {
  // Stop any existing subscription before trying to start a new one.
  stop_subscription_thread_ = true;
  if (subscription_thread_) {
    assert(subscription_thread_->joinable());
    subscription_thread_->join();
    subscription_thread_.reset();
  }

  update_queue_->Clear();
  key_prefix_ = key_prefix_bytes;
  latest_verified_block_id_ = block_id;

  // Create and launch thread to stream updates from the servers and push them
  // into the queue.
  stop_subscription_thread_ = false;
  subscription_thread_.reset(
      new thread(&ThinReplicaClient::ReceiveUpdates, this));

  // TODO (Alex): Complete, revise, and clean up error handling for this
  //              function.
  LOG4CPLUS_WARN(
      logger_,
      "thin_replica_client::ThinReplicaClient::Subscribe is incomplete in its "
      "error handling and recovery; the worker thread Subscribe creates is "
      "also incomple in its error handling and recovery.");
}

// This is a placeholder implementation as the Unsubscribe gRPC call is not yet
// implemented on the server side.
//
// TODO (Alex):
//     - Add lines to actually send an unsubscription one the Thin Replica
//       Server supports receiving it.
//     - Add logic for signing the unsubscription once the signature scheme is
//       defined.
//     - Add logic to pick a different server to send the acknowledgement to if
//       server 0 is known to be down or faulty.
void ThinReplicaClient::Unsubscribe() {
  LOG4CPLUS_DEBUG(logger_, "Unsubscribe");
  stop_subscription_thread_ = true;
  if (subscription_thread_) {
    assert(subscription_thread_->joinable());
    subscription_thread_->join();
    subscription_thread_.reset();
  }

  size_t server_to_send_unsubscription_to = 0;
  assert(server_stubs_.size() > server_to_send_unsubscription_to);
  assert(server_stubs_[server_to_send_unsubscription_to]);
}

// This is a placeholder implementation as the AckUpdate gRPC call is not yet
// implemented on the server side.
//
// TODO (Alex):
//     - Add lines to actually send message once the Thin Replica Server
//       supports receiving it.
//     - Add logic for signing the acknowledgement once the signature scheme is
//       defined.
//     - Add logic to pick a different server to send the acknowledgement to if
//       server 0 is known to be down or faulty.
void ThinReplicaClient::AcknowledgeBlockID(uint64_t block_id) {
  BlockId AckMessage;
  AckMessage.set_block_id(block_id);

  size_t server_to_acknowledge_to = 0;
  assert(server_stubs_.size() > server_to_acknowledge_to);
  assert(server_stubs_[server_to_acknowledge_to]);
}

void ThinReplicaClient::SetupTracing(const std::string& jaeger_agent) {
  LOG4CPLUS_INFO(logger_, "Tracing to jaeger agent: " << jaeger_agent);

  // No sampling for now - report all traces
  jaegertracing::samplers::Config sampler_config(
      jaegertracing::kSamplerTypeConst, 1.0);
  jaegertracing::reporters::Config reporter_config(
      jaegertracing::reporters::Config::kDefaultQueueSize,
      jaegertracing::reporters::Config::defaultBufferFlushInterval(),
      false /* do not log spans */, jaeger_agent);
  jaegertracing::Config config(false /* not disabled */, sampler_config,
                               reporter_config);
  auto tracer = jaegertracing::Tracer::make(
      "thin_replica_client", config,
      std::unique_ptr<jaegertracing::logging::Logger>(new JaegerLogger()));
  opentracing::Tracer::InitGlobal(
      std::static_pointer_cast<opentracing::Tracer>(tracer));
}
