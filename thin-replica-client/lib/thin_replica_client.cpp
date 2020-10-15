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
#include "trs_connection.hpp"

using com::vmware::concord::thin_replica::BlockId;
using com::vmware::concord::thin_replica::Data;
using com::vmware::concord::thin_replica::Hash;
using com::vmware::concord::thin_replica::KVPair;
using com::vmware::concord::thin_replica::ReadStateHashRequest;
using com::vmware::concord::thin_replica::ReadStateRequest;
using com::vmware::concord::thin_replica::SubscriptionRequest;
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
    return trs_conns_[server_index]->readHash(&hash);
  });
  auto status = reader.wait_for(timeout_read_hash_stream_);
  if (status == future_status::timeout || status == future_status::deferred) {
    // If a stream Read launched with std::async times out, it is necessary to
    // cancel it and close the stream before the asynchronous thread goes out of
    // scope, as std::async's destructor will try to join the reader thread and
    // could get stuck waiting on the already-timed-out Read call to return if
    // we didn't cancel it.
    CloseStream(trs_conns_[server_index]);

    LOG4CPLUS_DEBUG(logger_, "Hash stream " << server_index << " timed out.");
    read_timeouts_per_update_++;
    return;
  }
  assert(status == future_status::ready);

  if (!reader.get()) {
    LOG4CPLUS_DEBUG(logger_, "Hash stream " << server_index << " read failed.");
    read_failures_per_update_++;
    return;
  }

  if (hash.block_id() < latest_verified_block_id_) {
    LOG4CPLUS_WARN(logger_,
                   "Hash stream "
                       << server_index
                       << " gave an update with decreasing block number: "
                       << hash.block_id());
    read_ignored_per_update_++;
    return;
  }

  if (hash.hash().length() > sizeof(UpdateHashType)) {
    LOG4CPLUS_WARN(logger_, "Hash stream "
                                << server_index << " gave an update (block "
                                << hash.block_id()
                                << ") with an unexpectedly long hash: "
                                << hash.hash().length());
    read_ignored_per_update_++;
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

void ThinReplicaClient::CloseStream(unique_ptr<TrsConnection>& stream) {
  assert(stream);
  stream->closeDataStream();
  stream->closeHashStream();
}

std::pair<bool, ThinReplicaClient::SpanPtr> ThinReplicaClient::ReadBlock(
    Data& update_in, AgreeingSubsetMembers& agreeing_subset_members,
    size_t& most_agreeing, BlockIdHashPair& most_agreed_block) {
  auto reader = std::async(std::launch::async, [this, &update_in] {
    return trs_conns_[data_conn_index_]->readData(&update_in);
  });
  auto status = reader.wait_for(timeout_read_data_stream_);
  if (status == future_status::timeout || status == future_status::deferred) {
    // If a stream Read launched with std::async times out, it is necessary to
    // cancel it and close the stream before the asynchronous thread goes out of
    // scope, as std::async's destructor will try to join the reader thread and
    // could get stuck waiting on the already-timed-out Read call to return if
    // we didn't cancel it.
    CloseStream(trs_conns_[data_conn_index_]);

    LOG4CPLUS_DEBUG(logger_,
                    "Data stream " << data_conn_index_ << " timed out");
    read_timeouts_per_update_++;
    return {false, nullptr};
  }
  assert(status == future_status::ready);

  if (!reader.get()) {
    LOG4CPLUS_DEBUG(logger_,
                    "Data stream " << data_conn_index_ << " read failed");
    read_failures_per_update_++;
    return {false, nullptr};
  }

  auto span = GetSpan(update_in, "trc_read_block");
  if (update_in.block_id() < latest_verified_block_id_) {
    LOG4CPLUS_WARN(logger_,
                   "Data stream "
                       << data_conn_index_
                       << " gave an update with decreasing block number: "
                       << update_in.block_id());
    read_ignored_per_update_++;
    return {false, nullptr};
  }

  UpdateHashType update_data_hash = HashUpdate(update_in);
  RecordCollectedHash(data_conn_index_, update_in.block_id(), update_data_hash,
                      agreeing_subset_members, most_agreeing,
                      most_agreed_block);
  return {true, std::move(span)};
}

void ThinReplicaClient::StartHashStreamWith(size_t server_index) {
  assert(server_index != data_conn_index_);
  trs_conns_[server_index]->closeHashStream();

  SubscriptionRequest request;
  request.set_block_id(latest_verified_block_id_ + 1);
  request.set_key_prefix(key_prefix_);
  trs_conns_[server_index]->openHashStream(request);
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
  std::vector<size_t> sorted_servers(trs_conns_.size());
  std::iota(sorted_servers.begin(), sorted_servers.end(), 0);
  std::stable_sort(
      sorted_servers.begin(), sorted_servers.end(), [this](auto a, auto b) {
        return trs_conns_[a]->hasHashStream() > trs_conns_[b]->hasHashStream();
      });

  for (auto server_index : sorted_servers) {
    assert(trs_conns_[server_index]);
    if (servers_tried[server_index]) {
      continue;
    }
    if (stop_subscription_thread_) {
      return;
    }

    if (!trs_conns_[server_index]->hasHashStream()) {
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
  assert(trs_conns_[server_index]);
  CloseStream(trs_conns_[server_index]);
  CloseStream(trs_conns_[data_conn_index_]);

  SubscriptionRequest request;
  request.set_block_id(latest_verified_block_id_ + 1);
  request.set_key_prefix(key_prefix_);
  trs_conns_[server_index]->openDataStream(request);

  data_conn_index_ = server_index;
}

void ThinReplicaClient::CloseAllHashStreams() {
  for (size_t i = 0; i < trs_conns_.size(); ++i) {
    if (i != data_conn_index_) {
      trs_conns_[i]->closeHashStream();
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
    assert(server_index < trs_conns_.size());
    if (stop_subscription_thread_) {
      return false;
    }

    ResetDataStreamTo(server_index);

    auto reader = std::async(std::launch::async, [this, &update_in] {
      return trs_conns_[data_conn_index_]->readData(&update_in);
    });
    auto status = reader.wait_for(timeout_read_hash_stream_);
    if (status == future_status::timeout || status == future_status::deferred) {
      // If a stream Read launched with std::async times out, it is necessary to
      // cancel it and close the stream before the asynchronous thread goes out
      // of scope, as std::async's destructor will try to join the reader thread
      // and could get stuck waiting on the already-timed-out Read call to
      // return if we didn't cancel it.
      CloseStream(trs_conns_[data_conn_index_]);

      LOG4CPLUS_DEBUG(
          logger_,
          "Read timed out on a data subscription stream (to server index "
              << server_index << ").");
      read_timeouts_per_update_++;
      continue;
    }
    assert(status == future_status::ready);

    if (!reader.get()) {
      LOG4CPLUS_DEBUG(
          logger_, "Read failed on a data subscription stream (to server index "
                       << server_index << ").");
      read_failures_per_update_++;
      continue;
    }

    if (update_in.block_id() != most_agreed_block.first) {
      LOG4CPLUS_WARN(logger_, "Data stream "
                                  << server_index
                                  << " gave an update with a block number ("
                                  << update_in.block_id()
                                  << ") in "
                                     "disagreement with the consensus and "
                                     "contradicting its own hash update.");
      read_ignored_per_update_++;
      continue;
    }

    UpdateHashType update_data_hash = HashUpdate(update_in);
    if (update_data_hash != most_agreed_block.second) {
      LOG4CPLUS_WARN(logger_,
                     "Data stream "
                         << server_index
                         << " gave an update hashing to a value "
                            "in disagreement with the consensus on the "
                            "hash for this block ("
                         << update_in.block_id()
                         << ") and contradicting the "
                            "server's own hash update.");
      read_ignored_per_update_++;
      continue;
    }

    return true;
  }
  return false;
}

void ThinReplicaClient::ReceiveUpdates() {
  assert(trs_conns_.size() > 0);

  if (stop_subscription_thread_) {
    LOG4CPLUS_WARN(logger_, "Need to stop receiving updates");
    return;
  }

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
    vector<bool> servers_tried(trs_conns_.size(), false);

    AgreeingSubsetMembers agreeing_subset_members;
    BlockIdHashPair most_agreed_block;
    size_t most_agreeing = 0;
    bool has_data = false;
    bool has_verified_data = false;

    assert(trs_conns_[data_conn_index_]->hasDataStream());

    // First, we collect updates from all subscription streams we have which
    // are already open, starting with the data stream and followed by any hash
    // streams.
    LOG4CPLUS_DEBUG(logger_, "Read from data stream " << data_conn_index_);
    std::tie(has_data, span) = ReadBlock(update_in, agreeing_subset_members,
                                         most_agreeing, most_agreed_block);
    servers_tried[data_conn_index_] = true;

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
      ResetDataStreamTo((data_conn_index_ + 1) % trs_conns_.size());
      continue;
    }

    // If we have data, check whether its hash is the agreement.
    if (has_data && agreeing_subset_members[most_agreed_block].count(
                        data_conn_index_) > 0) {
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
        ResetDataStreamTo((data_conn_index_ + 1) % trs_conns_.size());
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

    if (read_timeouts_per_update_ > 0 || read_failures_per_update_ > 0 ||
        read_ignored_per_update_ > 0) {
      LOG4CPLUS_WARN(logger_, read_timeouts_per_update_
                                  << " timeouts, " << read_failures_per_update_
                                  << " failures, and "
                                  << read_ignored_per_update_
                                  << " ignored while retrieving block id "
                                  << update_in.block_id());
    }

    // Update metrics
    trc_read_timeouts_.Increment(read_timeouts_per_update_);
    trc_read_failures_.Increment(read_failures_per_update_);
    trc_read_ignored_.Increment(read_ignored_per_update_);
    read_timeouts_per_update_ = 0;
    read_failures_per_update_ = 0;
    read_ignored_per_update_ = 0;
    trc_updates_counter_.Increment();
    trc_queue_size_.Set(update_queue_->Size());

    update_queue_->Push(move(update));
    latest_verified_block_id_ = update_in.block_id();
    trc_last_verified_block_id_.Set(latest_verified_block_id_);

    // Cleanup before the next update

    // The main subscription loop should not be leaving any more than
    // (max_faulty_ + 1) subscription streams open before ending each iteration;
    // the fact it shouldn't may or not be used as a simplifying assumption in
    // the loop's implementation.
    for (size_t trsc = 0; trsc < trs_conns_.size(); ++trsc) {
      if (agreeing_subset_members[most_agreed_block].count(trsc) < 1) {
        LOG4CPLUS_DEBUG(logger_, "Close hash stream " << trsc << " after block "
                                                      << update_in.block_id());
        trs_conns_[trsc]->closeHashStream();
      }
    }
  }

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
  assert(trs_conns_.size() > 0);
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

  while (!has_verified_state && (data_server_index < trs_conns_.size())) {
    state.clear();
    block_id = 0;
    StateHashType expected_hash = 0;
    bool received_state_invalid = false;

    LOG4CPLUS_DEBUG(logger_, "Read state from " << data_server_index);
    ReadStateRequest request;
    request.set_key_prefix(key_prefix_bytes);
    trs_conns_[data_server_index]->openStateStream(request);

    Data response;
    while (!received_state_invalid &&
           trs_conns_[data_server_index]->readState(&response)) {
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
    if (!trs_conns_[data_server_index]->closeStateStream()) {
      LOG4CPLUS_WARN(logger_,
                     "While trying to fetch initial state for a subscription, "
                     "ThinRepliThinReplicaClient received response to "
                     "ReadStateRequest from ThinReplicaServer (server index: "
                         << data_server_index << ")");
      received_state_invalid = true;
    }

    LOG4CPLUS_DEBUG(logger_, "Got initial state from " << data_server_index);

    // We count the server we got the initial state data from as the first of
    // (max_faulty + 1) servers we need to find agreeing upon this state in
    // order to accept it.
    size_t agreeing_servers = 1;
    size_t hash_server_index = 0;
    while (!received_state_invalid && (hash_server_index < trs_conns_.size()) &&
           (agreeing_servers <= (size_t)max_faulty_)) {
      if (hash_server_index == data_server_index) {
        ++hash_server_index;
        continue;
      }
      LOG4CPLUS_DEBUG(logger_, "Read state hash from " << hash_server_index);
      Hash hash_response;
      ReadStateHashRequest hash_request;
      hash_request.set_block_id(block_id);
      hash_request.set_key_prefix(key_prefix_bytes);
      bool success = trs_conns_[hash_server_index]->readStateHash(
          hash_request, &hash_response);
      hash_server_index++;

      // Check whether the hash came back with an ok status, matches the Block
      // ID we requested, and matches the hash we computed locally of the data,
      // and only count it as agreeing if we complete all this verification.
      if (!success) {
        LOG4CPLUS_WARN(
            logger_,
            "Server "
                << hash_server_index - 1
                << " gave error response to ReadStateHash (requested Block ID: "
                << block_id << ").");
        continue;
      }
      if (hash_response.block_id() != block_id) {
        LOG4CPLUS_WARN(logger_,
                       "Server "
                           << hash_server_index - 1
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
                         << hash_server_index - 1
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
  assert(trs_conns_.size() > server_to_send_unsubscription_to);
  assert(trs_conns_[server_to_send_unsubscription_to]);
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
  assert(trs_conns_.size() > server_to_acknowledge_to);
  assert(trs_conns_[server_to_acknowledge_to]);
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
