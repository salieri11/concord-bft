// Copyright 2019-2020 VMware, all rights reserved

#include "thin_replica_client.hpp"

using com::vmware::concord::thin_replica::Data;
using com::vmware::concord::thin_replica::Hash;
using com::vmware::concord::thin_replica::KVPair;
using com::vmware::concord::thin_replica::ReadStateHashRequest;
using com::vmware::concord::thin_replica::ReadStateRequest;
using com::vmware::concord::thin_replica::SubscriptionRequest;
using grpc::ClientContext;
using grpc::ClientReader;
using grpc::Status;
using std::hash;
using std::lock_guard;
using std::mutex;
using std::pair;
using std::runtime_error;
using std::string;
using std::stringstream;
using std::thread;
using std::to_string;
using std::unique_lock;
using std::unique_ptr;
using std::vector;
using thin_replica_client::BasicUpdateQueue;
using thin_replica_client::ThinReplicaClient;
using thin_replica_client::Update;

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

void ThinReplicaClient::ReceiveUpdates() {
  assert(!stop_subscription_thread_);
  assert(!subscription_data_stream_);
  assert(subscription_hash_streams_.size() == 0);
  assert(server_stubs_.size() > 0);

  SubscriptionRequest sub_request;
  sub_data_context_.reset(new ClientContext());
  sub_request.set_block_id(latest_verified_block_id_);
  sub_request.set_key_prefix(key_prefix_);

  assert(server_stubs_[0]);
  subscription_data_stream_ = server_stubs_[0]->SubscribeToUpdates(
      sub_data_context_.get(), sub_request);
  subscription_hash_streams_ =
      vector<unique_ptr<ClientReader<Hash>>>(server_stubs_.size());

  sub_hash_contexts_ = vector<unique_ptr<ClientContext>>(server_stubs_.size());

  Data update_in;
  while (!stop_subscription_thread_ &&
         (subscription_data_stream_->Read(&update_in))) {
    if (update_in.block_id() < latest_verified_block_id_) {
      LOG4CPLUS_ERROR(logger_,
                      "ThinReplicaClient subscription received an update with "
                      "a decreasing Block ID.");
      stop_subscription_thread_ = true;
      continue;
    }

    unique_ptr<Update> update(new Update());
    UpdateHashType expected_hash =
        hash<string>{}(to_string(update_in.block_id()));

    update->block_id = update_in.block_id();
    for (const auto& kvp_in : update_in.data()) {
      pair<string, string> kvp_out = make_pair(kvp_in.key(), kvp_in.value());
      update->kv_pairs.push_back(kvp_out);
      expected_hash = AppendToSubscribeToUpdatesHash(expected_hash, kvp_out);
    }

    size_t agreeing_hashes = 0;
    vector<bool> servers_checked(server_stubs_.size(), false);

    // Search for hashes agreeing with the update we have.
    // First, we check any hash subscriptions that we already have open (closing
    // any that are found to be in disagreement). After that, if we still do not
    // have enough agreeing hashes, then we try opening hash subscriptions to
    // other replicas. We do both of these searches with a single loop in order
    // to share certain logic that would otherwise be duplicated between
    // separate loops for each of these steps.
    size_t i = 1;
    bool has_tried_all_open_subscriptions = false;
    while (!stop_subscription_thread_ && (agreeing_hashes < max_faulty_) &&
           (!has_tried_all_open_subscriptions || (i < server_stubs_.size()))) {
      if (has_tried_all_open_subscriptions && !servers_checked[i]) {
        assert(server_stubs_[i]);
        sub_hash_contexts_[i].reset(new ClientContext());
        sub_request.set_block_id(latest_verified_block_id_);
        subscription_hash_streams_[i] =
            server_stubs_[i]->SubscribeToUpdateHashes(
                sub_hash_contexts_[i].get(), sub_request);
      }

      // Logic to get a hash from the selected hash subscription and compare it
      // to the update (note this is the same whether this is an existing or a
      // new subscription).
      if (subscription_hash_streams_[i]) {
        Hash hash;

        if (subscription_hash_streams_[i]->Read(&hash)) {
          if ((hash.block_id() == update->block_id) &&
              (hash.hash() == string(reinterpret_cast<char*>(&expected_hash),
                                     sizeof(StateHashType)))) {
            ++agreeing_hashes;
          } else {
            LOG4CPLUS_WARN(logger_,
                           "A hash subscription stream (from server "
                               << to_string(i)
                               << ") was found to be in disagreement with the "
                                  "active data subscription.");
            sub_hash_contexts_[i]->TryCancel();
            subscription_hash_streams_[i]->Finish();
            subscription_hash_streams_[i].reset();
          }

        } else {
          // Case where a hash subscription stream ended unexpectedly.
          Status status = subscription_hash_streams_[i]->Finish();
          if (!status.ok()) {
            LOG4CPLUS_WARN(logger_, "A hash subscription stream (from server "
                                        << to_string(i)
                                        << ") to a Thin Replica Server was "
                                           "closed unexpectedly (error code: "
                                        << status.error_code()
                                        << ", error message:\""
                                        << status.error_message() << "\")");
          } else {
            LOG4CPLUS_WARN(
                logger_,
                "A hash subscription stream (from server "
                    << to_string(i)
                    << ") to a Thin Replica Server finished unexpectedly, but "
                       "without reporting an error.");
          }
          subscription_hash_streams_[i].reset();
        }
        servers_checked[i] = true;
      }

      ++i;
      if (!has_tried_all_open_subscriptions && i >= server_stubs_.size()) {
        i = 1;
        has_tried_all_open_subscriptions = true;
      }
    }

    // We need to find at least (max_faulty_ + 1) Thin Replica Servers in
    // agreement to consider an update verified; agreeing_hashes only needs to
    // be at least max_faulty_, however, as the server we received the update
    // data itself (and computed the expected hash) from is also counted as part
    // of the agreeing set.
    if (agreeing_hashes >= max_faulty_) {
      latest_verified_block_id_ = update->block_id;
      update_queue_->Push(move(update));
    } else {
      LOG4CPLUS_ERROR(logger_,
                      "Could not find enough hashes in agreement with a "
                      "received update; ending subscription.");
      stop_subscription_thread_ = true;
    }
  }

  if (stop_subscription_thread_) {
    sub_data_context_->TryCancel();
  }

  Status status = subscription_data_stream_->Finish();

  if (!stop_subscription_thread_) {
    if (status.ok()) {
      LOG4CPLUS_WARN(logger_,
                     "The data subscription stream ended unexpectedly without "
                     "a reported error.");
    } else {
      LOG4CPLUS_WARN(
          logger_,
          "The data subscription stream ended unexpectedly(error code: "
              << status.error_code() << ", error message: \""
              << status.error_message() << "\").");
    }
  }
  subscription_data_stream_.reset();

  for (size_t i = 0; i < subscription_hash_streams_.size(); ++i) {
    if (subscription_hash_streams_[i]) {
      sub_hash_contexts_[i]->TryCancel();
      subscription_hash_streams_[i]->Finish();
      subscription_hash_streams_[i].reset();
    }
  }

  stop_subscription_thread_ = true;

  // TODO (Alex): Complete, revise, and clean up error handling for this
  //              function.
}

ThinReplicaClient::StateHashType ThinReplicaClient::AppendToReadStateHash(
    ThinReplicaClient::StateHashType preceding_hash,
    const pair<string, string>& kvp) const {
  StateHashType pair_hash =
      hash<string>{}(string{kvp.first.data(), kvp.first.length()});
  pair_hash ^= hash<string>{}(string{kvp.second.data(), kvp.second.length()});
  preceding_hash ^= pair_hash;
  return preceding_hash;
}

ThinReplicaClient::UpdateHashType
ThinReplicaClient::AppendToSubscribeToUpdatesHash(
    ThinReplicaClient::UpdateHashType preceding_hash,
    const pair<string, string>& kvp) const {
  // The hash implementation for streaming update hashes currently happens to be
  // the same as the implementation for initial state hashes, so we re-use its
  // implementation to minimize code duplication; however, the equivalence of
  // the hash functions remains subject to change at this time, so
  // AppendToReadStateHash should not be called directly as a substitute for
  // AppendToSubscribeToUpdateHash.
  return AppendToReadStateHash(preceding_hash, kvp);
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
  // TODO (Alex): Stop, cleanup, and destroy any existing subscription thread.

  // XXX: The following implementation does not achieve Subscribe's specified
  //      interface and behavior (see the comments with Subscribe's declaration
  //      in the Thin Replica Client Library header file for documentation of
  //      that interface); this implementation is intended to establish minimal
  //      end-to-end connectivity with a non-faulty Thin Replica Server in order
  //      to preserve the general behavior of the example Thin Replica Client
  //      application (which at this time just connects to a server and checks
  //      the status returned for a ReadStateRequest).

  ReadStateRequest request;
  ClientContext read_context;

  std::list<unique_ptr<Update>> state;
  StateHashType expected_hash = 0;
  uint64_t block_id = 0;

  request.set_key_prefix(key_prefix_bytes);
  auto stream = server_stubs_[0]->ReadState(&read_context, request);
  Data response;
  while (stream->Read(&response)) {
    if ((state.size() > 0) && (response.block_id() < block_id)) {
      throw runtime_error(
          "ThinReplicaClient subscription failed: data from ReadState included "
          "an update with a decreasing Block ID.");
    }
    block_id = response.block_id();
    unique_ptr<Update> update(new Update());
    update->block_id = block_id;
    expected_hash ^= hash<string>{}(to_string(block_id));

    for (int i = 0; i < response.data_size(); ++i) {
      KVPair kvp = response.data(i);
      update->kv_pairs.push_back(pair<string, string>());
      size_t update_tail_index = update->kv_pairs.size() - 1;
      update->kv_pairs[update_tail_index].first = kvp.key();
      update->kv_pairs[update_tail_index].second = kvp.value();
      expected_hash = AppendToReadStateHash(
          expected_hash, update->kv_pairs[update_tail_index]);
    }
    state.push_back(move(update));
  }

  Status status = stream->Finish();
  if (!status.ok()) {
    stringstream error_message;
    error_message << "ThinReplicaClient subscription failed: response to "
                     "ReadStateRequest from ThinReplicaServer gave a failure "
                     "status (error code: "
                  << status.error_code() << ", error message: \""
                  << status.error_message() << "\").";
    throw runtime_error(error_message.str());
  }

  size_t agreeing_servers = 1;
  size_t i = 1;
  while ((i < server_stubs_.size()) &&
         (agreeing_servers <= (size_t)max_faulty_)) {
    ReadStateHashRequest hash_request;
    hash_request.set_block_id(block_id);
    hash_request.set_key_prefix(key_prefix_bytes);
    Hash hash_response;
    ClientContext hash_context;
    status = server_stubs_[i]->ReadStateHash(&hash_context, hash_request,
                                             &hash_response);
    ++i;

    // Check whether the hash came back with an ok status, matches the Block ID
    // we requested, and matches the hash we computed locally of the data, and
    // only count it as agreeing if we complete all this verification.
    if (!status.ok()) {
      LOG4CPLUS_WARN(
          logger_,
          "Server "
              << i
              << " gave error response to ReadStateHash (requested Block ID: "
              << block_id << ").");
      continue;
    }
    if (hash_response.block_id() != block_id) {
      LOG4CPLUS_WARN(
          logger_, "Server " << i
                             << " gave response to ReadStateHash disagreeing "
                                "with requested Block ID (requested Block ID: "
                             << block_id << ", response contained Block ID: "
                             << hash_response.block_id() << ").");
      continue;
    }
    if (hash_response.hash() != string(reinterpret_cast<char*>(&expected_hash),
                                       sizeof(StateHashType))) {
      LOG4CPLUS_WARN(logger_,
                     "Server "
                         << i
                         << " gave response to ReadStateHash in disagreement "
                            "with the expected hash value (requested Block ID: "
                         << block_id << ").");
      continue;
    }

    ++agreeing_servers;
  }
  if (agreeing_servers <= (size_t)max_faulty_) {
    throw runtime_error("ThinReplicaClient subsctription failed: only " +
                        to_string(agreeing_servers) +
                        " server(s) found in agreement about fetched state "
                        "(including the source of this state), but at least " +
                        to_string((uint32_t)max_faulty_ + 1) +
                        " are required to confirm consensus.");
  }

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
                                  uint64_t last_known_block_id) {
  // TODO (Alex): Implement.
  LOG4CPLUS_FATAL(logger_,
                  "thin_replica_client::ThinReplicaClient::Subscribe(const "
                  "string&, uint64_t) is unimplemented.");
}

void ThinReplicaClient::Unsubscribe() {
  // TODO (Alex): Implement.
  LOG4CPLUS_FATAL(
      logger_,
      "thin_replica_client::ThinReplicaClient::Unsubscribe is unimplemented.");
}

void ThinReplicaClient::AcknowledgeBlockID(uint64_t block_id) {
  // TODO (Alex): Implement.
  LOG4CPLUS_FATAL(logger_,
                  "thin_replica_client::ThinReplicaClient::AcknowledgeBlockID "
                  "is unimplemented.");
}
