// Copyright 2019 VMware, all rights reserved

#include "thin_replica_client.hpp"

using com::vmware::concord::thin_replica::Data;
using com::vmware::concord::thin_replica::Hash;
using com::vmware::concord::thin_replica::KVPair;
using com::vmware::concord::thin_replica::ReadStateHashRequest;
using com::vmware::concord::thin_replica::ReadStateRequest;
using grpc::ClientContext;
using grpc::Status;
using std::hash;
using std::lock_guard;
using std::mutex;
using std::pair;
using std::runtime_error;
using std::string;
using std::stringstream;
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

ThinReplicaClient::StateHashType ThinReplicaClient::AppendToReadStateHash(
    ThinReplicaClient::StateHashType preceding_hash,
    const pair<string, string>& kvp) const {
  StateHashType pair_hash =
      hash<string>{}(string{kvp.first.data(), kvp.first.length()});
  pair_hash ^= hash<string>{}(string{kvp.second.data(), kvp.second.length()});
  preceding_hash ^= pair_hash;
  return preceding_hash;
}

ThinReplicaClient::~ThinReplicaClient() {}

void ThinReplicaClient::Subscribe(const string& key_prefix_bytes) {
  assert(server_stubs_.size() > 0);

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
    update->block_id = response.block_id();

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

  // TODO (Alex): Complete, revise, and clean up error handling for this
  //              function.
  // TODO (Alex): Add logic to setup actual subscription.
  LOG4CPLUS_WARN(logger_,
                 "concord::thin_replica_client::ThinReplicaClient::Subscribe "
                 "is incompletely implemented.");
}

void ThinReplicaClient::Subscribe(const string& key_prefix_bytes,
                                  uint64_t last_known_block_id) {
  // TODO (Alex): Provide real implementation.
  LOG4CPLUS_FATAL(logger_,
                  "concord::thin_replica_client::ThinReplicaClient::Subscribe "
                  "is unimplemented.");
}

void ThinReplicaClient::Unsubscribe() {
  // TODO (Alex): Implement.
  LOG4CPLUS_FATAL(logger_,
                  "concord::thin_replica_client::ThinReplicaClient::"
                  "Unsubscribe is unimplemented.");
}

void ThinReplicaClient::AcknowledgeBlockID(uint64_t block_id) {
  // TODO (Alex): Implement.
  LOG4CPLUS_FATAL(logger_,
                  "concord::thin_replica_client::ThinReplicaClient::"
                  "AcknowledgeBlockID is unimplemented.");
}
