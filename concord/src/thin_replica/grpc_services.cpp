// Copyright 2019 VMware, all rights reserved

#include "grpc_services.hpp"

#include <atomic>
#include <boost/lockfree/spsc_queue.hpp>
#include <chrono>
#include <sstream>

#include "hash_defs.h"
#include "kv_types.hpp"
#include "storage/kvb_app_filter.h"

using namespace std::chrono_literals;

using boost::lockfree::spsc_queue;
using grpc::ServerContext;
using grpc::ServerWriter;

using com::vmware::concord::thin_replica::Data;
using com::vmware::concord::thin_replica::Hash;
using com::vmware::concord::thin_replica::KVPair;
using concord::storage::KvbAppFilter;
using concordUtils::BlockId;
using concordUtils::Key;
using concordUtils::KeyValuePair;
using concordUtils::SetOfKeyValuePairs;
using concordUtils::Status;
using concordUtils::Value;

namespace concord {
namespace thin_replica {

grpc::Status ThinReplicaImpl::ReadState(
    ServerContext* context,
    const com::vmware::concord::thin_replica::ReadStateRequest* request,
    ServerWriter<com::vmware::concord::thin_replica::Data>* stream) {
  spsc_queue<KeyValuePair*> queue{10};
  BlockId current_block_id = rostorage_->getLastBlock();
  KvbAppFilter kvb_filter(rostorage_, KvbAppFilter::kDaml);
  std::atomic_bool stop_filter = false;

  LOG4CPLUS_INFO(logger_, "ReadState");

  // Create a future which pushes kv-pairs into the given queue
  std::string key_prefix = request->key_prefix();
  auto filter = std::async(std::launch::async, &KvbAppFilter::ReadState,
                           &kvb_filter, current_block_id, std::ref(key_prefix),
                           std::ref(queue), std::ref(stop_filter));

  KeyValuePair* kvp;
  // Read from the queue until the future returns and the queue is empty
  while (filter.wait_for(0s) != std::future_status::ready || !queue.empty()) {
    while (queue.pop(kvp)) {
      Data data;
      data.set_block_id(current_block_id);

      KVPair* kvp_out = data.add_data();
      kvp_out->set_key(kvp->first.data(), kvp->first.length());
      kvp_out->set_value(kvp->second.data(), kvp->second.length());
      delete kvp;

      if (!stop_filter && !stream->Write(data)) {
        LOG4CPLUS_ERROR(logger_, "ReadState gRPC stream has been closed");
        stop_filter = true;
      }
    }
  }

  Status status = filter.get();
  if (!status.isOK()) {
    return grpc::Status(grpc::StatusCode::UNKNOWN, status.toString());
  }

  return grpc::Status::OK;
}

grpc::Status ThinReplicaImpl::ReadStateHash(
    ServerContext* context,
    const com::vmware::concord::thin_replica::ReadStateHashRequest* request,
    com::vmware::concord::thin_replica::Hash* hash) {
  KvbAppFilter kvb_filter(rostorage_, KvbAppFilter::kDaml);
  concord::storage::KvbStateHash kvb_hash;

  LOG4CPLUS_INFO(logger_, "ReadStateHash");

  BlockId block_id = request->block_id();
  std::string key_prefix = request->key_prefix();
  Status status = kvb_filter.ReadStateHash(block_id, key_prefix, kvb_hash);
  if (!status.isOK()) {
    LOG4CPLUS_ERROR(logger_,
                    "Reading StateHash for block " << block_id << " failed");
    return grpc::Status(grpc::StatusCode::UNKNOWN, status.toString());
  }

  hash->set_block_id(block_id);
  hash->set_hash(&kvb_hash, sizeof kvb_hash);

  return grpc::Status::OK;
}

grpc::Status ThinReplicaImpl::AckUpdate(
    ServerContext* context,
    const com::vmware::concord::thin_replica::BlockId* block_id,
    google::protobuf::Empty* empty) {
  return grpc::Status(grpc::StatusCode::UNIMPLEMENTED, "AckUpdate");
}

grpc::Status ThinReplicaImpl::SubscribeToUpdates(
    ServerContext* context,
    const com::vmware::concord::thin_replica::SubscriptionRequest* request,
    ServerWriter<com::vmware::concord::thin_replica::Data>* stream) {
  bool close_write_stream = false;
  std::pair<BlockId, SetOfKeyValuePairs> update;

  auto updates = std::make_shared<SubUpdateBuffer>(100);
  subscriber_list_.AddBuffer(updates);

  // TODO: Read from KVB, filter, and sync with updates in the ring buffer
  // For now: Read from the ring buffer and send to stream
  while (!close_write_stream) {
    SubUpdate update = updates->Pop();

    Data data;
    data.set_block_id(update.first);

    SetOfKeyValuePairs kv_updates(update.second);
    for (const auto& [key, value] : kv_updates) {
      KVPair* kvp_out = data.add_data();
      kvp_out->set_key(key.data(), key.length());
      kvp_out->set_value(value.data(), value.length());
    }

    if (stream->Write(data)) {
      continue;
    }

    LOG4CPLUS_INFO(logger_, "SubscibeToUpdates stream has been closed.");

    // Remove from the subscriber list so that the cmds handler stops pushing
    // new updates
    subscriber_list_.RemoveBuffer(updates);
    close_write_stream = true;
  }

  // Clear the update list
  while (!updates->Empty()) {
    updates->Pop();
  }

  return grpc::Status::OK;
}

grpc::Status ThinReplicaImpl::SubscribeToUpdateHashes(
    ServerContext* context,
    const com::vmware::concord::thin_replica::SubscriptionRequest* request,
    ServerWriter<com::vmware::concord::thin_replica::Hash>* stream) {
  bool close_write_stream = false;
  std::pair<BlockId, SetOfKeyValuePairs> update;

  auto updates = std::make_shared<SubUpdateBuffer>(100);
  subscriber_list_.AddBuffer(updates);

  // TODO: Read from KVB, filter, and sync with updates in the ring buffer
  // For now: Read from the ring buffer, compute hash, and send to stream
  while (!close_write_stream) {
    SubUpdate update = updates->Pop();
    SetOfKeyValuePairs kv_updates(update.second);

    size_t hash_out = 0;

    // TODO: Same implementation in two places (see kvb_app_filter)
    for (const auto& [key, value] : kv_updates) {
      // (key1 XOR value1) XOR (key2 XOR value2) ...
      auto key_hash = std::hash<string>{}(string{key.data(), key.length()});
      key_hash ^= std::hash<string>{}(string{value.data(), value.length()});
      hash_out ^= key_hash;
    }

    com::vmware::concord::thin_replica::Hash hash;
    hash.set_block_id(update.first);
    hash.set_hash(&hash_out, sizeof hash_out);

    if (stream->Write(hash)) {
      continue;
    }

    LOG4CPLUS_INFO(logger_, "SubscibeToUpdateHashes stream has been closed.");

    // Remove from the subscriber list so that the cmds handler stops pushing
    // new updates
    subscriber_list_.RemoveBuffer(updates);
    close_write_stream = true;
  }

  // Clear the update list
  while (!updates->Empty()) {
    updates->Pop();
  }

  return grpc::Status::OK;
}

grpc::Status ThinReplicaImpl::Unsubscribe(
    ServerContext* context, const google::protobuf::Empty* request,
    google::protobuf::Empty* response) {
  return grpc::Status(grpc::StatusCode::UNIMPLEMENTED, "Unsubscribe");
}

}  // namespace thin_replica
}  // namespace concord
