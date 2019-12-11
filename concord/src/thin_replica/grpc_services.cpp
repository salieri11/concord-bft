// Copyright 2019 VMware, all rights reserved

#include "grpc_services.hpp"

#include <atomic>
#include <boost/lockfree/spsc_queue.hpp>
#include <chrono>
#include <sstream>

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
  auto filter =
      std::async(std::launch::async, &KvbAppFilter::ReadState, &kvb_filter,
                 current_block_id, std::ref(queue), std::ref(stop_filter));

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
  Status status = kvb_filter.ReadStateHash(block_id, kvb_hash);
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
  return grpc::Status(grpc::StatusCode::UNIMPLEMENTED, "SubscribeToUpdates");
}

grpc::Status ThinReplicaImpl::SubscribeToUpdateHashes(
    ServerContext* context,
    const com::vmware::concord::thin_replica::SubscriptionRequest* request,
    ServerWriter<com::vmware::concord::thin_replica::Hash>* stream) {
  return grpc::Status(grpc::StatusCode::UNIMPLEMENTED,
                      "SubscribeToUpdateHashes");
}

grpc::Status ThinReplicaImpl::Unsubscribe(
    ServerContext* context, const google::protobuf::Empty* request,
    google::protobuf::Empty* response) {
  return grpc::Status(grpc::StatusCode::UNIMPLEMENTED, "Unsubscribe");
}

}  // namespace thin_replica
}  // namespace concord
