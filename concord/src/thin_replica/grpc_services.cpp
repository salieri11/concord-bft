// Copyright 2019 VMware, all rights reserved

#include "grpc_services.hpp"

using grpc::ServerContext;
using grpc::ServerWriter;

using com::vmware::concord::thin_replica::BlockId;
using com::vmware::concord::thin_replica::Data;
using com::vmware::concord::thin_replica::Hash;
using com::vmware::concord::thin_replica::KVPair;
using com::vmware::concord::thin_replica::ReadStateHashRequest;
using com::vmware::concord::thin_replica::ReadStateRequest;
using com::vmware::concord::thin_replica::SubscriptionRequest;

// NOTE: Make sure that all the logic is located in the implementation

namespace concord {
namespace thin_replica {

grpc::Status ThinReplicaService::ReadState(ServerContext* context,
                                           const ReadStateRequest* request,
                                           ServerWriter<Data>* stream) {
  return impl_->ReadState(context, request, stream);
}

grpc::Status ThinReplicaService::ReadStateHash(
    ServerContext* context, const ReadStateHashRequest* request, Hash* hash) {
  return impl_->ReadStateHash(context, request, hash);
}

grpc::Status ThinReplicaService::AckUpdate(ServerContext* context,
                                           const BlockId* block_id,
                                           google::protobuf::Empty* empty) {
  return impl_->AckUpdate(context, block_id, empty);
}

grpc::Status ThinReplicaService::SubscribeToUpdates(
    ServerContext* context, const SubscriptionRequest* request,
    ServerWriter<Data>* stream) {
  return impl_->SubscribeToUpdates<ServerContext, ServerWriter<Data>, Data>(
      context, request, stream);
}

grpc::Status ThinReplicaService::SubscribeToUpdateHashes(
    ServerContext* context, const SubscriptionRequest* request,
    ServerWriter<Hash>* stream) {
  return impl_->SubscribeToUpdates<ServerContext, ServerWriter<Hash>, Hash>(
      context, request, stream);
}

grpc::Status ThinReplicaService::Unsubscribe(
    ServerContext* context, const google::protobuf::Empty* request,
    google::protobuf::Empty* response) {
  return impl_->Unsubscribe(context, request, response);
}

}  // namespace thin_replica
}  // namespace concord
