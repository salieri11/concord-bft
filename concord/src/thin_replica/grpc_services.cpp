// Copyright 2019 VMware, all rights reserved

#include "grpc_services.hpp"

using grpc::ServerContext;
using grpc::ServerWriter;

using namespace std;

namespace concord {
namespace thin_replica {

grpc::Status ThinReplicaImpl::ReadState(
    ServerContext* context,
    const com::vmware::concord::thin_replica::ReadStateRequest* request,
    ServerWriter<com::vmware::concord::thin_replica::Data>* stream) {
  return grpc::Status(grpc::StatusCode::UNIMPLEMENTED, "ReadState");
}

grpc::Status ThinReplicaImpl::ReadStateHash(
    ServerContext* context,
    const com::vmware::concord::thin_replica::ReadStateHashRequest* request,
    com::vmware::concord::thin_replica::Hash* hash) {
  return grpc::Status(grpc::StatusCode::UNIMPLEMENTED, "ReadStateHash");
}

grpc::Status ThinReplicaImpl::AckCursor(
    ServerContext* context,
    const com::vmware::concord::thin_replica::Cursor* cursor,
    google::protobuf::Empty* empty) {
  return grpc::Status(grpc::StatusCode::UNIMPLEMENTED, "AckCursor");
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
