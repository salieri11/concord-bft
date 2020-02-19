// Copyright 2019 VMware, all rights reserved

#ifndef CONCORD_THIN_REPLICA_GRPC_SERVICES_HPP_
#define CONCORD_THIN_REPLICA_GRPC_SERVICES_HPP_

#include <grpcpp/grpcpp.h>
#include <grpcpp/impl/codegen/sync_stream.h>

#include <memory>

#include "thin_replica.grpc.pb.h"
#include "thin_replica/thin_replica_impl.hpp"

namespace concord {
namespace thin_replica {

class ThinReplicaService final
    : public com::vmware::concord::thin_replica::ThinReplica::Service {
 public:
  explicit ThinReplicaService(std::unique_ptr<ThinReplicaImpl>&& impl)
      : impl_(std::move(impl)) {}

  grpc::Status ReadState(
      grpc::ServerContext* context,
      const com::vmware::concord::thin_replica::ReadStateRequest* request,
      grpc::ServerWriter<com::vmware::concord::thin_replica::Data>* stream)
      override;

  grpc::Status ReadStateHash(
      grpc::ServerContext* context,
      const com::vmware::concord::thin_replica::ReadStateHashRequest* request,
      com::vmware::concord::thin_replica::Hash* hash) override;

  grpc::Status AckUpdate(
      grpc::ServerContext* context,
      const com::vmware::concord::thin_replica::BlockId* block_id,
      google::protobuf::Empty* empty) override;

  grpc::Status SubscribeToUpdates(
      grpc::ServerContext* context,
      const com::vmware::concord::thin_replica::SubscriptionRequest* request,
      grpc::ServerWriter<com::vmware::concord::thin_replica::Data>* stream)
      override;

  grpc::Status SubscribeToUpdateHashes(
      grpc::ServerContext* context,
      const com::vmware::concord::thin_replica::SubscriptionRequest* request,
      grpc::ServerWriter<com::vmware::concord::thin_replica::Hash>* stream)
      override;

  grpc::Status Unsubscribe(grpc::ServerContext* context,
                           const google::protobuf::Empty* request,
                           google::protobuf::Empty* response) override;

 private:
  std::unique_ptr<ThinReplicaImpl> impl_;
};

}  // namespace thin_replica
}  // namespace concord

#endif  // CONCORD_THIN_REPLICA_GRPC_SERVICES_HPP_
