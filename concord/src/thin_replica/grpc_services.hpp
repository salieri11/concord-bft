// Copyright 2019 VMware, all rights reserved

#ifndef CONCORD_THIN_REPLICA_GRPC_SERVICES_HPP_
#define CONCORD_THIN_REPLICA_GRPC_SERVICES_HPP_

#include <grpcpp/grpcpp.h>
#include <log4cplus/loggingmacros.h>

#include "blockchain/db_interfaces.h"
#include "thin_replica.grpc.pb.h"

namespace concord {
namespace thin_replica {

class ThinReplicaImpl final
    : public com::vmware::concord::thin_replica::ThinReplica::Service {
 private:
  log4cplus::Logger logger_;
  const concord::storage::blockchain::ILocalKeyValueStorageReadOnly* rostorage_;

 public:
  ThinReplicaImpl(
      const concord::storage::blockchain::ILocalKeyValueStorageReadOnly*
          rostorage)
      : logger_(
            log4cplus::Logger::getInstance("com.vmware.concord.thin_replica")),
        rostorage_(rostorage) {}

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
};

}  // namespace thin_replica
}  // namespace concord

#endif  // CONCORD_THIN_REPLICA_GRPC_SERVICES_HPP_
