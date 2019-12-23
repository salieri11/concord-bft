// Copyright 2019 VMware, all rights reserved

#ifndef CONCORD_THIN_REPLICA_GRPC_SERVICES_HPP_
#define CONCORD_THIN_REPLICA_GRPC_SERVICES_HPP_

#include <grpcpp/grpcpp.h>
#include <log4cplus/loggingmacros.h>
#include <boost/circular_buffer.hpp>

#include "blockchain/db_interfaces.h"
#include "thin_replica.grpc.pb.h"
#include "thin_replica/subscription_buffer.hpp"

namespace concord {
namespace thin_replica {

class ThinReplicaImpl final
    : public com::vmware::concord::thin_replica::ThinReplica::Service {
 private:
  log4cplus::Logger logger_;
  const concord::storage::blockchain::ILocalKeyValueStorageReadOnly* rostorage_;
  SubBufferList& subscriber_list_;

 public:
  ThinReplicaImpl(
      const concord::storage::blockchain::ILocalKeyValueStorageReadOnly*
          rostorage,
      SubBufferList& subscriber_list)
      : logger_(
            log4cplus::Logger::getInstance("com.vmware.concord.thin_replica")),
        rostorage_(rostorage),
        subscriber_list_(subscriber_list) {}

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
  template <typename T>
  void SyncAndSend(concordUtils::BlockId start, const std::string& key_prefix,
                   std::shared_ptr<SubUpdateBuffer> live_updates,
                   grpc::ServerWriter<T>* stream);
};

}  // namespace thin_replica
}  // namespace concord

#endif  // CONCORD_THIN_REPLICA_GRPC_SERVICES_HPP_
