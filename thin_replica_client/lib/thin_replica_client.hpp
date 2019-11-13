// Copyright 2019 VMware, all rights reserved
 
#ifndef THIN_REPLICA_CLIENT_HPP_
#define THIN_REPLICA_CLIENT_HPP_

#include "thin_replica.grpc.pb.h"

#include <grpcpp/grpcpp.h>
#include <log4cplus/loggingmacros.h>

class ThinReplicaClient {
 private:
  log4cplus::Logger logger_;
  std::unique_ptr<com::vmware::concord::thin_replica::ThinReplica::Stub> stub_;

 public:
  ThinReplicaClient(std::shared_ptr<grpc::Channel> channel)
      : logger_(log4cplus::Logger::getInstance("com.vmware.thin_replica_client")),
      stub_(com::vmware::concord::thin_replica::ThinReplica::NewStub(channel)) {}

  grpc::Status ReadState();
};

#endif  // THIN_REPLICA_CLIENT_HPP_
