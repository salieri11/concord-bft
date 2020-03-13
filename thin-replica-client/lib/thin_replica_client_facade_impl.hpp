// Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential

#include "thin_replica.grpc.pb.h"
#include "thin_replica_client.hpp"
#include "thin_replica_client_facade.hpp"

class thin_replica_client::ThinReplicaClientFacade::Impl {
 public:
  std::shared_ptr<thin_replica_client::UpdateQueue> update_queue;
  std::unique_ptr<thin_replica_client::ThinReplicaClient> trc;
  log4cplus::Logger logger;
  Impl() : logger(log4cplus::Logger::getInstance("thin_replica.facade")) {
    update_queue.reset(new thin_replica_client::BasicUpdateQueue());
  }
};
