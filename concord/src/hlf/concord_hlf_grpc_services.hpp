// Copyright 2018-2019 VMware, all rights reserved

#include <grpc++/grpc++.h>
#include <grpc/grpc.h>
#include <grpcpp/server.h>
#include <grpcpp/server_builder.h>
#include <grpcpp/server_context.h>
#include <log4cplus/loggingmacros.h>
#include <iostream>
#include "concord.pb.h"
#include "concord_hlf_handler.hpp"
#include "consensus/kvb_client.hpp"
#include "hlf_services.grpc.pb.h"

namespace concord {
namespace hlf {

class GrpcServiceImpl final
    : public com::vmware::concord::hlf::services::GrpcService::Service {
 private:
  concord::hlf::HlfHandler* _hlfHandler;
  concord::consensus::KVBClientPool& _pool;
  log4cplus::Logger _logger;

 public:
  GrpcServiceImpl(HlfHandler* hlfHandler,
                  concord::consensus::KVBClientPool& pool)
      : _hlfHandler(hlfHandler),
        _pool(pool),
        _logger(log4cplus::Logger::getInstance("com.vmware.concord.hlf.grpc")) {
  }

  ~GrpcServiceImpl() {}

  // APIs for HLF peer
  grpc::Status GetState(
      grpc::ServerContext*,
      const com::vmware::concord::hlf::services::KvbMessage*,
      com::vmware::concord::hlf::services::KvbMessage*) override;
  grpc::Status PutState(
      grpc::ServerContext*,
      const com::vmware::concord::hlf::services::KvbMessage*,
      com::vmware::concord::hlf::services::KvbMessage*) override;
  grpc::Status WriteBlock(
      grpc::ServerContext*,
      const com::vmware::concord::hlf::services::KvbMessage*,
      com::vmware::concord::hlf::services::KvbMessage*) override;

  // API for Concord client
  grpc::Status TriggerChaincode(
      grpc::ServerContext*, const com::vmware::concord::ConcordRequest*,
      com::vmware::concord::ConcordResponse*) override;

  // General functions to check tx's input
  bool isValidManageOpt(const com::vmware::concord::HlfRequest&);
  bool isValidInvokeOpt(const com::vmware::concord::HlfRequest&);
};

void RunHlfServer(concord::hlf::HlfHandler*,
                  concord::consensus::KVBClientPool&);
}  // namespace hlf
}  // namespace concord
