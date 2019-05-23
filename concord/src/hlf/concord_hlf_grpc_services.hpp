// Copyright 2018-2019 VMware, all rights reserved

#ifndef CONCORD_CONSENSUS_HLF_GRPC_SERVICE_H_
#define CONCORD_CONSENSUS_HLF_GRPC_SERVICE_H_

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

class ConcordKeyValueServiceImpl final
    : public com::vmware::concord::hlf::services::ConcordKeyValueService::
          Service {
 private:
  concord::hlf::HlfHandler* hlf_handler_;
  concord::consensus::KVBClientPool& pool_;
  log4cplus::Logger logger_;

 public:
  ConcordKeyValueServiceImpl(HlfHandler* hlf_handler,
                             concord::consensus::KVBClientPool& pool)
      : hlf_handler_(hlf_handler),
        pool_(pool),
        logger_(log4cplus::Logger::getInstance("com.vmware.concord.hlf.grpc")) {
  }

  ~ConcordKeyValueServiceImpl() {}

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
  bool IsValidManageOpt(const com::vmware::concord::HlfRequest&);
  bool IsValidInvokeOpt(const com::vmware::concord::HlfRequest&);
};

void RunHlfServer(concord::hlf::HlfHandler*,
                  concord::consensus::KVBClientPool&);
}  // namespace hlf
}  // namespace concord

#endif  // CONCORD_CONSENSUS_HLF_GRPC_SERVICE_H_
