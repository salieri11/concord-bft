// Copyright 2018-2019 VMware, all rights reserved

#ifndef CONCORD_HLF_GRPC_SERVICE_H_
#define CONCORD_HLF_GRPC_SERVICE_H_

#include <grpc++/grpc++.h>
#include <grpc/grpc.h>
#include <grpcpp/server.h>
#include <grpcpp/server_builder.h>
#include <grpcpp/server_context.h>
#include <boost/filesystem.hpp>
#include <iostream>
#include "Logger.hpp"
#include "concord.pb.h"
#include "consensus/kvb_client.hpp"
#include "hlf/kvb_storage.hpp"
#include "hlf_services.grpc.pb.h"

namespace concord {
namespace hlf {

// Service to handle request from HLF peer
class HlfKeyValueServiceImpl final
    : public com::vmware::concord::hlf::services::HlfKeyValueService::Service {
 private:
  concord::hlf::HlfKvbStorage kvb_storage_;
  logging::Logger logger_;

 public:
  // restrict the max chaincode size to 1 MB
  const static long kMaxChaincodeBytesize = 1024 * 1024 * 1;

  HlfKeyValueServiceImpl(concord::hlf::HlfKvbStorage& kvb_storage)
      : kvb_storage_(kvb_storage),
        logger_(logging::getLogger("com.vmware.concord.hlf.grpc")) {}

  ~HlfKeyValueServiceImpl() {}

  // APIs for HLF peer
  grpc::Status GetState(
      grpc::ServerContext*,
      const com::vmware::concord::hlf::services::KvbMessage*,
      com::vmware::concord::hlf::services::KvbMessage*) override;

  grpc::Status PutState(
      grpc::ServerContext*,
      const com::vmware::concord::hlf::services::KvbMessage*,
      com::vmware::concord::hlf::services::KvbMessage*) override;
};

// Service to handle request from Concord client
class HlfChaincodeServiceImpl final
    : public com::vmware::concord::hlf::services::HlfChaincodeService::Service {
 private:
  concord::consensus::KVBClientPool& pool_;
  logging::Logger logger_;

 public:
  HlfChaincodeServiceImpl(concord::consensus::KVBClientPool& pool)
      : pool_(pool),
        logger_(logging::getLogger("com.vmware.concord.hlf.grpc")) {}

  ~HlfChaincodeServiceImpl() {}

  grpc::Status TriggerChaincode(
      grpc::ServerContext*, const com::vmware::concord::ConcordRequest*,
      com::vmware::concord::ConcordResponse*) override;

  // General functions to check tx's input
  bool IsValidManageOpt(const com::vmware::concord::HlfRequest&);

  bool IsValidInvokeOpt(const com::vmware::concord::HlfRequest&);

 private:
  bool VerifyChaincodeBytes(com::vmware::concord::HlfRequest&, std::string&);

  std::string GenerateRandomString(int len);
};

void RunHlfGrpcServer(concord::hlf::HlfKvbStorage& hlf_storage,
                      concord::consensus::KVBClientPool& kvb_client_pool,
                      std::string key_value_service_address,
                      std::string chaincode_service_address);

}  // namespace hlf
}  // namespace concord

#endif  // CONCORD_HLF_GRPC_SERVICE_H_
