// Copyright 2020 VMware, all rights reserved

#pragma once

#include "Logger.hpp"
#include "concord.pb.h"
#include "consensus/kvb_client.hpp"
#include "performance.grpc.pb.h"
#include "performance.pb.h"

namespace concord {
namespace performance {

class PerformanceServiceImp final
    : public com::vmware::concord::performance::PerformanceService::Service {
 public:
  explicit PerformanceServiceImp(concord::consensus::KVBClientPool& p)
      : logger_{concordlogger::Log::getLogger("concord.perf.service")},
        pool_{p} {}

  virtual ::grpc::Status PerfInit(
      ::grpc::ServerContext* context,
      const ::com::vmware::concord::performance::PerfInitRequest* request,
      ::com::vmware::concord::performance::PerfInitResponse* response) override;
  virtual ::grpc::Status PerfClear(
      ::grpc::ServerContext* context,
      const ::com::vmware::concord::performance::PerfCleanRequest* request,
      ::com::vmware::concord::performance::PerfCleanResponse* response)
      override;
  virtual ::grpc::Status PerfWrite(
      ::grpc::ServerContext* context,
      const ::com::vmware::concord::performance::PerfWriteRequest* request,
      ::com::vmware::concord::performance::PerfWriteResponse* response)
      override;

 private:
  concordlogger::Logger logger_;
  concord::consensus::KVBClientPool& pool_;
  ::grpc::Status Send(::com::vmware::concord::PerfRequest_PerfRequestType type,
                      std::string& inData, std::string& outData);
};

}  // namespace performance
}  // namespace concord
