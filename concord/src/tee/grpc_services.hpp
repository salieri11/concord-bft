// Copyright 2020 VMware, all rights reserved
//
// This is the service which accompanies the test execution engine commands
// handler.

#ifndef CONCORD_TEE_GRPC_SERVICES_HPP_
#define CONCORD_TEE_GRPC_SERVICES_HPP_

#include <grpcpp/grpcpp.h>
#include "Logger.hpp"

#include "concord.pb.h"
#include "consensus/kvb_client.hpp"
#include "tee.grpc.pb.h"

using com::vmware::concord::TeeRequest;
using com::vmware::concord::TeeResponse;
using com::vmware::concord::tee::KVData;
using com::vmware::concord::tee::RawSkvbcRequest;
using com::vmware::concord::tee::RawSkvbcResponse;
using com::vmware::concord::tee::TestInput;
using com::vmware::concord::tee::TestOutput;

namespace concord {
namespace tee {

class TeeServiceImpl final
    : public com::vmware::concord::tee::TeeService::Service {
 private:
  logging::Logger logger_;
  concord::consensus::KVBClientPool& pool_;

 public:
  explicit TeeServiceImpl(concord::consensus::KVBClientPool& p)
      : logger_(logging::getLogger("com.vmware.concord.tee")), pool_(p) {}

  grpc::Status RunTest(grpc::ServerContext* context, const TestInput* request,
                       TestOutput* response) override;

  grpc::Status SkvbcRead(
      ::grpc::ServerContext* context,
      const ::com::vmware::concord::tee::RawSkvbcRequest* request,
      ::com::vmware::concord::tee::RawSkvbcResponse* response) override;

  grpc::Status SkvbcWrite(
      ::grpc::ServerContext* context,
      const ::com::vmware::concord::tee::RawSkvbcRequest* request,
      ::com::vmware::concord::tee::RawSkvbcResponse* response) override;

  grpc::Status WriteBlock(::grpc::ServerContext* context, const KVData*,
                          TestOutput*) override;
};

}  // namespace tee
}  // namespace concord

#endif  // CONCORD_TEE_GRPC_SERVICES_HPP_
