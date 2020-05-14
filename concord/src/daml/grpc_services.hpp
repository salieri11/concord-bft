// Copyright 2019 VMware, all rights reserved

#ifndef CONCORD_DAML_GRPC_SERVICES_HPP_
#define CONCORD_DAML_GRPC_SERVICES_HPP_

#include <grpcpp/grpcpp.h>
#include <log4cplus/loggingmacros.h>

#include "consensus/kvb_client.hpp"
#include "daml_commit.grpc.pb.h"
#include "db_interfaces.h"

namespace concord {
namespace daml {

class CommitServiceImpl final
    : public com::digitalasset::kvbc::CommitService::Service {
 private:
  log4cplus::Logger logger_;
  concord::consensus::KVBClientPool& pool;
  std::mutex mutex;

  const bool pre_execute_all_requests;

  static bool IsPreExecuteAllRequestsEnabled(
      const config::ConcordConfiguration& config);

 public:
  explicit CommitServiceImpl(
      concord::consensus::KVBClientPool& p,
      const concord::config::ConcordConfiguration& config)
      : logger_(
            log4cplus::Logger::getInstance("com.vmware.concord.daml.commit")),
        pool(p),
        pre_execute_all_requests(IsPreExecuteAllRequestsEnabled(config)) {}

  grpc::Status CommitTransaction(
      grpc::ServerContext* context,
      const com::digitalasset::kvbc::CommitRequest* request,
      com::digitalasset::kvbc::CommitResponse* reply) override;
};

}  // namespace daml
}  // namespace concord

#endif  // CONCORD_DAML_GRPC_SERVICES_HPP_
