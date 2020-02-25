// Copyright 2020 VMware, all rights reserved
//
// This is the test execution engine commands handler.  It enables the testing
// of concord without an execution engine (e.g. ethereum, daml, hlf).

#ifndef CONCORD_TEE_COMMANDS_HANDLER_HPP_
#define CONCORD_TEE_COMMANDS_HANDLER_HPP_

#include <grpcpp/grpcpp.h>
#include <opentracing/span.h>

#include "concord.pb.h"
#include "consensus/concord_commands_handler.hpp"
#include "daml_commit.grpc.pb.h"
#include "hash_defs.h"
#include "sliver.hpp"
#include "thin_replica/subscription_buffer.hpp"

namespace concord {
namespace tee {

concordUtils::Sliver CreateSliver(char* content, const size_t size);
concordUtils::Sliver CreateSliver(const std::string& content);
concordUtils::Sliver CreateDamlKvbKey(const std::string& content);

class TeeCommandsHandler : public concord::consensus::ConcordCommandsHandler {
 private:
  log4cplus::Logger logger_;
  prometheus::Counter& write_ops_;
  prometheus::Counter& read_ops_;

 public:
  TeeCommandsHandler(
      const concord::config::ConcordConfiguration& config,
      const concord::config::ConcordConfiguration& node_config,
      const concord::storage::blockchain::ILocalKeyValueStorageReadOnly& ros,
      concord::storage::blockchain::IBlocksAppender& ba,
      concord::thin_replica::SubBufferList& subscriber_list,
      std::shared_ptr<concord::utils::PrometheusRegistry> prometheus_registry)
      : ConcordCommandsHandler(config, node_config, ros, ba, subscriber_list,
                               prometheus_registry),
        logger_(log4cplus::Logger::getInstance("com.vmware.concord.tee")),
        write_ops_{prometheus_registry->createCounter(
            command_handler_counters_,
            {{"layer", "TeeCommandsHandler"}, {"operation", "tee_writes"}})},
        read_ops_{prometheus_registry->createCounter(
            command_handler_counters_,
            {{"layer", "TeeCommandsHandler"}, {"operation", "tee_reads"}})} {}

  bool Execute(const com::vmware::concord::ConcordRequest& request,
               uint8_t flags, concord::time::TimeContract* time_contract,
               opentracing::Span& parent_span,
               com::vmware::concord::ConcordResponse& response) override;

  void WriteEmptyBlock(concord::time::TimeContract* time_contract) override;

 private:
  void RecordTransaction(const concordUtils::SetOfKeyValuePairs& updates,
                         com::vmware::concord::TeeResponse* tee_response);
};

}  // namespace tee
}  // namespace concord

#endif  // CONCORD_TEE_COMMANDS_HANDLER_HPP_
