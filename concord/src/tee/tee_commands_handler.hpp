// Copyright 2020 VMware, all rights reserved
//
// This is the test execution engine commands handler.  It enables the testing
// of concord without an execution engine (e.g. ethereum, daml, hlf).

#ifndef CONCORD_TEE_COMMANDS_HANDLER_HPP_
#define CONCORD_TEE_COMMANDS_HANDLER_HPP_

#include <opentracing/span.h>

#include "concord.pb.h"
#include "consensus/concord_commands_handler.hpp"

namespace concord {
namespace tee {

class TeeCommandsHandler : public concord::consensus::ConcordCommandsHandler {
 private:
  log4cplus::Logger logger_;

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
        logger_(
            log4cplus::Logger::getInstance("com.vmware.concord.testhandler")) {
    LOG4CPLUS_INFO(logger_, "TeeCommandsHandler created");
  }

  bool Execute(const com::vmware::concord::ConcordRequest& request,
               uint8_t flags, concord::time::TimeContract* time_contract,
               opentracing::Span& parent_span,
               com::vmware::concord::ConcordResponse& response) override;
  void WriteEmptyBlock(concord::time::TimeContract* time_contract) override;
};

}  // namespace tee
}  // namespace concord

#endif  // CONCORD_TEE_COMMANDS_HANDLER_HPP_
