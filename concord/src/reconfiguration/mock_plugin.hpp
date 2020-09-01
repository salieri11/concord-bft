// Copyright 2020 VMware, all rights reserved

#ifndef VMWATHENA_BLOCKCHAIN_CONCORD_SRC_RECONFIGURATION_MOCK_PLUGIN_HPP_
#define VMWATHENA_BLOCKCHAIN_CONCORD_SRC_RECONFIGURATION_MOCK_PLUGIN_HPP_

#include "IReconfigurationPlugin.hpp"
#include "Logger.hpp"
namespace concord {
namespace reconfiguration {

class MockPlugin : public IReconfigurationPlugin {
 public:
  MockPlugin() {
    pluginId_ = com::vmware::concord::ReconfigurationSmRequest_PluginId_MOCK;
  }

  void Handle(
      const std::string& command, uint64_t sequence_num, bool readOnly,
      opentracing::Span& parent_span,
      com::vmware::concord::ConcordResponse& concord_response,
      com::vmware::concord::ConcordReplicaSpecificInfoResponse& rsi_response,
      bftEngine::ControlStateManager& control_state_manager,
      ConcordControlHandler& control_handlers) override {
    LOG_INFO(logger_, "msg from operator: " << command);
    auto res = concord_response.mutable_reconfiguration_sm_response();
    res->set_additionaldata("Hello From Reconfiguration Mock plugin");
    res->set_success(true);
  }

 private:
  logging::Logger logger_ = logging::getLogger("reconfiguration.mockPlugin");
};
}  // namespace reconfiguration
}  // namespace concord
#endif  // VMWATHENA_BLOCKCHAIN_CONCORD_SRC_RECONFIGURATION_MOCK_PLUGIN_HPP_
