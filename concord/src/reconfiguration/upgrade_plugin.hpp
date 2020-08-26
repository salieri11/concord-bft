// Copyright 2020 VMware, all rights reserved

#ifndef CONCORD_CONCORD_SRC_RECONFIGURATION_UPGRADE_PLUGIN_HPP_
#define CONCORD_CONCORD_SRC_RECONFIGURATION_UPGRADE_PLUGIN_HPP_

#include "IReconfigurationPlugin.hpp"

namespace concord {
namespace reconfiguration {

class UpgradePlugin : public IReconfigurationPlugin {
  void executeGetVersionCommand(
      const com::vmware::concord::ReconfigurationSmRequest_UpgradeCommand& cmd,
      com::vmware::concord::ConcordResponse& concord_response,
      com::vmware::concord::ConcordReplicaSpecificInfoResponse& rsi_response) {
    auto res = concord_response.mutable_reconfiguration_sm_response();
    res->set_success(true);
    res->set_additionaldata("Upgrading");
  }

  void executeValidateVersionCommand(
      const com::vmware::concord::ReconfigurationSmRequest_UpgradeCommand& cmd,
      com::vmware::concord::ConcordResponse& concord_response,
      com::vmware::concord::ConcordReplicaSpecificInfoResponse& rsi_response) {
    auto res = concord_response.mutable_reconfiguration_sm_response();
    res->set_success(true);
    res->set_additionaldata("Valid");
  }

  void executeUpgradeCommand(
      const com::vmware::concord::ReconfigurationSmRequest_UpgradeCommand& cmd,
      uint64_t sequence_num,
      bftEngine::ControlStateManager& control_state_manager,
      com::vmware::concord::ConcordResponse& concord_response,
      com::vmware::concord::ConcordReplicaSpecificInfoResponse& rsi_response) {
    auto res = concord_response.mutable_reconfiguration_sm_response();
    res->set_success(true);
    res->set_additionaldata("Upgraded");
  }

 public:
  explicit UpgradePlugin() {
    pluginId_ = com::vmware::concord::ReconfigurationSmRequest_PluginId_UPGRADE;
  }

  void Handle(
      const std::string& command, uint64_t sequence_num, bool readOnly,
      opentracing::Span& parent_span,
      com::vmware::concord::ConcordResponse& concord_response,
      com::vmware::concord::ConcordReplicaSpecificInfoResponse& rsi_response,
      bftEngine::ControlStateManager& control_state_manager,
      ConcordControlHandler& control_handlers) override {
    com::vmware::concord::ReconfigurationSmRequest_UpgradeCommand cmd;
    cmd.ParseFromString(command);
    switch (cmd.type()) {
      case com::vmware::concord::
          ReconfigurationSmRequest_UpgradeCommand_UpgradeType_GET_VERSION:
        executeGetVersionCommand(cmd, concord_response, rsi_response);
        break;
      case com::vmware::concord::
          ReconfigurationSmRequest_UpgradeCommand_UpgradeType_VALIDATE_VERSION:
        executeValidateVersionCommand(cmd, concord_response, rsi_response);
        break;
      case com::vmware::concord::
          ReconfigurationSmRequest_UpgradeCommand_UpgradeType_EXECUTE_UPGRADE:
        executeUpgradeCommand(cmd, sequence_num, control_state_manager,
                              concord_response, rsi_response);
        break;
      default:
        auto res = concord_response.mutable_reconfiguration_sm_response();
        res->set_success(false);
        res->set_additionaldata("Unknown upgrade command");
    }
  }
};

}  // namespace reconfiguration
}  // namespace concord
#endif  // CONCORD_CONCORD_SRC_RECONFIGURATION_UPGRADE_PLUGIN_HPP_
