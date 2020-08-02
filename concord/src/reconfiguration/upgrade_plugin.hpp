// Copyright 2020 VMware, all rights reserved

#ifndef CONCORD_CONCORD_SRC_RECONFIGURATION_UPGRADE_PLUGIN_HPP_
#define CONCORD_CONCORD_SRC_RECONFIGURATION_UPGRADE_PLUGIN_HPP_

#include "IReconfigurationPlugin.hpp"

namespace concord {
namespace reconfiguration {

class UpgradePlugin : public IReconfigurationPlugin {
  PluginReply executeGetVersionCommand(
      const com::vmware::concord::ReconfigurationSmRequest_UpgradeCommand&
          cmd) {
    return {true, "Upgrading"};
  }

  PluginReply executeValidateVersionCommand(
      const com::vmware::concord::ReconfigurationSmRequest_UpgradeCommand&
          cmd) {
    return {true, "Valid"};
  }

  PluginReply executeUpgradeCommand(
      const com::vmware::concord::ReconfigurationSmRequest_UpgradeCommand& cmd,
      uint64_t sequence_num,
      bftEngine::ControlStateManager& control_state_manager) {
    // write in reserved pages the required checkpoint (probably the next one)
    // to stop at.s
    return {true, "Upgraded"};
  }

 public:
  explicit UpgradePlugin() {
    pluginId_ = com::vmware::concord::ReconfigurationSmRequest_PluginId_UPGRADE;
  }

  PluginReply Handle(const std::string& command, uint64_t sequence_num,
                     bool readOnly, opentracing::Span& parent_span,
                     bftEngine::ControlStateManager& control_state_manager,
                     ConcordControlHandler& control_handlers) override {
    com::vmware::concord::ReconfigurationSmRequest_UpgradeCommand cmd;
    cmd.ParseFromString(command);
    switch (cmd.type()) {
      case com::vmware::concord::
          ReconfigurationSmRequest_UpgradeCommand_UpgradeType_GET_VERSION:
        return executeGetVersionCommand(cmd);
      case com::vmware::concord::
          ReconfigurationSmRequest_UpgradeCommand_UpgradeType_VALIDATE_VERSION:
        return executeValidateVersionCommand(cmd);
      case com::vmware::concord::
          ReconfigurationSmRequest_UpgradeCommand_UpgradeType_EXECUTE_UPGRADE:
        return executeUpgradeCommand(cmd, sequence_num, control_state_manager);
    }
    return PluginReply{false, "Unknown_upgrade_command"};
  }
};

}  // namespace reconfiguration
}  // namespace concord
#endif  // CONCORD_CONCORD_SRC_RECONFIGURATION_UPGRADE_PLUGIN_HPP_
