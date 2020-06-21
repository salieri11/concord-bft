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
      const com::vmware::concord::ReconfigurationSmRequest_UpgradeCommand&
          cmd) {
    return {true, "Upgraded"};
  }

 public:
  explicit UpgradePlugin(
      com::vmware::concord::ReconfigurationSmRequest::PluginId pluginId)
      : IReconfigurationPlugin(pluginId) {}
  virtual PluginReply Handle(const std::string& command, bool readOnly,
                             opentracing::Span& parent_span) override {
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
        return executeUpgradeCommand(cmd);
    }
    return PluginReply{false, "Unknown_upgrade_command"};
  }
};

}  // namespace reconfiguration
}  // namespace concord
#endif  // CONCORD_CONCORD_SRC_RECONFIGURATION_UPGRADE_PLUGIN_HPP_
