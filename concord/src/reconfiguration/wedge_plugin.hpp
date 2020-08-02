// Copyright 2020 VMware, all rights reserved

#ifndef CONCORD_CONCORD_SRC_RECONFIGURATION_WEDGE_PLUGIN_HPP_
#define CONCORD_CONCORD_SRC_RECONFIGURATION_WEDGE_PLUGIN_HPP_

#include "IReconfigurationPlugin.hpp"
#include "bftengine/Replica.hpp"
namespace concord {
namespace reconfiguration {
class WedgePlugin : public IReconfigurationPlugin {
 public:
  explicit WedgePlugin() {
    pluginId_ = com::vmware::concord::ReconfigurationSmRequest_PluginId_WEDGE;
  }

  PluginReply Handle(const std::string& command, uint64_t sequence_num,
                     bool readOnly, opentracing::Span& parent_span,
                     bftEngine::ControlStateManager& control_state_manager,
                     ConcordControlHandler& control_handlers) override {
    if (readOnly) {
      bool hasStopped = control_handlers.isOnNOutOfNCheckpoint();
      return {true, hasStopped ? "true" : "false"};
    }

    com::vmware::concord::ReconfigurationSmRequest_WedgeCommand cmd;
    cmd.ParseFromString(command);
    uint64_t seqNumToStopAt = sequence_num;
    if (cmd.has_seqnumtostopat()) seqNumToStopAt = cmd.seqnumtostopat();
    control_state_manager.setStopAtNextCheckpoint(seqNumToStopAt);
    return {true, std::string()};
  }
};
}  // namespace reconfiguration
}  // namespace concord
#endif  // CONCORD_CONCORD_SRC_RECONFIGURATION_WEDGE_PLUGIN_HPP_
