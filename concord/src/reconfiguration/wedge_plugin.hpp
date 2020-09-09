// Copyright 2020 VMware, all rights reserved

#ifndef CONCORD_CONCORD_SRC_RECONFIGURATION_WEDGE_PLUGIN_HPP_
#define CONCORD_CONCORD_SRC_RECONFIGURATION_WEDGE_PLUGIN_HPP_

#include "IReconfigurationPlugin.hpp"
#include "bftengine/Replica.hpp"
namespace concord {
namespace reconfiguration {
class WedgePlugin : public IReconfigurationPlugin {
 public:
  com::vmware::concord::ReconfigurationSmRequest::CommandCase GetPluginId()
      const override {
    return com::vmware::concord::ReconfigurationSmRequest::CommandCase::
        kWedgeCmd;
  }

  bool Handle(
      const com::vmware::concord::ReconfigurationSmRequest& command,
      uint64_t sequence_num, bool readOnly, opentracing::Span& parent_span,
      com::vmware::concord::ConcordResponse& concord_response,
      com::vmware::concord::ConcordReplicaSpecificInfoResponse& rsi_response,
      bftEngine::ControlStateManager& control_state_manager,
      ConcordControlHandler& control_handlers) override {
    if (readOnly) {
      rsi_response.mutable_wedge_response()->set_stopped(
          control_handlers.isOnNOutOfNCheckpoint());
      return true;
    }
    auto& cmd = command.wedge_cmd();
    uint64_t seqNumToStopAt = sequence_num;
    if (cmd.has_seqnumtostopat()) seqNumToStopAt = cmd.seqnumtostopat();
    control_state_manager.setStopAtNextCheckpoint(seqNumToStopAt);
    auto reconfig_res = concord_response.mutable_reconfiguration_sm_response();
    LOG_INFO(logger_, "initiated wedge-stop command");
    reconfig_res->set_success(true);
    reconfig_res->set_additionaldata("set stop flag");
    return true;
  }

 private:
  logging::Logger logger_ = logging::getLogger("reconfiguration.wedgePlugin");
};
}  // namespace reconfiguration
}  // namespace concord
#endif  // CONCORD_CONCORD_SRC_RECONFIGURATION_WEDGE_PLUGIN_HPP_
