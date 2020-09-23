// Copyright 2020 VMware, all rights reserved

#include "reconfiguration_sm_open.hpp"

#include "Logger.hpp"

using com::vmware::concord::ConcordReplicaSpecificInfoResponse;
using com::vmware::concord::ConcordResponse;
using com::vmware::concord::ReconfigurationSmRequest;
using com::vmware::concord::ReconfigurationSmResponse;

using concord::messages::DownloadCommand;
using concord::messages::GetVersionCommand;
using concord::messages::UpgradeCommand;
using concord::messages::WedgeCommand;

namespace concord::reconfiguration {

ReconfigurationSMOpen::ReconfigurationSMOpen()
    : logger_(logging::getLogger("concord.reconfiguration.handler")) {}

bool ReconfigurationSMOpen::handle(
    const WedgeCommand& cmd, uint64_t sequence_num, bool readOnly,
    ConcordReplicaSpecificInfoResponse& rsi_response,
    opentracing::Span& parent_span) {
  if (readOnly) {
    rsi_response.mutable_wedge_response()->set_stopped(
        control_handlers_->isOnNOutOfNCheckpoint());
    return true;
  }
  uint64_t stop_seq_num = cmd.stop_seq_num.value_or(sequence_num);
  LOG_INFO(logger_,
           "Wedge command instructs replica to stop at sequence number "
               << stop_seq_num);
  control_state_manager_->setStopAtNextCheckpoint(stop_seq_num);

  return true;
}

bool ReconfigurationSMOpen::handle(const GetVersionCommand& cmd) {
  return false;
}

bool ReconfigurationSMOpen::handle(const DownloadCommand& cmd) { return false; }

bool ReconfigurationSMOpen::handle(const UpgradeCommand& cmd) { return false; }

void ReconfigurationSMOpen::setControlHandlers(
    std::shared_ptr<ConcordControlHandler> control_handlers) {
  control_handlers_ = control_handlers;
}

void ReconfigurationSMOpen::setControlStateManager(
    std::shared_ptr<bftEngine::ControlStateManager> control_state_manager) {
  control_state_manager_ = control_state_manager;
}

}  // namespace concord::reconfiguration
