// Copyright 2020 VMware, all rights reserved

#include "reconfiguration_sm_open.hpp"

#include "Logger.hpp"

using com::vmware::concord::ConcordReplicaSpecificInfoResponse;
using com::vmware::concord::ConcordResponse;
using com::vmware::concord::ReconfigurationSmRequest;
using com::vmware::concord::ReconfigurationSmResponse;

using concord::messages::DownloadCommand;
using concord::messages::GetVersionCommand;
using concord::messages::LatestPrunableBlockRequest;
using concord::messages::PruneRequest;
using concord::messages::UpgradeCommand;
using concord::messages::WedgeCommand;

namespace concord::reconfiguration {
ReconfigurationSMOpen::ReconfigurationSMOpen()
    : logger_(logging::getLogger("concord.reconfiguration.handler")) {}

bool ReconfigurationSMOpen::handle(const WedgeCommand& cmd,
                                   uint64_t sequence_num, bool readOnly,
                                   concord::messages::WedgeResponse& response_,
                                   opentracing::Span& parent_span) {
  if (readOnly) {
    response_.stopped = control_handlers_->isOnNOutOfNCheckpoint();
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

bool ReconfigurationSMOpen::handle(
    const LatestPrunableBlockRequest& cmd,
    concord::messages::LatestPrunableBlock& last_pruneable_block, bool readOnly,
    opentracing::Span& parent_span) {
  if (pruningSM_ == nullptr || !readOnly) {
    LOG_ERROR(logger_, "unable to handle LatestPrunableBlockRequest message"
                           << KVLOG(pruningSM_ == nullptr, readOnly));
    return false;
  }
  try {
    pruningSM_->Handle(cmd, last_pruneable_block, parent_span);
  } catch (const std::exception& e) {
    return false;
  }
  return true;
}
bool ReconfigurationSMOpen::handle(const concord::messages::PruneRequest& cmd,
                                   std::string& ret_data, bool read_only,
                                   opentracing::Span& parent_span) {
  if (pruningSM_ == nullptr) {
    LOG_ERROR(logger_, "unable to handle PruneRequest message"
                           << KVLOG(pruningSM_ == nullptr));
    return false;
  }
  bool ret = false;
  ret_data = std::string();
  try {
    ret = pruningSM_->Handle(cmd, read_only, parent_span);
  } catch (const std::exception& e) {
    LOG_ERROR(logger_, e.what());
    ret_data = e.what();
  }
  return ret;
}
void ReconfigurationSMOpen::setControlHandlers(
    std::shared_ptr<ConcordControlHandler> control_handlers) {
  control_handlers_ = control_handlers;
}

void ReconfigurationSMOpen::setControlStateManager(
    std::shared_ptr<bftEngine::ControlStateManager> control_state_manager) {
  control_state_manager_ = control_state_manager;
}

}  // namespace concord::reconfiguration
