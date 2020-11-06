// Copyright 2020 VMware, all rights reserved

#include "reconfiguration_sm.hpp"

#include <memory>

#include "Logger.hpp"

using com::vmware::concord::ReconfigurationSmRequest;

namespace concord {
namespace reconfiguration {

bool ReconfigurationSM::handle(
    const concord::messages::GetVersionCommand& cmd) {
  LOG_INFO(logger_, "In GetVersionCommand handler");
  return true;
}

bool ReconfigurationSM::handle(const concord::messages::DownloadCommand& cmd) {
  LOG_INFO(logger_, "In DownloadCommand handler " << cmd.version);
  return true;
}

bool ReconfigurationSM::handle(const concord::messages::UpgradeCommand& cmd) {
  LOG_INFO(logger_, "In UpgradeCommand handler " << cmd.version);
  return true;
}
ReconfigurationSM::ReconfigurationSM(
    const kvbc::ILocalKeyValueStorageReadOnly& ro_storage,
    kvbc::IBlocksAppender& blocks_appender,
    kvbc::IBlocksDeleter& blocks_deleter,
    bftEngine::IStateTransfer& state_transfer,
    const config::ConcordConfiguration& config,
    const config::ConcordConfiguration& node_config,
    concord::time::TimeContract* time_contract) {
  pruningSM_ = std::make_unique<pruning::KVBPruningSM>(
      ro_storage, blocks_appender, blocks_deleter, state_transfer, config,
      node_config, time_contract);
}

}  // namespace reconfiguration
}  // namespace concord
