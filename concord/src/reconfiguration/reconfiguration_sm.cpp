// Copyright 2020 VMware, all rights reserved

#include "reconfiguration_sm.hpp"

#include "Logger.hpp"

using com::vmware::concord::ReconfigurationSmRequest;

namespace concord {
namespace reconfiguration {

bool ReconfigurationSM::handle(
    const ReconfigurationSmRequest::GetVersionCommand& cmd) {
  LOG_INFO(logger_, "In GetVersionCommand handler");
  return true;
}

bool ReconfigurationSM::handle(
    const ReconfigurationSmRequest::DownloadCommand& cmd) {
  LOG_INFO(logger_, "In DownloadCommand handler");
  return true;
}

bool ReconfigurationSM::handle(
    const ReconfigurationSmRequest::UpgradeCommand& cmd) {
  LOG_INFO(logger_, "In UpgradeCommand handler");
  return true;
}

}  // namespace reconfiguration
}  // namespace concord
