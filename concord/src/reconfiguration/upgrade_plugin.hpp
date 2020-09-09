// Copyright 2020 VMware, all rights reserved

#ifndef CONCORD_CONCORD_SRC_RECONFIGURATION_UPGRADE_PLUGIN_HPP_
#define CONCORD_CONCORD_SRC_RECONFIGURATION_UPGRADE_PLUGIN_HPP_

#include <concord.pb.h>
#include "IReconfigurationPlugin.hpp"

namespace concord {
namespace reconfiguration {

class DownLoadSwVersionPlugin : public IReconfigurationPlugin {
  com::vmware::concord::ReconfigurationSmRequest::CommandCase GetPluginId()
      const override {
    return com::vmware::concord::ReconfigurationSmRequest::CommandCase::
        kDownloadSwVersionCmd;
  }
  bool Handle(
      const com::vmware::concord::ReconfigurationSmRequest &command,
      uint64_t sequence_num, bool readOnly, opentracing::Span &parent_span,
      com::vmware::concord::ConcordResponse &concord_response,
      com::vmware::concord::ConcordReplicaSpecificInfoResponse &rsi_response,
      bftEngine::ControlStateManager &control_state_manager,
      ConcordControlHandler &control_handlers) override {
    auto res = concord_response.mutable_reconfiguration_sm_response();
    res->set_success(true);
    res->set_additionaldata("Upgrading");
    return true;
  }
};

class HasSwVersionPlugin : public IReconfigurationPlugin {
  com::vmware::concord::ReconfigurationSmRequest::CommandCase GetPluginId()
      const override {
    return com::vmware::concord::ReconfigurationSmRequest::CommandCase::
        kHasSwVersionCmd;
  }
  bool Handle(
      const com::vmware::concord::ReconfigurationSmRequest &command,
      uint64_t sequence_num, bool readOnly, opentracing::Span &parent_span,
      com::vmware::concord::ConcordResponse &concord_response,
      com::vmware::concord::ConcordReplicaSpecificInfoResponse &rsi_response,
      bftEngine::ControlStateManager &control_state_manager,
      ConcordControlHandler &control_handlers) override {
    auto res = concord_response.mutable_reconfiguration_sm_response();
    res->set_success(true);
    res->set_additionaldata("Valid");
    return true;
  }
};
class UpgradeSwVersionPlugin : public IReconfigurationPlugin {
 public:
  com::vmware::concord::ReconfigurationSmRequest::CommandCase GetPluginId()
      const override {
    return com::vmware::concord::ReconfigurationSmRequest::CommandCase::
        kUpgradeSwVersionCmd;
  }
  bool Handle(
      const com::vmware::concord::ReconfigurationSmRequest &command,
      uint64_t sequence_num, bool readOnly, opentracing::Span &parent_span,
      com::vmware::concord::ConcordResponse &concord_response,
      com::vmware::concord::ConcordReplicaSpecificInfoResponse &rsi_response,
      bftEngine::ControlStateManager &control_state_manager,
      ConcordControlHandler &control_handlers) override {
    auto res = concord_response.mutable_reconfiguration_sm_response();
    res->set_success(true);
    res->set_additionaldata("Upgraded");
    return true;
  }
};

}  // namespace reconfiguration
}  // namespace concord
#endif  // CONCORD_CONCORD_SRC_RECONFIGURATION_UPGRADE_PLUGIN_HPP_
