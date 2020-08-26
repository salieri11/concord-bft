// Copyright 2020 VMware, all rights reserved

#ifndef CONCORD_CONCORD_SRC_RECONFIGURATION_IRECONFIGURATIONPLUGIN_HPP_
#define CONCORD_CONCORD_SRC_RECONFIGURATION_IRECONFIGURATIONPLUGIN_HPP_

#include <opentracing/tracer.h>
#include <string>
#include "bftengine/ControlStateManager.hpp"
#include "concord.pb.h"
#include "concord_control_handler.hpp"

namespace concord {
namespace reconfiguration {

class IReconfigurationPlugin {
 protected:
  com::vmware::concord::ReconfigurationSmRequest::PluginId pluginId_;

 public:
  virtual ~IReconfigurationPlugin() {}
  com::vmware::concord::ReconfigurationSmRequest::PluginId GetPluginId() const {
    return pluginId_;
  }

  virtual void Handle(
      const std::string& command, uint64_t sequence_num, bool readOnly,
      opentracing::Span& parent_span,
      com::vmware::concord::ConcordResponse& concord_response,
      com::vmware::concord::ConcordReplicaSpecificInfoResponse& rsi_response,
      bftEngine::ControlStateManager& control_state_manager,
      ConcordControlHandler& control_handlers) = 0;
};
}  // namespace reconfiguration
}  // namespace concord

#endif  // CONCORD_CONCORD_SRC_RECONFIGURATION_IRECONFIGURATIONPLUGIN_HPP_
