// Copyright 2020 VMware, all rights reserved

#ifndef CONCORD_CONCORD_SRC_RECONFIGURATION_IRECONFIGURATIONPLUGIN_HPP_
#define CONCORD_CONCORD_SRC_RECONFIGURATION_IRECONFIGURATIONPLUGIN_HPP_

#include <concord.pb.h>
#include <opentracing/tracer.h>
#include <string>
#include "bftengine/ControlStateManager.hpp"
#include "concord.pb.h"
#include "concord_control_handler.hpp"

namespace concord {
namespace reconfiguration {

class IReconfigurationPlugin {
 public:
  virtual ~IReconfigurationPlugin() {}
  virtual com::vmware::concord::ReconfigurationSmRequest::CommandCase
  GetPluginId() const = 0;

  virtual bool Handle(
      const com::vmware::concord::ReconfigurationSmRequest& command,
      uint64_t sequence_num, bool readOnly, opentracing::Span& parent_span,
      com::vmware::concord::ConcordResponse& concord_response,
      com::vmware::concord::ConcordReplicaSpecificInfoResponse& rsi_response,
      bftEngine::ControlStateManager& control_state_manager,
      ConcordControlHandler& control_handlers) = 0;
};
}  // namespace reconfiguration
}  // namespace concord

#endif  // CONCORD_CONCORD_SRC_RECONFIGURATION_IRECONFIGURATIONPLUGIN_HPP_
