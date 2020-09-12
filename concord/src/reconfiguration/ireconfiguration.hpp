// Copyright 2020 VMware, all rights reserved

#ifndef CONCORD_RECONFIGURATION_IRECONFIGURATION_HPP_
#define CONCORD_RECONFIGURATION_IRECONFIGURATION_HPP_

#include <opentracing/tracer.h>
#include "bftengine/ControlStateManager.hpp"
#include "concord.pb.h"
#include "concord_control_handler.hpp"

namespace concord {
namespace reconfiguration {

// Reconfiguration state machine interface used in Concord's commands handler
class IReconfiguration {
 public:
  virtual ~IReconfiguration() {}

  // Concord's commands handler will forward any reconfiguration request to this
  // function. Return true if the request was handled successfully.
  virtual bool handle(
      const com::vmware::concord::ReconfigurationSmRequest& request,
      com::vmware::concord::ConcordResponse& response, uint64_t sequence_num,
      bool readOnly,
      com::vmware::concord::ConcordReplicaSpecificInfoResponse& rsi_response,
      opentracing::Span& parent_span) = 0;

  // The reconfiguration state machine can communicate with the BFT engine via
  // the control state manager. Concord's commands handler will call this
  // function.
  virtual void setControlStateManager(
      std::shared_ptr<bftEngine::ControlStateManager> control_state_manager) {}

  // The control state handlers can be queried to learn about events inside the
  // BFT engine. Concord's commands handler will call this function.
  virtual void setControlHandlers(
      std::shared_ptr<concord::reconfiguration::ConcordControlHandler>
          control_handlers) {}
};

}  // namespace reconfiguration
}  // namespace concord

#endif  // CONCORD_RECONFIGURATION_IRECONFIGURATION_HPP_
