// Copyright 2020 VMware, all rights reserved

#ifndef CONCORD_RECONFIGURATION_IRECONFIGURATION_HPP_
#define CONCORD_RECONFIGURATION_IRECONFIGURATION_HPP_

#include <opentracing/tracer.h>
#include "IStateTransfer.hpp"
#include "bftengine/ControlStateManager.hpp"
#include "concord.cmf.hpp"
#include "concord.pb.h"
#include "concord_control_handler.hpp"
#include "db_interfaces.h"
#include "kv_types.hpp"
#include "sliver.hpp"
#include "time/time_contract.hpp"
#include "utils/openssl_crypto_utils.hpp"

namespace concord {
namespace reconfiguration {

// The IReconfigurationHandler interface defines all message handler. It is
// tightly coupled with the messages inside ReconfigurationSmRequest in the
// message definition.
class IReconfigurationHandler {
 public:
  virtual ~IReconfigurationHandler() {}

  // Message handler
  virtual bool handle(const concord::messages::WedgeCommand& cmd,
                      uint64_t sequence_num, bool readOnly,
                      concord::messages::WedgeResponse& response_,
                      opentracing::Span& parent_span) = 0;
  virtual bool handle(const concord::messages::GetVersionCommand&) = 0;
  virtual bool handle(const concord::messages::DownloadCommand&) = 0;
  virtual bool handle(const concord::messages::UpgradeCommand&) = 0;
  virtual bool handle(const concord::messages::LatestPrunableBlockRequest&,
                      concord::messages::LatestPrunableBlock&, bool readOnly,
                      opentracing::Span& parent_span) = 0;
  virtual bool handle(const concord::messages::PruneRequest&,
                      std::string& ret_data, bool read_only,
                      opentracing::Span& parent_span) = 0;
  // The reconfiguration state machine can communicate with the BFT engine via
  // the control state manager. The dipatcher will call this function.
  virtual void setControlStateManager(
      std::shared_ptr<bftEngine::ControlStateManager>
          control_state_manager) = 0;

  // The control state handlers can be queried to learn about events inside the
  // BFT engine. The dipatcher will call this function.
  virtual void setControlHandlers(
      std::shared_ptr<concord::reconfiguration::ConcordControlHandler>
          control_handlers) = 0;
};

// The IReconfigurationDispatcher interface is used by Concord's command
// handler to deal with ReconfigurationSmRequest messages
class IReconfigurationDispatcher {
 public:
  virtual ~IReconfigurationDispatcher() {}

  // Concord's commands handler will forward any reconfiguration request to this
  // function. Return true if the request was handled successfully.
  virtual bool dispatch(
      const concord::messages::ReconfigurationRequest& request,
      com::vmware::concord::ConcordResponse& response, uint64_t sequence_num,
      bool readOnly,
      com::vmware::concord::ConcordReplicaSpecificInfoResponse& rsi_response,
      opentracing::Span& parent_span) = 0;

  // The reconfiguration state machine can communicate with the BFT engine via
  // the control state manager. Concord's commands handler will call this
  // function.
  virtual void setControlStateManager(
      std::shared_ptr<bftEngine::ControlStateManager>
          control_state_manager) = 0;

  // The control state handlers can be queried to learn about events inside the
  // BFT engine. Concord's commands handler will call this function.
  virtual void setControlHandlers(
      std::shared_ptr<concord::reconfiguration::ConcordControlHandler>
          control_handlers) = 0;
};

}  // namespace reconfiguration
}  // namespace concord

#endif  // CONCORD_RECONFIGURATION_IRECONFIGURATION_HPP_
