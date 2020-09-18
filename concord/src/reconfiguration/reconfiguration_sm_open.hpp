// Copyright 2020 VMware, all rights reserved

#ifndef CONCORD_RECONFIGURATION_RECONFIGURATION_SM_OPEN_HPP_
#define CONCORD_RECONFIGURATION_RECONFIGURATION_SM_OPEN_HPP_

#include <Logger.hpp>
#include <bftengine/ControlStateManager.hpp>
#include "concord.pb.h"
#include "config/configuration_manager.hpp"
#include "reconfiguration/concord_control_handler.hpp"
#include "reconfiguration/ireconfiguration.hpp"

namespace concord {
namespace reconfiguration {

class ReconfigurationSMOpen : public IReconfigurationHandler {
 public:
  ReconfigurationSMOpen();

  virtual bool handle(
      const com::vmware::concord::ReconfigurationSmRequest::WedgeCommand& cmd,
      uint64_t sequence_num, bool readOnly,
      com::vmware::concord::ConcordReplicaSpecificInfoResponse& rsi_response,
      opentracing::Span& parent_span) override;
  virtual bool handle(
      const com::vmware::concord::ReconfigurationSmRequest::GetVersionCommand&)
      override;
  virtual bool handle(
      const com::vmware::concord::ReconfigurationSmRequest::DownloadCommand&)
      override;
  virtual bool handle(
      const com::vmware::concord::ReconfigurationSmRequest::UpgradeCommand&)
      override;

  virtual void setControlStateManager(
      std::shared_ptr<bftEngine::ControlStateManager> control_state_manager)
      override;
  virtual void setControlHandlers(
      std::shared_ptr<concord::reconfiguration::ConcordControlHandler>
          control_handlers) override;

 protected:
  logging::Logger logger_;
  std::shared_ptr<bftEngine::ControlStateManager> control_state_manager_;
  std::shared_ptr<ConcordControlHandler> control_handlers_;
};

}  // namespace reconfiguration
}  // namespace concord

#endif  // CONCORD_RECONFIGURATION_RECONFIGURATION_SM_OPEN_HPP_
