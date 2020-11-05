// Copyright 2020 VMware, all rights reserved

#ifndef CONCORD_RECONFIGURATION_RECONFIGURATION_SM_OPEN_HPP_
#define CONCORD_RECONFIGURATION_RECONFIGURATION_SM_OPEN_HPP_

#include <Logger.hpp>
#include <bftengine/ControlStateManager.hpp>
#include <reconfiguration/pruning/kvb_pruning_sm.hpp>
#include "concord.cmf.hpp"
#include "concord.pb.h"
#include "config/configuration_manager.hpp"
#include "pruning/kvb_pruning_sm.hpp"
#include "reconfiguration/concord_control_handler.hpp"
#include "reconfiguration/ireconfiguration.hpp"

namespace concord {
namespace reconfiguration {

class ReconfigurationSMOpen : public IReconfigurationHandler {
 public:
  ReconfigurationSMOpen();

  virtual bool handle(const concord::messages::WedgeCommand& cmd,
                      uint64_t sequence_num, bool readOnly,
                      concord::messages::WedgeResponse& response_,
                      opentracing::Span& parent_span) override;
  virtual bool handle(const concord::messages::GetVersionCommand&) override;
  virtual bool handle(const concord::messages::DownloadCommand&) override;
  virtual bool handle(const concord::messages::UpgradeCommand&) override;
  virtual bool handle(const concord::messages::LatestPrunableBlockRequest&,
                      concord::messages::LatestPrunableBlock&, bool readOnly,
                      opentracing::Span& parent_span) override;
  virtual bool handle(const concord::messages::PruneRequest&,
                      std::string& ret_data, bool read_only,
                      opentracing::Span& parent_span) override;
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
  std::unique_ptr<pruning::KVBPruningSM> pruningSM_;
};

}  // namespace reconfiguration
}  // namespace concord

#endif  // CONCORD_RECONFIGURATION_RECONFIGURATION_SM_OPEN_HPP_
