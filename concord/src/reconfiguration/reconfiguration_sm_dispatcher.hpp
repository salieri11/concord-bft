// Copyright 2020 VMware, all rights reserved

#ifndef CONCORD_RECONFIGURATION_RECONFIGURATION_SM_DISPATCHER_HPP_
#define CONCORD_RECONFIGURATION_RECONFIGURATION_SM_DISPATCHER_HPP_

#include <storage/kvb_key_types.h>
#include <Logger.hpp>
#include <bftengine/ControlStateManager.hpp>
#include <utils/concord_prometheus_metrics.hpp>
#include <utils/openssl_crypto_utils.hpp>
#include "bftengine/Replica.hpp"
#include "concord.cmf.hpp"
#include "concord.pb.h"
#include "concord_control_handler.hpp"
#include "config/configuration_manager.hpp"
#include "reconfiguration/ireconfiguration.hpp"

namespace concord {
namespace reconfiguration {

// The dispatcher forwards all messages to their appropriate handlers.
// All handled messages are defined in the IReconfigurationHandler interface.
class ReconfigurationSMDispatcher : public IReconfigurationDispatcher {
 public:
  ReconfigurationSMDispatcher(
      std::unique_ptr<concord::reconfiguration::IReconfigurationHandler>
          handler,
      const config::ConcordConfiguration& config,
      std::shared_ptr<concord::utils::PrometheusRegistry> prometheus_registry);

  // IReconfigurationDispatcher API
  void setControlHandlers(
      std::shared_ptr<ConcordControlHandler> control_handlers) override;
  void setControlStateManager(std::shared_ptr<bftEngine::ControlStateManager>
                                  control_state_manager) override;

  // This method is the gate for all reconfiguration actions. It works as
  // follows:
  // 1. Validate the request against the reconfiguration system operator (RSO)
  // public key
  // 2. Direct the request to the relevant handler
  // 3. Wrap theresponse in the concordResponse message
  //
  // Basically, we would like to write each reconfiguration write command to the
  // blockchain and document it as part of the state. This will be under the
  // responsibility of each handler to write its own commands to the blockchain.
  bool dispatch(
      const concord::messages::ReconfigurationRequest& request,
      com::vmware::concord::ConcordResponse& response, uint64_t sequence_num,
      bool readOnly,
      com::vmware::concord::ConcordReplicaSpecificInfoResponse& rsi_response,
      opentracing::Span& parent_span) override;

 private:
  logging::Logger logger_;
  std::unique_ptr<concord::reconfiguration::IReconfigurationHandler> handler_;
  prometheus::Family<prometheus::Counter>& reconfiguration_counters_;
  std::unique_ptr<concord::utils::openssl_crypto::AsymmetricPublicKey>
      operator_public_key_{nullptr};
  std::shared_ptr<bftEngine::ControlStateManager> control_state_manager_;
  std::shared_ptr<ConcordControlHandler> control_handlers_;

  bool validateReconfigurationSmRequest(
      const concord::messages::ReconfigurationRequest& request);
};

}  // namespace reconfiguration
}  // namespace concord

#endif  // CONCORD_RECONFIGURATION_RECONFIGURATION_SM_DISPATCHER_HPP_
