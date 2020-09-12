// Copyright 2020 VMware, all rights reserved

#ifndef CONCORD_RECONFIGURATION_RECONFIGURATION_SM_HPP_
#define CONCORD_RECONFIGURATION_RECONFIGURATION_SM_HPP_

#include <db_interfaces.h>
#include <opentracing/tracer.h>
#include <storage/kvb_key_types.h>
#include <Logger.hpp>
#include <array>
#include <bftengine/ControlStateManager.hpp>
#include <cstdint>
#include <unordered_map>
#include <utils/concord_prometheus_metrics.hpp>
#include <utils/openssl_crypto_utils.hpp>
#include "IReconfigurationPlugin.hpp"
#include "bftengine/Replica.hpp"
#include "concord.pb.h"
#include "concord_control_handler.hpp"
#include "config/configuration_manager.hpp"
#include "ireconfiguration.hpp"

namespace concord {
namespace reconfiguration {

class ReconfigurationSM : public IReconfiguration {
  std::unordered_map<
      com::vmware::concord::ReconfigurationSmRequest::CommandCase,
      std::unique_ptr<IReconfigurationPlugin>>
      plugins_;
  logging::Logger logger_;
  prometheus::Family<prometheus::Counter>& reconfiguration_counters_;
  std::unique_ptr<concord::utils::openssl_crypto::AsymmetricPublicKey>
      operator_public_key_{nullptr};
  std::shared_ptr<bftEngine::ControlStateManager> control_state_manager_;
  std::shared_ptr<ConcordControlHandler> control_handlers_;

  bool ValidateReconfigurationRequest(
      const com::vmware::concord::ReconfigurationSmRequest& request);
  IReconfigurationPlugin* GetPlugin(
      com::vmware::concord::ReconfigurationSmRequest::CommandCase cmd) const;

 public:
  ReconfigurationSM(
      const config::ConcordConfiguration& config,
      std::shared_ptr<concord::utils::PrometheusRegistry> prometheus_registry);
  void LoadPlugin(std::unique_ptr<IReconfigurationPlugin> plugin);

  void LoadAllPlugins();

  // IReconfiguration API
  void setControlHandlers(
      std::shared_ptr<ConcordControlHandler> control_handlers) override;
  void setControlStateManager(std::shared_ptr<bftEngine::ControlStateManager>
                                  control_state_manager) override;

  // This method is the gate for all reconfiguration actions. It works as
  // follows:
  // 1. Validate the request against the reconfiguration system operator (RSO)
  // public key
  // 2. Direct the request to the relevant plugin
  // 3. Wrap the plugin response in the concordResponse message
  //
  // Basically, we would like to write each reconfiguration write command to the
  // blockchain and document it as part of the state. This will be under the
  // responsibility of each plugin to write its own commands to the blockchain.
  bool handle(
      const com::vmware::concord::ReconfigurationSmRequest& request,
      com::vmware::concord::ConcordResponse& response, uint64_t sequence_num,
      bool readOnly,
      com::vmware::concord::ConcordReplicaSpecificInfoResponse& rsi_response,
      opentracing::Span& parent_span) override;
};

}  // namespace reconfiguration
}  // namespace concord

#endif  // CONCORD_RECONFIGURATION_RECONFIGURATION_SM_HPP_
