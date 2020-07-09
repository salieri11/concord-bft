// Copyright 2020 VMware, all rights reserved

#ifndef CONCORD_RECONFIGURATION_RECONFIGURATION_SM_HPP_
#define CONCORD_RECONFIGURATION_RECONFIGURATION_SM_HPP_

#include <db_interfaces.h>
#include <opentracing/tracer.h>
#include <storage/kvb_key_types.h>
#include <Logger.hpp>
#include <array>
#include <cstdint>
#include <utils/concord_prometheus_metrics.hpp>
#include <utils/openssl_crypto_utils.hpp>
#include <vector>
#include "IReconfigurationPlugin.hpp"
#include "concord.pb.h"
#include "config/configuration_manager.hpp"

namespace concord {
namespace reconfiguration {

class ReconfigurationSM {
  std::vector<std::unique_ptr<IReconfigurationPlugin>> plugins_;
  logging::Logger logger_;
  prometheus::Family<prometheus::Counter>& reconfiguration_counters_;
  std::unique_ptr<concord::utils::openssl_crypto::AsymmetricPublicKey>
      operator_public_key_{nullptr};

  bool ValidateReconfigurationRequest(
      const com::vmware::concord::ReconfigurationSmRequest& request);
  IReconfigurationPlugin* GetPlugin(
      com::vmware::concord::ReconfigurationSmRequest::PluginId pluginId) const;

 public:
  ReconfigurationSM(
      const config::ConcordConfiguration& config,
      std::shared_ptr<concord::utils::PrometheusRegistry> prometheus_registry);
  void LoadPlugin(std::unique_ptr<IReconfigurationPlugin> plugin);
  /*
   * This method is the gate for all reconfiguration actions. It works as
   * follows:
   * 1. Validate the request against the reconfiguration system operator (RSO)
   * public key
   * 2. Direct the request to the relevant plugin
   * 3. Wrap the plugin response in the concordResponse message
   *
   * Basically, we would like to write each reconfiguration write command to the
   * blockchain and document it as part of the state. This will be under the
   * responsibility of each plugin to write its own commands to the blockchain.
   */
  void Handle(const com::vmware::concord::ReconfigurationSmRequest& request,
              com::vmware::concord::ConcordResponse& response, bool readOnly,
              opentracing::Span& parent_span);
};
}  // namespace reconfiguration
}  // namespace concord

#endif  // CONCORD_RECONFIGURATION_RECONFIGURATION_SM_HPP_