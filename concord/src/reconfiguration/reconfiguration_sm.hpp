// Copyright 2020 VMware, all rights reserved

#ifndef CONCORD_RECONFIGURATION_RECONFIGURATION_SM_HPP_
#define CONCORD_RECONFIGURATION_RECONFIGURATION_SM_HPP_

#include <db_interfaces.h>
#include <opentracing/tracer.h>
#include <storage/kvb_key_types.h>
#include <Logger.hpp>
#include <array>
#include <cstdint>
#include <utils/openssl_crypto_utils.hpp>
#include <vector>
#include "concord.pb.h"
#include "config/configuration_manager.hpp"

namespace concord {
namespace reconfiguration {

struct PluginReply {
  bool succ;
  std::string data;
};

class IReconfigurationPlugin {
  com::vmware::concord::ReconfigurationSmRequest::PluginId pluginId_;

 public:
  explicit IReconfigurationPlugin(
      com::vmware::concord::ReconfigurationSmRequest::PluginId pluginId)
      : pluginId_(pluginId) {}
  virtual ~IReconfigurationPlugin() {}
  com::vmware::concord::ReconfigurationSmRequest::PluginId GetPluginId() const {
    return pluginId_;
  }
  virtual PluginReply Handle(const std::string& command, bool readOnly,
                             opentracing::Span& parent_span) = 0;
};

class ReconfigurationSM {
  std::vector<std::unique_ptr<IReconfigurationPlugin>> plugins_;
  log4cplus::Logger logger_;
  std::unique_ptr<concord::utils::openssl_crypto::AsymmetricPublicKey>
      operator_public_key_{nullptr};

  bool ValidateReconfigurationRequest(
      const com::vmware::concord::ReconfigurationSmRequest& request);
  IReconfigurationPlugin* GetPlugin(
      com::vmware::concord::ReconfigurationSmRequest::PluginId pluginId) const;

 public:
  ReconfigurationSM(const config::ConcordConfiguration& config);
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
