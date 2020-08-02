// Copyright 2020 VMware, all rights reserved

#include "reconfiguration_sm.hpp"
#include <utils/openssl_crypto_utils.hpp>
#include "Logger.hpp"
#include "upgrade_plugin.hpp"

using namespace concord::reconfiguration;
using namespace com::vmware::concord;

void ReconfigurationSM::LoadPlugin(
    std::unique_ptr<IReconfigurationPlugin> plugin) {
  plugins_.emplace_back(std::move(plugin));
}

IReconfigurationPlugin* ReconfigurationSM::GetPlugin(
    ReconfigurationSmRequest::PluginId pluginId) const {
  for (const auto& p : plugins_) {
    if (p->GetPluginId() == pluginId) return p.get();
  }
  return nullptr;
}

void ReconfigurationSM::Handle(const ReconfigurationSmRequest& request,
                               ConcordResponse& response, uint64_t sequence_num,
                               bool readOnly, opentracing::Span& parent_span) {
  SCOPED_MDC("r.p.id", std::to_string(request.pluginid()));
  auto reconfiguration_span = opentracing::Tracer::Global()->StartSpan(
      "reconfiguration_request",
      {opentracing::ChildOf(&parent_span.context())});
  if (!ValidateReconfigurationRequest(request)) {
    LOG_WARN(logger_, "Reconfiguration request validation failed");
    response.mutable_reconfiguration_sm_response()->set_success(false);
    return;
  }

  auto plugin = GetPlugin(request.pluginid());
  if (!plugin) {
    LOG_WARN(logger_, "Invalid plugin number");
    response.mutable_reconfiguration_sm_response()->set_success(false);
    return;
  }
  auto rep = plugin->Handle(request.command(), sequence_num, readOnly,
                            *reconfiguration_span, *control_state_manager_,
                            *control_handlers_);
  auto res = response.mutable_reconfiguration_sm_response();
  res->set_success(rep.succ);
  res->set_additionaldata(rep.data);
}

/*
 * The reconfiguration state machine is a plugin based state machine.
 * Each request (read or write) is directed to its corresponding plugin.
 * The request handling is in the responsibility of the plugin.
 * In addition, to preserve the SM properties, the reconfiguration state machine
 * writes each request write request and its reply in a new block in the
 * blockchain. That way we can track all the write reconfiguration commands in
 * the blockchain.
 */
ReconfigurationSM::ReconfigurationSM(
    const config::ConcordConfiguration& config,
    std::shared_ptr<concord::utils::PrometheusRegistry> prometheus_registry)
    : logger_(logging::getLogger("concord.reconfiguration")),
      reconfiguration_counters_(prometheus_registry->createCounterFamily(
          "concord_reconfiguration_operation_counters_total",
          "counts how many operations the reconfiguration command handler has "
          "done",
          {})) {
  if (!config.hasValue<std::string>("reconfiguration_operator_public_key")) {
    LOG_WARN(logger_,
             "System operator public key is missing, concord won't be able to "
             "execute reconfiguration commands");
  } else {
    operator_public_key_ = utils::openssl_crypto::DeserializePublicKey(
        config.getValue<std::string>("reconfiguration_operator_public_key"));
    LOG_INFO(logger_, "Successfully read the system operator public key");
  }
}

bool ReconfigurationSM::ValidateReconfigurationRequest(
    const ReconfigurationSmRequest& request) {
  if (!request.has_command() || !request.has_signature() ||
      !request.has_pluginid() || operator_public_key_ == nullptr)
    return false;
  return operator_public_key_->Verify(request.command(), request.signature());
}
void ReconfigurationSM::setControlHandlers(
    std::shared_ptr<ConcordControlHandler> control_handlers) {
  control_handlers_ = control_handlers;
}
void ReconfigurationSM::setControlStateManager(
    std::shared_ptr<bftEngine::ControlStateManager> control_state_manager) {
  control_state_manager_ = control_state_manager;
}
