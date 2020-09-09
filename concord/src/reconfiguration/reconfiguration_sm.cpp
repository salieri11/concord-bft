// Copyright 2020 VMware, all rights reserved

#include "reconfiguration_sm.hpp"
#include <utils/openssl_crypto_utils.hpp>
#include "Logger.hpp"
#include "upgrade_plugin.hpp"
#include "wedge_plugin.hpp"

using namespace concord::reconfiguration;
using namespace com::vmware::concord;

void ReconfigurationSM::LoadPlugin(
    std::unique_ptr<IReconfigurationPlugin> plugin) {
  plugins_.emplace(plugin->GetPluginId(), std::move(plugin));
}

IReconfigurationPlugin* ReconfigurationSM::GetPlugin(
    ReconfigurationSmRequest::CommandCase cmd) const {
  if (plugins_.find(cmd) == plugins_.end()) return nullptr;
  return plugins_.at(cmd).get();
}

bool ReconfigurationSM::Handle(
    const ReconfigurationSmRequest& request, ConcordResponse& response,
    uint64_t sequence_num, bool readOnly,
    com::vmware::concord::ConcordReplicaSpecificInfoResponse& rsi_response,
    opentracing::Span& parent_span) {
  auto reconfiguration_span = opentracing::Tracer::Global()->StartSpan(
      "reconfiguration_request",
      {opentracing::ChildOf(&parent_span.context())});
  if (!ValidateReconfigurationRequest(request)) {
    response.mutable_reconfiguration_sm_response()->set_success(false);
    return false;
  }
  auto plugin = GetPlugin(request.command_case());
  if (!plugin) {
    LOG_WARN(logger_, "Invalid plugin number");
    response.mutable_reconfiguration_sm_response()->set_success(false);
    return false;
  }
  plugin->Handle(request, sequence_num, readOnly, *reconfiguration_span,
                 response, rsi_response, *control_state_manager_,
                 *control_handlers_);
  return true;
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
  if (!config.hasValue<std::string>("signing_key_path")) {
    LOG_WARN(logger_,
             "System operator public key is missing, concord won't be able to "
             "execute reconfiguration commands");
  } else {
    std::string op_pub_key_path =
        config.getValue<std::string>("signing_key_path") + "/operator_pub.pem";
    operator_public_key_ =
        concord::utils::openssl_crypto::DeserializePublicKeyFromPem(
            op_pub_key_path, "secp256r1");
    if (operator_public_key_ == nullptr) {
      LOG_WARN(
          logger_,
          "System operator public key is missing, concord won't be able to \"\n"
          "             \"execute reconfiguration commands");
    } else {
      LOG_INFO(logger_, "Successfully read the system operator public key");
    }
  }
}

bool ReconfigurationSM::ValidateReconfigurationRequest(
    const ReconfigurationSmRequest& request) {
  if (request.command_case() ==
          ReconfigurationSmRequest::CommandCase::COMMAND_NOT_SET ||
      !request.has_signature() || operator_public_key_ == nullptr) {
    LOG_WARN(logger_,
             "Reconfiguration request validation failed" << KVLOG(
                 request.command_case() ==
                     ReconfigurationSmRequest::CommandCase::COMMAND_NOT_SET,
                 request.has_signature(), operator_public_key_ == nullptr));
    return false;
  }
  ReconfigurationSmRequest request_without_sig;
  request_without_sig.CopyFrom(request);
  request_without_sig.clear_signature();
  auto serialized_cmd = request_without_sig.SerializeAsString();
  auto valid =
      operator_public_key_->Verify(serialized_cmd, request.signature());
  if (!valid)
    LOG_WARN(logger_,
             "Reconfiguration request validation failed" << KVLOG(valid));
  return valid;
}
void ReconfigurationSM::setControlHandlers(
    std::shared_ptr<ConcordControlHandler> control_handlers) {
  control_handlers_ = control_handlers;
}
void ReconfigurationSM::setControlStateManager(
    std::shared_ptr<bftEngine::ControlStateManager> control_state_manager) {
  control_state_manager_ = control_state_manager;
}
void ReconfigurationSM::LoadAllPlugins() {
  LoadPlugin(std::make_unique<concord::reconfiguration::WedgePlugin>());
  LoadPlugin(
      std::make_unique<concord::reconfiguration::DownLoadSwVersionPlugin>());
  LoadPlugin(std::make_unique<concord::reconfiguration::HasSwVersionPlugin>());
  LoadPlugin(
      std::make_unique<concord::reconfiguration::UpgradeSwVersionPlugin>());
}
