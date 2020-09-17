// Copyright 2020 VMware, all rights reserved

#include "reconfiguration_sm_dispatcher.hpp"

#include <utils/openssl_crypto_utils.hpp>
#include "Logger.hpp"

using com::vmware::concord::ConcordReplicaSpecificInfoResponse;
using com::vmware::concord::ConcordResponse;
using com::vmware::concord::ReconfigurationSmRequest;
using com::vmware::concord::ReconfigurationSmResponse;

using concord::messages::DownloadCommand;
using concord::messages::GetVersionCommand;
using concord::messages::ReconfigurationRequest;
using concord::messages::UpgradeCommand;
using concord::messages::WedgeCommand;

using std::holds_alternative;

namespace concord::reconfiguration {

ReconfigurationSMDispatcher::ReconfigurationSMDispatcher(
    std::unique_ptr<IReconfigurationHandler> handler,
    const config::ConcordConfiguration& config,
    std::shared_ptr<concord::utils::PrometheusRegistry> prometheus_registry)
    : logger_(logging::getLogger("concord.reconfiguration")),
      handler_(std::move(handler)),
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

bool ReconfigurationSMDispatcher::dispatch(
    const ReconfigurationRequest& request, ConcordResponse& response,
    uint64_t sequence_num, bool read_only,
    com::vmware::concord::ConcordReplicaSpecificInfoResponse& rsi_response,
    opentracing::Span& parent_span) {
  auto reconfiguration_span = opentracing::Tracer::Global()->StartSpan(
      "reconfiguration_request",
      {opentracing::ChildOf(&parent_span.context())});
  auto reconf_response = response.mutable_reconfiguration_sm_response();
  if (!validateReconfigurationSmRequest(request)) {
    reconf_response->set_success(false);
    return false;
  }

  bool success = false;
  if (holds_alternative<WedgeCommand>(request.command)) {
    success =
        handler_->handle(std::get<WedgeCommand>(request.command), sequence_num,
                         read_only, rsi_response, parent_span);
  } else if (holds_alternative<GetVersionCommand>(request.command)) {
    success = handler_->handle(std::get<GetVersionCommand>(request.command));
    reconf_response->set_additionaldata("Version");
  } else if (holds_alternative<DownloadCommand>(request.command)) {
    success = handler_->handle(std::get<DownloadCommand>(request.command));
    reconf_response->set_additionaldata("Downloading");
  } else if (holds_alternative<UpgradeCommand>(request.command)) {
    success = handler_->handle(std::get<UpgradeCommand>(request.command));
    reconf_response->set_additionaldata("Upgrading");
  }
  reconf_response->set_success(success);

  return success;
}

void ReconfigurationSMDispatcher::setControlHandlers(
    std::shared_ptr<ConcordControlHandler> control_handlers) {
  control_handlers_ = control_handlers;
  handler_->setControlHandlers(control_handlers);
}

void ReconfigurationSMDispatcher::setControlStateManager(
    std::shared_ptr<bftEngine::ControlStateManager> control_state_manager) {
  control_state_manager_ = control_state_manager;
  handler_->setControlStateManager(control_state_manager);
}

bool ReconfigurationSMDispatcher::validateReconfigurationSmRequest(
    const ReconfigurationRequest& request) {
  if (!holds_alternative<GetVersionCommand>(request.command) &&
      !holds_alternative<DownloadCommand>(request.command) &&
      !holds_alternative<WedgeCommand>(request.command) &&
      !holds_alternative<UpgradeCommand>(request.command)) {
    LOG_WARN(logger_,
             "Reconfiguration request validation failed: No command set");
    return false;
  }
  if (request.signature.empty() || operator_public_key_ == nullptr) {
    LOG_WARN(logger_,
             "Reconfiguration request validation failed" << KVLOG(
                 request.signature.empty(), operator_public_key_ == nullptr));
    return false;
  }

  ReconfigurationRequest request_without_sig = request;
  request_without_sig.signature = {};
  std::vector<uint8_t> serialized_cmd;
  concord::messages::serialize(serialized_cmd, request_without_sig);

  auto valid = operator_public_key_->Verify(
      std::string(serialized_cmd.begin(), serialized_cmd.end()),
      std::string(request.signature.begin(), request.signature.end()));
  if (!valid) {
    LOG_WARN(logger_,
             "Reconfiguration request validation failed" << KVLOG(valid));
    return false;
  }
  return true;
}

}  // namespace concord::reconfiguration
