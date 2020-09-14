// Copyright 2020 VMware, all rights reserved

#include "reconfiguration_sm_dispatcher.hpp"

#include <utils/openssl_crypto_utils.hpp>
#include "Logger.hpp"

using com::vmware::concord::ConcordReplicaSpecificInfoResponse;
using com::vmware::concord::ConcordResponse;
using com::vmware::concord::ReconfigurationSmRequest;
using com::vmware::concord::ReconfigurationSmResponse;

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
    const ReconfigurationSmRequest& request, ConcordResponse& response,
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
  if (request.has_wedge_cmd()) {
    success = handler_->handle(request.wedge_cmd(), sequence_num, read_only,
                               rsi_response, parent_span);
  } else if (request.has_get_version_cmd()) {
    success = handler_->handle(request.get_version_cmd());
    reconf_response->set_additionaldata("Version");
  } else if (request.has_download_cmd()) {
    success = handler_->handle(request.download_cmd());
    reconf_response->set_additionaldata("Downloading");
  } else if (request.has_upgrade_cmd()) {
    success = handler_->handle(request.upgrade_cmd());
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

}  // namespace concord::reconfiguration
