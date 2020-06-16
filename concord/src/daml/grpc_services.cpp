// Copyright 2019 VMware, all rights reserved

#include "grpc_services.hpp"

#include <opentracing/tracer.h>
#include <sstream>
#include <string>
#include "utils/open_tracing_utils.hpp"

#include "Logger.hpp"
#include "concord_storage.pb.h"
#include "daml/daml_kvb_commands_handler.hpp"

using com::digitalasset::kvbc::Command;
using com::digitalasset::kvbc::CommandReply;
using com::digitalasset::kvbc::CommitRequest;
using com::digitalasset::kvbc::CommitResponse;
using com::digitalasset::kvbc::CommitResponse_CommitStatus;
using com::digitalasset::kvbc::CommitResponse_CommitStatus_OK;
using com::vmware::concord::ConcordRequest;
using com::vmware::concord::ConcordResponse;
using com::vmware::concord::DamlRequest;
using com::vmware::concord::DamlResponse;
using grpc::ServerContext;

using namespace std;

namespace concord {
namespace daml {

grpc::Status CommitServiceImpl::CommitTransaction(ServerContext* context,
                                                  const CommitRequest* request,

                                                  CommitResponse* reply) {
  ConcordResponse resp;
  ConcordRequest req;
  DamlRequest* daml_request = req.mutable_daml_request();

  Command cmd;
  cmd.mutable_commit()->CopyFrom(*request);

  SCOPED_MDC_CID(request->correlation_id());

  auto span =
      concord::utils::ExtractSpan(request->span_context(), "commit_transaction",
                                  logger_, request->correlation_id());

  // temporary solution, when DA will start sending spans we may consider to
  // remove this line,however, the result should be the same since tag is
  // overwritten.
  span->SetTag(concord::utils::kCorrelationIdTag, request->correlation_id());

  std::string cmd_string;
  cmd.SerializeToString(&cmd_string);
  daml_request->set_command(cmd_string.c_str(), cmd_string.size());
  auto flags = request->flags();
  if (pre_execute_all_requests) {
    flags |= bftEngine::MsgFlag::PRE_PROCESS_FLAG;
  }

  LOG_INFO(logger_,
           "Received DAML commit command cid=" << request->correlation_id());
  if (!pool.send_request_sync(req, flags, *span.get(), resp,
                              request->correlation_id())) {
    LOG_ERROR(logger_, "DAML commit command cid=" << request->correlation_id()
                                                  << " failed");
    return grpc::Status::CANCELLED;
  }

  if (resp.error_response_size() >= 1) {
    LOG_ERROR(logger_, "DAML commit command cid="
                           << request->correlation_id()
                           << " failed with concord error: "
                           << resp.error_response(0).description());
    return grpc::Status::CANCELLED;
  }

  if (!resp.has_daml_response()) {
    return grpc::Status::CANCELLED;
  }

  assert(resp.daml_response().has_command_reply());
  CommandReply cmd_reply;
  if (!cmd_reply.ParseFromString(resp.daml_response().command_reply())) {
    LOG_ERROR(logger_, "Failed to parse DAML/CommandReply cid="
                           << request->correlation_id());
    return grpc::Status::CANCELLED;
  }
  assert(cmd_reply.has_commit());

  reply->CopyFrom(cmd_reply.commit());
  return grpc::Status::OK;
}

bool CommitServiceImpl::IsPreExecuteAllRequestsEnabled(
    const config::ConcordConfiguration& config) {
  const auto kPreExecuteAllRequests = "pre_execute_all_requests";
  return config.hasValue<bool>(kPreExecuteAllRequests) &&
         config.getValue<bool>(kPreExecuteAllRequests);
}

}  // namespace daml
}  // namespace concord
