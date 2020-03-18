// Copyright 2019 VMware, all rights reserved

#include "grpc_services.hpp"

#include <opentracing/tracer.h>
#include <sstream>
#include <string>
#include <strstream>
#include "utils/concord_logging.hpp"

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

static std::unique_ptr<opentracing::Span> CreateSpan(
    const CommitRequest* request, log4cplus::Logger logger) {
  if (!request->span_context().empty()) {
    std::istringstream context_stream{request->span_context()};
    auto parent_span_context =
        opentracing::Tracer::Global()->Extract(context_stream);
    if (parent_span_context.has_value()) {
      return opentracing::Tracer::Global()->StartSpan(
          "commit_transaction",
          {opentracing::ChildOf(parent_span_context->get())});
    }
    LOG4CPLUS_DEBUG(logger, "CreateSpan: Failed to initialize parent span from"
                                << request->span_context());
  }
  LOG4CPLUS_DEBUG(logger,
                  "CreateSpan: No parent span provided, initialize a new one");
  return opentracing::Tracer::Global()->StartSpan(
      "commit_transaction",
      {opentracing::SetTag{"cid", request->correlation_id()}});
}

namespace concord {
namespace daml {

grpc::Status CommitServiceImpl::CommitTransaction(ServerContext* context,
                                                  const CommitRequest* request,

                                                  CommitResponse* reply) {
  LOG4CPLUS_DEBUG(logger_, "CommitService: Transactions...");

  ConcordResponse resp;
  ConcordRequest req;
  DamlRequest* daml_request = req.mutable_daml_request();

  Command cmd;
  cmd.mutable_commit()->CopyFrom(*request);

  concord::utils::RAIIMDC mdc("cid", request->correlation_id());

  auto span = CreateSpan(request, logger_);

  std::string cmd_string;
  cmd.SerializeToString(&cmd_string);
  daml_request->set_command(cmd_string.c_str(), cmd_string.size());
  if (!pool.send_request_sync(req, false /* read-only */, *span.get(), resp,
                              request->correlation_id())) {
    LOG4CPLUS_ERROR(logger_, "DAML commit transaction failed");
    return grpc::Status::CANCELLED;
  }

  if (resp.error_response_size() >= 1) {
    LOG4CPLUS_ERROR(logger_,
                    "DAML commit transaction failed with concord error: "
                        << resp.error_response(0).description());
    return grpc::Status::CANCELLED;
  }

  if (!resp.has_daml_response()) {
    return grpc::Status::CANCELLED;
  }

  assert(resp.daml_response().has_command_reply());
  CommandReply cmd_reply;
  if (!cmd_reply.ParseFromString(resp.daml_response().command_reply())) {
    LOG4CPLUS_ERROR(logger_, "Failed to parse DAML/CommandReply");
    return grpc::Status::CANCELLED;
  }
  assert(cmd_reply.has_commit());

  reply->CopyFrom(cmd_reply.commit());
  return grpc::Status::OK;
}

}  // namespace daml
}  // namespace concord
