// Copyright 2019 VMware, all rights reserved

#include "grpc_services.hpp"

#include <opentracing/tracer.h>
#include <sstream>
#include <string>
#include <strstream>
#include "utils/concord_logging.hpp"
#include "utils/open_tracing_utils.hpp"

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
  LOG4CPLUS_DEBUG(logger_, "CommitService: Transactions...");

  ConcordResponse resp;
  ConcordRequest req;
  DamlRequest* daml_request = req.mutable_daml_request();

  Command cmd;
  cmd.mutable_commit()->CopyFrom(*request);

  concord::utils::RAIIMDC mdc("cid", request->correlation_id());

  auto span =
      concord::utils::ExtractSpan(request->span_context(), "commit_transaction",
                                  logger_, request->correlation_id());
  std::string cmd_string;
  cmd.SerializeToString(&cmd_string);
  daml_request->set_command(cmd_string.c_str(), cmd_string.size());
  if (!pool.send_request_sync(req, request->flags(), *span.get(), resp,
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
