// Copyright 2019 VMware, all rights reserved

#include "grpc_services.hpp"

#include <log4cplus/mdc.h>
#include <opentracing/tracer.h>
#include <string>

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

  log4cplus::getMDC().put("cid", request->correlation_id());
  auto span = opentracing::Tracer::Global()->StartSpan("commit_transaction");

  std::string cmd_string;
  cmd.SerializeToString(&cmd_string);
  daml_request->set_command(cmd_string.c_str(), cmd_string.size());

  if (!pool.send_request_sync(req, false /* read-only */, *span.get(), resp)) {
    LOG4CPLUS_ERROR(logger_, "DAML commit transaction failed");
    log4cplus::getMDC().clear();
    return grpc::Status::CANCELLED;
  }

  if (resp.error_response_size() >= 1) {
    LOG4CPLUS_ERROR(logger_,
                    "DAML commit transaction failed with concord error: "
                        << resp.error_response(0).description());
    log4cplus::getMDC().clear();
    return grpc::Status::CANCELLED;
  }

  if (!resp.has_daml_response()) {
    log4cplus::getMDC().clear();
    return grpc::Status::CANCELLED;
  }

  assert(resp.daml_response().has_command_reply());
  CommandReply cmd_reply;
  if (!cmd_reply.ParseFromString(resp.daml_response().command_reply())) {
    LOG4CPLUS_ERROR(logger_, "Failed to parse DAML/CommandReply");
    log4cplus::getMDC().clear();
    return grpc::Status::CANCELLED;
  }
  assert(cmd_reply.has_commit());

  reply->CopyFrom(cmd_reply.commit());
  log4cplus::getMDC().clear();
  return grpc::Status::OK;
}

}  // namespace daml
}  // namespace concord
