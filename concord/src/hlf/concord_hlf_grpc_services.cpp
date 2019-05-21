// Copyright 2018-2019 VMware, all rights reserved

#include "concord_hlf_grpc_services.hpp"

using com::vmware::concord::hlf::services::GrpcService;
using com::vmware::concord::hlf::services::KvbMessage;
using com::vmware::concord::hlf::services::KvbMessage_type_INVALID;
using com::vmware::concord::hlf::services::KvbMessage_type_VALID;
using grpc::Server;
using grpc::ServerBuilder;
using grpc::ServerContext;
using grpc::ServerReader;
using grpc::ServerReaderWriter;
using grpc::ServerWriter;
using grpc::Status;

using concord::consensus::KVBClientPool;
using concord::hlf::HlfHandler;
using log4cplus::Logger;
using std::string;

using com::vmware::concord::ConcordRequest;
using com::vmware::concord::ConcordResponse;
using com::vmware::concord::ErrorResponse;
using com::vmware::concord::HlfRequest;
using com::vmware::concord::HlfRequest_HlfMethod_INSTALL;
using com::vmware::concord::HlfRequest_HlfMethod_INSTANTIATE;
using com::vmware::concord::HlfRequest_HlfMethod_INVOKE;
using com::vmware::concord::HlfRequest_HlfMethod_QUERY;
using com::vmware::concord::HlfRequest_HlfMethod_UPGRADE;
using com::vmware::concord::HlfResponse;

namespace concord {
namespace hlf {

Status GrpcServiceImpl::GetState(ServerContext* context,
                                 const KvbMessage* request,
                                 KvbMessage* response) {
  if (request->key() != "") {
    string value = _hlfHandler->getState(request->key());

    LOG4CPLUS_DEBUG(_logger, "[GET] " << request->key() << ":" << value);

    response->set_value(value);
    response->set_state(KvbMessage_type_VALID);
    return Status::OK;
  } else {
    response->set_state(KvbMessage_type_INVALID);
    return Status::CANCELLED;
  }
}

Status GrpcServiceImpl::PutState(ServerContext* context,
                                 const KvbMessage* request,
                                 KvbMessage* response) {
  if (request->key() != "" and request->value() != "") {
    Blockchain::Status status =
        _hlfHandler->putState(request->key(), request->value());

    LOG4CPLUS_DEBUG(_logger,
                    "[PUT] " << request->key() << ":" << request->value());

    if (status.isOK()) {
      response->set_state(KvbMessage_type_VALID);
      return Status::OK;
    }
  }

  response->set_state(KvbMessage_type_INVALID);
  return Status::CANCELLED;
}

// WriteBlock is only called by Client
Status GrpcServiceImpl::WriteBlock(ServerContext* context,
                                   const KvbMessage* request,
                                   KvbMessage* response) {
  Blockchain::Status status = _hlfHandler->writeBlock();
  if (status.isOK()) {
    response->set_state(KvbMessage_type_VALID);
    return Status::OK;
  } else {
    response->set_state(KvbMessage_type_INVALID);
    return Status::CANCELLED;
  }
}

Status GrpcServiceImpl::TriggerChaincode(ServerContext* context,
                                         const ConcordRequest* concordRequest,
                                         ConcordResponse* concordResponse) {
  if (concordRequest->hlf_request_size() == 0) {
    ErrorResponse* e = concordResponse->add_error_response();
    e->mutable_description()->assign(
        "Concord request did not contain any HLF requset");
    return Status::CANCELLED;
  }

  for (int i = 0; i < concordRequest->hlf_request_size(); i++) {
    const HlfRequest hlfRequest = concordRequest->hlf_request(i);

    // verify the input
    bool validRequest;
    bool isReadOnly = true;

    switch (hlfRequest.method()) {
      case HlfRequest_HlfMethod_INSTALL:
        validRequest = isValidManageOpt(hlfRequest);
        isReadOnly = false;
        break;

      case HlfRequest_HlfMethod_INSTANTIATE:
        validRequest = isValidManageOpt(hlfRequest);
        isReadOnly = false;
        break;

      case HlfRequest_HlfMethod_UPGRADE:
        validRequest = isValidManageOpt(hlfRequest);
        isReadOnly = false;
        break;

      case HlfRequest_HlfMethod_INVOKE:
        validRequest = isValidInvokeOpt(hlfRequest);
        isReadOnly = false;
        break;

      case HlfRequest_HlfMethod_QUERY:
        validRequest = isValidInvokeOpt(hlfRequest);
        break;

      default:
        validRequest = false;
        ErrorResponse* e = concordResponse->add_error_response();
        e->mutable_description()->assign("HLF Method Not Implemented");
    }

    if (validRequest) {
      ConcordRequest internalRequest;
      HlfRequest* internalHlfRequest = internalRequest.add_hlf_request();
      internalHlfRequest->CopyFrom(hlfRequest);

      ConcordResponse internalResponse;
      if (_pool.send_request_sync(internalRequest, isReadOnly,
                                  internalResponse)) {
        concordResponse->MergeFrom(internalResponse);
      } else {
        LOG4CPLUS_ERROR(_logger, "Error parsing response");
        ErrorResponse* resp = concordResponse->add_error_response();
        resp->set_description("Internal concord Error");
      }
    }
  }
  return Status::OK;
}

bool GrpcServiceImpl::isValidManageOpt(
    const com::vmware::concord::HlfRequest& request) {
  if (request.has_chaincode_name() && request.has_input() &&
      request.has_version()) {
    return true;
  }
  return false;
}

bool GrpcServiceImpl::isValidInvokeOpt(
    const com::vmware::concord::HlfRequest& request) {
  if (request.has_chaincode_name() && request.has_input()) {
    return true;
  }
  return false;
}

void RunHlfServer(HlfHandler* hlfHandler, KVBClientPool& pool) {
  // get server address from the HLF handler
  string server_address = hlfHandler->getConcordKvService();

  ServerBuilder builder;
  GrpcServiceImpl* service = new GrpcServiceImpl(hlfHandler, pool);
  builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
  builder.RegisterService(service);
  std::unique_ptr<Server> server(builder.BuildAndStart());

  log4cplus::Logger logger;
  logger = Logger::getInstance("com.vmware.concord.hlf");
  LOG4CPLUS_INFO(
      logger, "Concord HLF gRPC service is listening on: " << server_address);

  server->Wait();
}
}  // namespace hlf
}  // namespace concord
