// Copyright 2018-2019 VMware, all rights reserved

#include "hlf/grpc_services.hpp"
#include "kv_types.hpp"

#include <opentracing/tracer.h>

using com::vmware::concord::hlf::services::HlfKeyValueService;
using com::vmware::concord::hlf::services::KvbMessage;
using com::vmware::concord::hlf::services::KvbMessage_type_INVALID;
using com::vmware::concord::hlf::services::KvbMessage_type_VALID;
using grpc::Server;
using grpc::ServerBuilder;
using grpc::ServerContext;
using grpc::ServerReader;
using grpc::ServerReaderWriter;
using grpc::ServerWriter;

using concord::consensus::KVBClientPool;
using concord::hlf::HlfKvbStorage;
using logging::Logger;
using std::endl;
using std::string;

using com::vmware::concord::ConcordRequest;
using com::vmware::concord::ConcordResponse;
using com::vmware::concord::ErrorResponse;
using com::vmware::concord::HlfRequest;
using com::vmware::concord::HlfRequest_HlfMethod_INSTALL;
using com::vmware::concord::HlfRequest_HlfMethod_INVOKE;
using com::vmware::concord::HlfRequest_HlfMethod_QUERY;
using com::vmware::concord::HlfRequest_HlfMethod_UPGRADE;
using com::vmware::concord::HlfResponse;

namespace concord {
namespace hlf {

// service of chaincode
grpc::Status HlfKeyValueServiceImpl::GetState(ServerContext* context,
                                              const KvbMessage* request,
                                              KvbMessage* response) {
  if (request->key() != "") {
    string value;

    // if the value is not empty, the query is restricted by
    // the upper bound(block number)
    if (request->value() != "") {
      // convert the block number from string in unsigned long type
      kvbc::BlockId block_number = std::stoul(request->value());
      value = kvb_storage_.GetHlfState(request->key(), block_number);
    } else {
      value = kvb_storage_.GetHlfState(request->key());
    }

    LOG_DEBUG(logger_, "[GET] " << request->key() << ":" << value);

    response->set_value(value);
    response->set_state(KvbMessage_type_VALID);
    return grpc::Status::OK;
  } else {
    response->set_state(KvbMessage_type_INVALID);
    return grpc::Status::CANCELLED;
  }
}

grpc::Status HlfKeyValueServiceImpl::PutState(ServerContext* context,
                                              const KvbMessage* request,
                                              KvbMessage* response) {
  if (request->key() != "" && request->value() != "") {
    concordUtils::Status status =
        kvb_storage_.SetHlfState(request->key(), request->value());

    LOG_DEBUG(logger_, "[PUT] " << request->key() << ":" << request->value());

    if (status.isOK()) {
      response->set_state(KvbMessage_type_VALID);
      return grpc::Status::OK;
    }
  }

  response->set_state(KvbMessage_type_INVALID);
  return grpc::Status::CANCELLED;
}

// service of client
grpc::Status HlfChaincodeServiceImpl::TriggerChaincode(
    ServerContext* context, const ConcordRequest* concord_request,
    ConcordResponse* concord_response) {
  // TODO: add correlation ID and/or tracing context from request
  auto span = opentracing::Tracer::Global()->StartSpan("trigger_chaincode");

  if (concord_request->hlf_request_size() == 0) {
    ErrorResponse* e = concord_response->add_error_response();
    e->mutable_description()->assign(
        "Concord request did not contain any HLF requset");
    return grpc::Status::CANCELLED;
  }

  for (int i = 0; i < concord_request->hlf_request_size(); i++) {
    HlfRequest hlf_request = concord_request->hlf_request(i);

    // verify the request
    bool valid_request;
    bool is_read_only = true;

    // variables to store intermediate status
    bool valid_input;
    bool valid_chaincode;

    // default error message
    string error_msg = "Unable to handle request";

    if (hlf_request.type() == "ping") {
      valid_request = true;
      is_read_only = false;
    } else {
      switch (hlf_request.method()) {
        case HlfRequest_HlfMethod_INSTALL:
          is_read_only = false;

          valid_input = IsValidManageOpt(hlf_request);
          valid_chaincode = VerifyChaincodeBytes(hlf_request, error_msg);

          if (!valid_input || !valid_chaincode) {
            valid_request = false;
            ErrorResponse* resp = concord_response->add_error_response();
            resp->set_description(error_msg);
          } else {
            valid_request = true;
          }

          break;

        case HlfRequest_HlfMethod_UPGRADE:
          is_read_only = false;

          valid_input = IsValidManageOpt(hlf_request);
          valid_chaincode = VerifyChaincodeBytes(hlf_request, error_msg);

          if (!valid_input || !valid_chaincode) {
            valid_request = false;
            ErrorResponse* resp = concord_response->add_error_response();
            resp->set_description(error_msg);
          } else {
            valid_request = true;
          }

          break;

        case HlfRequest_HlfMethod_INVOKE:
          valid_request = IsValidInvokeOpt(hlf_request);
          is_read_only = false;
          break;

        case HlfRequest_HlfMethod_QUERY:
          valid_request = IsValidInvokeOpt(hlf_request);
          break;

        default:
          valid_request = false;
          ErrorResponse* e = concord_response->add_error_response();
          e->mutable_description()->assign("HLF Method Not Implemented");
      }
    }
    if (valid_request) {
      ConcordRequest internal_request;
      HlfRequest* internal_hlfRequest = internal_request.add_hlf_request();
      internal_hlfRequest->CopyFrom(hlf_request);

      ConcordResponse internal_response;
      if (pool_.send_request_sync(internal_request, is_read_only, *span.get(),
                                  internal_response)) {
        concord_response->MergeFrom(internal_response);
      } else {
        LOG_ERROR(logger_, "Error parsing response");
        ErrorResponse* resp = concord_response->add_error_response();
        resp->set_description("Internal concord Error");
      }
    }
  }
  return grpc::Status::OK;
}

bool HlfChaincodeServiceImpl::IsValidManageOpt(
    const com::vmware::concord::HlfRequest& request) {
  if (request.has_chaincode_name() && request.has_input() &&
      request.has_version() && request.has_type()) {
    return true;
  }
  return false;
}

bool HlfChaincodeServiceImpl::IsValidInvokeOpt(
    const com::vmware::concord::HlfRequest& request) {
  if (request.has_chaincode_name() && request.has_input()) {
    return true;
  }
  return false;
}

bool HlfChaincodeServiceImpl::VerifyChaincodeBytes(HlfRequest& hlf_request,
                                                   string& msg) {
  if (!hlf_request.has_chaincode_source_bytes()) {
    LOG_ERROR(logger_, "chaincode is empty");
    msg = "Failed to upload since the chaincode field empty";
    return false;
  }

  // check size of chaincode
  if (hlf_request.chaincode_source_bytes().length() >
      HlfKeyValueServiceImpl::kMaxChaincodeBytesize) {
    LOG_ERROR(logger_, "Size of Chaincode file is too large."
                           << " The chaincode size is "
                           << std::to_string(
                                  hlf_request.chaincode_source_bytes().length())
                           << " Bytes");

    msg =
        "Failed to upload because the chaincode size is larger "
        "than MAX allowed size: " +
        std::to_string(HlfKeyValueServiceImpl::kMaxChaincodeBytesize / 1024) +
        " MB";

    return false;
  }

  // check chaincode type
  if (hlf_request.type() != "go") {
    if (hlf_request.type() == "js" || hlf_request.type() == "java")
      msg = hlf_request.type() + " chaincode type is not supported";
    else
      msg = "Unknown chaincode type: " + hlf_request.type();
    return false;
  }

  // define a path to store the chaincode source file
  string chaincode_path = GenerateRandomString(5);
  string chaincode_name = chaincode_path + "." + hlf_request.type();
  boost::filesystem::path chaincode = boost::filesystem::path(chaincode_path) /
                                      boost::filesystem::path(chaincode_name);

  hlf_request.set_path(chaincode.string());

  return true;
}

string HlfChaincodeServiceImpl::GenerateRandomString(int len) {
  static const std::string letter = "abcdefghijklmnopqrstuvwxyz";
  std::string result = "";

  srand(std::time(nullptr));
  for (int i = 0; i < len; i++) {
    int pos = rand() % letter.length();
    result += letter[pos];
  }
  return result;
}

void RunHlfGrpcServer(HlfKvbStorage& kvb_storage,
                      KVBClientPool& kvb_client_pool,
                      string key_value_service_address,
                      string chaincode_service_address) {
  logging::Logger logger;

  logger = Logger::getInstance("concord.hlf");

  // build key value grpc service
  ServerBuilder key_value_service_builder;

  HlfKeyValueServiceImpl* key_value_service =
      new HlfKeyValueServiceImpl(kvb_storage);

  key_value_service_builder.AddListeningPort(key_value_service_address,
                                             grpc::InsecureServerCredentials());

  key_value_service_builder.RegisterService(key_value_service);

  std::unique_ptr<Server> key_value_server(
      key_value_service_builder.BuildAndStart());

  // build chaincode grpc service
  ServerBuilder chaincode_service_builder;

  HlfChaincodeServiceImpl* chaincode_service =
      new HlfChaincodeServiceImpl(kvb_client_pool);

  chaincode_service_builder.AddListeningPort(chaincode_service_address,
                                             grpc::InsecureServerCredentials());

  chaincode_service_builder.RegisterService(chaincode_service);

  std::unique_ptr<Server> chaincode_server(
      chaincode_service_builder.BuildAndStart());

  LOG_INFO(logger,
           "Concord HLF chaincode gRPC service is listening on: "
               << chaincode_service_address << endl
               << " Concord HLF Key Value gRPC service is listening on: "
               << key_value_service_address << endl);

  chaincode_server->Wait();
}

}  // namespace hlf
}  // namespace concord
