// Copyright 2018 VMware, all rights reserved
//
// Send a transaction to concord directly.

#include <boost/filesystem.hpp>
#include <boost/program_options.hpp>

#include <google/protobuf/text_format.h>
#include <grpc/support/log.h>
#include <grpcpp/grpcpp.h>
#include <inttypes.h>
#include <stdlib.h>
#include <fstream>
#include <iostream>
#include "concmdconn.hpp"
#include "concmdex.hpp"
#include "concmdfmt.hpp"
#include "concmdopt.hpp"
#include "concord.pb.h"
#include "hlf_services.grpc.pb.h"

using namespace boost::program_options;
using namespace com::vmware::concord;

using com::vmware::concord::hlf::services::HlfChaincodeService;
using grpc::Channel;
using grpc::ClientContext;
using grpc::Status;

#define OPT_METHOD "method"
#define OPT_CHAINCODE_NAME "chaincode"
#define OPT_INPUT "input"
#define OPT_VERSION "version"
#define OPT_FILE "file"
#define OPT_CHAINCODE_TYPE "type"

const static std::string kChannelName = "mychannel";

void add_options(options_description &desc) {
  desc.add_options()(OPT_METHOD ",m", value<std::string>(), "Chaincode method")(
      OPT_CHAINCODE_NAME ",c", value<std::string>(), "Chaincode name")(
      OPT_CHAINCODE_TYPE ",t", value<std::string>(), "Chaincode type")(
      OPT_INPUT ",i", value<std::string>(), "Parameters for calling chaincode")(
      OPT_VERSION ",v", value<std::string>(), "Chaincode Version")(
      OPT_FILE ",f", value<std::string>(), "Chaincode source file");
}

ConcordRequest ConstructConcordRequest(std::string method, std::string c_name,
                                       std::string c_type, std::string input,
                                       std::string version, std::string file);

int SendConcordRequest(ConcordRequest &, std::string service_address);

std::string GenerateRandomString(int len);

class ConcordClient {
 public:
  ConcordClient(std::shared_ptr<Channel> channel)
      : stub_(HlfChaincodeService::NewStub(channel)) {}

  grpc::Status TriggerChaincode(const ConcordRequest &concordRequest,
                                ConcordResponse &concordResponse) {
    ClientContext context;

    return stub_->TriggerChaincode(&context, concordRequest, &concordResponse);
  }

 private:
  std::unique_ptr<HlfChaincodeService::Stub> stub_;
};

int main(int argc, char *argv[]) {
  try {
    variables_map opts;
    if (!parse_options(argc, argv, &add_options, opts)) {
      return 0;
    }
    // init grpc client
    std::string grpc_service_address;
    std::string addr = opts[OPT_ADDRESS].as<std::string>();
    std::string port = opts[OPT_PORT].as<std::string>();
    grpc_service_address = addr + ":" + port;

    std::string method, c_name, c_type, input, version, file;

    // For upload method, the path of chaincode stored in remote Concord will
    // be: OPT_INPUT + '/' +  OPT_CHAINCODE
    if (opts.count(OPT_CHAINCODE_NAME) > 0) {
      c_name = opts[OPT_CHAINCODE_NAME].as<std::string>();
    } else {
      std::cout << "Empty chaincode name" << std::endl;
      return 0;
    }

    if (opts.count(OPT_INPUT) > 0) {
      input = opts[OPT_INPUT].as<std::string>();
    }

    if (opts.count(OPT_VERSION) > 0) {
      version = opts[OPT_VERSION].as<std::string>();
    } else {
      // set default version to "1"
      // version is only useful in the install and upgrade
      version = "1";
    }

    if (opts.count(OPT_CHAINCODE_TYPE) > 0) {
      c_type = opts[OPT_CHAINCODE_TYPE].as<std::string>();
    } else {
      // set default chaincode type to golang
      c_type = "go";
    }

    if (opts.count(OPT_FILE) > 0) {
      file = opts[OPT_FILE].as<std::string>();
    }

    if (opts.count(OPT_METHOD) > 0) {
      method = opts[OPT_METHOD].as<std::string>();
    } else {
      std::cout << "Need to provide chaincode method (install, "
                   "query, invoke, upgrade)"
                << std::endl;
      return 0;
    }

    ConcordRequest conc_req =
        ConstructConcordRequest(method, c_name, c_type, input, version, file);
    return SendConcordRequest(conc_req, grpc_service_address);

  } catch (std::exception &e) {
    std::cerr << "Exception: " << e.what() << std::endl;
    return -1;
  }
}

ConcordRequest ConstructConcordRequest(std::string type, std::string c_name,
                                       std::string c_type, std::string input,
                                       std::string version, std::string file) {
  ConcordRequest conc_req;
  HlfRequest *hlf_req = conc_req.add_hlf_request();

  HlfRequest_HlfMethod method;

  if (type == "install") {
    method = HlfRequest_HlfMethod_INSTALL;
  } else if (type == "upgrade") {
    method = HlfRequest_HlfMethod_UPGRADE;
  } else if (type == "invoke") {
    method = HlfRequest_HlfMethod_INVOKE;
  } else if (type == "query") {
    method = HlfRequest_HlfMethod_QUERY;
  } else {
    std::cout << "Unknown chaincode method" << std::endl;
    exit(EXIT_SUCCESS);
  }

  // hard code channel name here
  hlf_req->set_chain_id(kChannelName);

  hlf_req->set_method(method);
  hlf_req->set_chaincode_name(c_name);
  hlf_req->set_type(c_type);
  hlf_req->set_input(input);
  hlf_req->set_version(version);

  // read raw byte from the file
  if (file != "") {
    std::ifstream f(file, std::ios::in | std::ios::binary);

    if (!f.is_open()) {
      std::cout << "Unable to read " << file << std::endl;
      exit(EXIT_SUCCESS);
    }

    f.seekg(0, f.end);
    long length = f.tellg();
    f.seekg(0, f.beg);

    char *buffer = new char[length];
    f.read(buffer, length);

    hlf_req->set_chaincode_source_bytes(buffer, length);

    delete[] buffer;
  }
  return conc_req;
}

int SendConcordRequest(ConcordRequest &conc_req, std::string service_address) {
  ConcordClient concordClient(
      grpc::CreateChannel(service_address, grpc::InsecureChannelCredentials()));

  ConcordResponse conc_resp;
  std::string pbtext;
  google::protobuf::TextFormat::PrintToString(conc_req, &pbtext);
  std::cout << "Message Prepared: " << pbtext << std::endl;

  if (concordClient.TriggerChaincode(conc_req, conc_resp).ok()) {
    google::protobuf::TextFormat::PrintToString(conc_resp, &pbtext);
    std::cout << "Received response: " << pbtext << std::endl;

    // Handle Response

    if (conc_resp.hlf_response_size() == 1) {
      HlfResponse hlf_resp = conc_resp.hlf_response(0);
      if (hlf_resp.has_data()) {
        std::cout << "Transaction Receipt: " << hlf_resp.data() << std::endl;
      } else {
        std::cerr << "HlfResponse has no data" << std::endl;
        return -1;
      }
    } else if (conc_resp.error_response_size() == 1) {
      ErrorResponse errorResp = conc_resp.error_response(0);
      if (errorResp.has_description()) {
        std::cout << "Error Response: " << errorResp.description() << std::endl;
        return -1;
      } else {
        std::cout << "Error response had no description" << std::endl;
        return -1;
      }
    } else {
      std::cerr << "Wrong number of hlf_responses ("
                << conc_resp.hlf_response_size() << ") or errors ("
                << conc_resp.error_response_size() << ")"
                << " (expected 1)" << std::endl;
      return -1;
    }
  } else {
    std::cout << "Failed to call gRPC service" << std::endl;
    return -1;
  }
  return 0;
}
