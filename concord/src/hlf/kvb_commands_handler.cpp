// Copyright 2018-2019 VMware, all rights reserved

#include "hlf/kvb_commands_handler.hpp"
#include <boost/predef/detail/endian_compat.h>
#include <google/protobuf/text_format.h>
#include <fstream>
#include <iterator>
#include <vector>
#include "common/concord_exception.hpp"
#include "concord.pb.h"
#include "config/configuration_manager.hpp"
#include "utils/concord_eth_hash.hpp"

using namespace boost::program_options;

// Protobuf interface
using namespace com::vmware::concord;

using boost::filesystem::create_directories;
using boost::filesystem::exists;
using boost::filesystem::remove_all;

using concord::common::BlockNotFoundException;
using concord::common::HexPrintBytes;
using concord::common::TransactionNotFoundException;
using concord::common::zero_address;
using concord::common::zero_hash;
using concord::common::operator<<;
using concord::consensus::Status;
using concord::hlf::ChaincodeInvoker;
using concord::hlf::HlfKvbStorage;
using concord::storage::IBlocksAppender;
using concord::storage::ILocalKeyValueStorageReadOnly;

namespace concord {
namespace hlf {

HlfKvbCommandsHandler::HlfKvbCommandsHandler(
    ChaincodeInvoker* chaincode_invoker,
    const concord::config::ConcordConfiguration& config,
    concord::config::ConcordConfiguration& node_config,
    ILocalKeyValueStorageReadOnly* ptr_ro_storage,
    IBlocksAppender* ptr_block_appender)
    : logger_(log4cplus::Logger::getInstance("com.vmware.concord.hlf.handler")),
      chaincode_invoker_(chaincode_invoker),
      node_config_(node_config),
      ptr_ro_storage_(ptr_ro_storage),
      ptr_block_appender_(ptr_block_appender) {
  assert(ptr_block_appender_);
  assert(ptr_ro_storage_);
}

HlfKvbCommandsHandler::~HlfKvbCommandsHandler() {
  // no other deinitialization necessary
}

int HlfKvbCommandsHandler::execute(uint16_t client_id, uint64_t sequence_num,
                                   bool read_only, uint32_t request_size,
                                   const char* request, uint32_t max_reply_size,
                                   char* out_reply,
                                   uint32_t& out_actual_reply_size) {
  bool res;
  if (read_only) {
    res = ExecuteReadOnlyCommand(request_size, request, *ptr_ro_storage_,
                                 max_reply_size, out_reply,
                                 out_actual_reply_size);
  } else {
    res = ExecuteCommand(request_size, request, sequence_num, *ptr_ro_storage_,
                         *ptr_block_appender_, max_reply_size, out_reply,
                         out_actual_reply_size);
  }

  return res ? 0 : 1;
}

// Callback from SBFT/KVB. Process the request Returns
// false if the command is illegal or invalid; true otherwise.
bool HlfKvbCommandsHandler::ExecuteReadOnlyCommand(
    uint32_t request_size, const char* request,
    const ILocalKeyValueStorageReadOnly& ro_storage,
    const size_t max_reply_size, char* out_reply,
    uint32_t& out_reply_size) const {
  HlfKvbStorage kvb_hlf_storage(ro_storage);

  ConcordRequest command;
  bool result;
  ConcordResponse command_response;
  if (command.ParseFromArray(request, request_size)) {
    if (command.hlf_request_size() > 0) {
      result =
          HandleHlfRequestReadOnly(command, &kvb_hlf_storage, command_response);
    } else {
      std::string pbtext;
      google::protobuf::TextFormat::PrintToString(command, &pbtext);
      LOG4CPLUS_ERROR(logger_, "Unknown read-only command: " << pbtext);
      ErrorResponse* resp = command_response.add_error_response();
      resp->set_description("Internal concord Error");
      result = false;
    }
  } else {
    LOG4CPLUS_ERROR(logger_, "Unable to parse read-only command: "
                                 << (HexPrintBytes{request, request_size}));
    ErrorResponse* resp = command_response.add_error_response();
    resp->set_description("Internal concord Error");
    result = false;
  }

  if (command_response.SerializeToArray(out_reply, max_reply_size)) {
    out_reply_size = command_response.ByteSize();
  } else {
    LOG4CPLUS_ERROR(logger_, "Reply is too large");
    out_reply_size = 0;
  }

  return result;
}

// Callback from SBFT/KVB. Process the request Returns
// false if the command is illegal or invalid; true otherwise.
bool HlfKvbCommandsHandler::ExecuteCommand(
    uint32_t request_size, const char* request, const uint64_t sequence_num,
    const ILocalKeyValueStorageReadOnly& ro_storage,
    IBlocksAppender& block_appender, const size_t max_reply_size,
    char* out_reply, uint32_t& out_reply_size) const {
  HlfKvbStorage kvb_hlf_storage(ro_storage, &block_appender, sequence_num);

  ConcordRequest command;
  bool result;
  ConcordResponse command_response;
  if (command.ParseFromArray(request, request_size)) {
    if (command.hlf_request_size() > 0) {
      // pass the addr of kvb_hlf_storage
      result = HandleHlfRequest(command, &kvb_hlf_storage, command_response);
    } else {
      // SBFT may decide to try one of our read-only commands in read-write
      // mode, for example if it has failed several times. So, go check the
      // read-only list if othing matched here.
      LOG4CPLUS_DEBUG(logger_, "Unknown read-write command. Trying read-only.");
      return ExecuteReadOnlyCommand(request_size, request, ro_storage,
                                    max_reply_size, out_reply, out_reply_size);
    }
  } else {
    LOG4CPLUS_ERROR(logger_, "Unable to parse command: "
                                 << (HexPrintBytes{request, request_size}));
    ErrorResponse* resp = command_response.add_error_response();
    resp->set_description("Internal concord Error");
    result = false;
  }

  if (command_response.SerializeToArray(out_reply, max_reply_size)) {
    out_reply_size = command_response.ByteSize();
  } else {
    LOG4CPLUS_ERROR(logger_, "Reply is too large");
    out_reply_size = 0;
  }

  return result;
}

//  Handle a HLF gRPC request. Return false if the command was invalid
bool HlfKvbCommandsHandler::HandleHlfRequest(
    ConcordRequest& command, HlfKvbStorage* kvb_hlf_storage,
    ConcordResponse& command_response) const {
  LOG4CPLUS_DEBUG(logger_, "Triggered the chaincode invoke operation");
  switch (command.hlf_request(0).method()) {
    case HlfRequest_HlfMethod_INSTALL:
      return HandleHlfInstallChaincode(command, kvb_hlf_storage,
                                       command_response);

    case HlfRequest_HlfMethod_UPGRADE:
      return HandleHlfUpgradeChaincode(command, kvb_hlf_storage,
                                       command_response);

    case HlfRequest_HlfMethod_INVOKE:
      return HandleHlfInvokeChaincode(command, kvb_hlf_storage,
                                      command_response);

    // handle read only
    default:
      return HandleHlfRequestReadOnly(command, kvb_hlf_storage,
                                      command_response);
  }
}

bool HlfKvbCommandsHandler::HandleHlfRequestReadOnly(
    ConcordRequest& command, HlfKvbStorage* kvb_hlf_storage,
    ConcordResponse& command_response) const {
  switch (command.hlf_request(0).method()) {
    case HlfRequest_HlfMethod_QUERY:
      LOG4CPLUS_INFO(logger_, "Triggered the chaincode query operation");
      return HandleHlfQueryChaincode(command, command_response);

    default:
      ErrorResponse* e = command_response.add_error_response();
      e->mutable_description()->assign("HLF Method Not Implemented");
      return false;
  }
}

Status HlfKvbCommandsHandler::StorageUpdate(
    const HlfRequest& request, HlfKvbStorage* kvb_hlf_storage) const {
  Status status_transaction = kvb_hlf_storage->AddHlfTransaction(request);
  Status status_block = kvb_hlf_storage->WriteHlfBlock();
  if (status_transaction.isOK() && status_block.isOK()) {
    return Status::OK();
  } else {
    LOG4CPLUS_ERROR(logger_, "Failed to write block");
    return Status::NotFound("undable to update storage");
  }
}

bool HlfKvbCommandsHandler::HandleHlfInstallChaincode(
    ConcordRequest& command, HlfKvbStorage* kvb_hlf_storage,
    ConcordResponse& command_response) const {
  // fetch the first hlf request
  const HlfRequest request = command.hlf_request(0);
  HlfResponse* response = command_response.add_hlf_response();

  // set default response
  string data = "Failed to install chaincode";
  int status = 1;

  Status install_status = HandleIntermediateChaincodeFile(request, data);

  Status instantiate_status = chaincode_invoker_->SendInstantiate(
      request.chaincode_name(), request.input(), request.version());

  if (install_status.isOK() && instantiate_status.isOK() &&
      StorageUpdate(request, kvb_hlf_storage).isOK()) {
    data = "Successfully installed chaincode: " + request.chaincode_name();
    status = 0;
  } else {
    // clean the cache
    kvb_hlf_storage->reset();
  }

  response->set_status(status);
  response->set_data(data);

  return true;
}

bool HlfKvbCommandsHandler::HandleHlfUpgradeChaincode(
    ConcordRequest& command, HlfKvbStorage* kvb_hlf_storage,
    ConcordResponse& command_response) const {
  // fetch the first hlf request
  const HlfRequest request = command.hlf_request(0);
  HlfResponse* response = command_response.add_hlf_response();

  // set default response
  string data = "Failed to upgrade chaincode";
  int status = 1;

  Status install_status = HandleIntermediateChaincodeFile(request, data);

  Status upgrade_status = chaincode_invoker_->SendUpgrade(
      request.chaincode_name(), request.input(), request.version());

  if (install_status.isOK() && upgrade_status.isOK() &&
      StorageUpdate(request, kvb_hlf_storage).isOK()) {
    data = "Successfully upgraded chaincode: " + request.chaincode_name();
    status = 0;
  } else {
    // clean the cache
    kvb_hlf_storage->reset();
  }

  response->set_status(status);
  response->set_data(data);

  return true;
}

bool HlfKvbCommandsHandler::HandleHlfInvokeChaincode(
    ConcordRequest& command, HlfKvbStorage* kvb_hlf_storage,
    ConcordResponse& command_response) const {
  // fetch the first hlf request
  const HlfRequest request = command.hlf_request(0);
  HlfResponse* response = command_response.add_hlf_response();

  // set default response
  string data = "Failed to invoke chaincode";
  int status = 1;

  Status invoke_status =
      chaincode_invoker_->SendInvoke(request.chaincode_name(), request.input());

  if (invoke_status.isOK() && StorageUpdate(request, kvb_hlf_storage).isOK()) {
    data = "Successfully invoked chaincode: " + request.chaincode_name() +
           " with input: " + request.input();
    status = 0;
  } else {
    // clean the cache
    kvb_hlf_storage->reset();
  }

  response->set_status(status);
  response->set_data(data);

  return true;
}

// should be read-only
bool HlfKvbCommandsHandler::HandleHlfQueryChaincode(
    ConcordRequest& command, ConcordResponse& command_response) const {
  // fetch the first hlf request
  const HlfRequest request = command.hlf_request(0);
  HlfResponse* response = command_response.add_hlf_response();

  // set default response
  string data = "Failed to query chaincode";
  int status = 1;

  string result =
      chaincode_invoker_->SendQuery(request.chaincode_name(), request.input());

  if (result != "") {
    data = "Successfully queried chaincode: " + request.chaincode_name() +
           "\n The input is : " + request.input() +
           "\n The result is : " + result;
    status = 0;
  }

  response->set_status(status);
  response->set_data(data);

  return true;
}

Status HlfKvbCommandsHandler::HandleIntermediateChaincodeFile(
    const HlfRequest& request, string& error_msg) const {
  // write chaincode raw bytes to local disk
  // chaincode in golang need to be stored under $GOPATH/src/
  boost::filesystem::path chaincode_file;
  boost::filesystem::path golang_chaincode_path =
      boost::filesystem::path(GetChaincodePath());
  boost::filesystem::path relative_file_path =
      boost::filesystem::path(request.path());
  chaincode_file = golang_chaincode_path / relative_file_path;

  LOG4CPLUS_DEBUG(logger_, "Writting temporary chaincode file to: "
                               << chaincode_file.string());

  const char* content = request.chaincode_source_bytes().c_str();
  size_t content_length = request.chaincode_source_bytes().length();

  // initial default reponse
  Status status = Status::GeneralError("");

  // Handle the chaincode upload:
  // 1. check if the directory does not exist
  // 2. check if create directory is success
  // 3. check if write to file is success
  // 4. check if update kv is success
  if (!exists(chaincode_file.parent_path())) {
    if (create_directories(chaincode_file.parent_path())) {
      std::ofstream output_file(chaincode_file.string(), std::ios::binary);
      if (output_file.is_open()) {
        output_file.write(content, content_length);
        output_file.close();

        status = Status::OK();
        LOG4CPLUS_DEBUG(logger_, "Successfully uploaded chaincode "
                                     << chaincode_file.string());
      } else {
        error_msg = "Unable to write chaincode file.";
      }
    } else {
      error_msg = "Unable to create directory.";
    }
  } else {
    error_msg =
        "Unable to find a path to store temporary chaincode file, "
        "please try again later";
  }

  // Install the chaincode to HLF peer if upload is success:
  if (status.isOK()) {
    Status install_status = chaincode_invoker_->SendInstall(
        request.chaincode_name(), relative_file_path.parent_path().string(),
        request.version());

    if (!install_status.isOK()) {
      status = Status::GeneralError("");
      error_msg = "Unable to install chaincode to HLF peer";
    }
  }

  // remove temporary directory
  remove_all(chaincode_file.parent_path());

  return status;
}

const string HlfKvbCommandsHandler::GetChaincodePath() const {
  return node_config_.getValue<string>("hlf_chaincode_path");
}

}  // namespace hlf
}  // namespace concord
