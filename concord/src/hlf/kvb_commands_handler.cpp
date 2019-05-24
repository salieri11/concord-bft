// Copyright 2018-2019 VMware, all rights reserved

#include "hlf/kvb_commands_handler.hpp"
#include <boost/predef/detail/endian_compat.h>
#include <google/protobuf/text_format.h>
#include <iterator>
#include <vector>
#include "common/concord_exception.hpp"
#include "concord.pb.h"
#include "config/configuration_manager.hpp"
#include "consensus/kvb/BlockchainInterfaces.h"
#include "utils/concord_eth_hash.hpp"

using namespace boost::program_options;

// Protobuf interface
using namespace com::vmware::concord;

using concord::common::BlockNotFoundException;
using concord::common::HexPrintBytes;
using concord::common::TransactionNotFoundException;
using concord::common::zero_address;
using concord::common::zero_hash;
using concord::common::operator<<;

using Blockchain::IBlocksAppender;
using Blockchain::ILocalKeyValueStorageReadOnly;
using Blockchain::Status;
using concord::hlf::HlfKvbStorage;

using concord::hlf::HlfHandler;

namespace concord {
namespace hlf {

HlfKvbCommandsHandler::HlfKvbCommandsHandler(
    HlfHandler* hlf_handler,
    const concord::config::ConcordConfiguration& config,
    concord::config::ConcordConfiguration& node_config,
    Blockchain::ILocalKeyValueStorageReadOnly* ptr_ro_storage,
    Blockchain::IBlocksAppender* ptr_block_appender)
    : logger_(log4cplus::Logger::getInstance("com.vmware.concord.hlf.handler")),
      hlf_handler_(hlf_handler),
      config_(config),
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
      LOG4CPLUS_INFO(logger_, "Unknown read-write command. Trying read-only.");
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
  LOG4CPLUS_INFO(logger_, "Triggered the chaincode invoke operation");
  switch (command.hlf_request(0).method()) {
    case HlfRequest_HlfMethod_INSTALL:
      return HandleHlfInstallChaincode(command, kvb_hlf_storage,
                                       command_response);

    case HlfRequest_HlfMethod_INSTANTIATE:
      return HandleHlfInstantiateChaincode(command, kvb_hlf_storage,
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
  LOG4CPLUS_INFO(logger_, "Triggered the chaincode query operation");
  switch (command.hlf_request(0).method()) {
    case HlfRequest_HlfMethod_QUERY:
      return HandleHlfQueryChaincode(command, kvb_hlf_storage,
                                     command_response);

    default:
      ErrorResponse* e = command_response.add_error_response();
      e->mutable_description()->assign("HLF Method Not Implemented");
      return false;
  }
}

bool HlfKvbCommandsHandler::HandleHlfInstallChaincode(
    ConcordRequest& command, HlfKvbStorage* kvb_hlf_storage,
    ConcordResponse& command_response) const {
  // fetch the first hlf request
  const HlfRequest request = command.hlf_request(0);
  HlfResponse* response = command_response.add_hlf_response();

  if (hlf_handler_->SetKvbHlfStoragePointer(kvb_hlf_storage).isOK()) {
    // input stores path
    if (request.has_chaincode_name() && request.has_input() &&
        request.has_version()) {
      kvb_hlf_storage->AddHlfTransaction(request);

      Status status = hlf_handler_->InstallChaincode(
          request.chaincode_name(), request.input(), request.version());

      // revoke KVB storage pointer
      hlf_handler_->RevokeKvbHlfStoragePointer();

      if (status.isOK()) {
        response->set_data("Successfully installed chaincode: " +
                           request.chaincode_name());
        // 0 indeicate request is valid
        response->set_status(0);
        return true;
      }
    }
  }

  response->set_status(1);
  response->set_data("Failed to install chaincode");

  return true;
}

bool HlfKvbCommandsHandler::HandleHlfInstantiateChaincode(
    ConcordRequest& command, HlfKvbStorage* kvb_hlf_storage,
    ConcordResponse& command_response) const {
  // fetch the first hlf request
  const HlfRequest request = command.hlf_request(0);
  HlfResponse* response = command_response.add_hlf_response();

  if (hlf_handler_->SetKvbHlfStoragePointer(kvb_hlf_storage).isOK()) {
    if (request.has_chaincode_name() && request.has_input() &&
        request.has_version()) {
      kvb_hlf_storage->AddHlfTransaction(request);

      Status status = hlf_handler_->InstantiateChaincode(
          request.chaincode_name(), request.input(), request.version());

      // revoke KVB storage pointer
      hlf_handler_->RevokeKvbHlfStoragePointer();

      if (status.isOK()) {
        response->set_data("Successfully instantiated chaincode: " +
                           request.chaincode_name());
        response->set_status(0);
        return true;
      }
    }
  }

  response->set_status(1);
  response->set_data("Failed to instantiate chaincode");

  return true;
}

bool HlfKvbCommandsHandler::HandleHlfUpgradeChaincode(
    ConcordRequest& command, HlfKvbStorage* kvb_hlf_storage,
    ConcordResponse& command_response) const {
  // fetch the first hlf request
  const HlfRequest request = command.hlf_request(0);
  HlfResponse* response = command_response.add_hlf_response();

  if (hlf_handler_->SetKvbHlfStoragePointer(kvb_hlf_storage).isOK()) {
    if (request.has_chaincode_name() && request.has_input() &&
        request.has_version()) {
      kvb_hlf_storage->AddHlfTransaction(request);

      Status status = hlf_handler_->UpgradeChaincode(
          request.chaincode_name(), request.input(), request.version());

      // revoke KVB storage pointer
      hlf_handler_->RevokeKvbHlfStoragePointer();

      if (status.isOK()) {
        response->set_data("Successfully upgraded chaincode: " +
                           request.chaincode_name());
        response->set_status(0);
        return true;
      }
    }
  }

  response->set_status(1);
  response->set_data("Failed to upgrade chaincode");

  return true;
}

bool HlfKvbCommandsHandler::HandleHlfInvokeChaincode(
    ConcordRequest& command, HlfKvbStorage* kvb_hlf_storage,
    ConcordResponse& command_response) const {
  // fetch the first hlf request
  const HlfRequest request = command.hlf_request(0);
  HlfResponse* response = command_response.add_hlf_response();

  if (hlf_handler_->SetKvbHlfStoragePointer(kvb_hlf_storage).isOK()) {
    if (request.has_chaincode_name() && request.has_input()) {
      // Add transaction
      kvb_hlf_storage->AddHlfTransaction(request);
      Status status = hlf_handler_->InvokeChaincode(request.chaincode_name(),
                                                    request.input());

      // revoke KVB storage pointer
      hlf_handler_->RevokeKvbHlfStoragePointer();

      if (status.isOK()) {
        response->set_data(
            "Successfully invoked chaincode: " + request.chaincode_name() +
            " with input: " + request.input());
        response->set_status(0);
        return true;
      }
    }
  }

  response->set_status(1);
  response->set_data("Failed to invoke chaincode");

  return true;
}

// should be read-only
bool HlfKvbCommandsHandler::HandleHlfQueryChaincode(
    ConcordRequest& command, HlfKvbStorage* kvb_hlf_storage,
    ConcordResponse& command_response) const {
  // fetch the first hlf request
  const HlfRequest request = command.hlf_request(0);
  HlfResponse* response = command_response.add_hlf_response();

  if (hlf_handler_->SetKvbHlfStoragePointer(kvb_hlf_storage).isOK()) {
    if (request.has_chaincode_name() && request.has_input()) {
      string result = hlf_handler_->QueryChaincode(request.chaincode_name(),
                                                   request.input());

      // revoke KVB storage pointer
      hlf_handler_->RevokeKvbHlfStoragePointer();

      if (result != "") {
        response->set_data(
            "Successfully queried chaincode: " + request.chaincode_name() +
            "\n The input is : " + request.input() +
            "\n The result is : " + result);
        response->set_status(0);
        return true;
      }
    }
  }

  response->set_status(1);
  response->set_data("Failed to query chaincode");

  return true;
}
}  // namespace hlf
}  // namespace concord
