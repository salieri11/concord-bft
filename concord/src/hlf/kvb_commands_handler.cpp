// Copyright 2018-2019 VMware, all rights reserved

#include "hlf/kvb_commands_handler.hpp"
#include <boost/predef/detail/endian_compat.h>
#include <google/protobuf/text_format.h>
#include <fstream>
#include <iterator>
#include <vector>
#include "OpenTracing.hpp"
#include "common/concord_exception.hpp"
#include "concord.pb.h"
#include "config/configuration_manager.hpp"
#include "consensus/concord_commands_handler.hpp"
#include "kv_types.hpp"
#include "time/time_contract.hpp"
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
using concord::hlf::ChaincodeInvoker;
using concord::hlf::HlfKvbStorage;
using concord::kvbc::BlockId;
using concord::kvbc::IBlocksAppender;
using concord::kvbc::ILocalKeyValueStorageReadOnly;
using concord::kvbc::SetOfKeyValuePairs;
using concord::time::TimeContract;
using concordUtils::Status;

namespace concord {
namespace hlf {

HlfKvbCommandsHandler::HlfKvbCommandsHandler(
    ChaincodeInvoker* chaincode_invoker,
    const concord::config::ConcordConfiguration& config,
    concord::config::ConcordConfiguration& node_config,
    const ILocalKeyValueStorageReadOnly& ro_storage,
    IBlocksAppender& block_appender,
    concord::thin_replica::SubBufferList& subscriber_list,
    std::shared_ptr<concord::utils::PrometheusRegistry> prometheus_registry)
    : ConcordCommandsHandler(config, node_config, ro_storage, block_appender,
                             subscriber_list, prometheus_registry),
      logger_(logging::getLogger("com.vmware.concord.hlf.handler")),
      chaincode_invoker_(chaincode_invoker),
      node_config_(node_config) {}

HlfKvbCommandsHandler::~HlfKvbCommandsHandler() {
  // no other deinitialization necessary
}

bool HlfKvbCommandsHandler::Execute(
    const ConcordRequest& request,
    const concord::consensus::ConcordRequestContext& request_context,
    uint8_t flags, TimeContract* time_contract, opentracing::Span& parent_span,
    ConcordResponse& response) {
  bool read_only = flags & bftEngine::MsgFlag::READ_ONLY_FLAG;
  bool pre_execute = flags & bftEngine::MsgFlag::PRE_PROCESS_FLAG;

  if (pre_execute) {
    LOG_ERROR(logger_, "Pre-execution not supported for Hyperledger requests.");
    // TODO: the HLF chaincode runner doesn't seem to return the read set, which
    // is essential for pre-execution
    return false;
  } else if (read_only) {
    return ExecuteReadOnlyCommand(request, time_contract, response);
  } else {
    return ExecuteCommand(request, time_contract, response);
  }
}

void HlfKvbCommandsHandler::WriteEmptyBlock(
    TimeContract* time_contract, const opentracing::Span& parent_span) {
  HlfKvbStorage kvb_hlf_storage(storage_, this);
  auto span = concordUtils::startChildSpanFromContext(parent_span.context(),
                                                      "write_empty_block");
  kvb_hlf_storage.WriteHlfBlock(span);
}

// Callback from SBFT/KVB. Process the request Returns
// false if the command is illegal or invalid; true otherwise.
bool HlfKvbCommandsHandler::ExecuteReadOnlyCommand(
    const ConcordRequest& command, TimeContract* time_contract,
    ConcordResponse& command_response) const {
  HlfKvbStorage kvb_hlf_storage(storage_);

  if (command.hlf_request_size() > 0) {
    return HandleHlfRequestReadOnly(command, &kvb_hlf_storage,
                                    command_response);
  } else {
    std::string pbtext;
    google::protobuf::TextFormat::PrintToString(command, &pbtext);
    LOG_ERROR(logger_, "Unknown read-only command: " << pbtext);
    ErrorResponse* resp = command_response.add_error_response();
    resp->set_description("Internal concord Error");
    return false;
  }
}

// Callback from SBFT/KVB. Process the request Returns
// false if the command is illegal or invalid; true otherwise.
bool HlfKvbCommandsHandler::ExecuteCommand(const ConcordRequest& command,
                                           TimeContract* time_contract,
                                           ConcordResponse& command_response) {
  HlfKvbStorage kvb_hlf_storage(storage_, this);

  if (command.hlf_request_size() > 0) {
    // pass the addr of kvb_hlf_storage
    return HandleHlfRequest(command, &kvb_hlf_storage, command_response);
  } else {
    std::string pbtext;
    google::protobuf::TextFormat::PrintToString(command, &pbtext);
    LOG_DEBUG(logger_, "Unknown command: " << pbtext);
    // We have to silently ignore this command if there is nothing in it we
    // recognize. It might be a request that only contained something like a
    // time update, which is handled by the calling layer.
    return true;
  }
}

//  Handle a HLF gRPC request. Return false if the command was invalid
bool HlfKvbCommandsHandler::HandleHlfRequest(
    const ConcordRequest& command, HlfKvbStorage* kvb_hlf_storage,
    ConcordResponse& command_response) const {
  LOG_INFO(logger_, "Triggered hlf request");

  if (command.hlf_request(0).type() == "ping")
    return HandleHlfPing(command, kvb_hlf_storage, command_response);
  else
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

bool HlfKvbCommandsHandler::HandleHlfPing(
    const ConcordRequest& command, HlfKvbStorage* kvb_hlf_storage,
    ConcordResponse& command_response) const {
  HlfResponse* response = command_response.add_hlf_response();
  response->set_status(0);
  response->set_data("Pong");
  LOG_INFO(logger_, "Triggered hlf ping/pong");
  return true;
}

bool HlfKvbCommandsHandler::HandleHlfRequestReadOnly(
    const ConcordRequest& command, HlfKvbStorage* kvb_hlf_storage,
    ConcordResponse& command_response) const {
  switch (command.hlf_request(0).method()) {
    case HlfRequest_HlfMethod_QUERY:
      LOG_DEBUG(logger_, "Triggered the chaincode query operation");
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
    LOG_ERROR(logger_, "Failed to write block");
    return Status::NotFound("undable to update storage");
  }
}

bool HlfKvbCommandsHandler::HandleHlfInstallChaincode(
    const ConcordRequest& command, HlfKvbStorage* kvb_hlf_storage,
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
    const ConcordRequest& command, HlfKvbStorage* kvb_hlf_storage,
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
    const ConcordRequest& command, HlfKvbStorage* kvb_hlf_storage,
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
    const ConcordRequest& command, ConcordResponse& command_response) const {
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

  LOG_DEBUG(logger_, "Writting temporary chaincode file to: "
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
        LOG_DEBUG(logger_, "Successfully uploaded chaincode "
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
