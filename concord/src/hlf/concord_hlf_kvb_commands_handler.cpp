// Copyright 2018-2019 VMware, all rights reserved

#include "concord_hlf_kvb_commands_handler.hpp"

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

using concord::blockchain::hlf::KVBHlfStorage;
using concord::common::BlockNotFoundException;
using concord::common::HexPrintBytes;
using concord::common::TransactionNotFoundException;
using concord::common::zero_address;
using concord::common::zero_hash;
using concord::common::operator<<;

using Blockchain::IBlocksAppender;
using Blockchain::ILocalKeyValueStorageReadOnly;
using Blockchain::Status;

using concord::hlf::HlfHandler;

namespace concord {
namespace hlf {

KVBCommandsHandlerForHlf::KVBCommandsHandlerForHlf(
    HlfHandler *hlfHandler, const concord::config::ConcordConfiguration &config,
    concord::config::ConcordConfiguration &nodeConfig,
    Blockchain::ILocalKeyValueStorageReadOnly *roStorage,
    Blockchain::IBlocksAppender *appender)
    : logger(log4cplus::Logger::getInstance("com.vmware.concord")),
      hlfHandler_(hlfHandler),
      config_(config),
      nodeConfiguration(nodeConfig),
      m_ptrRoStorage(roStorage),
      m_ptrBlockAppender(appender) {
  assert(m_ptrBlockAppender);
  assert(m_ptrRoStorage);
}

KVBCommandsHandlerForHlf::~KVBCommandsHandlerForHlf() {
  // no other deinitialization necessary
}

int KVBCommandsHandlerForHlf::execute(uint16_t clientId, uint64_t sequenceNum,
                                      bool readOnly, uint32_t requestSize,
                                      const char *request,
                                      uint32_t maxReplySize, char *outReply,
                                      uint32_t &outActualReplySize) {
  bool res;
  if (readOnly) {
    res = executeReadOnlyCommand(requestSize, request, *m_ptrRoStorage,
                                 maxReplySize, outReply, outActualReplySize);
  } else {
    res = executeCommand(requestSize, request, sequenceNum, *m_ptrRoStorage,
                         *m_ptrBlockAppender, maxReplySize, outReply,
                         outActualReplySize);
  }

  return res ? 0 : 1;
}

/**
 * Callback from SBFT/KVB. Process the request Returns
 * false if the command is illegal or invalid; true otherwise.
 */
bool KVBCommandsHandlerForHlf::executeReadOnlyCommand(
    uint32_t requestSize, const char *request,
    const ILocalKeyValueStorageReadOnly &roStorage, const size_t maxReplySize,
    char *outReply, uint32_t &outReplySize) const {
  KVBHlfStorage kvbHlfStorage(roStorage);

  ConcordRequest command;
  bool result;
  ConcordResponse athresp;
  if (command.ParseFromArray(request, requestSize)) {
    if (command.hlf_request_size() > 0) {
      result = handle_hlf_request_read_only(command, &kvbHlfStorage, athresp);
    } else {
      std::string pbtext;
      google::protobuf::TextFormat::PrintToString(command, &pbtext);
      LOG4CPLUS_ERROR(logger, "Unknown read-only command: " << pbtext);
      ErrorResponse *resp = athresp.add_error_response();
      resp->set_description("Internal concord Error");
      result = false;
    }
  } else {
    LOG4CPLUS_ERROR(logger, "Unable to parse read-only command: "
                                << (HexPrintBytes{request, requestSize}));
    ErrorResponse *resp = athresp.add_error_response();
    resp->set_description("Internal concord Error");
    result = false;
  }

  if (athresp.SerializeToArray(outReply, maxReplySize)) {
    outReplySize = athresp.ByteSize();
  } else {
    LOG4CPLUS_ERROR(logger, "Reply is too large");
    outReplySize = 0;
  }

  return result;
}

/**
 * Callback from SBFT/KVB. Process the request Returns
 * false if the command is illegal or invalid; true otherwise.
 */
bool KVBCommandsHandlerForHlf::executeCommand(
    uint32_t requestSize, const char *request, const uint64_t sequenceNum,
    const ILocalKeyValueStorageReadOnly &roStorage,
    IBlocksAppender &blockAppender, const size_t maxReplySize, char *outReply,
    uint32_t &outReplySize) const {
  KVBHlfStorage kvbHlfStorage(roStorage, &blockAppender, sequenceNum);

  ConcordRequest command;
  bool result;
  ConcordResponse athresp;
  if (command.ParseFromArray(request, requestSize)) {
    if (command.hlf_request_size() > 0) {
      // pass the addr of kvbHlfStorage
      result = handle_hlf_request(command, &kvbHlfStorage, athresp);
    } else {
      // SBFT may decide to try one of our read-only commands in read-write
      // mode, for example if it has failed several times. So, go check the
      // read-only list if othing matched here.
      LOG4CPLUS_INFO(logger, "Unknown read-write command. Trying read-only.");
      return executeReadOnlyCommand(requestSize, request, roStorage,
                                    maxReplySize, outReply, outReplySize);
    }
  } else {
    LOG4CPLUS_ERROR(logger, "Unable to parse command: "
                                << (HexPrintBytes{request, requestSize}));
    ErrorResponse *resp = athresp.add_error_response();
    resp->set_description("Internal concord Error");
    result = false;
  }

  if (athresp.SerializeToArray(outReply, maxReplySize)) {
    outReplySize = athresp.ByteSize();
  } else {
    LOG4CPLUS_ERROR(logger, "Reply is too large");
    outReplySize = 0;
  }

  return result;
}

/*
 * Handle a HLF gRPC request. Return false if the command was invalid
 */

bool KVBCommandsHandlerForHlf::handle_hlf_request(
    ConcordRequest &athreq, KVBHlfStorage *kvbHlfStorage,
    ConcordResponse &athresp) const {
  LOG4CPLUS_INFO(logger, "Triggered the chaincode invoke operation");
  switch (athreq.hlf_request(0).method()) {
    case HlfRequest_HlfMethod_INSTALL:
      return handle_hlf_install_chaincode(athreq, kvbHlfStorage, athresp);

    case HlfRequest_HlfMethod_INSTANTIATE:
      return handle_hlf_instantiate_chaincode(athreq, kvbHlfStorage, athresp);

    case HlfRequest_HlfMethod_UPGRADE:
      return handle_hlf_upgrade_chaincode(athreq, kvbHlfStorage, athresp);

    case HlfRequest_HlfMethod_INVOKE:
      return handle_hlf_invoke_chaincode(athreq, kvbHlfStorage, athresp);

    // handle read only
    default:
      return handle_hlf_request_read_only(athreq, kvbHlfStorage, athresp);
  }
}

bool KVBCommandsHandlerForHlf::handle_hlf_request_read_only(
    ConcordRequest &athreq, KVBHlfStorage *kvbHlfStorage,
    ConcordResponse &athresp) const {
  LOG4CPLUS_INFO(logger, "Triggered the chaincode query operation");
  switch (athreq.hlf_request(0).method()) {
    case HlfRequest_HlfMethod_QUERY:
      return handle_hlf_query_chaincode(athreq, kvbHlfStorage, athresp);

    default:
      ErrorResponse *e = athresp.add_error_response();
      e->mutable_description()->assign("HLF Method Not Implemented");
      return false;
  }
}

bool KVBCommandsHandlerForHlf::handle_hlf_install_chaincode(
    ConcordRequest &athreq, KVBHlfStorage *kvbHlfStorage,
    ConcordResponse &athresp) const {
  // fetch the first hlf request
  const HlfRequest request = athreq.hlf_request(0);
  HlfResponse *response = athresp.add_hlf_response();

  if (hlfHandler_->setKVBHlfStoragePointer(kvbHlfStorage).isOK()) {
    // input stores path
    if (request.has_chaincode_name() && request.has_input() &&
        request.has_version()) {
      kvbHlfStorage->add_hlf_transaction(request);

      Status status = hlfHandler_->installChaincode(
          request.chaincode_name(), request.input(), request.version());

      // revoke KVB storage pointer
      hlfHandler_->revokeKVBHlfStoragePointer();

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

bool KVBCommandsHandlerForHlf::handle_hlf_instantiate_chaincode(
    ConcordRequest &athreq, KVBHlfStorage *kvbHlfStorage,
    ConcordResponse &athresp) const {
  // fetch the first hlf request
  const HlfRequest request = athreq.hlf_request(0);
  HlfResponse *response = athresp.add_hlf_response();

  if (hlfHandler_->setKVBHlfStoragePointer(kvbHlfStorage).isOK()) {
    if (request.has_chaincode_name() && request.has_input() &&
        request.has_version()) {
      kvbHlfStorage->add_hlf_transaction(request);

      Status status = hlfHandler_->instantiateChaincode(
          request.chaincode_name(), request.input(), request.version());

      // revoke KVB storage pointer
      hlfHandler_->revokeKVBHlfStoragePointer();

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

bool KVBCommandsHandlerForHlf::handle_hlf_upgrade_chaincode(
    ConcordRequest &athreq, KVBHlfStorage *kvbHlfStorage,
    ConcordResponse &athresp) const {
  // fetch the first hlf request
  const HlfRequest request = athreq.hlf_request(0);
  HlfResponse *response = athresp.add_hlf_response();

  if (hlfHandler_->setKVBHlfStoragePointer(kvbHlfStorage).isOK()) {
    if (request.has_chaincode_name() && request.has_input() &&
        request.has_version()) {
      kvbHlfStorage->add_hlf_transaction(request);

      Status status = hlfHandler_->upgradeChaincode(
          request.chaincode_name(), request.input(), request.version());

      // revoke KVB storage pointer
      hlfHandler_->revokeKVBHlfStoragePointer();

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

bool KVBCommandsHandlerForHlf::handle_hlf_invoke_chaincode(
    ConcordRequest &athreq, KVBHlfStorage *kvbHlfStorage,
    ConcordResponse &athresp) const {
  // fetch the first hlf request
  const HlfRequest request = athreq.hlf_request(0);
  HlfResponse *response = athresp.add_hlf_response();

  if (hlfHandler_->setKVBHlfStoragePointer(kvbHlfStorage).isOK()) {
    if (request.has_chaincode_name() && request.has_input()) {
      // Add transaction
      kvbHlfStorage->add_hlf_transaction(request);
      Status status = hlfHandler_->invokeChaincode(request.chaincode_name(),
                                                   request.input());

      // revoke KVB storage pointer
      hlfHandler_->revokeKVBHlfStoragePointer();

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
bool KVBCommandsHandlerForHlf::handle_hlf_query_chaincode(
    ConcordRequest &athreq, KVBHlfStorage *kvbHlfStorage,
    ConcordResponse &athresp) const {
  // fetch the first hlf request
  const HlfRequest request = athreq.hlf_request(0);
  HlfResponse *response = athresp.add_hlf_response();

  if (hlfHandler_->setKVBHlfStoragePointer(kvbHlfStorage).isOK()) {
    if (request.has_chaincode_name() && request.has_input()) {
      string result = hlfHandler_->queryChaincode(request.chaincode_name(),
                                                  request.input());

      // revoke KVB storage pointer
      hlfHandler_->revokeKVBHlfStoragePointer();

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
