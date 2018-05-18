// Copyright 2018 VMware, all rights reserved
//
// KVBlockchain replica command handler interface for EVM.

#include "kvb/BlockchainInterfaces.h"
#include "kvb/slice.h"
#include "athena_evm.hpp"
#include "athena_kvb.hpp"
#include "athena.pb.h"

using Blockchain::Slice;
using Blockchain::ILocalKeyValueStorageReadOnly;
using Blockchain::IBlocksAppender;

com::vmware::athena::KVBCommandsHandler::KVBCommandsHandler(EVM &athevm) :
   athevm_(athevm),
   logger(log4cplus::Logger::getInstance("com.vmware.athena"))
{
   // no other initialization necessary
}

com::vmware::athena::KVBCommandsHandler::~KVBCommandsHandler()
{
   // no other deinitialization necessary
}


bool com::vmware::athena::KVBCommandsHandler::executeCommand(
      const Slice cmdSlice,
      const ILocalKeyValueStorageReadOnly &roStorage,
      IBlocksAppender &blockAppender,
      const size_t maxReplySize,
      char *outReply,
      size_t &outReplySize) const
{
   AthenaRequest command;
   command.ParseFromArray(cmdSlice.data(), cmdSlice.size());

   AthenaResponse athresp;
   if (command.eth_request_size() > 0) {
      handle_eth_request(command, roStorage, blockAppender, athresp);
   } else {
      LOG4CPLUS_ERROR(logger, "Unknown command");
      ErrorResponse *resp = athresp.add_error_response();
      resp->set_description("Internal Athena Error");
   }

   if (!athresp.SerializeToArray(outReply, maxReplySize)) {
      LOG4CPLUS_ERROR(logger, "Reply is too large");

      // TODO(BWF): is false the right thing here?
      return false;
   }

   outReplySize = athresp.ByteSize();

   // TODO(BWF): what is the boolean for here?
   return true;
}

/*
 * Handle an ETH RPC request.
 */
void com::vmware::athena::KVBCommandsHandler::handle_eth_request(
   AthenaRequest &athreq,
   const ILocalKeyValueStorageReadOnly &roStorage,
   IBlocksAppender &blockAppender,
   AthenaResponse &athresp) const
{
   switch (athreq.eth_request(0).method()) {
   case EthRequest_EthMethod_SEND_TX:
      handle_eth_sendTransaction(athreq, roStorage, blockAppender, athresp);
      break;
      //TODO(BWF): move over all other api_connection::handle_eth_request cases
      //           some may go to a ready-only version
   default:
      ErrorResponse *e = athresp.add_error_response();
      e->mutable_description()->assign("ETH Method Not Implemented");
   }
}

/**
 * Handle an eth_sendTransaction request.
 */
void com::vmware::athena::KVBCommandsHandler::handle_eth_sendTransaction(
   AthenaRequest &athreq,
   const ILocalKeyValueStorageReadOnly &roStorage,
   IBlocksAppender &blockAppender,
   AthenaResponse &athresp) const
{
   const EthRequest request = athreq.eth_request(0);

   evm_uint256be txhash;
   evm_result &&result =
      run_evm(request, true, roStorage, blockAppender, txhash);
   EthResponse *response = athresp.add_eth_response();
   response->set_id(request.id());
   response->set_data(txhash.bytes, sizeof(evm_uint256be));
}

bool com::vmware::athena::KVBCommandsHandler::executeReadOnlyCommand(
   const Slice cmdSlice,
   const ILocalKeyValueStorageReadOnly &roStorage,
   const size_t maxReplySize,
   char *outReply,
   size_t &outReplySize) const
{
   AthenaRequest command;
   command.ParseFromArray(cmdSlice.data(), cmdSlice.size());

   AthenaResponse athresp;
   if (command.has_transaction_request()) {
      handle_transaction_request(command, roStorage, athresp);
   } else {
      LOG4CPLUS_ERROR(logger, "Unknown read-only command");
      ErrorResponse *resp = athresp.add_error_response();
      resp->set_description("Internal Athena Error");
   }

   if (!athresp.SerializeToArray(outReply, maxReplySize)) {
      LOG4CPLUS_ERROR(logger, "Reply is too large");

      // TODO(BWF): is false the right thing here?
      return false;
   }

   outReplySize = athresp.ByteSize();

   // TODO(BWF): what is the boolean for here?
   return true;
}

void com::vmware::athena::KVBCommandsHandler::handle_transaction_request(
   AthenaRequest &athreq,
   const ILocalKeyValueStorageReadOnly &roStorage,
   AthenaResponse &athresp) const
{
   const TransactionRequest request = athreq.transaction_request();

   try {
      evm_uint256be hash;
      std::copy(request.hash().begin(), request.hash().end(), hash.bytes);
      EthTransaction tx = athevm_.get_transaction(hash, roStorage);

      TransactionResponse* response = athresp.mutable_transaction_response();
      response->set_hash(request.hash());
      response->set_from(tx.from.bytes, sizeof(evm_address));
      if (tx.to != zero_address) {
         response->set_to(tx.to.bytes, sizeof(evm_address));
      }
      if (tx.contract_address != zero_address) {
         response->set_contract_address(tx.contract_address.bytes,
                                        sizeof(evm_address));
      }
      if (tx.input.size()) {
         response->set_input(std::string(tx.input.begin(), tx.input.end()));
      }
      response->set_status(tx.status);
      response->set_nonce(tx.nonce);
      response->set_value(tx.value);
   } catch (TransactionNotFoundException) {
      ErrorResponse *resp = athresp.add_error_response();
      resp->set_description("transaction not found");
   }
}

evm_result com::vmware::athena::KVBCommandsHandler::run_evm(
   const EthRequest &request,
   bool isTransaction,
   const ILocalKeyValueStorageReadOnly &roStorage,
   IBlocksAppender &blockAppender,
   evm_uint256be &txhash /* OUT */) const
{
   evm_message message;
   evm_result result;

   memset(&message, 0, sizeof(message));
   memset(&result, 0, sizeof(result));

   if (request.has_addr_from()) {
      // TODO: test & return error if needed
      assert(20 == request.addr_from().length());
      memcpy(message.sender.bytes, request.addr_from().c_str(), 20);
   }

   if (request.has_data()) {
      message.input_data =
         reinterpret_cast<const uint8_t*>(request.data().c_str());
      message.input_size = request.data().length();
   }

   if (request.has_value()) {
      size_t req_offset, val_offset;
      if (request.value().size() > sizeof(evm_uint256be)) {
         // TODO: this should probably throw an error instead
         req_offset = request.value().size()-sizeof(evm_uint256be);
         val_offset = 0;
      } else {
         req_offset = 0;
         val_offset = sizeof(evm_uint256be)-request.value().length();
      }
      std::copy(request.value().begin()+req_offset, request.value().end(),
                message.value.bytes+val_offset);
   }

   // TODO: get this from the request
   message.gas = 1000000;

   if (request.has_addr_to()) {
      message.kind = EVM_CALL;

      // TODO: test & return error if needed
      assert(20 == request.addr_to().length());
      memcpy(message.destination.bytes, request.addr_to().c_str(), 20);

      athevm_.run(message, isTransaction, roStorage, blockAppender, result, txhash);
   } else {
      message.kind = EVM_CREATE;

      athevm_.create(message, roStorage, blockAppender, result, txhash);
   }

   LOG4CPLUS_INFO(logger, "Execution result -" <<
                  " status_code: " << result.status_code <<
                  " gas_left: " << result.gas_left <<
                  " output_size: " << result.output_size);
   return result;
}
