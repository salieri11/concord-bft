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
      const Slice command,
      const ILocalKeyValueStorageReadOnly &roStorage,
      IBlocksAppender &blockAppender,
      const size_t maxReplySize,
      char *outReply,
      size_t &outReplySize) const
{
   LOG4CPLUS_INFO(logger, "TODO: executeCommand!");
   return true;
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
      EthTransaction tx = athevm_.get_transaction(hash);

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
