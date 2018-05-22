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


/**
 * Callback from SBFT/KVB. Process the request (mostly by talking to
 * EVM). Returns false if the command is illegal or invalid; true otherwise.
 */
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

   bool result;
   AthenaResponse athresp;
   if (command.eth_request_size() > 0) {
      result = handle_eth_request(command, roStorage, blockAppender, athresp);
   } else {
      LOG4CPLUS_ERROR(logger, "Unknown command");
      ErrorResponse *resp = athresp.add_error_response();
      resp->set_description("Internal Athena Error");
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
 * Handle an ETH RPC request. Returns false if the command was invalid; true
 * otherwise.
 */
bool com::vmware::athena::KVBCommandsHandler::handle_eth_request(
   AthenaRequest &athreq,
   const ILocalKeyValueStorageReadOnly &roStorage,
   IBlocksAppender &blockAppender,
   AthenaResponse &athresp) const
{
   switch (athreq.eth_request(0).method()) {
   case EthRequest_EthMethod_SEND_TX:
      return handle_eth_sendTransaction(
         athreq, roStorage, blockAppender, athresp);
      break;
      //TODO(BWF): move over all other api_connection::handle_eth_request cases
      //           some may go to a ready-only version
   default:
      ErrorResponse *e = athresp.add_error_response();
      e->mutable_description()->assign("ETH Method Not Implemented");
      return false;
   }
}

/**
 * Handle an eth_sendTransaction request.
 */
bool com::vmware::athena::KVBCommandsHandler::handle_eth_sendTransaction(
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

   // nothing would cause us to fail to try to run a transaction yet, and even
   // failed transactions get recorded, so commands here are always valid, so
   // always return true (for now)
   return true;
}

/**
 * Callback from SBFT/KVB. Process the request (mostly by talking to
 * EVM). Returns false if the command is illegal or invalid; true otherwise.
 */
bool com::vmware::athena::KVBCommandsHandler::executeReadOnlyCommand(
   const Slice cmdSlice,
   const ILocalKeyValueStorageReadOnly &roStorage,
   const size_t maxReplySize,
   char *outReply,
   size_t &outReplySize) const
{
   AthenaRequest command;
   command.ParseFromArray(cmdSlice.data(), cmdSlice.size());

   bool result;
   AthenaResponse athresp;
   if (command.has_transaction_request()) {
      result = handle_transaction_request(command, roStorage, athresp);
   } else if (command.has_block_list_request()) {
      result = handle_block_list_request(command, roStorage, athresp);
   } else if (command.has_block_request()) {
      result = handle_block_request(command, roStorage, athresp);
   } else {
      LOG4CPLUS_ERROR(logger, "Unknown read-only command");
      ErrorResponse *resp = athresp.add_error_response();
      resp->set_description("Internal Athena Error");
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

bool com::vmware::athena::KVBCommandsHandler::handle_transaction_request(
   AthenaRequest &athreq,
   const ILocalKeyValueStorageReadOnly &roStorage,
   AthenaResponse &athresp) const
{
   try {
      const TransactionRequest request = athreq.transaction_request();
      evm_uint256be hash;
      std::copy(request.hash().begin(), request.hash().end(), hash.bytes);
      EthTransaction tx = athevm_.get_transaction(hash, roStorage);

      TransactionResponse* response = athresp.mutable_transaction_response();
      build_transaction_response(hash, tx, response);
   } catch (TransactionNotFoundException) {
      ErrorResponse *resp = athresp.add_error_response();
      resp->set_description("transaction not found");
   } catch (EVMException) {
      ErrorResponse *resp = athresp.add_error_response();
      resp->set_description("error retrieving transaction");
   }

   // even requests for non-existent transactions are legal/valid
   return true;
}

void com::vmware::athena::KVBCommandsHandler::build_transaction_response(
   evm_uint256be &hash,
   EthTransaction &tx,
   TransactionResponse *response) const
{
   response->set_hash(hash.bytes, sizeof(hash.bytes));
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
   // send evm status as it is to helen
   response->set_status(tx.status);
   response->set_nonce(tx.nonce);
   response->set_value(tx.value);
   response->set_block_hash(tx.block_hash.bytes, sizeof(evm_uint256be));
   response->set_block_number(tx.block_number);
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

bool com::vmware::athena::KVBCommandsHandler::handle_block_list_request(
   AthenaRequest &athreq,
   const ILocalKeyValueStorageReadOnly &roStorage,
   AthenaResponse &athresp) const
{
   const BlockListRequest request = athreq.block_list_request();

   uint64_t latest = std::numeric_limits<uint64_t>::max();
   if (request.has_latest()) {
      latest = request.latest();
   }

   uint64_t count = 10;
   if (request.has_count()) {
      count = request.count();
   }

   BlockListResponse* response = athresp.mutable_block_list_response();

   std::vector<std::shared_ptr<EthBlock>> blocks =
      athevm_.get_block_list(latest, count, roStorage);
   for (auto b: blocks) {
      BlockBrief* bb = response->add_block();
      bb->set_number(b->number);
      bb->set_hash(b->hash.bytes, sizeof(evm_uint256be));
   }

   // all list requests are valid
   return true;
}

bool com::vmware::athena::KVBCommandsHandler::handle_block_request(
   AthenaRequest &athreq,
   const ILocalKeyValueStorageReadOnly &roStorage,
   AthenaResponse &athresp) const
{
   const BlockRequest request = athreq.block_request();

   // According to ethRPC requests the block number string can be either a hex
   // number or it can be one of "latest", "earliest", "pending". Since athena
   // only accepts uint64_t for block number helen will replace "latest" with -1
   // "earliest" with 0 (genesis block) and "pending" with -1 (since in athena
   // blocks are generated instantaneously we can say that "latest" =
   // "pending". Here we will have to first convert -1 to current block number
   // in that case.
   // TODO: Once SBFT is implemented blocks will not be generated instantaneously
   // this will have to be changed at that time.
   try {
      std::shared_ptr<EthBlock> block;
      if (request.has_number()) {
         uint64_t requested_block_number = athevm_.current_block_number();
         if (request.number() >= 0 &&
             request.number() < requested_block_number) {
            requested_block_number = request.number();
         }
         block = athevm_.get_block_for_number(requested_block_number,
                                              roStorage);
      } else if (request.has_hash()) {
         evm_uint256be blkhash;
         std::copy(request.hash().begin(), request.hash().end(), blkhash.bytes);
         block = athevm_.get_block_for_hash(blkhash, roStorage);
      }

      BlockResponse* response = athresp.mutable_block_response();
      response->set_number(block->number);
      response->set_hash(block->hash.bytes, sizeof(evm_uint256be));
      response->set_parent_hash(block->parent_hash.bytes, sizeof(evm_uint256be));

      // TODO: We're not mining, so nonce is mostly irrelevant. Maybe there will
      // be something relevant from KVBlockchain to put in here?
      response->set_nonce(zero_hash.bytes, sizeof(evm_uint256be));

      // TODO: This is supposed to be "the size of this block in bytes". This is
      // a sum of transaction inputs, storage updates, log events, and maybe
      // other things. It needs to be counted when the block is
      // recorded. Does KVBlockchain have this facility built in?
      response->set_size(1);

      for (auto t: block->transactions) {
         try {
            EthTransaction tx = athevm_.get_transaction(t, roStorage);
            TransactionResponse *txresp = response->add_transaction();
            build_transaction_response(t, tx, txresp);
         } catch (...) {
            LOG4CPLUS_ERROR(logger,
                            "Error fetching block transaction " << t <<
                            " from block " << block->number);

	    // we can still fill out some of the info, though, which may help an
	    // operator debug
            TransactionResponse *txresp = response->add_transaction();
	    txresp->set_hash(t.bytes, sizeof(evm_uint256be));
         }
      }
   } catch (BlockNotFoundException) {
      ErrorResponse *resp = athresp.add_error_response();
      resp->set_description("block not found");
   }

   // even requests for non-existent blocks are legal/valid
   return true;
}
