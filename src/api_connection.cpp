// Copyright 2018 VMware, all rights reserved
//
// Handler for connections from the API/UI servers.
//
// We have one handler per connection. It is expected that connections
// from Helen will be long-lived. We will multiplex client requests
// from Helen's frontend API onto these connections.
//
// This version also handles one request at a time. The next request
// is read from the socket, the request is handled, and a response is
// sent to the socket. TODO: we probably want to toss requests at a
// threadpool of handlers (see SEDA system design).
//
// Requests are sent as two bytes encoding the length as an unsigned
// little-endian integer, followed by that number of bytes encoding an
// AthenaRequest protocol buffers message (see
// athena.proto). Responses are encoded the same way, though as
// AthenaResponse messages. TODO: do we need to support requests or
// responses more than 64k in length?

#include <iostream>
#include <limits>
#include <boost/bind.hpp>
#include <boost/predef/detail/endian_compat.h>

#include "evm.h"
#include "athena_evm.hpp"
#include "api_connection.hpp"
#include "connection_manager.hpp"
#include "athena_log.hpp"

using boost::asio::ip::tcp;
using boost::asio::mutable_buffer;
using boost::asio::buffer;
using boost::asio::write;
using boost::asio::read;
using boost::asio::io_service;
using boost::asio::transfer_at_least;
using boost::system::error_code;

using namespace boost::asio;
using namespace std;
using namespace com::vmware::athena;

api_connection::pointer
api_connection::create(io_service &io_service,
                       connection_manager &connManager,
                       EVM &athevm,
                       Blockchain::IClient *client)
{
   return pointer(new api_connection(io_service, connManager, athevm, client));
}

tcp::socket&
api_connection::socket()
{
   return socket_;
}

void
api_connection::start_async()
{
   LOG4CPLUS_TRACE(logger_, "start_async enter");
   remotePeer_ = socket_.remote_endpoint();
   LOG4CPLUS_INFO(logger_,
                  "Connection to " << remotePeer_ << " opened by peer");

   read_async_header();

   LOG4CPLUS_TRACE(logger_, "start_async exit");
}

uint16_t
api_connection::get_message_length(const char * buffer)
{
   uint16_t msgLen = *(static_cast<const uint16_t*>(
                          static_cast<const void*>(buffer)));
#ifndef BOOST_LITTLE_ENDIAN
   // swap byte order for big endian and pdp endian
   msgLen = (msglen << 8) | (msglen >> 8);
#endif
   return msgLen;
}

bool
api_connection::check_async_error(const boost::system::error_code &ec)
{
   bool res = false;
   if (boost::asio::error::eof == ec) {
      LOG4CPLUS_INFO(logger_, "connection closed by peer");
      res = true;
   } else if (boost::asio::error::operation_aborted == ec) {
      LOG4CPLUS_ERROR(logger_, ec.message());
      res = true;
   }
   return res;
}

void
api_connection::on_read_async_header_completed(
   const boost::system::error_code &ec,
   const size_t bytesRead)
{
   LOG4CPLUS_TRACE(logger_, "on_read_async_header_completed enter");
   auto err = check_async_error(ec);
   if(err) {
      LOG4CPLUS_DEBUG(logger_,
                      "on_read_async_header_completed, ec: " <<
                      ec.message());
      close();
      return;
   }

   auto msgLen = get_message_length(inMsgBuffer_);
   if(msgLen == 0) {
      LOG4CPLUS_FATAL(logger_, "on_read_async_header_completed, msgLen=0");
      return;
   }

   LOG4CPLUS_DEBUG(logger_,
                   "on_read_async_header_completed, msgLen: " << msgLen);

   read_async_message(MSG_LENGTH_BYTES, msgLen);

   LOG4CPLUS_TRACE(logger_, "on_read_async_header_completed exit");
}

void
api_connection::read_async_header()
{
   LOG4CPLUS_TRACE(logger_, "read_async_header enter");

   // clean all previous data
   memset(inMsgBuffer_ , 0, BUFFER_LENGTH);
   athenaRequest_.Clear();
   athenaResponse_.Clear();

   // async operation will finish when either expectedBytes are read
   // or error occured
   async_read(socket_,
              boost::asio::buffer(inMsgBuffer_, MSG_LENGTH_BYTES),
              boost::bind(&api_connection::on_read_async_header_completed,
                          shared_from_this(),
                          boost::asio::placeholders::error,
                          boost::asio::placeholders::bytes_transferred));

   LOG4CPLUS_TRACE(logger_, "read_async exit");
}

void
api_connection::read_async_message( uint16_t offset,
                                    uint16_t expectedBytes)
{
   LOG4CPLUS_TRACE(logger_, "read_async_message enter");
   LOG4CPLUS_DEBUG(logger_,
                   "offset: " << offset <<
                   ", expectedBytes: " << expectedBytes);

   // async operation will finish when either expectedBytes are read
   // or error occured
   async_read(socket_,
              boost::asio::buffer( inMsgBuffer_ + offset,
                                   expectedBytes),
              boost::bind(&api_connection::on_read_async_message_completed,
                          shared_from_this(),
                          boost::asio::placeholders::error,
                          boost::asio::placeholders::bytes_transferred));

   LOG4CPLUS_TRACE(logger_, "read_async_message exit");
}

// this is the handler to async_read, it will be called only if the
// supplied data buffer for read is full OR error occured
void
api_connection::on_read_async_message_completed(
   const boost::system::error_code &ec,
   const size_t bytesRead)
{
   LOG4CPLUS_TRACE(logger_, "on_read_async_completed enter");

   auto err = check_async_error(ec);
   if(err) {
      LOG4CPLUS_DEBUG(logger_,
                      "on_read_async_message_completed, ec: " <<
                      ec.message());
      return;
   }

   LOG4CPLUS_DEBUG(logger_, "msg data read, msgLen: " << bytesRead);
   process_incoming();
   read_async_header();

   LOG4CPLUS_TRACE(logger_, "on_read_async_completed exit");
}

void
api_connection::close()
{
   // we should not close socket_ explicitly since we use shared_from_this,
   // so the current api_connetion object and its socket_ object should be
   // destroyed automatically. However, this should be profiled during
   // stress tests for memory leaks
   LOG4CPLUS_DEBUG(logger_, "closing connection");
   connManager_.close_connection(shared_from_this());
}

void
api_connection::on_write_completed(const boost::system::error_code &ec)
{
   if(!ec) {
      LOG4CPLUS_DEBUG(logger_, "sent completed");
   } else {
      LOG4CPLUS_ERROR(logger_, "sent failed with error: " << ec.message());
   }
}

/*
 * Start handling a connection. Read requests from the connection and
 * send back responses until the client disconnects.
 */
void
api_connection::process_incoming()
{
   std::string pb;
   LOG4CPLUS_TRACE(logger_, "process_incoming enter");

   // Parse the protobuf
   if (athenaRequest_.ParseFromArray(inMsgBuffer_ + MSG_LENGTH_BYTES,
                                     get_message_length(inMsgBuffer_))) {
      LOG4CPLUS_DEBUG(logger_, "Parsed!");

      // handle the request
      dispatch();
   } else {
      // Parsing failed
      ErrorResponse *e = athenaResponse_.add_error_response();
      e->mutable_description()->assign("Unable to parse request");
   }

   // marshal the protobuf
   athenaResponse_.SerializeToString(&pb);
   uint16_t msgLen = pb.length();
#ifndef BOOST_LITTLE_ENDIAN
   msgLen = (msgLen << 8) || (msgLen >> 8);
#endif

   memset(outMsgBuffer_, 0, BUFFER_LENGTH);
   memcpy(outMsgBuffer_, &msgLen, MSG_LENGTH_BYTES);
   memcpy(outMsgBuffer_ + MSG_LENGTH_BYTES, pb.c_str(), msgLen);

   LOG4CPLUS_DEBUG(logger_, "sending back " << to_string(msgLen) << " bytes");
   boost::asio::async_write(socket_,
                            boost::asio::buffer(outMsgBuffer_,
                                                msgLen + MSG_LENGTH_BYTES),
                            boost::bind(&api_connection::on_write_completed,
                                        shared_from_this(),
                                        boost::asio::placeholders::error));

   LOG4CPLUS_DEBUG(logger_, "responded!");
   LOG4CPLUS_TRACE(logger_, "process_incoming exit");
}

/*
 * Based on what requests are in the message, dispatch to the proper
 * handler.
 */
void
api_connection::dispatch()
{
   // The idea behind checking each request field every time, instead
   // of checking at most one, is that a client could batch
   // requests. We'll see if that's a thing that is reasonable.
   if (athenaRequest_.has_protocol_request()) {
      handle_protocol_request();
   }
   if (athenaRequest_.has_peer_request()) {
      handle_peer_request();
   }
   for (int i = 0; i < athenaRequest_.eth_request_size(); i++) {
      // Similarly, a list of ETH RPC requests is supported to allow
      // batching. This seems like a good idea, but may not fit this
      // mode exactly.
      handle_eth_request(i);
   }
   if (athenaRequest_.has_block_list_request()) {
      handle_block_list_request();
   }
   if (athenaRequest_.has_block_request()) {
      handle_block_request();
   }
   if (athenaRequest_.has_transaction_request()) {
      handle_transaction_request();
   }
   if (athenaRequest_.has_test_request()) {
      handle_test_request();
   }
}

/*
 * Handle a protocol request, which is where the client announces its
 * version, and the server responds with its own. A request without a
 * client version might be considered a ping for keep-alive purposes.
 */
void
api_connection::handle_protocol_request()
{
   LOG4CPLUS_TRACE(logger_, "protocol_request enter");

   const ProtocolRequest request = athenaRequest_.protocol_request();
   LOG4CPLUS_DEBUG(logger_, "protocol_request, client_version: " <<
                   request.client_version());

   // create a response even if the request does not have a client
   // version, as this could be used as a keep-alive ping
   ProtocolResponse *response = athenaResponse_.mutable_protocol_response();

   if (request.has_client_version()) {
      response->set_server_version(1);
      if (request.client_version() > 1) {
         // This is just a basic demonstration of how we may want to
         // protect against clients that have been upgraded before
         // servers.
         ErrorResponse *e = athenaResponse_.add_error_response();
         e->mutable_description()->assign("Client version unknown");
      }
   }

   // We don't know what we'll do to support this yet. It is being added
   // because Truffle's testing framework requests it.
   // In geth, this is supplied via the --networkid command line parameter
   // and is required for nodes to communicate with each other.  The JSON
   // RPC API which retrieves it is net_version.
   //
   // Regarding chain vs network IDs, see:
   // https://github.com/ethereumproject/go-ethereum/wiki/FAQ
   response->set_net_version(DEFAULT_NETWORK_ID);
   LOG4CPLUS_TRACE(logger_, "protocol_request exit");
}

/*
 * Handle a peer request, which is the client asking for the list of
 * consensus participants, and maybe also asking to change that list
 * (add/remove members).
 */
void
api_connection::handle_peer_request()
{
   const PeerRequest request = athenaRequest_.peer_request();
   PeerResponse *response = athenaResponse_.mutable_peer_response();
   if (request.return_peers()) {
      // Dummy Data to prove the roundtrip to Helen (TODO)
      Peer *p1 = response->add_peer();
      p1->mutable_address()->assign("realathena1");
      p1->set_port(8001);
      p1->mutable_status()->assign("connected");

      Peer *p2 = response->add_peer();
      p2->mutable_address()->assign("realathena2");
      p2->set_port(8002);
      p2->mutable_status()->assign("offline");
   }
}

/*
 * Handle an ETH RPC request.
 */
void
api_connection::handle_eth_request(int i)
{
   // TODO: forward to SBFT/KVBlockchain; just calling directly for now to
   // demonstrate

   // TODO: this is safe because we only handle one connection at a time
   // currently
   const EthRequest request = athenaRequest_.eth_request(i);

   AthenaRequest internalRequest;
   EthRequest *internalEthRequest = internalRequest.add_eth_request();
   internalEthRequest->CopyFrom(request);
   AthenaResponse internalResponse;

   switch (request.method()) {
   case EthRequest_EthMethod_SEND_TX:
      if (send_request(internalRequest,
                       false /* not read only */,
                       internalResponse)) {
         athenaResponse_.MergeFrom(internalResponse);
      } else {
         LOG4CPLUS_ERROR(logger_, "Error parsing response");
         ErrorResponse *resp = athenaResponse_.add_error_response();
         resp->set_description("Internal Athena Error");
      }
      break;
   case EthRequest_EthMethod_NEW_ACCOUNT:
      // TODO(BWF): this will be a fall-through, as it is also modifying
      handle_personal_newAccount(request);
      break;
   //TODO(BWF): The rest of these will be read-only
   case EthRequest_EthMethod_CALL_CONTRACT:
      handle_eth_callContract(request);
      break;
   case EthRequest_EthMethod_GET_TX_RECEIPT:
      handle_eth_getTxReceipt(request);
      break;
   case EthRequest_EthMethod_GET_STORAGE_AT:
      handle_eth_getStorageAt(request);
      break;
   case EthRequest_EthMethod_FILTER_REQUEST:
      handle_filter_requests(request);
      break;
   case EthRequest_EthMethod_GET_CODE:
      handle_eth_getCode(request);
      break;
   default:
      ErrorResponse *e = athenaResponse_.add_error_response();
      e->mutable_description()->assign("ETH Method Not Implemented");
   }
}

/**
 * Handle a personal.newAccount request.
 * This method currently sets the account address as the last 20 bytes
 * of the hash of the passphrase provided by the user.
 */
void
api_connection::handle_personal_newAccount(const EthRequest &request)
{

   if (request.has_data()) {
      const string& passphrase = request.data();

      LOG4CPLUS_INFO(logger_, "Creating new account with passphrase : "
                     << passphrase);

      evm_address address;
      bool error = athevm_.new_account(passphrase, address);

      /**
       * This is an extremely hacky approach for setting the user address
       * as this means that multiple accounts cannot have the same password.
       * TODO : Implement the ethereum way of setting account addresses.
       * (Note : See https://github.com/vmwathena/athena/issues/55)
       */
      if (error == false) {
         LOG4CPLUS_INFO(logger_, "Use another passphrase : "
                        << passphrase);
         ErrorResponse *error = athenaResponse_.add_error_response();
         error->set_description("Use another passphrase");
      } else {
         EthResponse *response = athenaResponse_.add_eth_response();
         response->set_data(address.bytes, sizeof(evm_address));
      }
   } else {
      ErrorResponse *error = athenaResponse_.add_error_response();
      error->set_description("Missing passphrase");
   }
}

/**
 * Handle a request for the block list.
 */
void
api_connection::handle_block_list_request()
{
   const BlockListRequest request = athenaRequest_.block_list_request();

   AthenaRequest internalAthRequest;
   BlockListRequest *internalBlockRequest =
      internalAthRequest.mutable_block_list_request();
   internalBlockRequest->CopyFrom(request);
   AthenaResponse internalAthResponse;

   if (send_request(internalAthRequest,
                    true /* read only */,
                    internalAthResponse)) {
      athenaResponse_.MergeFrom(internalAthResponse);
   } else {
      ErrorResponse *error = athenaResponse_.add_error_response();
      error->set_description("Internal Athena Error");
   }
}

/**
 * Handle a request for a specific block.
 */
void
api_connection::handle_block_request()
{
   const BlockRequest request = athenaRequest_.block_request();

   if (!(request.has_number() || request.has_hash())) {
      ErrorResponse *resp = athenaResponse_.add_error_response();
      resp->set_description("invalid block request: no id or hash");
      return;
   }

   try {
      shared_ptr<EthBlock> block;
      if (request.has_number()) {
         block = athevm_.get_block_for_number(request.number());
      } else if (request.has_hash()) {
         evm_uint256be blkhash;
         std::copy(request.hash().begin(), request.hash().end(), blkhash.bytes);
         block = athevm_.get_block_for_hash(blkhash);
      }

      BlockResponse* response = athenaResponse_.mutable_block_response();
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
         std::string *tp = response->add_transaction();
         tp->assign(t.bytes, t.bytes+sizeof(evm_uint256be));
      }
   } catch (BlockNotFoundException) {
      ErrorResponse *resp = athenaResponse_.add_error_response();
      resp->set_description("block not found");
      return;
   }
}

/**
 * Handle a request for a specific transaction.
 */
void
api_connection::handle_transaction_request()
{
   const TransactionRequest request = athenaRequest_.transaction_request();

   if (!request.has_hash()) {
      ErrorResponse *resp = athenaResponse_.add_error_response();
      resp->set_description("invalid transaction request: no hash");
      return;
   }

   AthenaRequest internalRequest;
   TransactionRequest *txReq = internalRequest.mutable_transaction_request();
   txReq->CopyFrom(request);

   AthenaResponse internalResponse;
   if (send_request(internalRequest, true /* read only */, internalResponse)) {
      athenaResponse_.MergeFrom(internalResponse);
   } else {
      LOG4CPLUS_ERROR(logger_, "Error parsing read-only response");
      ErrorResponse *resp = athenaResponse_.add_error_response();
      resp->set_description("Internal Athena Error");
   }
}

/**
 * Send a request to the replicas. Returns true if the response contains
 * something to forward (either a response message or an appropriate error
 * message). Returns false if the response is empty (for example, if parsing
 * failed).
 */
bool api_connection::send_request(AthenaRequest &req,
                                  bool isReadOnly,
                                  AthenaResponse &resp)
{
   std::string command;
   req.SerializeToString(&command);
   Blockchain::Slice cmdslice(command);
   Blockchain::Slice replyslice;

   Blockchain::Status status = client_->invokeCommandSynch(
      cmdslice, isReadOnly, replyslice);

   if (status.ok()) {
      return resp.ParseFromArray(replyslice.data(), replyslice.size());
   } else {
      LOG4CPLUS_ERROR(logger_, "Error invoking read-only command: " <<
                      status.ToString());
      ErrorResponse *resp = athenaResponse_.add_error_response();
      resp->set_description("Internal Athena Error");
      return true;
   }
}

/**
 * Handle the 'contract.method.call()' functionality of ethereum. This is
 * used when the method being called does not make any changes to the state
 * of the system. Hence, in this case, we also do not record any transaction
 * Instead the return value of the contract function call will be returned
 * as the 'data' of EthResponse.
 */
void
api_connection::handle_eth_callContract(const EthRequest &request)
{
   //TODO(BWF): send_read_only_request
   LOG4CPLUS_WARN(logger_,
                  "TODO: callContract disabled during KVB integration");

   /*
   evm_uint256be txhash;
   evm_result &&result = run_evm(request, false, txhash);
   // Here we don't care about the txhash. Transaction was never
   // recorded, instead we focus on the result object and the
   // output_data field in it.
   if (result.status_code == EVM_SUCCESS) {
      EthResponse *response = athenaResponse_.add_eth_response();
      response->set_id(request.id());
      if (result.output_data != NULL && result.output_size > 0)
         response->set_data(result.output_data, result.output_size);
   } else {
      ErrorResponse *err = athenaResponse_.add_error_response();
      err->mutable_description()->assign("Error while calling contract");
   }
   */
}

/**
 * Handle an eth_getTransactionReceipt request.
 */
void
api_connection::handle_eth_getTxReceipt(const EthRequest &request)
{
   if (request.has_data() && request.data().size() == sizeof(evm_uint256be)) {
      LOG4CPLUS_DEBUG(logger_, "Looking up transaction receipt " <<
                      request.data());

      AthenaRequest internalRequest;
      TransactionRequest *txReq =
         internalRequest.mutable_transaction_request();
      txReq->set_hash(request.data());
      AthenaResponse internalResponse;

      if (send_request(internalRequest,
                       true /* read only */,
                       internalResponse)) {
         if (internalResponse.has_transaction_response()) {
            TransactionResponse txResp =
               internalResponse.transaction_response();

            EthResponse *response = athenaResponse_.add_eth_response();
            response->set_id(request.id());
            response->set_status(txResp.status() == EVM_SUCCESS ? 1 : 0);
            if (txResp.has_contract_address()) {
               response->set_contract_address(txResp.contract_address());
            }
         } else {
            ErrorResponse *error = athenaResponse_.add_error_response();
            error->MergeFrom(internalResponse.error_response(0));
         }
      } else {
         ErrorResponse *error = athenaResponse_.add_error_response();
         error->set_description("Internal Athena Error");
      }
   } else {
      ErrorResponse *error = athenaResponse_.add_error_response();
      error->set_description("Missing or invalid transaction hash");
   }
}

/**
 * Handle an eth_getStorageAt request.
 */
void
api_connection::handle_eth_getStorageAt(const EthRequest &request)
{
   if (request.has_addr_to() && request.addr_to().size() == sizeof(evm_address)
       && request.has_data() && request.data().size() == sizeof(evm_uint256be))
   {
      evm_address account;
      std::copy(request.addr_to().begin(), request.addr_to().end(),
                account.bytes);
      evm_uint256be key;
      std::copy(request.data().begin(), request.data().end(), key.bytes);
      //TODO: ignoring block number at the moment

      evm_uint256be data = athevm_.get_storage_at(account, key);
      EthResponse *response = athenaResponse_.add_eth_response();
      response->set_id(request.id());
      response->set_data(data.bytes, sizeof(data));
   } else {
      ErrorResponse *error = athenaResponse_.add_error_response();
      error->set_description("Missing account/contract or storage address");
   }
}

/**
 * Handle an eth_getCode request.
 */
void
api_connection::handle_eth_getCode(const EthRequest &request)
{
   if (request.has_addr_to() && request.addr_to().size() == sizeof(evm_address))
   {
      evm_address account;
      std::copy(request.addr_to().begin(), request.addr_to().end(),
                account.bytes);
      //TODO: ignoring block number at the moment

      std::vector<uint8_t> code;
      evm_uint256be hash;
      if (athevm_.get_code(account, code, hash)) {
         EthResponse *response = athenaResponse_.add_eth_response();
         response->set_id(request.id());
         response->set_data(std::string(code.begin(), code.end()));
      } else {
         ErrorResponse *error = athenaResponse_.add_error_response();
         error->set_description("No code found at given address");
      }
   } else {
      ErrorResponse *error = athenaResponse_.add_error_response();
      error->set_description("Missing contract address");
   }
}

/*
 * Handle test request, where the client requests an echo. This is
 * likely something we won't include in the final release, but has
 * been useful for testing.
 */
void
api_connection::handle_test_request()
{
   const TestRequest request = athenaRequest_.test_request();
   if (request.has_echo()) {
      TestResponse *response = athenaResponse_.mutable_test_response();
      std::string *echo = response->mutable_echo();
      echo->assign(request.echo());
   }
}


void
api_connection::handle_filter_requests(const EthRequest &request)
{
   if (request.has_filter_request()) {
      const FilterRequest &filter_request = request.filter_request();
      switch (filter_request.type()) {
      case FilterRequest_FilterRequestType_NEW_FILTER:
      case FilterRequest_FilterRequestType_NEW_BLOCK_FILTER:
         handle_new_block_filter(request);
         break;
      case FilterRequest_FilterRequestType_NEW_PENDING_TRANSACTION_FILTER:
      case FilterRequest_FilterRequestType_FILTER_CHANGE_REQUEST:
         handle_get_filter_changes(request);
         break;
      case FilterRequest_FilterRequestType_UNINSTALL_FILTER:
         handle_uninstall_filter(request);
      default:
         break;
      }
   } else {
      ErrorResponse *error = athenaResponse_.add_error_response();
      error->set_description("Invalid Filter request.");
   }
}


/**
 * Creates a new block filter and returns the associated ID.
 */
void
api_connection::handle_new_block_filter(const EthRequest &request) {
   FilterManager *filterManager = athevm_.get_filter_manager();
   evm_uint256be filterId = filterManager->create_new_block_filter();
   EthResponse *response = athenaResponse_.add_eth_response();
   response->set_id(request.id());
   FilterResponse *fresponse = response->mutable_filter_response();
   fresponse->set_filter_id(filterId.bytes, sizeof(filterId));
}

/**
 * Returns all the new changes that happend after the last call to this method.
 * This method may returns different data based on the type of filter.
 */
void
api_connection::handle_get_filter_changes(const EthRequest &request)
{
   const FilterRequest frequest = request.filter_request();
   evm_uint256be filterId;
   try {
      if (frequest.filter_id().size() != sizeof(filterId)) {
         throw FilterException("Filter ID should be exactly " +
                               std::to_string(sizeof(filterId)) + " bytes");
      }
      std::copy(frequest.filter_id().begin(),
                frequest.filter_id().end(),
                filterId.bytes);
      FilterManager *filterManager = athevm_.get_filter_manager();
      EthResponse *response = athenaResponse_.add_eth_response();
      response->set_id(request.id());
      if (filterManager->get_filter_type(filterId) ==
          EthFilterType::LOG_FILTER) {
         LOG4CPLUS_WARN(logger_,
                        "newFilter API (LOG_FILTER) is not implemented yet");
      } else if (filterManager->get_filter_type(filterId) ==
                 EthFilterType::NEW_BLOCK_FILTER) {
         vector<evm_uint256be>  block_changes =
            filterManager->get_new_block_filter_changes(filterId);
         if (block_changes.size() > 0) {
            FilterResponse *filterResponse = response->mutable_filter_response();
            for (auto block_hash : block_changes) {
               filterResponse->add_block_hashes(block_hash.bytes,
                                                sizeof(block_hash));
            }
         }
      } else if (filterManager->get_filter_type(filterId) ==
                 EthFilterType::NEW_PENDING_TRANSACTION_FILTER) {
         LOG4CPLUS_WARN(logger_, "newPendingTransactionFilter API"
                        "(NEW_PENDING_TRANSACTION_FILTER) is not implemented yet");
      }
   } catch (FilterException e) {
      LOG4CPLUS_DEBUG(logger_, e.what());
      // We might have added response to AthenaResponse, clear it first
      athenaResponse_.clear_eth_response();
      ErrorResponse *resp = athenaResponse_.add_error_response();
      resp->set_description(e.what());
   }
   return;
}

void
api_connection::handle_uninstall_filter(const EthRequest &request)
{
   const FilterRequest frequest = request.filter_request();
   evm_uint256be filterId;
   try {
      if (frequest.filter_id().size() != sizeof(filterId)) {
         throw FilterException("Filter ID should be exactly " +
                               std::to_string(sizeof(filterId)) + " bytes");
      }
      std::copy(frequest.filter_id().begin(),
                frequest.filter_id().end(),
                filterId.bytes);
      FilterManager *filterManager = athevm_.get_filter_manager();
      filterManager->uninstall_filter(filterId);
      EthResponse *response = athenaResponse_.add_eth_response();
      response->set_id(request.id());
      FilterResponse *filterResponse = response->mutable_filter_response();
      filterResponse->set_success(true);
   } catch (FilterException e) {
      LOG4CPLUS_DEBUG(logger_, e.what());
      // We might have added response to AthenaResponse, clear it first
      athenaResponse_.clear_eth_response();
      ErrorResponse *resp = athenaResponse_.add_error_response();
      resp->set_description(e.what());
   }
}

// TODO(BWF): When KVB integration is complete, this class should not hold a
// reference to the EVM. Communication with it should happen through
// KVBlockchain/SBFT.
api_connection::api_connection(
   io_service &io_service,
   connection_manager &manager,
   EVM& athevm,
   Blockchain::IClient *client)
   : socket_(io_service),
     logger_(
        log4cplus::Logger::getInstance("com.vmware.athena.api_connection")),
     connManager_(manager),
     athevm_(athevm),
     client_(client)
{
   // nothing to do here yet other than initialize the socket and logger
}
