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
                       EVM &athevm)
{
   return pointer(new api_connection(io_service, connManager, athevm));
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
api_connection::dispatch() {
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
api_connection::handle_protocol_request() {
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
api_connection::handle_peer_request() {
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
api_connection::handle_eth_request(int i) {
   // TODO: forward to SBFT/KVBlockchain; just calling directly for now to
   // demonstrate

   // TODO: this is safe because we only handle one connection at a time
   // currently
   const EthRequest request = athenaRequest_.eth_request(i);

   switch (request.method()) {
   case EthRequest_EthMethod_SEND_TX:
      handle_eth_sendTransaction(request);
      break;
   case EthRequest_EthMethod_CALL_CONTRACT:
      handle_eth_callContract(request);
      break;
   case EthRequest_EthMethod_GET_TX_RECEIPT:
      handle_eth_getTxReceipt(request);
      break;
   case EthRequest_EthMethod_GET_STORAGE_AT:
      handle_eth_getStorageAt(request);
      break;
   default:
      ErrorResponse *e = athenaResponse_.add_error_response();
      e->mutable_description()->assign("ETH Method Not Implemented");
   }
}



evm_result
api_connection::run_evm(const EthRequest &request,
                        bool isTransaction,
                        evm_uint256be &txhash /* OUT */) {
   // TODO: this is the thing we'll forward to SBFT/KVBlockchain/EVM
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
      memcpy(message.value.bytes,
             request.value().c_str(),
             request.value().length());
   }

   // TODO: get this from the request
   message.gas = 1000000;

   if (request.has_addr_to()) {
      message.kind = EVM_CALL;

      // TODO: test & return error if needed
      assert(20 == request.addr_to().length());
      memcpy(message.destination.bytes, request.addr_to().c_str(), 20);

      athevm_.run(message, isTransaction, result, txhash);
   } else {
      message.kind = EVM_CREATE;

      athevm_.create(message, result, txhash);
   }

   LOG4CPLUS_INFO(logger_, "Execution result -" <<
                  " status_code: " << result.status_code <<
                  " gas_left: " << result.gas_left <<
                  " output_size: " << result.output_size);
   return result;
}



/**
 * Handle the 'contract.method.call()' functionality of ethereum. This is
 * used when the method being called does not make any changes to the state
 * of the system. Hence, in this case, we also do not record any transaction
 * Instead the return value of the contract function call will be returned
 * as the 'data' of EthResponse.
 */
void
api_connection::handle_eth_callContract(const EthRequest &request) {
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
}



/**
 * Handle an eth_sendTransaction request.
 */
void
api_connection::handle_eth_sendTransaction(const EthRequest &request) {
   evm_uint256be txhash;
   evm_result &&result = run_evm(request, true, txhash);
   EthResponse *response = athenaResponse_.add_eth_response();
   response->set_id(request.id());
   response->set_data(txhash.bytes, sizeof(evm_uint256be));
}

/**
 * Handle an eth_getTransactionReceipt request.
 */
void
api_connection::handle_eth_getTxReceipt(const EthRequest &request) {
   if (request.has_data() && request.data().size() == sizeof(evm_uint256be)) {
      evm_uint256be txhash;
      std::copy(request.data().begin(), request.data().end(), txhash.bytes);

      LOG4CPLUS_DEBUG(logger_, "Looking up transaction receipt " << txhash);

      try {
         EthTransaction tx = athevm_.get_transaction(txhash);

         EthResponse *response = athenaResponse_.add_eth_response();
         response->set_id(request.id());
         response->set_status(tx.status == EVM_SUCCESS ? 1 : 0);
         if (tx.contract_address != zero_address) {
            response->set_contract_address(tx.contract_address.bytes,
                                           sizeof(evm_address));
         }
      } catch (TransactionNotFoundException) {
         ErrorResponse *error = athenaResponse_.add_error_response();
         error->set_description("Transaction not found");
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
api_connection::handle_eth_getStorageAt(const EthRequest &request) {
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

/*
 * Handle test request, where the client requests an echo. This is
 * likely something we won't include in the final release, but has
 * been useful for testing.
 */
void
api_connection::handle_test_request() {
   const TestRequest request = athenaRequest_.test_request();
   if (request.has_echo()) {
      TestResponse *response = athenaResponse_.mutable_test_response();
      std::string *echo = response->mutable_echo();
      echo->assign(request.echo());
   }
}

api_connection::api_connection(
   io_service &io_service,
   connection_manager &manager,
   EVM& athevm)
   : socket_(io_service),
     logger_(
        log4cplus::Logger::getInstance("com.vmware.athena.api_connection")),
     connManager_(manager),
     athevm_(athevm)
{
   // nothing to do here yet other than initialize the socket and logger
}
