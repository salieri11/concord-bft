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

using boost::asio::ip::tcp;
using boost::asio::mutable_buffer;
using boost::asio::buffer;
using boost::asio::write;
using boost::asio::read;
using boost::asio::io_service;
using boost::asio::transfer_at_least;
using boost::system::error_code;

using namespace com::vmware::athena;

api_connection::pointer
api_connection::create(io_service &io_service, EVM &athevm)
{
   return pointer(new api_connection(io_service, athevm));
}

tcp::socket&
api_connection::socket()
{
   return socket_;
}

/*
 * Start handling a connection. Read requests from the connection and
 * send back responses until the client disconnects.
 */
void
api_connection::start()
{
   // length of serialized protobuf message
   uint16_t msglen;
   // wrapper around msglen to make reading into/out of it easier
   mutable_buffer lengthbuf(&msglen, sizeof(msglen));
   // buffer for reading/writing protobuf
   std::vector<uint8_t> pbuf;
   error_code error;

   // start by reading the length of the next message (a 16-bit
   // integer, encoded little endian, lower byte first)
   while (read(socket_, buffer(lengthbuf), error)) {
#ifndef BOOST_LITTLE_ENDIAN
      // swap byte order for big endian and pdp endian
      msglen = (msglen << 8) | (msglen >> 8);
#endif
      LOG4CPLUS_TRACE(logger_, "Bytes read! expecting " << msglen << " more");

      // we are only growing the buffer right now, because the max message size
      // is 64k, which isn't a huge amount per connection
      if (pbuf.size() < msglen) {
         pbuf.resize(msglen);
      }

      // now read the actual message
      if (read(socket_, buffer(pbuf), transfer_at_least(msglen), error)
          == msglen) {
         LOG4CPLUS_TRACE(logger_, "Correctly read bytes. decoding...");

         // Parse the protobuf
         athenaRequest_.ParseFromArray(&pbuf[0], msglen);
         LOG4CPLUS_TRACE(logger_, "Parsed!");

         // handle the request
         dispatch();

         // marshal the protobuf
         msglen = athenaResponse_.ByteSize();
         if (pbuf.size() < msglen) {
            pbuf.resize(msglen);
         }
         athenaResponse_.SerializeToArray(&pbuf[0], msglen);
#ifndef BOOST_LITTLE_ENDIAN
         msglen = (msglen << 8) || (msglen >> 8);
#endif

         // send the response back
         write(socket_, buffer(lengthbuf));
         write(socket_, buffer(pbuf, msglen), error);
         LOG4CPLUS_TRACE(logger_, "Responded!");
      } else {
         LOG4CPLUS_DEBUG(logger_, "Did not read enough bytes: " << error);
      }

      // prepare to read the next request
      athenaRequest_.Clear();
      athenaResponse_.Clear();
   }

   if (error != boost::asio::error::eof) {
      // the client didn't just disconnect - warn someone that
      // something went wrong
      LOG4CPLUS_ERROR(logger_, "Read failed: " << error);
   } else {
      LOG4CPLUS_DEBUG(logger_, "Connection closed");
   }
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
   const ProtocolRequest request = athenaRequest_.protocol_request();

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
   case EthRequest_EthMethod_GET_TX_RECEIPT:
      handle_eth_getTxReceipt(request);
      break;
   default:
      ErrorResponse *e = athenaResponse_.add_error_response();
      e->mutable_description()->assign("ETH Method Not Implemented");
   }
}

/**
 * Handle and eth_sendTransaction request.
 */
void
api_connection::handle_eth_sendTransaction(const EthRequest &request) {
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

   std::vector<uint8_t> txhash;
   if (request.has_addr_to()) {
      message.kind = EVM_CALL;

      // TODO: test & return error if needed
      assert(20 == request.addr_to().length());
      memcpy(message.destination.bytes, request.addr_to().c_str(), 20);

      athevm_.call(message, result, txhash);
   } else {
      message.kind = EVM_CREATE;

      athevm_.create(message, result, txhash);
   }

   LOG4CPLUS_INFO(logger_, "Execution result -" <<
                  " status_code: " << result.status_code <<
                  " gas_left: " << result.gas_left <<
                  " output_size: " << result.output_size);

   EthResponse *response = athenaResponse_.add_eth_response();
   response->set_id(request.id());
   response->set_data(std::string(txhash.begin(), txhash.end()));
}

/**
 * Handle and eth_getTransactionReceipt request.
 */
void
api_connection::handle_eth_getTxReceipt(const EthRequest &request) {
   if (request.has_data() && request.data().size() == sizeof(evm_uint256be)) {
      std::vector<uint8_t> txhash(request.data().begin(),
                                  request.data().end());
      LOG4CPLUS_DEBUG(logger_, "Looking up transaction receipt " <<
                      HexPrintVector{txhash});
      EthTransaction tx = athevm_.get_transaction(txhash);
      EthResponse *response = athenaResponse_.add_eth_response();
      response->set_id(request.id());
      response->set_status(tx.status == EVM_SUCCESS ? 1 : 0);
      if (tx.contract_address.size() > 0) {
         response->set_contract_address(
            std::string(tx.contract_address.begin(),
                        tx.contract_address.end()));
      }
   } else {
      ErrorResponse *error = athenaResponse_.add_error_response();
      error->set_description("Missing or invalid transaction hash");
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
   io_service &io_service, EVM &athevm)
   : socket_(io_service),
     logger_(log4cplus::Logger::getInstance("com.vmware.athena.api_connection")),
     athevm_(athevm)
{
   // nothing to do here yet other than initialize the socket and logger
}
