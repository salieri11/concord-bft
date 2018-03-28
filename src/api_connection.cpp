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

#include "api_connection.hpp"
#include "connection_manager.hpp"

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
                        connection_manager &connManager)
{
   return pointer(new api_connection(io_service, connManager));
}

tcp::socket&
api_connection::socket()
{
   return socket_;
}

void
api_connection::start_async()
{
   LOG4CPLUS_DEBUG(logger_, "start_async enter");
   remotePeer_ = socket_.remote_endpoint();
   LOG4CPLUS_INFO(logger_, "Connection to " << remotePeer_ << " opened by peer");

   read_async();
   LOG4CPLUS_DEBUG(logger_, "start_async exit");
}

void
api_connection::read_async()
{
   LOG4CPLUS_DEBUG(logger_, "read_async enter");

   memset(msgBuffer_, 0, BUFFER_LENGTH);
   // prepare to read the next request
   athenaRequest_.Clear();
   athenaResponse_.Clear();

   socket_.async_read_some(buffer(
                           msgBuffer_,
                           BUFFER_LENGTH),
                           boost::bind(
                              &api_connection::on_read_async_completed,
                              this,
                              boost::asio::placeholders::error,
                              boost::asio::placeholders::bytes_transferred));

   LOG4CPLUS_DEBUG(logger_, "read_async exit");
}

void
api_connection::on_read_async_completed (const boost::system::error_code &ec,
                                          const size_t bytes)
{
   LOG4CPLUS_DEBUG(logger_, "on_read_async_completed enter");
   LOG4CPLUS_TRACE(logger_, "on_read_async_completed, bytes: " +
                  to_string(bytes));

   // here if less then 2 bytes are available for
   if (!ec && bytes > MSG_LENGTH_BYTES) {
      process_incoming();
      read_async();
   } else if (boost::asio::error::eof == ec) {
      LOG4CPLUS_ERROR(logger_, "connection closed by peer");
      close();
   } else if (boost::asio::error::operation_aborted == ec) {
      LOG4CPLUS_ERROR(logger_, ec.message());
      close();
   } else {
      read_async();
   }

   LOG4CPLUS_DEBUG(logger_, "on_read_async_completed exit");
}

void
api_connection::close()
{
   // we should not close socket_ explicitly since we use shared_from_this,
   // so the current api_connetion object and its socket_ object should be
   // destroyed automatically. However, this should be profiled during
   // stress tests for memory leaks
   LOG4CPLUS_TRACE(logger_, "closing connection");
   connManager_.close_connection(shared_from_this());
}

void
api_connection::on_write_completed(const boost::system::error_code &ec)
{
   if(!ec)
      LOG4CPLUS_TRACE(logger_, "sent completed");
   else
      LOG4CPLUS_ERROR(logger_, "sent failed with error: " + ec.message());
}

/*
 * Start handling a connection. Read requests from the connection and
 * send back responses until the client disconnects.
 */
void
api_connection::process_incoming()
{
    std::string pb;
    LOG4CPLUS_DEBUG(logger_, "process_incoming enter");

    // start by getting the length of the next message (a 16-bit
    // integer, encoded little endian, lower byte first)
    uint16_t msgLen = *(static_cast<uint16_t*>(static_cast<void*>(msgBuffer_)));

#ifndef BOOST_LITTLE_ENDIAN
      // swap byte order for big endian and pdp endian
       msgLen = (msglen << 8) | (msglen >> 8);
#endif
    LOG4CPLUS_DEBUG(logger_, "msg length: " + to_string(msgLen));

    // Parse the protobuf
    athenaRequest_.ParseFromString(msgBuffer_);
    LOG4CPLUS_DEBUG(logger_, "Parsed!");

    // handle the request
    dispatch();

    // marshal the protobuf
    athenaResponse_.SerializeToString(&pb);
    msgLen = pb.length();
#ifndef BOOST_LITTLE_ENDIAN
    msgLen = (msgLen << 8) || (msgLen >> 8);
#endif
    memset(msgBuffer_, 0, BUFFER_LENGTH);
    memcpy(msgBuffer_, &msgLen, MSG_LENGTH_BYTES);
    memcpy(msgBuffer_ + MSG_LENGTH_BYTES, pb.c_str(), msgLen);

   LOG4CPLUS_TRACE(logger_, "sending back " + to_string(msgLen) + " bytes");
   async_write(socket_, buffer(msgBuffer_, msgLen + MSG_LENGTH_BYTES),
               boost::bind(&api_connection::on_write_completed,
                           this,
                           boost::asio::placeholders::error));

    LOG4CPLUS_TRACE(logger_, "responded!");
    LOG4CPLUS_DEBUG(logger_, "process_incoming exit");
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
   // TODO: this is the thing we'll forward to SBFT/KVBlockchain/EVM
   ErrorResponse *e = athenaResponse_.add_error_response();
   e->mutable_description()->assign("ETH Not Implemented");
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

api_connection::api_connection(io_service &io_service,
                              connection_manager &manager) :
      socket_(io_service),
      logger_(
         log4cplus::Logger::getInstance("com.vmware.athena.api_connection")),
      connManager_(manager)
{
   // nothing to do here yet other than initialize the socket and logger
}
