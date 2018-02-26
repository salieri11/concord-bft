// Copyright 2018 VMware, all rights reserved
//
// Acceptor for connections from the API/UI servers.

#include <iostream>
#include <boost/bind.hpp>
#include <boost/predef/detail/endian_compat.h>

#include "athena_api_connection.hpp"

using boost::asio::ip::tcp;
using namespace com::vmware::athena;

athena_api_connection::pointer
athena_api_connection::create(boost::asio::io_service &io_service)
{
   return pointer(new athena_api_connection(io_service));
}

tcp::socket&
athena_api_connection::socket()
{
   return socket_;
}

void
athena_api_connection::start()
{
    uint16_t msglen;
    char msg[65536];
    std::string pb;
    boost::asio::mutable_buffer lengthbuf(&msglen, sizeof(msglen));
    boost::asio::mutable_buffer msgbuf(&msg, 65536);
    boost::system::error_code error;
    
    while (boost::asio::read(socket_, boost::asio::buffer(lengthbuf), error)) {
#ifndef BOOST_LITTLE_ENDIAN
        // swap byte order for big endian and pdp endian
        msglen = (msglen << 8) | (msglen >> 8)
#endif
        std::cout << "Bytes read! expecting " << msglen << " more" << std::endl;

        if (boost::asio::read(socket_, boost::asio::buffer(msgbuf),
                              boost::asio::transfer_at_least(msglen), error)
            == msglen) {
            std::cout << "Correctly read bytes. decoding..." << std::endl;

            athenaRequest_.ParseFromString(msg);
            std::cout << "Parsed!" << std::endl;

            dispatch();

            athenaResponse_.SerializeToString(&pb);
            msglen = pb.length();
#ifndef BOOST_LITTLE_ENDIAN
            msglen = (msglen << 8) || (msglen >> 8);
#endif
            boost::asio::write(socket_, boost::asio::buffer(lengthbuf));
            boost::asio::write(socket_, boost::asio::buffer(pb), error);
            std::cout << "Responded!" << std::endl;
        } else {
            std::cout << "Did not read enough bytes (" << error << ")" << std::endl;
        }

        athenaRequest_.Clear();
        athenaResponse_.Clear();
    }

    if (error != boost::asio::error::eof) {
        std::cout << "Read failed: " << error << std::endl;
    } else {
        std::cout << "Connection closed" << std::endl;
    }
}

void
athena_api_connection::dispatch() {
    if (athenaRequest_.has_protocol_request()) {
        handle_protocol_request();
    }
    if (athenaRequest_.has_peer_request()) {
        handle_peer_request();
    }
    for (int i = 0; i < athenaRequest_.eth_request_size(); i++) {
        handle_eth_request(i);
    }
    if (athenaRequest_.has_test_request()) {
        handle_test_request();
    }
}

void
athena_api_connection::handle_protocol_request() {
    const ProtocolRequest request = athenaRequest_.protocol_request();
    ProtocolResponse *response = athenaResponse_.mutable_protocol_response();
    if (request.has_client_version()) {
        response->set_server_version(1);
        if (request.client_version() > 1) {
            ErrorResponse *e = athenaResponse_.add_error_response();
            e->mutable_description()->assign("Client version unknown");
        }
    }
}

void
athena_api_connection::handle_peer_request() {
    const PeerRequest request = athenaRequest_.peer_request();
    PeerResponse *response = athenaResponse_.mutable_peer_response();
    if (request.return_peers()) {
        // Dummy Data
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

void
athena_api_connection::handle_eth_request(int i) {
    ErrorResponse *e = athenaResponse_.add_error_response();
    e->mutable_description()->assign("ETH Not Implemented");
}

void
athena_api_connection::handle_test_request() {
    const TestRequest request = athenaRequest_.test_request();
    if (request.has_echo()) {
        TestResponse *response = athenaResponse_.mutable_test_response();
        std::string *echo = response->mutable_echo();
        echo->assign(request.echo());
    }
}

athena_api_connection::athena_api_connection(
   boost::asio::io_service &io_service)
   : socket_(io_service)
{
}

void
athena_api_connection::handle_write(const boost::system::error_code &error,
                                    size_t bytes_transferred)
{
}
