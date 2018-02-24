// Copyright 2018 VMware, all rights reserved
//
// Acceptor for connections from the API/UI servers.

#include <iostream>
#include <boost/bind.hpp>
#include <boost/predef/detail/endian_compat.h>

#include "athena_api_connection.hpp"

using boost::asio::ip::tcp;
using com::vmware::athena::AthenaRequest;
using com::vmware::athena::AthenaRequest_TestRequest;
using com::vmware::athena::AthenaResponse;
using com::vmware::athena::AthenaResponse_TestResponse;

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

            if (athenaRequest_.has_testrequest()) {
               const AthenaRequest_TestRequest testRequest =
                   athenaRequest_.testrequest();
               if (testRequest.has_echo()) {
                   AthenaResponse_TestResponse *response =
                       athenaResponse_.mutable_testresponse();
                   std::string *echo = response->mutable_echo();
                   echo->assign(testRequest.echo());
                  athenaResponse_.SerializeToString(&pb);
                  msglen = pb.length();
#ifndef BOOST_LITTLE_ENDIAN
                  msglen = (msglen << 8) || (msglen >> 8);
#endif
                  boost::asio::write(socket_, boost::asio::buffer(lengthbuf));
                  boost::asio::write(socket_, boost::asio::buffer(pb), error);
                  std::cout << "Responded!" << std::endl;
               }
            }
            std::cout << "Finished handling request" << std::endl;
        } else {
            std::cout << "Did not read enough bytes (" << error << ")" << std::endl;
        }
    }

    std::cout << "Read failed: " << error << std::endl;
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
