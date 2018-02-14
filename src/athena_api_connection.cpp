// Copyright 2018 VMware, all rights reserved
//
// Acceptor for connections from the API/UI servers.

#include <boost/bind.hpp>

#include "athena_api_connection.hpp"

using boost::asio::ip::tcp;

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
   // might actually be able to use a local variable here, because "Hello,
   // World!" is static, but in general we can't, because the lifetime of the
   // variable must be as long as needed for the write to finish
   message_ = "Hello, World!";

   boost::asio::async_write(
      socket_,
      boost::asio::buffer(message_),
      boost::bind(&athena_api_connection::handle_write,
                  shared_from_this(),
                  boost::asio::placeholders::error,
                  boost::asio::placeholders::bytes_transferred));
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
