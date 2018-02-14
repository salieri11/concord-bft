// Copyright 2018 VMware, all rights reserved
//
// Acceptor for connections from the API/UI servers.

#include <boost/bind.hpp>

#include "athena_api_acceptor.hpp"

using boost::asio::ip::tcp;

athena_api_acceptor::athena_api_acceptor(boost::asio::io_service &io_service,
                                         tcp::endpoint endpoint)
   : acceptor_(io_service, endpoint)
{
   start_accept();
}

void
athena_api_acceptor::start_accept()
{
   athena_api_connection::pointer new_connection =
      athena_api_connection::create(acceptor_.get_io_service());

   acceptor_.async_accept(new_connection->socket(),
                          boost::bind(&athena_api_acceptor::handle_accept,
                                      this,
                                      new_connection,
                                      boost::asio::placeholders::error));
}

void
athena_api_acceptor::handle_accept(
   athena_api_connection::pointer new_connection,
   const boost::system::error_code &error)
{
   if (!error) {
      new_connection->start();
   }

   start_accept();
}
