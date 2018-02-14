// Copyright 2018 VMware, all rights reserved
//
// Acceptor for connections from the API/UI servers.

#include <boost/asio.hpp>
#include <boost/shared_ptr.hpp>

#include "athena_api_connection.hpp"

class athena_api_acceptor {

public:

   athena_api_acceptor(boost::asio::io_service &io_service,
                       boost::asio::ip::tcp::endpoint endpoint);

private:

   void
   start_accept();

   void
   handle_accept(athena_api_connection::pointer new_connection,
                 const boost::system::error_code &error);

   boost::asio::ip::tcp::acceptor acceptor_;
};
