// Copyright 2018 VMware, all rights reserved
//
// Acceptor for connections from the API/UI servers.

#include <boost/asio.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>

class athena_api_connection
   : public boost::enable_shared_from_this<athena_api_connection>
{

public:

   typedef boost::shared_ptr<athena_api_connection> pointer;

   static pointer
   create(boost::asio::io_service &io_service);

   boost::asio::ip::tcp::socket&
   socket();

   void
   start();

private:

   athena_api_connection(boost::asio::io_service &io_service);

   void
   handle_write(const boost::system::error_code &error,
                size_t bytes_transferred);

   boost::asio::ip::tcp::socket socket_;
   std::string message_;
};
