// Copyright 2018 VMware, all rights reserved
//
// Acceptor for connections from the API/UI servers.

#include <boost/asio.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>

#include "athena.pb.h"

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

    void
    dispatch();

    void
    handle_protocol_request();

    void
    handle_peer_request();

    void
    handle_eth_request(int i);

    void
    handle_test_request();

   athena_api_connection(boost::asio::io_service &io_service);

   void
   handle_write(const boost::system::error_code &error,
                size_t bytes_transferred);

   boost::asio::ip::tcp::socket socket_;
   com::vmware::athena::AthenaRequest athenaRequest_;
   com::vmware::athena::AthenaResponse athenaResponse_;
};
