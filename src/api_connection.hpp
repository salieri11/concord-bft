// Copyright 2018 VMware, all rights reserved
//
// Handler for connections from the API/UI servers.

#ifndef ATHENA_API_CONNECTION_HPP
#define ATHENA_API_CONNECTION_HPP

#include <boost/asio.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>
#include <log4cplus/loggingmacros.h>

#include "athena.pb.h"
#include "athena_evm.hpp"

namespace com {
namespace vmware {
namespace athena {
class connection_manager;
class api_connection
   : public boost::enable_shared_from_this<api_connection>
{
#define _BUFFER_LENGTH_ 65536
public:

   typedef boost::shared_ptr<api_connection> pointer;

   static pointer
   create(boost::asio::io_service &io_service,
          connection_manager &connManager,
          com::vmware::athena::EVM &athevm);

   boost::asio::ip::tcp::socket&
   socket();

   void
   start_async();

   void
   close();
private:

   void
   dispatch();

   /* Handlers for each type of request in the protobuf definition. */
   void
   handle_protocol_request();

   void
   handle_peer_request();

   void
   handle_eth_request(int i);

   void
   handle_test_request();

   /* Specific Ethereum Method handlers. */

   void
   handle_eth_sendTransaction(const EthRequest &request);

   /* Constructor. */
   api_connection(boost::asio::io_service &io_service,
                  connection_manager &connManager,
                  com::vmware::athena::EVM &athevm);

   void
   read_async();

   void
   on_read_async_completed(
      const boost::system::error_code &ec,
      const size_t bytes);

   void
   on_write_completed(const boost::system::error_code &ec);

   void
   process_incoming();

   /* Socket being handled. */
   boost::asio::ip::tcp::socket socket_;

   /*
    * Most recent request read. Currently only one request is read at a time, so
    * this is also the request currently being processed.
    */
   com::vmware::athena::AthenaRequest athenaRequest_;

   /*
    * Response being built. See above: only one request is read at a time, so
    * only one response is built at a time.
    */
   com::vmware::athena::AthenaResponse athenaResponse_;

   /* Logger. */
   log4cplus::Logger logger_;

   /* The VM to execute transactions in. */
   com::vmware::athena::EVM &athevm_;

   connection_manager &connManager_;

   boost::asio::ip::tcp::endpoint remotePeer_;

   /* need to be adjusted to real msg max size */
   const int BUFFER_LENGTH = _BUFFER_LENGTH_;

   /* buffer for messages */
   char msgBuffer_ [_BUFFER_LENGTH_];

   const uint8_t MSG_LENGTH_BYTES = 2;
};
}
}
}

#endif
