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
   void
   handle_eth_getTxReceipt(const EthRequest &request);
   void
   handle_eth_getStorageAt(const EthRequest &request);

   /* Constructor. */
   api_connection(boost::asio::io_service &io_service,
                  connection_manager &connManager,
                  com::vmware::athena::EVM &athevm);

   uint16_t
   get_message_length(const char * buffer);

   bool
   check_async_error(const boost::system::error_code &ec);

   void
   read_async_header();

   void
   on_read_async_header_completed(const boost::system::error_code &ec,
                                  const size_t bytesRead);

   void
   read_async_message(uint16_t offset, uint16_t expectedBytes);

   void
   on_read_async_message_completed(const boost::system::error_code &ec,
                                   const size_t bytes);

   void
   on_write_completed(const boost::system::error_code &ec);

   void
   process_incoming();

   /* Socket being handled. */
   boost::asio::ip::tcp::socket socket_;

   /*
    * Most recent request read. Currently only one request is read at a time,
    * so this is also the request currently being processed.
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
   static constexpr uint32_t BUFFER_LENGTH = 65536;

   /* buffer for incoming messages */
   char inMsgBuffer_ [BUFFER_LENGTH];

   /* buffer for incoming messages */
   char outMsgBuffer_ [BUFFER_LENGTH];

   const uint8_t MSG_LENGTH_BYTES = 2;
};

}
}
}

#endif
