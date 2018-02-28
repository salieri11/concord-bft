// Copyright 2018 VMware, all rights reserved
//
// Handler for connections from the API/UI servers.

#ifndef ATHENA_API_CONNECTION_HPP
#define ATHENA_API_CONNECTION_HPP

#include <boost/asio.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>

#include "athena.pb.h"

namespace com {
   namespace vmware {
      namespace athena {
         class api_connection
            : public boost::enable_shared_from_this<api_connection>
         {

         public:

            typedef boost::shared_ptr<api_connection> pointer;

            static pointer
            create(boost::asio::io_service &io_service);

            boost::asio::ip::tcp::socket&
            socket();

            void
            start();

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

            api_connection(boost::asio::io_service &io_service);

            /* Socket being handled. */
            boost::asio::ip::tcp::socket socket_;

            /* Most recent request read. Currently only one request is read
               at a time, so this is also the request currently being
               processed. */
            com::vmware::athena::AthenaRequest athenaRequest_;

            /* Response being built. See above: only one request is read at
               a time, so only one response is built at a time. */
            com::vmware::athena::AthenaResponse athenaResponse_;
         };
      }
   }
}

#endif
