// Copyright 2018 VMware, all rights reserved
//
// Acceptor for connections from the API/UI servers.

#include <boost/asio.hpp>
#include <boost/shared_ptr.hpp>

#include "api_connection.hpp"

namespace com {
   namespace vmware {
      namespace athena {
         class api_acceptor {

         public:

            api_acceptor(boost::asio::io_service &io_service,
                         boost::asio::ip::tcp::endpoint endpoint,
                         EVM &athevm);

         private:
            EVM &athevm_;

            void
            start_accept();

            void
            handle_accept(api_connection::pointer new_connection,
                          const boost::system::error_code &error);

            boost::asio::ip::tcp::acceptor acceptor_;
         };
      }
   }
}
