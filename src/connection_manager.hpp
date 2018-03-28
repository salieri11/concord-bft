#pragma once

#include <set>
#include <boost/shared_ptr.hpp>
#include "api_connection.hpp"

namespace com {
   namespace vmware {
      namespace athena {
         class connection_manager {
         public:
            void
            start_connection(api_connection::pointer pConn);

            void
            close_connection(api_connection::pointer pConn);
         private:

            /* Socket being handled. */
            std::set<com::vmware::athena::api_connection::pointer> connections_;

         };
      }
   }
}
