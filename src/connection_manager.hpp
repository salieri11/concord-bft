// Copyright 2018 VMware, all rights reserved
//

#ifndef CONNECTION_MANAGER_HPP
#define CONNECTION_MANAGER_HPP

#include <set>
#include <boost/thread.hpp>
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
   /* Mutex used to protect updates to connections_ set */
   boost::mutex mutex_;
};

}
}
}

#endif
