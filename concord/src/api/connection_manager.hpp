// Copyright 2018 VMware, all rights reserved
//

#ifndef CONNECTION_MANAGER_HPP
#define CONNECTION_MANAGER_HPP

#include <boost/shared_ptr.hpp>
#include <boost/thread.hpp>
#include <set>
#include "api_connection.hpp"

namespace com {
namespace vmware {
namespace concord {

class connection_manager {
 public:
  void start_connection(api_connection::pointer pConn);

  void close_connection(api_connection::pointer pConn);

 private:
  /* Socket being handled. */
  std::set<com::vmware::concord::api_connection::pointer> connections_;
  /* Mutex used to protect updates to connections_ set */
  boost::mutex mutex_;
};

}  // namespace concord
}  // namespace vmware
}  // namespace com

#endif
