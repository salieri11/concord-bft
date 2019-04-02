// Copyright 2018 VMware, all rights reserved
//

#ifndef API_CONNECTION_MANAGER_HPP
#define API_CONNECTION_MANAGER_HPP

#include <boost/shared_ptr.hpp>
#include <boost/thread.hpp>
#include <set>
#include "api_connection.hpp"

namespace com {
namespace vmware {
namespace concord {

class ConnectionManager {
 public:
  void start_connection(ApiConnection::pointer pConn);

  void close_connection(ApiConnection::pointer pConn);

 private:
  /* Socket being handled. */
  std::set<com::vmware::concord::ApiConnection::pointer> connections_;
  /* Mutex used to protect updates to connections_ set */
  boost::mutex mutex_;
};

}  // namespace concord
}  // namespace vmware
}  // namespace com

#endif
