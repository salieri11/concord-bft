// Copyright 2018 VMware, all rights reserved
//

#include "connection_manager.hpp"
#include "Logger.hpp"

namespace concord {
namespace api {

/* statoc logger per class */
static logging::Logger logger_(logging::getLogger("concord.ConnectionManager"));

void ConnectionManager::start_connection(ApiConnection::pointer pConn) {
  LOG_TRACE(logger_, "start_connection enter");

  boost::unique_lock<boost::mutex> lock(mutex_);
  connections_.insert(pConn);
  lock.unlock();

  pConn->start_async();
  LOG_INFO(logger_,
           "new connection added, live connections: " << connections_.size());
  LOG_TRACE(logger_, "start_connection exit");
}

void ConnectionManager::close_connection(ApiConnection::pointer pConn) {
  LOG_TRACE(logger_, "close_connection enter");

  boost::unique_lock<boost::mutex> lock(mutex_);
  connections_.erase(pConn);
  lock.unlock();

  LOG_INFO(logger_, "connection closed and removed, live connections: "
                        << connections_.size());
  LOG_TRACE(logger_, "close_connection exit");
}

}  // namespace api
}  // namespace concord
