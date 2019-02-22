// Copyright 2018 VMware, all rights reserved
//

#include "connection_manager.hpp"
#include <log4cplus/loggingmacros.h>

using namespace com::vmware::concord;

/* statoc logger per class */
static log4cplus::Logger logger_(
    log4cplus::Logger::getInstance("com.vmware.concord.connection_manager"));

void connection_manager::start_connection(api_connection::pointer pConn) {
  LOG4CPLUS_TRACE(logger_, "start_connection enter");

  boost::unique_lock<boost::mutex> lock(mutex_);
  connections_.insert(pConn);
  lock.unlock();

  pConn->start_async();
  LOG4CPLUS_INFO(logger_, "new connection added, live connections: "
                              << connections_.size());
  LOG4CPLUS_TRACE(logger_, "start_connection exit");
}

void connection_manager::close_connection(api_connection::pointer pConn) {
  LOG4CPLUS_TRACE(logger_, "close_connection enter");

  boost::unique_lock<boost::mutex> lock(mutex_);
  connections_.erase(pConn);
  lock.unlock();

  LOG4CPLUS_INFO(logger_, "connection closed and removed, live connections: "
                              << connections_.size());
  LOG4CPLUS_TRACE(logger_, "close_connection exit");
}
