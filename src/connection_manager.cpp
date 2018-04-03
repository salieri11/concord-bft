#include "connection_manager.hpp"
#include <log4cplus/loggingmacros.h>

using namespace com::vmware::athena;

/* statoc logger per class */
static log4cplus::Logger logger_(log4cplus::Logger::getInstance(
                                 "com.vmware.athena.connection_manager"));

void
connection_manager::start_connection(api_connection::pointer pConn)
{
   LOG4CPLUS_TRACE(logger_, "start_connection enter");
   connections_.insert(pConn);
   pConn->start_async();
   LOG4CPLUS_INFO(logger_, "new connection added, live connections: "
                  << connections_.size());
   LOG4CPLUS_TRACE(logger_, "start_connection exit");
}

void
connection_manager::close_connection(api_connection::pointer pConn)
{
   LOG4CPLUS_TRACE(logger_, "close_connection enter");
   connections_.erase(pConn);
   LOG4CPLUS_INFO(logger_,
               "connection closed and removed, live connections: " <<
               connections_.size());
   LOG4CPLUS_TRACE(logger_, "close_connection exit");
}
