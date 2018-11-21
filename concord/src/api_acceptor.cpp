// Copyright 2018 VMware, all rights reserved
//
// Acceptor for connections from the API/UI servers.

#include <boost/bind.hpp>
#include <boost/thread.hpp>
#include "api_acceptor.hpp"

using boost::asio::ip::tcp;
using boost::asio::io_service;
using boost::system::error_code;

using namespace com::vmware::concord;

api_acceptor::api_acceptor(io_service &io_service,
                           tcp::endpoint endpoint,
                           FilterManager &filterManager,
                           KVBClientPool &clientPool,
                           StatusAggregator &sag)
   : acceptor_(io_service, endpoint),
     filterManager_(filterManager),
     clientPool_(clientPool),
     logger_(log4cplus::Logger::getInstance("com.vmware.concord.api_acceptor")),
     sag_(sag)
{
   // set SO_REUSEADDR option on this socket so that if listener thread fails
   // we can still bind again to this socket
   acceptor_.set_option(boost::asio::ip::tcp::acceptor::reuse_address(true));
   start_accept();
}

void
api_acceptor::start_accept()
{
   LOG4CPLUS_TRACE(logger_, "start_accept enter");

   api_connection::pointer new_connection =
      api_connection::create(acceptor_.get_io_service(),
                             connManager_,
                             filterManager_,
                             clientPool_,
                             sag_);

   acceptor_.async_accept(new_connection->socket(),
                          boost::bind(&api_acceptor::handle_accept,
                                      this,
                                      new_connection,
                                      boost::asio::placeholders::error));
   LOG4CPLUS_TRACE(logger_, "start_accept exit");
}

void
api_acceptor::handle_accept(api_connection::pointer new_connection,
                            const boost::system::error_code &error)
{
   LOG4CPLUS_TRACE(logger_, "handle_accept enter, thread id: " <<
                   boost::this_thread::get_id());
   if (!error) {
      connManager_.start_connection(new_connection);
   } else {
      LOG4CPLUS_ERROR(logger_, error.message());
   }

   LOG4CPLUS_DEBUG(logger_, "handle_accept before start_accept");
   start_accept();
   LOG4CPLUS_TRACE(logger_, "handle_accept exit");
}
