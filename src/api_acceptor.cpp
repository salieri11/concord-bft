// Copyright 2018 VMware, all rights reserved
//
// Acceptor for connections from the API/UI servers.

#include <boost/bind.hpp>
#include "api_acceptor.hpp"

using boost::asio::ip::tcp;
using boost::asio::io_service;
using boost::system::error_code;

using namespace com::vmware::athena;

api_acceptor::api_acceptor(io_service &io_service, tcp::endpoint endpoint)
   : acceptor_(io_service, endpoint),
   logger_(log4cplus::Logger::getInstance("com.vmware.athena.api_acceptor"))
{
   start_accept();
}

void
api_acceptor::start_accept()
{
  LOG4CPLUS_DEBUG(logger_, "start_accept enter");

  api_connection::pointer new_connection =
      api_connection::create(acceptor_.get_io_service(),
			connManager_);

  acceptor_.async_accept(new_connection->socket(),
                          boost::bind(&api_acceptor::handle_accept,
                                       this,
                                       new_connection,
                                       boost::asio::placeholders::error));
  LOG4CPLUS_DEBUG(logger_, "start_accept exit");
}

void
api_acceptor::handle_accept(api_connection::pointer new_connection,
                            const error_code &error)
{
   LOG4CPLUS_DEBUG(logger_, "handle_accept enter");
   if (!error)
      connManager_.start_connection(new_connection);
   else
      LOG4CPLUS_ERROR(logger_, error.message());

   LOG4CPLUS_DEBUG(logger_, "handle_accept before start_accept");
   start_accept();
   LOG4CPLUS_DEBUG(logger_, "handle_accept exit");
}
