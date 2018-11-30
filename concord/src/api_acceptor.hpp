// Copyright 2018 VMware, all rights reserved
//
// Acceptor for connections from the API/UI servers.

#ifndef API_ACCEPTOR_HPP
#define API_ACCEPTOR_HPP

#include <boost/asio.hpp>
#include <boost/shared_ptr.hpp>
#include <log4cplus/loggingmacros.h>
#include "api_connection.hpp"
#include "connection_manager.hpp"
#include "concord_kvb_client.hpp"
#include "status_aggregator.hpp"

namespace com {
namespace vmware {
namespace concord {
class api_acceptor {

public:
   api_acceptor(boost::asio::io_service &io_service,
                boost::asio::ip::tcp::endpoint endpoint,
                KVBClientPool &clientPool,
                StatusAggregator &sag);

private:
   boost::asio::ip::tcp::acceptor acceptor_;
   KVBClientPool &clientPool_;
   log4cplus::Logger logger_;
   connection_manager connManager_;
   StatusAggregator sag_;

   void
   start_accept();

   void
   handle_accept(api_connection::pointer new_connection,
                 const boost::system::error_code &error);
};
}
}
}

#endif
