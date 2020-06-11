// Copyright 2018 VMware, all rights reserved
//
// Acceptor for connections from the API/UI servers.

#ifndef API_API_ACCEPTOR_HPP
#define API_API_ACCEPTOR_HPP

#include <boost/asio.hpp>
#include <boost/shared_ptr.hpp>
#include "Logger.hpp"
#include "api/api_connection.hpp"
#include "api/connection_manager.hpp"
#include "common/status_aggregator.hpp"
#include "config/configuration_manager.hpp"
#include "consensus/kvb_client.hpp"

namespace concord {
namespace api {

class ApiAcceptor {
 public:
  ApiAcceptor(boost::asio::io_service &io_service,
              boost::asio::ip::tcp::endpoint endpoint,
              concord::consensus::KVBClientPool &clientPool,
              concord::common::StatusAggregator &sag, uint64_t gasLimit,
              uint64_t chainID, bool ethEnabled,
              const concord::config::ConcordConfiguration &nodeConfig);

 private:
  boost::asio::ip::tcp::acceptor acceptor_;
  concord::consensus::KVBClientPool &clientPool_;
  logging::Logger logger_;
  ConnectionManager connManager_;
  concord::common::StatusAggregator sag_;
  uint64_t gasLimit_;
  uint64_t chainID_;
  bool ethEnabled_;
  const concord::config::ConcordConfiguration &nodeConfig_;

  void start_accept();

  void handle_accept(ApiConnection::pointer new_connection,
                     const boost::system::error_code &error);
};

}  // namespace api
}  // namespace concord

#endif  // API_API_ACCEPTOR_HPP
