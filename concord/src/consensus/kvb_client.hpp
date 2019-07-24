// Copyright 2018 VMware, all rights reserved
//
// Layer between api_connection and IClient

#ifndef CONCORD_CONSENSUS_KVB_CLIENT_HPP_
#define CONCORD_CONSENSUS_KVB_CLIENT_HPP_

#include <log4cplus/loggingmacros.h>
#include <boost/lockfree/queue.hpp>
#include <chrono>
#include <vector>

#include "concord.pb.h"
#include "consensus/client_imp.h"
#include "storage/blockchain_interfaces.h"
#include "time/time_pusher.hpp"

namespace concord {

// This breaks the circular dependency between TimePusher and
// KVBClient/KVBClientPool
namespace time {
class TimePusher;
}  // namespace time

namespace consensus {

class KVBClient {
 private:
  concord::storage::IClient *client_;
  std::chrono::milliseconds timeout_;
  std::shared_ptr<concord::time::TimePusher> timePusher_;
  log4cplus::Logger logger_;
  static constexpr size_t OUT_BUFFER_SIZE = 512000;
  char m_outBuffer[OUT_BUFFER_SIZE];

 public:
  KVBClient(concord::storage::IClient *client,
            std::chrono::milliseconds timeout,
            std::shared_ptr<concord::time::TimePusher> timePusher)
      : client_(client),
        timeout_(timeout),
        timePusher_(timePusher),
        logger_(log4cplus::Logger::getInstance("com.vmware.concord")) {}

  ~KVBClient() {
    client_->stop();
    releaseClient(client_);
  }

  bool send_request_sync(com::vmware::concord::ConcordRequest &req,
                         bool isReadOnly,
                         com::vmware::concord::ConcordResponse &resp);
};

class KVBClientPool {
 private:
  log4cplus::Logger logger_;
  boost::lockfree::queue<KVBClient *> clients_;

 public:
  KVBClientPool(std::vector<KVBClient *> &clients);
  ~KVBClientPool();

  bool send_request_sync(com::vmware::concord::ConcordRequest &req,
                         bool isReadOnly,
                         com::vmware::concord::ConcordResponse &resp);
};

}  // namespace consensus
}  // namespace concord

#endif  // CONCORD_CONSENSUS_KVB_CLIENT_HPP_
