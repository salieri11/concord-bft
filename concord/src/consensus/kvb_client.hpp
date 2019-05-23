// Copyright 2018 VMware, all rights reserved
//
// Layer between api_connection and IClient

#ifndef CONSENSUS_KVB_CLIENT_HPP_
#define CONSENSUS_KVB_CLIENT_HPP_

#include <log4cplus/loggingmacros.h>
#include <boost/lockfree/queue.hpp>
#include <vector>

#include "concord.pb.h"
#include "consensus/blockchain_interfaces.h"

namespace concord {
namespace consensus {

class KVBClient {
 private:
  IClient *client_;
  log4cplus::Logger logger_;
  static constexpr size_t OUT_BUFFER_SIZE = 512000;
  char m_outBuffer[OUT_BUFFER_SIZE];

 public:
  KVBClient(IClient *client)
      : client_(client),
        logger_(log4cplus::Logger::getInstance("com.vmware.concord")) {}

  ~KVBClient() {
    client_->stop();
    release(client_);
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

#endif  // CONSENSUS_KVB_CLIENT_HPP_
