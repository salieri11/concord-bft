// Copyright 2018 VMware, all rights reserved
//
// Layer between api_connection and Blockchain::IClient

#ifndef CONCORD_KVB_CLIENT_HPP
#define CONCORD_KVB_CLIENT_HPP

#include <vector>
#include <boost/lockfree/queue.hpp>

#include "kvb/BlockchainInterfaces.h"
#include "concord.pb.h"

namespace com {
namespace vmware {
namespace concord {

class KVBClient {
private:
   Blockchain::IClient *client_;
   log4cplus::Logger logger_;
   static constexpr size_t OUT_BUFFER_SIZE = 512000;
   char m_outBuffer[OUT_BUFFER_SIZE];
public:
   KVBClient(Blockchain::IClient *client) :
      client_(client),
      logger_(log4cplus::Logger::getInstance("com.vmware.concord")) { }

   ~KVBClient()
   {
      client_->stop();
      Blockchain::release(client_);
   }

   bool send_request_sync(com::vmware::concord::ConcordRequest &req,
                          bool isReadOnly,
                          com::vmware::concord::ConcordResponse &resp);
};

class KVBClientPool {
private:
   log4cplus::Logger logger_;
   boost::lockfree::queue<KVBClient*> clients_;

public:
   KVBClientPool(std::vector<KVBClient*> &clients);
   ~KVBClientPool();

   bool send_request_sync(com::vmware::concord::ConcordRequest &req,
                          bool isReadOnly,
                          com::vmware::concord::ConcordResponse &resp);
};

}
}
}

#endif
