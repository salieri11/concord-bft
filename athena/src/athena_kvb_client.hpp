// Copyright 2018 VMware, all rights reserved
//
// Layer between api_connection and Blockchain::IClient

#ifndef ATHENA_KVB_CLIENT_HPP
#define ATHENA_KVB_CLIENT_HPP

#include <vector>
#include <boost/lockfree/queue.hpp>

#include "kvb/BlockchainInterfaces.h"
#include "athena.pb.h"

namespace com {
namespace vmware {
namespace athena {

class KVBClient {
private:
   Blockchain::IClient *client_;
   log4cplus::Logger logger_;
   static constexpr size_t OUT_BUFFER_SIZE = 512000;
   char m_outBuffer[OUT_BUFFER_SIZE];
public:
   KVBClient(Blockchain::IClient *client) :
      client_(client),
      logger_(log4cplus::Logger::getInstance("com.vmware.athena")) { }

   ~KVBClient()
   {
      client_->stop();
      Blockchain::release(client_);
   }

   bool send_request_sync(com::vmware::athena::AthenaRequest &req,
                          bool isReadOnly,
                          com::vmware::athena::AthenaResponse &resp);
};

class KVBClientPool {
private:
   log4cplus::Logger logger_;
   boost::lockfree::queue<KVBClient*> clients_;

public:
   KVBClientPool(std::vector<KVBClient*> &clients);
   ~KVBClientPool();

   bool send_request_sync(com::vmware::athena::AthenaRequest &req,
                          bool isReadOnly,
                          com::vmware::athena::AthenaResponse &resp);
};

}
}
}

#endif
