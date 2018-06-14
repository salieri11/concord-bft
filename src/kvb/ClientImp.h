// Copyright 2018 VMware, all rights reserved
//
// KV Blockchain client definition.

#ifndef CLIENTIMP_H
#define CLIENTIMP_H

#include "BlockchainInterfaces.h"
#include "ThreadLocalStorage.h"
#include "SimpleThreadPool.h"
#include <map>

using namespace Blockchain::Utils;

namespace Blockchain {

   class ClientImp : public IClient
   {
   public:
      // IClient methods
      virtual Status start();
      virtual Status stop();

      virtual bool isRunning();

      virtual void invokeCommandAsynch(const Slice command,
                                       bool isReadOnly,
                                       uint64_t completionToken,
                                       CommandCompletion h);

      virtual Status invokeCommandSynch(const Slice command,
                                        bool isReadOnly,
                                        Slice &outReply);

      // release memory allocated by invokeCommandSynch
      virtual Status release(Slice& slice);

   protected:

      // ctor & dtor
      ClientImp(string byzConfig, string byzPrivateConfig);
      virtual ~ClientImp();

      const string m_byzConfig;
      const string m_byzPrivateConfig;

      const TlsIndex m_TlsData;

      SimpleThreadPool::Controller *m_pController;

      SimpleThreadPool m_threadPool;

      int m_status;

      friend class InternalClientJob;
      friend class InternalClientThreadPoolController;

      friend IClient* createClient(const ClientConsensusConfig &conf);
      friend void release(IClient *r);
   };
}

#endif
