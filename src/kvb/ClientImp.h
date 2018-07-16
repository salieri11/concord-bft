// Copyright 2018 VMware, all rights reserved
//
// KV Blockchain client definition.

#ifndef CLIENTIMP_H
#define CLIENTIMP_H

#include "BlockchainInterfaces.h"
#include "ThreadLocalStorage.h"
#include "SimpleThreadPool.h"
#include <map>
#include <boost/thread.hpp>

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

   private:
      // TODO(Amit): This mutex is only needed because SimpleThreadPool is
      // currently not able to handle multiple jobs in the queue. The
      // `waitForCompletion` function will go in infinite loop if two threads
      // submit a job at the same time before any of the thread actually call
      // `waitForCompletion` function. Once SimpleThreadPool is updated to handle
      // multiple pending jobs this mutex can be removed.
      boost::mutex job_mutex;
   };
}

#endif
