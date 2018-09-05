// Copyright 2018 VMware, all rights reserved
//
// KV Blockchain client definition.

#ifndef CLIENTIMP_H
#define CLIENTIMP_H

#include "BlockchainInterfaces.h"
#include "ThreadLocalStorage.h"
#include "SimpleThreadPool.h"
#include "../../submodules/concord-bft/bftengine/include/bftengine/SimpleClient.hpp"
#include "../../submodules/concord-bft/bftengine/include/bftengine/ICommunication.hpp"
#include <map>
#include <boost/thread.hpp>

using namespace Blockchain::Utils;
using namespace bftEngine;

namespace Blockchain {
   class ClientImp : public IClient
   {
   public:
      // IClient methods
      virtual Status start() override;
      virtual Status stop() override;

      virtual bool isRunning() override;

      virtual void invokeCommandAsynch(const Slice command,
                                       bool isReadOnly,
                                       uint64_t completionToken,
                                       CommandCompletion h) override;

      virtual Status invokeCommandSynch(const Slice command,
                                        bool isReadOnly,
                                        Slice &outReply) override;

      // release memory allocated by invokeCommandSynch
      virtual Status release(Slice& slice) override;

   protected:

      // ctor & dtor
      ClientImp(ICommunication *comm,
                const ClientConsensusConfig &conf);
      virtual ~ClientImp();

      int m_status;

      friend IClient* createClient( ICommunication *comm,
                                    const ClientConsensusConfig &conf);
      friend void release(IClient *r);

   private:
      SimpleClient *m_bftClient = nullptr;
      SeqNumberGeneratorForClientRequests *m_SeqNumGenerator = nullptr;
      static constexpr size_t OUT_BUFFER_SIZE = 512000;
      char m_outBuffer[OUT_BUFFER_SIZE];
   };
}

#endif
