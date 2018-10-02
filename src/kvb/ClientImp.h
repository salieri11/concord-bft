// Copyright 2018 VMware, all rights reserved
//
// KV Blockchain client definition.

#ifndef CLIENTIMP_H
#define CLIENTIMP_H

#include "BlockchainInterfaces.h"
#include "SimpleThreadPool.h"
#include "SimpleClient.hpp"
#include "ICommunication.hpp"
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

      virtual Status invokeCommandSynch(const Slice command,
                                        bool isReadOnly,
                                        Slice &outReply,
                                        uint32_t &outActualReplySize) override;

      // release memory allocated by invokeCommandSynch
      virtual Status release(Slice& slice) override;

   protected:

      // ctor & dtor
      ClientImp(Blockchain::CommConfig &commConfig,
                const ClientConsensusConfig &conf);
      virtual ~ClientImp();

      int m_status;

      friend IClient* createClient( Blockchain::CommConfig &commConfig,
                                    const ClientConsensusConfig &conf);
      friend void release(IClient *r);

   private:
      SimpleClient *m_bftClient = nullptr;
      SeqNumberGeneratorForClientRequests *m_SeqNumGenerator = nullptr;
   };
}

#endif
