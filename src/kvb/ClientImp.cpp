// Copyright 2018 VMware, all rights reserved
//
// KV Blockchain client implementation.

#include "ClientImp.h"
#include "CommFactory.hpp"

using namespace Blockchain::Utils;

namespace Blockchain {

/**
 * in current impl, no start semantics needed
 */
Status ClientImp::start()
{
   m_status = Running;
   return Status::OK();
}

/**
 * Switches status to "Stopping", then waits for jobs in thread pool to complete
 * before moving to "Idle".
 */
Status ClientImp::stop()
{
   m_status = Idle;
   return Status::OK();
}

bool ClientImp::isRunning()
{
   return (m_status == Running);
}

/**
 * execute the command synchronously
 */
Status ClientImp::invokeCommandSynch(const Slice command,
                                     bool isReadOnly,
                                     Slice& outReply,
                                     uint32_t &outActualReplySize)
{
   auto seqNum = m_SeqNumGenerator->generateUniqueSequenceNumberForRequest();
   auto res = m_bftClient->sendRequest(isReadOnly,
                                       command.data(),
                                       command.size(),
                                       seqNum,
                                       SimpleClient::INFINITE_TIMEOUT,
                                       outReply.size(),
                                       (char*)outReply.data(),
                                       outActualReplySize);

   assert(res >= -2 && res < 1);

   if(res == 0)
      return Status::OK();
   else if (res == -1)
      return Status::GeneralError(Slice("error"), Slice("timeout"));
   else
      return  Status::InvalidArgument(Slice("error"), Slice("small buffer"));
}

/**
 * TODO(IG):
 * not needed since the slice is owned by the caller
 * should be removed
 */
Status ClientImp::release(Slice& slice)
{
   return Status::OK();
}


IClient* createClient(Blockchain::CommConfig &commConfig,
                      const ClientConsensusConfig &conf)
{
   return new ClientImp(commConfig, conf);
}

void release(IClient* r)
{
   ClientImp* p = (ClientImp*)r;
   delete p;
}

ClientImp::ClientImp(Blockchain::CommConfig &commConfig,
                     const ClientConsensusConfig &conf) :
   m_status(Idle)
{
   /// TODO(IG): same as in ReplicaImp, actual comm type should be from config
   bftEngine::PlainUdpConfig config (  commConfig.listenIp,
                                       commConfig.listenPort,
                                       commConfig.bufferLength,
                                       commConfig.nodes);

   auto comm = bftEngine::CommFactory::create(config);
   m_bftClient = SimpleClient::createSimpleClient(comm,
                                                  conf.clientId,
                                                  conf.maxFaulty,
                                                  conf.maxSlow);
   m_SeqNumGenerator =
           SeqNumberGeneratorForClientRequests::
               createSeqNumberGeneratorForClientRequests();
}


ClientImp::~ClientImp()
{
   if (m_bftClient)
      delete m_bftClient;
   if (m_SeqNumGenerator)
      delete m_SeqNumGenerator;
}

}
