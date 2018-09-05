// Copyright 2018 VMware, all rights reserved
//
// KV Blockchain client implementation.

#include "ClientImp.h"

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
                                     Slice& outReply)
{
   auto seqNum = m_SeqNumGenerator->generateUniqueSequenceNumberForRequest();
   auto res = m_bftClient->sendRequest(isReadOnly,
                                       command.data(),
                                       command.size(),
                                       seqNum,
                                       SimpleClient::INFINITE_TIMEOUT,
                                       );

   assert(res >= -2 && res < 1);

   if(res == 0)
      return Status::OK();
   else if (res == -1)
      return Status::GeneralError(Slice("error"), Slice("timeout"));
   else
      return  Status::InvalidArgument(Slice("error"), Slice("small buffer"));
}


/**
 * Release memory used by slice.
 */
Status ClientImp::release(Slice& slice)
{
   memset(m_outBuffer, 0, OUT_BUFFER_SIZE);

   return Status::OK();
}


IClient* createClient(ICommunication *comm,
                      const ClientConsensusConfig &conf)
{
   return new ClientImp(comm, conf);
}

void release(IClient* r)
{
   ClientImp* p = (ClientImp*)r;
   delete p;
}

ClientImp::ClientImp(ICommunication *comm,
                     const ClientConsensusConfig &conf) :
   m_status(Idle)
{
   m_bftClient = SimpleClient::createSimpleClient(comm,
                                                   conf.clientId,
                                                   conf.fVal,
                                                   conf.cVal);
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
