// Copyright 2018 VMware, all rights reserved
//
// KV Blockchain client implementation.

#include "ClientImp.h"
#include "libbyz.h"

#include "HexTools.h"
#include <chrono>

using namespace Blockchain::Utils;

namespace Blockchain {

class InternalClientThreadPoolController : public SimpleThreadPool::Controller
{
public:

   InternalClientThreadPoolController(ClientImp& c) : client(c)
   {}

   virtual void onThreadBegin()
   {
      Byz_init_client(client.m_byzConfig.c_str(),
                      client.m_byzPrivateConfig.c_str(),
                      0);
   }

   virtual void onThreadEnd()
   {
      // TODO(GG): free byz_client resources ...
   }
private:
   ClientImp& client;
};


class InternalClientJob : public SimpleThreadPool::Job
{
public:

   InternalClientJob(bool isReadOnly,
                     Slice command,
                     int64_t completionToken,
                     IClient::CommandCompletion completionCallback)
   {
      // TODO(GG): what is the max value ???? + add error handling
      assert(command.size() > 0);
      m_isReadOnly = isReadOnly;
      size_t s = command.size();
      m_commandBufferLength = s;
      m_pCommandBuffer = new char[s];

       // TODO(GG): try not to copy the data more than once ...
      memcpy(m_pCommandBuffer, command.data(), s);

      m_completionToken = completionToken;
      m_completionCallback = completionCallback;
   }

   virtual ~InternalClientJob() {
      delete m_pCommandBuffer;
   }

   virtual void execute()
   {
      Byz_rep reply;
      Byz_req request;
      Byz_alloc_request(&request, m_commandBufferLength);
      memcpy(request.contents, m_pCommandBuffer, m_commandBufferLength);
      request.size = m_commandBufferLength;

      Byz_invoke(&request, &reply, m_isReadOnly);

      Status retVal = Status::OK(); // ??
      Slice  replySlice(reply.contents, reply.size);

      if (m_completionCallback) {
         m_completionCallback(m_completionToken, retVal, replySlice);
      }

      Byz_free_request(&request);
      Byz_free_reply(&reply);
   }

   virtual void release()
   {
      delete this;
   }

private:

   bool m_isReadOnly;
   size_t m_commandBufferLength;
   char *m_pCommandBuffer;

   int64_t m_completionToken;
   IClient::CommandCompletion m_completionCallback;
};


struct ClientImpDataForSynchOperation
{
   char *p;
   size_t s;
};

/**
 * Shim for getting SimpleThreadPool data back to requester.
 */
static void handlerForSynchOperations(uint64_t completionToken,
                                      Status returnedStatus,
                                      Slice outreply)
{
   ClientImpDataForSynchOperation* x =
      (ClientImpDataForSynchOperation*)completionToken;

   if (outreply.size() > 0) {
      x->p = new char[outreply.size()];
      memcpy(x->p, outreply.data(), outreply.size());
      x->s = outreply.size();
   } else {
      x->p = NULL;
      x->s = 0;
   }
}


static TlsIndex _allocTlsforClientImp()
{
   TlsIndex i;
   allocTlsIndex(&i);
   // TODO(GG): error handling
   return i;
}

/**
 * Starts the thread pool for the client.
 */
Status ClientImp::start()
{
   if (m_status != Idle) {
      return Status::IllegalOperation("todo");
   }

   m_threadPool.start(m_pController);
   m_status = Running;

   return Status::OK();
}

/**
 * Switches status to "Stopping", then waits for jobs in thread pool to complete
 * before moving to "Idle".
 */
Status ClientImp::stop()
{
   if (m_status != Running) {
      return Status::IllegalOperation("todo");
   }

   m_status = Stopping;

   // TODO(GG): notice that "stop" is not fully supported - (consider to add
   // ability to stop byz_client objects)
   m_threadPool.waitForCompletion();

   m_status = Idle;

   return Status::OK();
}

bool ClientImp::isRunning()
{
   return (m_status == Running);
}


/**
 * Submits a new job to the threadpool, to handle command.
 */
void ClientImp::invokeCommandAsynch(const Slice command,
                                    bool isReadOnly,
                                    uint64_t completionToken,
                                    CommandCompletion h)
{
   InternalClientJob* j =
      new InternalClientJob(isReadOnly, command, completionToken, h);
   m_threadPool.add(j);
}

/**
 * Submits a new job to the thread pool, to handle command, then waits for it to
 * complete, and assigns the result to outReply.
 */
Status ClientImp::invokeCommandSynch(const Slice command,
                                     bool isReadOnly,
                                     Slice& outReply)
{
   ClientImpDataForSynchOperation t;
   t.p = 0; t.s = 0;

   InternalClientJob* j = new InternalClientJob(isReadOnly,
                                                command,
                                                (uint64_t)&t,
                                                handlerForSynchOperations);
   m_threadPool.add(j);

   // TODO(GG): patch (work only becuase we use a single thread)
   m_threadPool.waitForCompletion();

   outReply = Slice(t.p, t.s);

   // TODO(GG): return the real result ...
   return Status::OK();
}


/**
 * Release memory used by slice.
 */
Status ClientImp::release(Slice& slice)
{
   if (slice.size() > 0)
   {
      char* p = (char*)slice.data();
      delete[] p;
      slice = Slice();
   }

   return Status::OK();
}


IClient* createClient(const ClientConsensusConfig& conf)
{
   return new ClientImp(conf.byzConfig, conf.byzPrivateConfig);
}

void release(IClient* r)
{
   ClientImp* p = (ClientImp*)r;
   delete p;
}

ClientImp::ClientImp(string byzConfig, string byzPrivateConfig) :
   m_byzConfig(byzConfig),
   m_byzPrivateConfig(byzPrivateConfig),
   m_TlsData(_allocTlsforClientImp()),
   m_pController(new InternalClientThreadPoolController(*this)),
   // TODO(GG): consider to support several threads (by using the TLS option in
   // the byz engine)
   m_threadPool(1),
   m_status(Idle)
{

}


ClientImp::~ClientImp()
{
   delete m_pController;
}

}
