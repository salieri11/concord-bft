// Copyright 2018 VMware, all rights reserved
//
// Mocks for SBFT for a couple of days, until the extraction is ready.
//
// Clean-room implementation, just looking at KVBlockchain source (not SBFT) to
// surmise what these functions were supposed to do.
//
// WARNING: this is an extremely naive mock. It supports exectuing one request
// at a tyime, communicating through global variables protected by mutexes and
// condition variables.

#include <log4cplus/loggingmacros.h>

#include "libbyz.h"
#include "Threading.h"

using log4cplus::Logger;

using namespace Blockchain::Utils;

/**
 * Communication structures.
 *
 * Client grabs the lock, points g_request/g_response/g_isReadOnly to its
 * request/response/isReadOnly variables, signals on g_reqCond, then begins
 * waiting on g_respCond.
 *
 * Replica begins by waiting on g_reqCond. When the signal arrives, it executes
 * the request and fills out the response, then signals on g_respCond.
 */
Mutex g_reqRespLock;
Byz_req *g_request;
Byz_rep *g_response;
bool g_isReadOnly;

CondVar g_reqCond;
CondVar g_respCond;

void initEnvironment() {
   LOG4CPLUS_INFO(Logger::getInstance("com.vmware.athena.libbyz"),
                  "Mock libbyz initEnvironment");

   init(&g_reqRespLock);
   init(&g_reqCond);
   init(&g_respCond);
   g_request = nullptr;
   g_response = nullptr;
}

void freeEnvironment() {
   LOG4CPLUS_INFO(Logger::getInstance("com.vmware.athena.libbyz"),
                  "Mock libbyz freeEnvironment");

   destroy(&g_reqRespLock);
}

// TODO(BWF): what is the int parameter on the end for?
void Byz_init_client(char *byzConfig, char *byzPrivateConfig, int todo)
{
   assert(byzConfig != NULL);
   assert(byzPrivateConfig != NULL);

   LOG4CPLUS_INFO(Logger::getInstance("com.vmware.athena.libbyz"),
                  "Mock libbyz client init. Public=" << byzConfig <<
                  " Private=" << byzPrivateConfig);
}

/**
 * Replica callback handles. These are only callable from the replica thread,
 * but it's just easiest to have them here.
 */
int (*g_exec_command)(Byz_req *inb,
                      Byz_rep *outb,
                      Byz_buffer *non_det,
                      int client,
                      bool isReadOnly);

int Byz_init_replica(char *byzConfig, char *byzPrivateConfig,
                     int todo1,
                     int (*exec_command)(Byz_req *inb,
                                         Byz_rep *outb,
                                         Byz_buffer *non_det,
                                         int client,
                                         bool isReadOnly),
                     int todo2,
                     int todo3,
                     bool (*check_nond)(Byz_buffer *b),
                     int (*get_block)(int n, char **page),
                     void (*put_blocks)(int count,
                                        int *sizes,
                                        int *indices,
                                        char **pages),
                     int todo4,
                     int todo5,
                     int todo6) {
   assert(byzConfig != NULL);
   assert(byzPrivateConfig != NULL);
   assert(exec_command != NULL);
   assert(check_nond != NULL);
   assert(get_block != NULL);
   assert(put_blocks != NULL);

   LOG4CPLUS_INFO(Logger::getInstance("com.vmware.athena.libbyz"),
                  "Mock libbyz replica init. Public=" << byzConfig <<
                  " Private=" << byzPrivateConfig);

   g_exec_command = exec_command;

   return 0;
}

void Byz_replica_run() {
   log4cplus::Logger logger = Logger::getInstance("com.vmware.athena.libbyz");
   LOG4CPLUS_INFO(logger, "Mock libbyz replica run.");

   while(true) {
      mutexLock(&g_reqRespLock);
      waitCondVar(&g_reqCond, &g_reqRespLock);

      LOG4CPLUS_INFO(logger, "Picked up request.");
      if (g_request) {
         //16384 max, because it's the max of the helen-athena link
         Byz_alloc_reply(g_response, 16384);

         g_exec_command(g_request, g_response,
                        nullptr, /* non_det */
                        0, /* client */
                        g_isReadOnly);
      } else {
         // empty request == shutdown
         break;
      }

      singleSignal(&g_respCond);

      LOG4CPLUS_INFO(logger, "Finished request.");
      mutexUnlock(&g_reqRespLock);
   }

   LOG4CPLUS_INFO(logger, "Replica shutting down.");
}

void Byz_alloc_request(Byz_req *request, size_t size) {
   assert(request != NULL);

   request->contents = (char*)malloc(size);
   if (request->contents != NULL) {
      request->size = size;
   } else {
      request->size = 0;
   }
}

void Byz_free_request(Byz_req *request) {
   assert(request != NULL);
   assert(request->contents != NULL);

   free(request->contents);
   request->contents = NULL;
   request->size = 0;
}

void Byz_alloc_reply(Byz_rep *reply, size_t size) {
   assert(reply != NULL);

   reply->contents = (char*)malloc(size);
   if (reply->contents != NULL) {
      reply->size = size;
   } else {
      reply->size = 0;
   }
}

void Byz_free_reply(Byz_rep *reply) {
   assert(reply != NULL);
   assert(reply->contents != NULL);

   free(reply->contents);
   reply->contents = NULL;
   reply->size = 0;
}

void Byz_invoke(Byz_req *request, Byz_rep *reply, bool isReadOnly) {
   assert(request != NULL);
   assert(reply != NULL);

   LOG4CPLUS_INFO(Logger::getInstance("com.vmware.athena.libbyz"),
                  "Mock libbyz invoke. request.size=" << request->size <<
                  " isReadOnly=" << isReadOnly);

   mutexLock(&g_reqRespLock);
   g_request = request;
   g_response = reply;
   g_isReadOnly = isReadOnly;
   mutexUnlock(&g_reqRespLock);

   singleSignal(&g_reqCond);

   mutexLock(&g_reqRespLock);
   waitCondVar(&g_respCond, &g_reqRespLock);

   // hygiene: don't accidentally overwrite those messages
   g_request = nullptr;
   g_response = nullptr;
   mutexUnlock(&g_reqRespLock);
}

void Byz_modify(int todo, int *page) {
   assert(page != NULL);

   LOG4CPLUS_INFO(Logger::getInstance("com.vmware.athena.libbyz"),
                  "Mock libbyz modify. todo=" << todo <<
                  " *page=" << *page);

   // I think there's nothing to do for this function in the mock.
}
