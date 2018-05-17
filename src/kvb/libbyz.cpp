// Copyright 2018 VMware, all rights reserved
//
// Mocks for SBFT for a couple of days, until the extraction is ready.
//
// Clean-room implementation, just looking at KVBlockchain source (not SBFT) to
// surmise what these functions were supposed to do.

#include <log4cplus/loggingmacros.h>

#include "libbyz.h"

using log4cplus::Logger;

void initEnvironment() {
   LOG4CPLUS_INFO(Logger::getInstance("com.vmware.athena.libbyz"),
                  "Mock libbyz initEnvironment");
}

void freeEnvironment() {
   LOG4CPLUS_INFO(Logger::getInstance("com.vmware.athena.libbyz"),
                  "Mock libbyz freeEnvironment");
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
   LOG4CPLUS_INFO(Logger::getInstance("com.vmware.athena.libbyz"),
                  "Mock libbyz replica run.");
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

   g_exec_command(request, reply,
                  nullptr, /* non_det */
                  0, /* client */
                  isReadOnly);
}

void Byz_modify(int todo, int *page) {
   assert(page != NULL);

   LOG4CPLUS_INFO(Logger::getInstance("com.vmware.athena.libbyz"),
                  "Mock libbyz modify. todo=" << todo <<
                  " *page=" << *page);

   //TODO(BWF): what is this supposed to do?
}
