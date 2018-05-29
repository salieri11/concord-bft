// Copyright 2018 VMware, all rights reserved
//
// Mocks for SBFT for a couple of days, until the extraction is ready.
//
// Clean-room implementation, just looking at KVBlockchain source (not SBFT) to
// surmise what these functions were supposed to do.

#ifndef MOCKLIBBYZ_H
#define MOCKLIBBYZ_H

#include <stdio.h>

void initEnvironment();
void freeEnvironment();

struct Byz_rep {
   size_t size;
   char *contents;
};

struct Byz_req {
   size_t size;
   char *contents;
};

struct Byz_buffer {
   size_t size;
   char *contents;
};

// TODO(BWF): what is the int parameter on the end for?
void Byz_init_client(char *byzConfig, char *byzPrivateConfig, int todo);
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
                     int todo6);

void Byz_replica_run();

void Byz_alloc_request(Byz_req *request, size_t size);
void Byz_free_request(Byz_req *request);
void Byz_alloc_reply(Byz_rep *reply, size_t size);
void Byz_free_reply(Byz_rep *reply);

void Byz_invoke(Byz_req *request, Byz_rep *reply, bool isReadOnly);
void Byz_modify(int todo, int *page);

#endif
