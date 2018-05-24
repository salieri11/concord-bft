// Copyright 2018 VMware, all rights reserved
//

#ifndef THREADLOCALSTORAGE_H
#define THREADLOCALSTORAGE_H

#if defined(_WIN32)
#include <WinSock2.h>
#include <windows.h>
#else
#include <pthread.h>
#endif
#include <assert.h>

namespace Blockchain {
   namespace Utils {

#if defined(_WIN32)

      typedef DWORD  TlsIndex;

      inline int allocTlsIndex(TlsIndex *i)
      {
         DWORD tIndex = TlsAlloc();
         if (tIndex == TLS_OUT_OF_INDEXES) {
            // error
            return 1;
         }
         *i = tIndex;
         return 0;
      }

      inline int freeTlsIndex(TlsIndex i)
      {
         BOOL r = TlsFree(i);
         return (r ? 0 : 1);
      }

      inline int getTlsVal(TlsIndex i, void **outVal)
      {
         LPVOID v = TlsGetValue(i);
         *outVal = v;
         return 0;
      }

      inline int setTlsVal(TlsIndex i, void *inVal)
      {
         BOOL r = TlsSetValue(i, inVal);
         return (r ? 0 : 1);
      }

#else

      typedef pthread_key_t  TlsIndex;

      inline int allocTlsIndex(TlsIndex *i)
      {
         int r = pthread_key_create(i, NULL);
         return r;
      }

      inline int freeTlsIndex(TlsIndex i)
      {
         int r = pthread_key_delete(i);
         return r;
      }

      inline int getTlsVal(TlsIndex i, void **outVal)
      {
         void *v = pthread_getspecific(i);
         *outVal = v;
         return 0;
      }

      inline int setTlsVal(TlsIndex i, void *inVal)
      {
         int r = pthread_setspecific(i,inVal);
         return r;
      }

#endif

   }
}

#endif
