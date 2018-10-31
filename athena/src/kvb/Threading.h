// Copyright 2018 VMware, all rights reserved
//

#ifndef THREADING_H
#define THREADING_H

#if defined(_WIN32)
#include <windows.h>
#else
#include <pthread.h>
#include <unistd.h>
#include <sys/syscall.h>
#include <cstdio>
#endif

// TODO(GG): replace all with standard C++11 classes
namespace Blockchain {
   namespace Utils {

#if defined(_WIN32)


      typedef HANDLE  Thread;

      inline bool createThread(Thread *thread,
                               LPTHREAD_START_ROUTINE  start_routine,
                               void *arg)
      {
         HANDLE t = CreateThread(
            NULL,           // default security attributes
            0,              // use default stack size
            start_routine,  // thread function name
            arg,            // argument to thread function
            0,              // use default creation flags
            NULL);

         if (t != NULL) {
            *thread = t;
            return true;
         } else {
            *thread = NULL;
            return false;
         }
      }

      inline void threadJoin(Thread t)
      {
         WaitForSingleObject(t, INFINITE);
      }

      typedef CRITICAL_SECTION  Mutex;

      // TODO: ALIN: init() and destroy() are pretty generic names that might
      // conflict.

      // TODO: ALIN: We should have a Mutex class that uses RAII to init() and
      // destroy()
      inline bool init(Mutex* m)
      {
         InitializeCriticalSection(m);

         return true;
      }

      // NOTE: ALIN: Added a corresponding destroy. Not sure what happens if
      // it's not called (memory leaks?)
      inline void destroy(Mutex *m)
      {
         DeleteCriticalSection(m);
      }

      inline void mutexLock(Mutex* m)
      {
         EnterCriticalSection(m);
      }

      inline void mutexUnlock(Mutex* m)
      {
         LeaveCriticalSection(m);
      }

      typedef CONDITION_VARIABLE  CondVar;

      inline void init(CondVar *c)
      {
         InitializeConditionVariable(c);
      }


      inline void broadcastSignal(CondVar *c)
      {
         WakeAllConditionVariable(c);
      }

      inline void singleSignal(CondVar *c)
      {
         WakeConditionVariable(c);
      }


      inline void waitCondVar(CondVar *c, Mutex *m)
      {
         SleepConditionVariableCS(c, m, INFINITE);
      }

      inline long GetMyTID()
      {
         return GetCurrentThreadId();
      }

#else

      typedef pthread_t Thread;

      inline bool createThread(Thread *thread,
                               void *(*start_routine) (void *),
                               void *arg)
      {
         int r = pthread_create(thread, NULL, start_routine, arg);
         if (r != 0) {
            printf("pthread_create ret val is %d", r);
         }
         return (r == 0);
      }

      inline void threadJoin(Thread t)
      {
         pthread_join(t, NULL);
      }


      typedef pthread_mutex_t Mutex;

      // TODO: ALIN: init() and destroy() are pretty generic names that might
      // conflict.

      // TODO: ALIN: We should have a Mutex class that uses RAII to init() and
      // destroy()
      inline bool init(Mutex *m)
      {
         int r = pthread_mutex_init(m, NULL);

         return (r==0);
      }

      // NOTE: ALIN: Added a corresponding destroy. Not sure what happens if
      // it's not called (memory leaks?)
      inline void destroy(Mutex *m)
      {
         pthread_mutex_destroy(m);
      }

      inline void mutexLock(Mutex *m)
      {
         pthread_mutex_lock(m);
      }

      inline void mutexUnlock(Mutex *m)
      {
         pthread_mutex_unlock(m);
      }

      typedef pthread_cond_t CondVar;

      inline void init(CondVar *c)
      {
         pthread_cond_init(c, NULL);
      }

      inline void broadcastSignal(CondVar *c)
      {
         pthread_cond_broadcast(c);
      }

      inline void singleSignal(CondVar *c)
      {
         pthread_cond_signal(c);
      }

      inline void waitCondVar(CondVar *c, Mutex *m)
      {
         pthread_cond_wait(c, m);
      }

      inline long GetMyTID()
      {
         // TODO: ALIN: We should have a getCurrentThreadId() function in an
         // Utils:: library, since we also do something similar in
         // threshold-sign/lib/Log.cpp
#ifdef __APPLE__
         uint64_t tid;
         pthread_threadid_np(NULL, &tid);
         return static_cast<long>(tid);
#elif defined(_WIN32)
         return GetCurrentThreadId();
#else
         return syscall(SYS_gettid);
#endif
      }
#endif
      /**
       * RAII Mutex: Useful for avoiding forgot-to-unlock bugs
       *
       * Usage:
       *      void foo(Mutex * m) {
       *          // do some stuff
       *          // do some stuff exclusively
       *          {
       *              LockMutex(m);
       *              exclusiveStuff();
       *          }
       *          // do some stuff
       *      }
       */
      class LockMutex {
      protected:
         Mutex *m;
      public:
      LockMutex(Mutex *m) : m(m) { mutexLock(m); }
         ~LockMutex() { mutexUnlock(m); }
      };
   }
}

#endif
