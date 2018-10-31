// Copyright 2018 VMware, all rights reserved
//
// Thread pool implementation.
//
// TODO(GG): this class should be improved (should be based on standard C++11
// classes)

#ifndef SIMPLETHREADPOOL_H
#define SIMPLETHREADPOOL_H

#include <queue>
#include "Threading.h"

using namespace std;

namespace Blockchain
{
namespace Utils
{

   class SimpleThreadPool
   {
   public:

      class Job
      {
      public:
         virtual void execute() = 0;
         virtual void release() = 0;
      protected:
         // should not be deleted directly - use  release()
         ~Job() {};
      };

      class Controller
      {
      public:
         // TODO(GG): use release() (and make the destructor protected)
         virtual ~Controller() {}
         virtual void* onThreadBegin() = 0;
         virtual void onThreadEnd()   = 0;
      };

      SimpleThreadPool() : SimpleThreadPool(1) { }

      SimpleThreadPool(unsigned char numOfThreads)
         : numberOfThreads(numOfThreads)
      {
         init(&jobQueue_lock);
         init(&jobQueue_cond);
         init(&completion_cond);

         stopped = true;
         threadsArray = NULL;
         runningJobs = 0;
         pController = NULL;
      }

      ~SimpleThreadPool()
      {
         // TODO(GG): if still running, then safely stop the threads

         if (pController) {
            delete pController;
         }

         destroy(&jobQueue_lock);

         //TODO(GG): free more resources ??
      }

      void start(Controller* c = NULL)
      {
         // TODO: ALIN: if(!stopped) throw std::runtime_error("cannot restart");
         // TODO(GG): should be thread safe

         pController = c;
         stopped = false;
         threadsArray = new Thread[numberOfThreads];
         for (int i = 0; i < numberOfThreads; i++) {
            createThread(&threadsArray[i],
                         &SimpleThreadPool::threadExecute,
                         (void*)this);
         }
      }

      void stop(bool executeAllJobs = false)
      {
         // TODO: ALIN: if(stopped) throw std::runtime_error("cannot stop twice");
         // TODO(GG): should be thread safe

         mutexLock(&jobQueue_lock);
         stopped = true;
         broadcastSignal(&jobQueue_cond);
         mutexUnlock(&jobQueue_lock);

         for (int i = 0; i < numberOfThreads; i++) {
            threadJoin(threadsArray[i]);
         }

         delete[] threadsArray;

         while (!jobQueue.empty()) {
            Job* j = jobQueue.front();
            jobQueue.pop();
            if (j) {
               if(executeAllJobs) j->execute();
               j->release();
            }
         }

         broadcastSignal(&completion_cond);

         // TODO(GG): TBD - free resources (e.g. condition vars) ??
      }

      void add(Job* j)
      {
         if (stopped) {
            // error
            return;
         }

         mutexLock(&jobQueue_lock);
         jobQueue.push(j);
         singleSignal(&jobQueue_cond);
         mutexUnlock(&jobQueue_lock);
      }

      void waitForCompletion()
      {
         mutexLock(&jobQueue_lock);
         while (!jobQueue.empty() || runningJobs > 0) {
            waitCondVar(&completion_cond, &jobQueue_lock);
         }
         mutexUnlock(&jobQueue_lock);
      }

   private:

      const unsigned char numberOfThreads;

      Controller *pController;

      Mutex jobQueue_lock;
      CondVar jobQueue_cond;
      CondVar completion_cond;

      bool stopped;
      queue<Job*> jobQueue;
      Thread *threadsArray;

      int runningJobs;

      bool load(Job *&outJob)
      {
         mutexLock(&jobQueue_lock);
         while (jobQueue.empty() && !stopped) {
            broadcastSignal(&completion_cond);
            waitCondVar(&jobQueue_cond, &jobQueue_lock);
         }

         if (!stopped) {
            outJob = jobQueue.front();
            jobQueue.pop();
            runningJobs++;
         }
         mutexUnlock(&jobQueue_lock);
         return !stopped;
      }

#if defined(_WIN32)
      static DWORD WINAPI threadExecute(LPVOID param)
#else
      static void *threadExecute(void *param)
#endif
      {
         SimpleThreadPool *obj = (SimpleThreadPool*)param;

         if (obj->pController) {
            obj->pController->onThreadBegin();
         }

         Job *j = NULL;
         while (obj->load(j)) {
            if (j) {
               j->execute();
               j->release();
               j = NULL;
               mutexLock(&obj->jobQueue_lock);
               obj->runningJobs--;
               mutexUnlock(&obj->jobQueue_lock);
            }
         }

         if (obj->pController) {
            obj->pController->onThreadEnd();
         }

         return 0;
      }
   };
}
}

#endif
