// Copyright 2018 VMware, all rights reserved
//
// KV Blockchain replica definition.

#ifndef REPLICAIMP_H

#include <log4cplus/loggingmacros.h>

#include "BlockchainInterfaces.h"
#include "HashDefs.h"
#include <map>
#include <functional>
#include "BlockchainDBAdapter.h"
#include "InMemoryDBClient.h"
#include "Threading.h"
#include "ThreadLocalStorage.h"
#include "libbyz.h"
#include <string>

using namespace Blockchain::Utils;

namespace Blockchain {

   class ReplicaInitException : public std::exception
   {
   public:
      explicit ReplicaInitException(const std::string &what): msg(what) {};

      virtual const char* what() const noexcept override
      {
         return msg.c_str();
      }

   private:
      std::string msg;
   };

   class ReplicaImp : public IReplica,
      public ILocalKeyValueStorageReadOnly,
      public IBlocksAppender
   {
   public:
      // IReplica methods
      virtual Status start();
      virtual Status stop();
      virtual Status wait();

      virtual RepStatus getReplicaStatus() const;

      virtual const ILocalKeyValueStorageReadOnly &getReadOnlyStorage();
      virtual Status addBlockToIdleReplica(const SetOfKeyValuePairs &updates);

      // ILocalKeyValueStorageReadOnly methods
      virtual Status get(Slice key, Slice &outValue) const;
      virtual Status get(BlockId readVersion,
                         Slice key,
                         Slice &outValue,
                         BlockId &outBlock) const;
      virtual BlockId getLastBlock() const;
      virtual Status getBlockData(BlockId blockId,
                                  SetOfKeyValuePairs &outBlockData) const;
      Status mayHaveConflictBetween(Slice key,
                                    BlockId fromBlock,
                                    BlockId toBlock,
                                    bool &outRes) const;
      virtual ILocalKeyValueStorageReadOnlyIterator* getSnapIterator() const;
      virtual Status freeSnapIterator(
         ILocalKeyValueStorageReadOnlyIterator *iter) const;
      virtual void monitor() const;

      //IBlocksAppender
      virtual Status addBlock(const SetOfKeyValuePairs &updates,
                              BlockId &outBlockId);

   protected:

      // CTOR & DTOR

      ReplicaImp(string byzConfig,
                 string byzPrivateConfig,
                 ICommandsHandler *cmdHandler,
                 BlockchainDBAdapter *dbAdapter,
                 std::function<void(
                         int64_t,
                         std::string,
                         int16_t,
                         std::string)> fPeerConnectivityCallback);
      virtual ~ReplicaImp();

      // METHODS

      Status addBlockInternal(const SetOfKeyValuePairs &updates,
                              BlockId &outBlockId);
      Status getInternal(BlockId readVersion,
                         Slice key,
                         Slice &outValue,
                         BlockId &outBlock) const;
      void insertBlockInternal(BlockId blockId, Slice block);
      Slice getBlockInternal(BlockId blockId) const;
      BlockchainDBAdapter* getBcDbAdapter() const { return m_bcDbAdapter; }
      void revertBlock(BlockId blockId);

      // CONSTANTS

      const std::string m_byzConfig;
      const std::string m_byzPrivateConfig;
      const ICommandsHandler *m_cmdHandler;

      // data
      std::function<void(
              int64_t, std::string, int16_t, std::string)>
              m_fPeerConnectivityCallback;

      // INTERNAL TYPES

      // represents <key,blockId>
      class KeyIDPair
      {
      public:
         const Slice key;
         const BlockId blockId;

         KeyIDPair(Slice s, BlockId i) : key(s), blockId(i) { }

         bool operator<(const KeyIDPair& k) const
         {
            int c = this->key.compare(k.key);
            if (c == 0) {
               return this->blockId > k.blockId;
            } else {
               return c < 0;
            }
         }

         bool operator==(const KeyIDPair& k) const
         {
            if (this->blockId != k.blockId) {
               return false;
            }
            return (this->key.compare(k.key) == 0);
         }
      };

      // TODO(GG): do we want synchronization here ?
      class StorageWrapperForIdleMode : public ILocalKeyValueStorageReadOnly
      {
      private:
         const ReplicaImp *rep;

      public:
         StorageWrapperForIdleMode(const ReplicaImp *r);
         virtual Status get(Slice key, Slice &outValue) const;
         virtual Status get(BlockId readVersion,
                            Slice key,
                            Slice &outValue,
                            BlockId &outBlock) const;
         virtual BlockId getLastBlock() const;
         virtual Status getBlockData(BlockId blockId,
                                     SetOfKeyValuePairs &outBlockData) const;
         Status mayHaveConflictBetween(Slice key,
                                       BlockId fromBlock,
                                       BlockId toBlock,
                                       bool &outRes) const;
         virtual ILocalKeyValueStorageReadOnlyIterator* getSnapIterator() const;
         virtual Status freeSnapIterator(
            ILocalKeyValueStorageReadOnlyIterator *iter) const;
         virtual void monitor() const;
      };

      class StorageIterator : public ILocalKeyValueStorageReadOnlyIterator
      {
      private:
         log4cplus::Logger logger;
         const ReplicaImp *rep;
         BlockId readVersion;
         KeyValuePair m_current;
         BlockId m_currentBlock;
         bool m_isEnd;
         IDBClient::IDBClientIterator *m_iter;

      public:
         StorageIterator(const ReplicaImp *r);
	 virtual ~StorageIterator() {
	    // allocated by calls to rep::...::getIterator
	    delete m_iter;
	 }

         virtual void setReadVersion(BlockId _readVersion)
         {
            readVersion = _readVersion;
         }

         virtual KeyValuePair first(BlockId readVersion,
                                    BlockId& actualVersion,
                                    bool& isEnd);

         // TODO(SG): Not implemented originally!
         virtual KeyValuePair first() override
         {
            BlockId block = m_currentBlock;
            BlockId dummy;
            bool dummy2;
            return first(block, dummy, dummy2);
         }

         // Assumes lexicographical ordering of the keys, seek the first element
         // k >= key
         virtual KeyValuePair seekAtLeast(BlockId readVersion,
                                          Key key,
                                          BlockId &actualVersion,
                                          bool &isEnd);

         // TODO(SG): Not implemented originally!
         virtual KeyValuePair seekAtLeast(Key key) override
         {
            BlockId block = m_currentBlock;
            BlockId dummy;
            bool dummy2;
            return seekAtLeast(block, key, dummy, dummy2);
         }

         // Proceed to next element and return it
         virtual KeyValuePair next(BlockId readVersion,
                                   Key key,
                                   BlockId &actualVersion,
                                   bool &isEnd);

         // TODO(SG): Not implemented originally!
         virtual KeyValuePair next() override {
            BlockId block = m_currentBlock;
            BlockId dummy;
            bool dummy2;
            return next(block, getCurrent().first, dummy, dummy2);
         }

         // Return current element without moving
         virtual KeyValuePair getCurrent();

         virtual bool		 isEnd();
         virtual Status		 freeInternalIterator();
      };


      // DATA

   private:
      log4cplus::Logger logger;

      //TODO(BWF): this was protected (not private) before adding logger
      bool m_running;
      Thread  m_thread;
      static TlsIndex m_sThreadLocalDataIdx;
      RepStatus m_currentRepStatus;
      StorageWrapperForIdleMode m_InternalStorageWrapperForIdleMode;


      // storage - TODO(GG): add support for leveldb/rocksdb
      BlockchainDBAdapter* m_bcDbAdapter;
      BlockId lastBlock = 0;

      // static methods
      static Slice createBlockFromUpdates(
         const SetOfKeyValuePairs& updates,
         SetOfKeyValuePairs& outUpdatesInNewBlock);
      static SetOfKeyValuePairs fetchBlockData(Slice block);

      static int get_block(int n, char **page);
      static void put_blocks(int count, int *sizes, int *indices, char **pages);
      static bool check_nond(Byz_buffer *b);
      static int exec_command(Byz_req *inb,
                              Byz_rep *outb,
                              Byz_buffer *non_det,
                              int client,
                              bool ro);

#if defined(_WIN32)
      static DWORD WINAPI replicaInternalThread(LPVOID param);
#else
      static void* replicaInternalThread(void *param);
#endif

      // FRIENDS

      friend IReplica* createReplica(
         const ReplicaConsensusConfig &consensusConfig,
         ICommandsHandler *cmdHandler,
         IDBClient *db,
         std::function<void(
                 int64_t,
                 std::string,
                 int16_t,
                 std::string)> fPeerConnectivityCallback);
      friend void release(IReplica *r);
   };
}

#endif
