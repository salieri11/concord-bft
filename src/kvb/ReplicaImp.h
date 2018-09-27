// Copyright 2018 VMware, all rights reserved
//
// KV Blockchain replica definition.

#ifndef REPLICAIMP_H
#include <map>
#include <functional>
#include <string>

#include <log4cplus/loggingmacros.h>
#include "BlockchainInterfaces.h"
#include "HashDefs.h"
#include "BlockchainDBAdapter.h"
#include "InMemoryDBClient.h"
#include "Threading.h"
#include "ThreadLocalStorage.h"
#include "Replica.hpp"
#include "ReplicaConfig.hpp"
#include "ICommunication.hpp"
#include "CommFactory.hpp"
#include "StatusInfo.h"

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
      virtual Status start() override;
      virtual Status stop() override;
      virtual Status wait() override;

      virtual RepStatus getReplicaStatus() const override;

      virtual const ILocalKeyValueStorageReadOnly &
      getReadOnlyStorage() override;

      virtual Status
      addBlockToIdleReplica(const SetOfKeyValuePairs &updates) override;

      virtual void
      set_command_handler(Blockchain::ICommandsHandler *handler) override;

      // ILocalKeyValueStorageReadOnly methods
      virtual Status get(Slice key, Slice &outValue) const override;

      virtual Status get(BlockId readVersion,
                         Slice key,
                         Slice &outValue,
                         BlockId &outBlock) const override;

      virtual BlockId getLastBlock() const override;

      virtual Status
      getBlockData(BlockId blockId,
                   SetOfKeyValuePairs &outBlockData) const override;

      virtual Status
      mayHaveConflictBetween(Slice key,
                             BlockId fromBlock,
                             BlockId toBlock,
                             bool &outRes) const  override;

      virtual ILocalKeyValueStorageReadOnlyIterator*
      getSnapIterator() const  override;

      virtual Status
      freeSnapIterator(
              ILocalKeyValueStorageReadOnlyIterator *iter) const override;

      virtual void monitor() const override;

      //IBlocksAppender
      virtual Status addBlock(const SetOfKeyValuePairs &updates,
                              BlockId &outBlockId) override;

      /// TODO(IG): these methods are made public since they are needed for
      /// the old state transfer and once the new one will be ready we should
      /// refactor this code
      /// this is to replace the existing static functions
      int get_block(int n, char **page);
      void put_blocks(int count, int *sizes, int *indices, char **pages);
      bool check_nond(char *buffer);

   protected:

      // CTOR & DTOR

      ReplicaImp( Blockchain::CommConfig &commConfig,
                  ReplicaConsensusConfig &config,
                  BlockchainDBAdapter *dbAdapter);
      virtual ~ReplicaImp() override;

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

      ICommandsHandler *m_cmdHandler = nullptr;

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

         virtual Status get(Slice key, Slice &outValue) const override;

         virtual Status get(BlockId readVersion,
                            Slice key,
                            Slice &outValue,
                            BlockId &outBlock) const override;
         virtual BlockId getLastBlock() const override;

         virtual Status
         getBlockData(BlockId blockId,
                     SetOfKeyValuePairs &outBlockData) const override;

         virtual Status mayHaveConflictBetween(Slice key,
                                       BlockId fromBlock,
                                       BlockId toBlock,
                                       bool &outRes) const override;

         virtual ILocalKeyValueStorageReadOnlyIterator*
         getSnapIterator() const override;

         virtual Status freeSnapIterator(
            ILocalKeyValueStorageReadOnlyIterator *iter) const override;
         virtual void monitor() const override;
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
                                    bool& isEnd) override;

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
                                          bool &isEnd) override;

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
                                   bool &isEnd) override;

         // TODO(SG): Not implemented originally!
         virtual KeyValuePair next() override {
            BlockId block = m_currentBlock;
            BlockId dummy;
            bool dummy2;
            return next(block, getCurrent().first, dummy, dummy2);
         }

         // Return current element without moving
         virtual KeyValuePair getCurrent() override;

         virtual bool isEnd() override;
         virtual Status freeInternalIterator();
      };


      // DATA

   private:
      log4cplus::Logger logger;
      //TODO(BWF): this was protected (not private) before adding logger
      //bool m_running;
      Thread  m_thread;
      RepStatus m_currentRepStatus;
      StorageWrapperForIdleMode m_InternalStorageWrapperForIdleMode;

      // storage - TODO(GG): add support for leveldb/rocksdb
      BlockchainDBAdapter* m_bcDbAdapter = nullptr;
      BlockId lastBlock = 0;
      bftEngine::ICommunication *m_ptrComm = nullptr;
      bftEngine::ReplicaConfig m_replicaConfig;
      bftEngine::Replica *m_replicaPtr = nullptr;

      // static methods
      static Slice createBlockFromUpdates(
         const SetOfKeyValuePairs& updates,
         SetOfKeyValuePairs& outUpdatesInNewBlock);
      static SetOfKeyValuePairs fetchBlockData(Slice block);


      // FRIENDS
      friend IReplica*
      createReplica(Blockchain::CommConfig &commConfig,
                    ReplicaConsensusConfig &config,
                    IDBClient* db);
      friend void release(IReplica *r);

   };
}

#endif
