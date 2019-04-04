// Copyright 2018 VMware, all rights reserved
//
// KV Blockchain replica definition.

#ifndef REPLICAIMP_H
#define REPLICAIMP_H

#include <functional>
#include <map>
#include <string>

#include <log4cplus/loggingmacros.h>
#include "BlockchainDBAdapter.h"
#include "BlockchainInterfaces.h"
#include "CommFactory.hpp"
#include "HashDefs.h"
#include "ICommunication.hpp"
#include "InMemoryDBClient.h"
#include "Replica.hpp"
#include "ReplicaConfig.hpp"
#include "StatusInfo.h"

namespace Blockchain {

class RocksDBMetadataStorage;

class ReplicaInitException : public std::exception {
 public:
  explicit ReplicaInitException(const std::string &what) : msg(what){};

  virtual const char *what() const noexcept override { return msg.c_str(); }

 private:
  std::string msg;
};

class ReplicaImp : public IReplica,
                   public ILocalKeyValueStorageReadOnly,
                   public IBlocksAppender {
 public:
  // IReplica methods
  virtual Status start() override;
  virtual Status stop() override;

  virtual RepStatus getReplicaStatus() const override;

  virtual const ILocalKeyValueStorageReadOnly &getReadOnlyStorage() override;

  virtual Status addBlockToIdleReplica(
      const SetOfKeyValuePairs &updates) override;

  virtual void set_command_handler(
      Blockchain::ICommandsHandler *handler) override;

  // ILocalKeyValueStorageReadOnly methods
  virtual Status get(Sliver key, Sliver &outValue) const override;

  virtual Status get(BlockId readVersion, Sliver key, Sliver &outValue,
                     BlockId &outBlock) const override;

  virtual BlockId getLastBlock() const override;

  virtual Status getBlockData(BlockId blockId,
                              SetOfKeyValuePairs &outBlockData) const override;

  virtual Status mayHaveConflictBetween(Sliver key, BlockId fromBlock,
                                        BlockId toBlock,
                                        bool &outRes) const override;

  virtual ILocalKeyValueStorageReadOnlyIterator *getSnapIterator()
      const override;

  virtual Status freeSnapIterator(
      ILocalKeyValueStorageReadOnlyIterator *iter) const override;

  virtual void monitor() const override;

  // IBlocksAppender
  virtual Status addBlock(const SetOfKeyValuePairs &updates,
                          BlockId &outBlockId) override;

 protected:
  // CTOR & DTOR

  ReplicaImp(Blockchain::CommConfig &commConfig, ReplicaConsensusConfig &config,
             BlockchainDBAdapter *dbAdapter);
  virtual ~ReplicaImp() override;

  // METHODS

  Status addBlockInternal(const SetOfKeyValuePairs &updates,
                          BlockId &outBlockId);
  Status getInternal(BlockId readVersion, Sliver key, Sliver &outValue,
                     BlockId &outBlock) const;
  void insertBlockInternal(BlockId blockId, Sliver block);
  Sliver getBlockInternal(BlockId blockId) const;
  BlockchainDBAdapter *getBcDbAdapter() const { return m_bcDbAdapter; }
  void revertBlock(BlockId blockId);

  // INTERNAL TYPES

  // represents <key,blockId>
  class KeyIDPair {
   public:
    const Sliver key;
    const BlockId blockId;

    KeyIDPair(Sliver s, BlockId i) : key(s), blockId(i) {}

    bool operator<(const KeyIDPair &k) const {
      int c = this->key.compare(k.key);
      if (c == 0) {
        return this->blockId > k.blockId;
      } else {
        return c < 0;
      }
    }

    bool operator==(const KeyIDPair &k) const {
      if (this->blockId != k.blockId) {
        return false;
      }
      return (this->key.compare(k.key) == 0);
    }
  };

  // TODO(GG): do we want synchronization here ?
  class StorageWrapperForIdleMode : public ILocalKeyValueStorageReadOnly {
   private:
    const ReplicaImp *rep;

   public:
    StorageWrapperForIdleMode(const ReplicaImp *r);

    virtual Status get(Sliver key, Sliver &outValue) const override;

    virtual Status get(BlockId readVersion, Sliver key, Sliver &outValue,
                       BlockId &outBlock) const override;
    virtual BlockId getLastBlock() const override;

    virtual Status getBlockData(
        BlockId blockId, SetOfKeyValuePairs &outBlockData) const override;

    virtual Status mayHaveConflictBetween(Sliver key, BlockId fromBlock,
                                          BlockId toBlock,
                                          bool &outRes) const override;

    virtual ILocalKeyValueStorageReadOnlyIterator *getSnapIterator()
        const override;

    virtual Status freeSnapIterator(
        ILocalKeyValueStorageReadOnlyIterator *iter) const override;
    virtual void monitor() const override;
  };

  class StorageIterator : public ILocalKeyValueStorageReadOnlyIterator {
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

    virtual void setReadVersion(BlockId _readVersion) {
      readVersion = _readVersion;
    }

    virtual KeyValuePair first(BlockId readVersion, BlockId &actualVersion,
                               bool &isEnd) override;

    // TODO(SG): Not implemented originally!
    virtual KeyValuePair first() override {
      BlockId block = m_currentBlock;
      BlockId dummy;
      bool dummy2;
      return first(block, dummy, dummy2);
    }

    // Assumes lexicographical ordering of the keys, seek the first element
    // k >= key
    virtual KeyValuePair seekAtLeast(BlockId readVersion, Key key,
                                     BlockId &actualVersion,
                                     bool &isEnd) override;

    // TODO(SG): Not implemented originally!
    virtual KeyValuePair seekAtLeast(Key key) override {
      BlockId block = m_currentBlock;
      BlockId dummy;
      bool dummy2;
      return seekAtLeast(block, key, dummy, dummy2);
    }

    // Proceed to next element and return it
    virtual KeyValuePair next(BlockId readVersion, Key key,
                              BlockId &actualVersion, bool &isEnd) override;

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

  class BlockchainAppState
      : public bftEngine::SimpleBlockchainStateTransfer::IAppState {
   public:
    BlockchainAppState(ReplicaImp *const parent);

    virtual bool hasBlock(uint64_t blockId) override;
    virtual bool getBlock(uint64_t blockId, char *outBlock,
                          uint32_t *outBlockSize) override;
    virtual bool getPrevDigestFromBlock(
        uint64_t blockId,
        bftEngine::SimpleBlockchainStateTransfer::StateTransferDigest
            *outPrevBlockDigest) override;
    virtual bool putBlock(uint64_t blockId, char *block,
                          uint32_t blockSize) override;
    virtual uint64_t getLastReachableBlockNum() override;
    virtual uint64_t getLastBlockNum() override;

   private:
    ReplicaImp *const m_ptrReplicaImpl = nullptr;
    log4cplus::Logger m_logger;

    // from IAppState. represents maximal block number n such that all
    // blocks 1 <= i <= n exist
    std::atomic<BlockId> m_lastReachableBlock{0};

    friend class ReplicaImp;
    friend IReplica *createReplica(Blockchain::CommConfig &commConfig,
                                   ReplicaConsensusConfig &config,
                                   IDBClient *db);
  };

  // DATA

 private:
  log4cplus::Logger logger;
  RepStatus m_currentRepStatus;
  StorageWrapperForIdleMode m_InternalStorageWrapperForIdleMode;

  // storage - TODO(GG): add support for leveldb/rocksdb
  BlockchainDBAdapter *m_bcDbAdapter = nullptr;
  BlockId lastBlock = 0;
  bftEngine::ICommunication *m_ptrComm = nullptr;
  bftEngine::ReplicaConfig m_replicaConfig;
  bftEngine::Replica *m_replicaPtr = nullptr;
  ICommandsHandler *m_cmdHandler = nullptr;
  bftEngine::IStateTransfer *m_stateTransfer = nullptr;
  BlockchainAppState *m_appState = nullptr;
  RocksDBMetadataStorage *m_metadataStorage = nullptr;

  // static methods
  static Sliver createBlockFromUpdates(
      const SetOfKeyValuePairs &updates,
      SetOfKeyValuePairs &outUpdatesInNewBlock,
      bftEngine::SimpleBlockchainStateTransfer::StateTransferDigest
          &parentDigest);
  static SetOfKeyValuePairs fetchBlockData(Sliver block);

  // FRIENDS
  friend IReplica *createReplica(Blockchain::CommConfig &commConfig,
                                 ReplicaConsensusConfig &config, IDBClient *db);
  friend void release(IReplica *r);
};
}  // namespace Blockchain

#endif
