// Copyright 2018-2019 VMware, all rights reserved
//
// KV Blockchain replica definition.

#pragma once

#include <functional>
#include <map>
#include <string>
#include <atomic>

#include "bftengine/ICommunication.hpp"
#include "communication/CommFactory.hpp"
#include "bftengine/Replica.hpp"
#include "bftengine/ReplicaConfig.hpp"
#include "bcstatetransfer/SimpleBCStateTransfer.hpp"
#include "communication/StatusInfo.h"
#include "Logger.hpp"
#include "KVBCInterfaces.h"
#include "replica_state_sync_imp.hpp"
#include "db_adapter.h"
#include "db_interfaces.h"
#include "memorydb/client.h"
#include "bftengine/DbMetadataStorage.hpp"

namespace concord::kvbc {

class ReplicaImp : public IReplica,
                   public ILocalKeyValueStorageReadOnly,
                   public IBlocksAppender,
                   public bftEngine::SimpleBlockchainStateTransfer::IAppState {
 public:
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // IReplica implementation
  Status start() override;
  Status stop() override;
  bool isRunning() const override { return (m_currentRepStatus == RepStatus::Running); }
  RepStatus getReplicaStatus() const override;
  const ILocalKeyValueStorageReadOnly &getReadOnlyStorage() override;
  Status addBlockToIdleReplica(const concord::storage::SetOfKeyValuePairs &updates) override;
  void set_command_handler(ICommandsHandler *handler) override;

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // ILocalKeyValueStorageReadOnly implementation
  Status get(const Sliver &key, Sliver &outValue) const override;
  Status get(BlockId readVersion, const Sliver &key, Sliver &outValue, BlockId &outBlock) const override;
  BlockId getLastBlock() const override { return getLastBlockNum(); }
  Status getBlockData(BlockId blockId, concord::storage::SetOfKeyValuePairs &outBlockData) const override;
  Status mayHaveConflictBetween(const Sliver &key, BlockId fromBlock, BlockId toBlock, bool &outRes) const override;
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // IBlocksAppender implementation
  Status addBlock(const concord::storage::SetOfKeyValuePairs &updates, BlockId &outBlockId) override;

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // IAppState implementation
  bool hasBlock(BlockId blockId) const override;
  bool getBlock(uint64_t blockId, char *outBlock, uint32_t *outBlockSize) override;
  bool getPrevDigestFromBlock(uint64_t blockId,
                              bftEngine::SimpleBlockchainStateTransfer::StateTransferDigest *) override;
  bool putBlock(const uint64_t blockId, const char *block, const uint32_t blockSize) override;
  uint64_t getLastReachableBlockNum() const override { return m_bcDbAdapter->getLastReachableBlockId(); }
  uint64_t getLastBlockNum() const override { return m_bcDbAdapter->getLatestBlockId(); }
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  ReplicaImp(bftEngine::ICommunication *comm,
             bftEngine::ReplicaConfig &config,
             std::unique_ptr<IDbAdapter> dbAdapter,
             std::shared_ptr<storage::IDBClient> mdt_dbclient,
             std::shared_ptr<concordMetrics::Aggregator> aggregator);

  void setReplicaStateSync(ReplicaStateSync *rss) { replicaStateSync_.reset(rss); }

  ~ReplicaImp() override;

 protected:
  Status addBlockInternal(const concord::storage::SetOfKeyValuePairs &updates, BlockId &outBlockId);
  Status getInternal(BlockId readVersion, Key key, Sliver &outValue, BlockId &outBlock) const;
  void insertBlockInternal(BlockId blockId, Sliver block);
  RawBlock getBlockInternal(BlockId blockId) const;

 private:
  friend class StorageWrapperForIdleMode;

  void createReplicaAndSyncState();

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

 private:
  concordlogger::Logger logger;
  RepStatus m_currentRepStatus;

  std::unique_ptr<IDbAdapter> m_bcDbAdapter;
  bftEngine::ICommunication *m_ptrComm = nullptr;
  bftEngine::ReplicaConfig m_replicaConfig;
  bftEngine::IReplica *m_replicaPtr = nullptr;
  ICommandsHandler *m_cmdHandler = nullptr;
  bftEngine::IStateTransfer *m_stateTransfer = nullptr;
  concord::storage::DBMetadataStorage *m_metadataStorage = nullptr;
  std::unique_ptr<ReplicaStateSync> replicaStateSync_;
  std::shared_ptr<concordMetrics::Aggregator> aggregator_;
};

}  // namespace concord::kvbc