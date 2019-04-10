// Copyright 2019 VMware, all rights reserved
/**
 * Test ReplicaStateSyncImp class.
 */

#define USE_ROCKSDB 1

#include <log4cplus/configurator.h>
#include <log4cplus/hierarchy.h>
#include <log4cplus/loggingmacros.h>
#include "blockchain/kvb_storage.hpp"
#include "consensus/kvb/BlockchainDBAdapter.h"
#include "consensus/kvb/Comparators.h"
#include "consensus/kvb/HashDefs.h"
#include "consensus/kvb/RocksDBClient.h"
#include "consensus/replica_state_sync_imp.hpp"
#include "gtest/gtest.h"

using namespace std;
using namespace Blockchain;
using namespace log4cplus;

namespace {

RocksDBClient *dbClient = nullptr;
BlockchainDBAdapter *bcDBAdapter = nullptr;
Logger *logger = nullptr;
ReplicaStateSyncImp replicaStateSync;
Value emptyValue;
const BlockId lastBlockId = 2;

class MockILocalKeyValueStorageReadOnly : public ILocalKeyValueStorageReadOnly {
 public:
  Status get(Key key, Value &outValue) const override;
  Status get(BlockId readVersion, Sliver key, Sliver &outValue,
             BlockId &outBlock) const override {
    return Status::OK();
  }
  BlockId getLastBlock() const override { return lastBlockId; }
  Status getBlockData(BlockId blockId,
                      SetOfKeyValuePairs &outBlockData) const override {
    return Status::OK();
  }
  Status mayHaveConflictBetween(Sliver key, BlockId fromBlock, BlockId toBlock,
                                bool &outRes) const override {
    return Status::OK();
  }
  ILocalKeyValueStorageReadOnlyIterator *getSnapIterator() const override {
    return nullptr;
  }
  Status freeSnapIterator(
      ILocalKeyValueStorageReadOnlyIterator *iter) const override {
    ;
  }
  void monitor() const override { ; }
};

class MockIBlocksAppender : public IBlocksAppender {
 public:
  Status addBlock(const SetOfKeyValuePairs &updates,
                  BlockId &outBlockId) override {
    return Status::OK();
  }
};

void fillBufAndAdvance(uint8_t *&buffer, const void *data,
                       const size_t dataSize) {
  memcpy(buffer, data, dataSize);
  buffer += dataSize;
}

Sliver setUpBlockContent(Key key, Value blockValue) {
  BlockHeader blockHeader = {0};
  blockHeader.numberOfElements = 1;

  BlockEntry entry = {0};
  size_t sizeOfMetadata = sizeof(blockHeader) + sizeof(entry);
  entry.keySize = key.length();
  entry.valSize = blockValue.length();
  entry.keyOffset = sizeOfMetadata;
  entry.valOffset = sizeOfMetadata + key.length();

  size_t sizeOfBuf = sizeOfMetadata + key.length() + blockValue.length();
  auto buf = new uint8_t[sizeOfBuf];
  uint8_t *ptr = buf;
  fillBufAndAdvance(ptr, &blockHeader, sizeof(blockHeader));
  fillBufAndAdvance(ptr, &entry, sizeof(entry));
  fillBufAndAdvance(ptr, &key, key.length());
  fillBufAndAdvance(ptr, &blockValue, blockValue.length());

  return Sliver(buf, sizeOfBuf);
}

const uint64_t lastSeqNum = 50;
const BlockId singleBlockId = 999;
const BlockId prevBlockId = lastBlockId - 1;
const BlockId prevPrevBlockId = lastBlockId - 2;

MockILocalKeyValueStorageReadOnly keyValueStorageMock;
MockIBlocksAppender blocksAppenderMock;

concord::blockchain::KVBStorage kvbStorage(keyValueStorageMock,
                                           &blocksAppenderMock, lastSeqNum);

const Sliver blockMetadataInternalKey = kvbStorage.block_metadata_key();

const Key lastBlockFullKey =
    KeyManipulator::genDataDbKey(blockMetadataInternalKey, lastBlockId);
const Value lastBlockValue =
    kvbStorage.set_block_metadata_value(lastSeqNum + 2);

const Key prevBlockFullKey =
    KeyManipulator::genDataDbKey(blockMetadataInternalKey, prevBlockId);
const Value prevBlockValue =
    kvbStorage.set_block_metadata_value(lastSeqNum + 1);

const Key prevPrevBlockFullKey =
    KeyManipulator::genDataDbKey(blockMetadataInternalKey, prevPrevBlockId);
const Value prevPrevBlockValue =
    kvbStorage.set_block_metadata_value(lastSeqNum);

const Key singleBlockValueFullKey =
    KeyManipulator::genDataDbKey(blockMetadataInternalKey, singleBlockId);
const Value singleBlockValue = kvbStorage.set_block_metadata_value(lastSeqNum);

Status MockILocalKeyValueStorageReadOnly::get(Key key, Value &outValue) const {
  if (key == lastBlockFullKey)
    outValue = lastBlockValue;
  else if (key == prevBlockFullKey)
    outValue = prevBlockValue;
  else if (key == prevPrevBlockFullKey)
    outValue = prevPrevBlockValue;
  else if (key == singleBlockValueFullKey)
    outValue = singleBlockValue;
  return Status::OK();
}

TEST(replicaStateSync_test, state_in_sync) {
  uint64_t removedBlocks = replicaStateSync.execute(
      *logger, *bcDBAdapter, keyValueStorageMock, singleBlockId, lastSeqNum);
  ASSERT_TRUE(removedBlocks == 0);
}

TEST(replicaStateSync_test, block_removed) {
  dbClient->put(prevPrevBlockFullKey, prevPrevBlockValue);
  dbClient->put(prevBlockFullKey, prevBlockValue);
  dbClient->put(lastBlockFullKey, lastBlockValue);

  Sliver prevPrevBlockDbKey = KeyManipulator::genBlockDbKey(prevPrevBlockId);
  Sliver prevBlockDbKey = KeyManipulator::genBlockDbKey(prevBlockId);
  Sliver lastBlockDbKey = KeyManipulator::genBlockDbKey(lastBlockId);

  dbClient->put(prevPrevBlockDbKey,
                setUpBlockContent(prevPrevBlockFullKey, prevPrevBlockValue));
  dbClient->put(prevBlockDbKey,
                setUpBlockContent(prevBlockFullKey, prevBlockValue));
  dbClient->put(lastBlockDbKey,
                setUpBlockContent(lastBlockFullKey, lastBlockValue));

  uint64_t removedBlocks = replicaStateSync.execute(
      *logger, *bcDBAdapter, keyValueStorageMock, lastBlockId, lastSeqNum);

  ASSERT_TRUE(removedBlocks == 2);
}

}  // end namespace

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  logger = new Logger(Logger::getInstance("com.vmware.test"));
  initialize();
  Hierarchy &hierarchy = Logger::getDefaultHierarchy();
  hierarchy.disableDebug();
  BasicConfigurator config(hierarchy, false);
  config.configure();
  const string dbPath = "./replicaStateSync_test";
  dbClient = new RocksDBClient(dbPath, new RocksKeyComparator());
  dbClient->init();
  bcDBAdapter = new BlockchainDBAdapter(dbClient);
  int res = RUN_ALL_TESTS();
  dbClient->close();
  delete bcDBAdapter;
  return res;
}
