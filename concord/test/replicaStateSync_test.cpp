// Copyright 2019 VMware, all rights reserved
/**
 * Test ReplicaStateSyncImp class.
 */

#define USE_ROCKSDB 1

#include <log4cplus/configurator.h>
#include <log4cplus/hierarchy.h>
#include <log4cplus/loggingmacros.h>
#include "direct_kv_block.h"
#include "direct_kv_db_adapter.h"
#include "direct_kv_storage_factory.h"
#include "gtest/gtest.h"
#include "replica_state_sync_imp.hpp"
#include "storage/db_interface.h"

#include "storage/concord_block_metadata.h"

#include <memory>

using namespace std;
using namespace log4cplus;

using BlockEntry = concord::kvbc::v1DirectKeyValue::block::detail::Entry;
using BlockHeader = concord::kvbc::v1DirectKeyValue::block::detail::Header;

using concord::kvbc::BlockId;
using concord::kvbc::IBlocksAppender;
using concord::kvbc::IDbAdapter;
using concord::kvbc::ILocalKeyValueStorageReadOnly;
using concord::kvbc::Key;
using concord::kvbc::ReplicaStateSyncImp;
using concord::kvbc::SetOfKeyValuePairs;
using concord::kvbc::Value;
using concord::kvbc::v1DirectKeyValue::IDataKeyGenerator;
using concord::kvbc::v1DirectKeyValue::RocksDBStorageFactory;
using concord::kvbc::v1DirectKeyValue::RocksKeyGenerator;
using concord::storage::ConcordBlockMetadata;
using concord::storage::IDBClient;

using concordUtils::Sliver;
using concordUtils::Status;

namespace {

std::shared_ptr<IDBClient> dbClient;
IDbAdapter *bcDBAdapter = nullptr;
Logger *logger = nullptr;
Value emptyValue;
const BlockId lastBlockId = 2;
const uint64_t lastSeqNum = 50;
const BlockId singleBlockId = 999;
const BlockId prevBlockId = lastBlockId - 1;
const BlockId prevPrevBlockId = lastBlockId - 2;
BlockId blockIdToBeRead = 0;
std::unique_ptr<IDataKeyGenerator> keyGen{
    std::make_unique<RocksKeyGenerator>()};

class MockILocalKeyValueStorageReadOnly : public ILocalKeyValueStorageReadOnly {
 public:
  Status get(const Key &key, Value &outValue) const override;
  Status get(BlockId readVersion, const Sliver &key, Sliver &outValue,
             BlockId &outBlock) const override {
    return Status::OK();
  }
  BlockId getLastBlock() const override { return lastBlockId; }
  Status getBlockData(BlockId blockId,
                      SetOfKeyValuePairs &outBlockData) const override {
    return Status::OK();
  }
  Status mayHaveConflictBetween(const Sliver &key, BlockId fromBlock,
                                BlockId toBlock, bool &outRes) const override {
    return Status::OK();
  }

  BlockId getGenesisBlock() const override {
    EXPECT_TRUE(false) << "getGenesisBlock() should not be called by this test";
    return 0;
  }
};

class MockIBlocksAppender : public IBlocksAppender {
 public:
  Status addBlock(const SetOfKeyValuePairs &updates,
                  BlockId &outBlockId) override {
    return Status::OK();
  }
};

void fillBufAndAdvance(char *&buffer, const void *data, const size_t dataSize) {
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
  char *buf = new char[sizeOfBuf];
  char *ptr = buf;
  fillBufAndAdvance(ptr, &blockHeader, sizeof(blockHeader));
  fillBufAndAdvance(ptr, &entry, sizeof(entry));
  fillBufAndAdvance(ptr, key.data(), key.length());
  fillBufAndAdvance(ptr, blockValue.data(), blockValue.length());

  return Sliver(buf, sizeOfBuf);
}

MockILocalKeyValueStorageReadOnly keyValueStorageMock;
MockIBlocksAppender blocksAppenderMock;
ReplicaStateSyncImp replicaStateSync(
    new ConcordBlockMetadata(keyValueStorageMock));

ConcordBlockMetadata kvbStorage(keyValueStorageMock);

const Sliver blockMetadataInternalKey = kvbStorage.getKey();

const Key lastBlockFullKey =
    keyGen->dataKey(blockMetadataInternalKey, lastBlockId);
const Value lastBlockValue = kvbStorage.serialize(lastSeqNum + 2);

const Key prevBlockFullKey =
    keyGen->dataKey(blockMetadataInternalKey, prevBlockId);
const Value prevBlockValue = kvbStorage.serialize(lastSeqNum + 1);

const Key prevPrevBlockFullKey =
    keyGen->dataKey(blockMetadataInternalKey, prevPrevBlockId);
const Value prevPrevBlockValue = kvbStorage.serialize(lastSeqNum);

const Key singleBlockValueFullKey =
    keyGen->dataKey(blockMetadataInternalKey, singleBlockId);
const Value singleBlockValue = kvbStorage.serialize(lastSeqNum);

Status MockILocalKeyValueStorageReadOnly::get(const Key &key,
                                              Value &outValue) const {
  switch (blockIdToBeRead) {
    case singleBlockId:
      outValue = singleBlockValue;
      break;
    case lastBlockId:
      outValue = lastBlockValue;
      blockIdToBeRead = prevBlockId;
      break;
    case prevBlockId:
      outValue = prevBlockValue;
      blockIdToBeRead = prevPrevBlockId;
      break;
    case prevPrevBlockId:
      outValue = prevPrevBlockValue;
      break;
    default:
      return Status::GeneralError("Block ID is out of range.");
  }
  return Status::OK();
}

TEST(replicaStateSync_test, state_in_sync) {
  blockIdToBeRead = singleBlockId;

  uint64_t removedBlocks = replicaStateSync.execute(*logger, *bcDBAdapter,
                                                    singleBlockId, lastSeqNum);
  ASSERT_EQ(removedBlocks, 0);
}

TEST(replicaStateSync_test, block_removed) {
  dbClient->put(prevPrevBlockFullKey, prevPrevBlockValue);
  dbClient->put(prevBlockFullKey, prevBlockValue);
  dbClient->put(lastBlockFullKey, lastBlockValue);

  Sliver prevPrevBlockDbKey = keyGen->blockKey(prevPrevBlockId);
  Sliver prevBlockDbKey = keyGen->blockKey(prevBlockId);
  Sliver lastBlockDbKey = keyGen->blockKey(lastBlockId);

  dbClient->put(prevPrevBlockDbKey,
                setUpBlockContent(prevPrevBlockFullKey, prevPrevBlockValue));
  dbClient->put(prevBlockDbKey,
                setUpBlockContent(prevBlockFullKey, prevBlockValue));
  dbClient->put(lastBlockDbKey,
                setUpBlockContent(lastBlockFullKey, lastBlockValue));

  blockIdToBeRead = lastBlockId;
  uint64_t removedBlocks =
      replicaStateSync.execute(*logger, *bcDBAdapter, lastBlockId, lastSeqNum);

  ASSERT_EQ(removedBlocks, 2);
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
  const auto storageFactory = RocksDBStorageFactory{dbPath};
  const auto databaseSet = storageFactory.newDatabaseSet();

  bcDBAdapter = databaseSet.dbAdapter.get();
  dbClient = databaseSet.dataDBClient;

  int res = RUN_ALL_TESTS();
  dbClient.reset();
  return res;
}
