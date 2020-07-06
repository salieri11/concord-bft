// Copyright 2019 VMware, all rights reserved
/**
 * Test ReplicaStateSyncImp class.
 */

#define USE_ROCKSDB 1

#include "Logger.hpp"
#include "direct_kv_block.h"
#include "direct_kv_db_adapter.h"
#include "direct_kv_storage_factory.h"
#include "gtest/gtest.h"
#include "replica_state_sync_imp.hpp"
#include "storage/concord_block_metadata.h"
#include "storage/db_interface.h"

#include <memory>

using namespace std;

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
logging::Logger logger = logging::Logger::getInstance("com.vmware.test");
const BlockId firstBlockId = 1;
const BlockId thirdBlockId = 2;
const BlockId secondBlockId = 3;
const BlockId lastBlockId = 4;
const uint64_t lastSeqNum = 50;
const BlockId lastReachableBlockId = 999;
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

Sliver setUpBlockContent(const Key &key, const Value &blockValue) {
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
ReplicaStateSyncImp replicaStateSync(
    new ConcordBlockMetadata(keyValueStorageMock));

ConcordBlockMetadata kvbStorage(keyValueStorageMock);

const Sliver blockMetadataInternalKey = kvbStorage.getKey();

const Key lastBlockFullKey =
    keyGen->dataKey(blockMetadataInternalKey, lastBlockId);
const Value lastBlockValue = kvbStorage.serialize(lastSeqNum + 3);

const Key thirdBlockFullKey =
    keyGen->dataKey(blockMetadataInternalKey, thirdBlockId);
const Value thirdBlockValue = kvbStorage.serialize(lastSeqNum + 2);

const Key secondBlockFullKey =
    keyGen->dataKey(blockMetadataInternalKey, secondBlockId);
const Value secondBlockValue = kvbStorage.serialize(lastSeqNum + 1);

const Key firstBlockFullKey =
    keyGen->dataKey(blockMetadataInternalKey, firstBlockId);
const Value firstBlockValue = kvbStorage.serialize(lastSeqNum);

const Value lastReachableBlockValue = kvbStorage.serialize(lastSeqNum);

Status MockILocalKeyValueStorageReadOnly::get(const Key &key,
                                              Value &outValue) const {
  switch (blockIdToBeRead) {
    case lastReachableBlockId:
      outValue = lastReachableBlockValue;
      break;
    case lastBlockId:
      outValue = lastBlockValue;
      blockIdToBeRead = thirdBlockId;
      break;
    case thirdBlockId:
      outValue = thirdBlockValue;
      blockIdToBeRead = secondBlockId;
      break;
    case secondBlockId:
      outValue = secondBlockValue;
      blockIdToBeRead = firstBlockId;
      break;
    case firstBlockId:
      outValue = firstBlockValue;
      break;
    default:
      return Status::GeneralError("Block ID is out of range.");
  }
  return Status::OK();
}

TEST(replicaStateSync_test, state_in_sync) {
  blockIdToBeRead = lastReachableBlockId;

  uint64_t removedBlocks = replicaStateSync.execute(
      logger, *bcDBAdapter, lastReachableBlockId, lastSeqNum);
  ASSERT_EQ(removedBlocks, 0);
}

TEST(replicaStateSync_test, all_blocks_removed) {
  dbClient->put(firstBlockFullKey, firstBlockValue);
  dbClient->put(secondBlockFullKey, secondBlockValue);
  dbClient->put(thirdBlockFullKey, thirdBlockValue);
  dbClient->put(lastBlockFullKey, lastBlockValue);

  Sliver firstBlockDbKey = keyGen->blockKey(secondBlockId - 1);
  Sliver secondBlockDbKey = keyGen->blockKey(secondBlockId);
  Sliver thirdBlockDbKey = keyGen->blockKey(thirdBlockId);
  Sliver lastBlockDbKey = keyGen->blockKey(lastBlockId);

  dbClient->put(firstBlockDbKey,
                setUpBlockContent(firstBlockFullKey, firstBlockValue));
  dbClient->put(secondBlockDbKey,
                setUpBlockContent(secondBlockFullKey, secondBlockValue));
  dbClient->put(thirdBlockDbKey,
                setUpBlockContent(thirdBlockFullKey, thirdBlockValue));
  dbClient->put(lastBlockDbKey,
                setUpBlockContent(lastBlockFullKey, lastBlockValue));

  blockIdToBeRead = lastBlockId;
  uint64_t removedBlocks = replicaStateSync.execute(
      logger, *bcDBAdapter, lastBlockId, lastSeqNum - 1);

  ASSERT_EQ(removedBlocks, 4);
}

TEST(replicaStateSync_test, some_blocks_removed) {
  dbClient->put(firstBlockFullKey, firstBlockValue);
  dbClient->put(secondBlockFullKey, secondBlockValue);
  dbClient->put(thirdBlockFullKey, thirdBlockValue);
  dbClient->put(lastBlockFullKey, lastBlockValue);

  Sliver firstBlockDbKey = keyGen->blockKey(secondBlockId - 1);
  Sliver secondBlockDbKey = keyGen->blockKey(secondBlockId);
  Sliver thirdBlockDbKey = keyGen->blockKey(thirdBlockId);
  Sliver lastBlockDbKey = keyGen->blockKey(lastBlockId);

  dbClient->put(firstBlockDbKey,
                setUpBlockContent(firstBlockFullKey, firstBlockValue));
  dbClient->put(secondBlockDbKey,
                setUpBlockContent(secondBlockFullKey, secondBlockValue));
  dbClient->put(thirdBlockDbKey,
                setUpBlockContent(thirdBlockFullKey, thirdBlockValue));
  dbClient->put(lastBlockDbKey,
                setUpBlockContent(lastBlockFullKey, lastBlockValue));

  blockIdToBeRead = lastBlockId;
  uint64_t removedBlocks =
      replicaStateSync.execute(logger, *bcDBAdapter, lastBlockId, lastSeqNum);

  ASSERT_EQ(removedBlocks, 3);
}

}  // end namespace

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  const string dbPath = "./replicaStateSync_test";
  const auto storageFactory = RocksDBStorageFactory{dbPath};
  const auto databaseSet = storageFactory.newDatabaseSet();

  bcDBAdapter = databaseSet.dbAdapter.get();
  dbClient = databaseSet.dataDBClient;

  int res = RUN_ALL_TESTS();
  dbClient.reset();
  return res;
}
