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
#include "gmock/gmock.h"
#include "gtest/gtest.h"

using namespace std;
using namespace Blockchain;
using namespace com::vmware::concord;
using namespace log4cplus;

namespace {

class MockILocalKeyValueStorageReadOnly : public ILocalKeyValueStorageReadOnly {
 public:
  MOCK_CONST_METHOD2(get, Status(Key key, Value &outValue));
  MOCK_CONST_METHOD4(get, Status(BlockId readVersion, Sliver key,
                                 Sliver &outValue, BlockId &outBlock));
  MOCK_CONST_METHOD0(getLastBlock, BlockId());
  MOCK_CONST_METHOD2(getBlockData,
                     Status(BlockId blockId, SetOfKeyValuePairs &outBlockData));
  MOCK_CONST_METHOD4(mayHaveConflictBetween,
                     Status(Sliver key, BlockId fromBlock, BlockId toBlock,
                            bool &outRes));
  MOCK_CONST_METHOD0(getSnapIterator,
                     ILocalKeyValueStorageReadOnlyIterator *());
  MOCK_CONST_METHOD1(freeSnapIterator,
                     Status(ILocalKeyValueStorageReadOnlyIterator *iter));
  MOCK_CONST_METHOD0(monitor, void());
};

class MockIBlocksAppender : public IBlocksAppender {
 public:
  MOCK_METHOD2(addBlock,
               Status(const SetOfKeyValuePairs &updates, BlockId &outBlockId));
};

RocksDBClient *dbClient = nullptr;
BlockchainDBAdapter *bcDBAdapter = nullptr;
Logger *logger = nullptr;
ReplicaStateSyncImp replicaStateSync;

const BlockId lastBlockId = 2;
const uint64_t lastSeqNum = 50;

Value emptyValue;
MockILocalKeyValueStorageReadOnly keyValueStorageMock;
MockIBlocksAppender blocksAppenderMock;
KVBStorage kvbStorage(keyValueStorageMock, &blocksAppenderMock, lastSeqNum);
const Sliver blockMetadataInternalKey = kvbStorage.build_block_metadata_key();
const Key lastBlockFullKey =
    KeyManipulator::genDataDbKey(blockMetadataInternalKey, lastBlockId);
const Key lastBlockDBKey = KeyManipulator::genBlockDbKey(lastBlockId);

void fillBufAndAdvance(uint8_t *&buffer, const void *data,
                       const size_t dataSize) {
  memcpy(buffer, data, dataSize);
  buffer += sizeof(dataSize);
}

Sliver setUpBlockContent(Key key, Value blockValue) {
  BlockEntryHeader blockHeader = {0};
  BlockEntry entry = {0};
  size_t sizeOfMetaData = sizeof(blockHeader);

  entry.keySize = key.length();
  entry.valSize = blockValue.length();
  entry.keyOffset = sizeOfMetaData;
  entry.valOffset = sizeOfMetaData + key.length();
  blockHeader.numberOfElements = 1;
  memcpy(blockHeader.entries, &entry, sizeof(entry));

  size_t sizeOfBuf = sizeOfMetaData + key.length() + blockValue.length();
  auto buf = new uint8_t[sizeOfBuf];
  uint8_t *ptr = buf;
  fillBufAndAdvance(ptr, &blockHeader, sizeof(blockHeader));
  fillBufAndAdvance(ptr, &key, key.length());
  fillBufAndAdvance(ptr, &blockValue, blockValue.length());

  return Sliver(buf, sizeOfBuf);
}

TEST(replicaStateSync_test, state_in_sync) {
  const Value lastBlockValue = kvbStorage.set_block_metadata_value(lastSeqNum);
  EXPECT_CALL(keyValueStorageMock, get(lastBlockFullKey, emptyValue))
      .WillRepeatedly(DoAll(testing::SetArgReferee<1>(lastBlockValue),
                            testing::Return(Status::OK())));
  uint64_t removedBlocks = replicaStateSync.execute(
      *logger, *bcDBAdapter, keyValueStorageMock, lastBlockId, lastSeqNum);
  ASSERT_TRUE(removedBlocks == 0);
  testing::Mock::AllowLeak(&keyValueStorageMock);
}

TEST(replicaStateSync_test, block_removed) {
  const uint64_t prevBlockId = lastBlockId - 1;
  const uint64_t prevPrevBlockId = prevBlockId - 1;

  const Key prevBlockFullKey =
      KeyManipulator::genDataDbKey(blockMetadataInternalKey, prevBlockId);
  const Value prevBlockValue =
      kvbStorage.set_block_metadata_value(lastSeqNum + 1);

  const Key prevPrevBlockFullKey =
      KeyManipulator::genDataDbKey(blockMetadataInternalKey, prevPrevBlockId);
  const Value prevPrevBlockValue =
      kvbStorage.set_block_metadata_value(lastSeqNum);

  const Value lastBlockValue =
      kvbStorage.set_block_metadata_value(lastSeqNum + 2);

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

  EXPECT_CALL(keyValueStorageMock, get(lastBlockFullKey, emptyValue))
      .WillRepeatedly(DoAll(testing::SetArgReferee<1>(lastBlockValue),
                            testing::Return(Status::OK())));
  EXPECT_CALL(keyValueStorageMock, get(prevBlockFullKey, emptyValue))
      .WillRepeatedly(DoAll(testing::SetArgReferee<1>(prevBlockValue),
                            testing::Return(Status::OK())));
  EXPECT_CALL(keyValueStorageMock, get(prevPrevBlockFullKey, emptyValue))
      .WillRepeatedly(DoAll(testing::SetArgReferee<1>(prevPrevBlockValue),
                            testing::Return(Status::OK())));

  uint64_t removedBlocks = replicaStateSync.execute(
      *logger, *bcDBAdapter, keyValueStorageMock, lastBlockId, lastSeqNum);

  ASSERT_TRUE(removedBlocks == 2);
  testing::Mock::AllowLeak(&keyValueStorageMock);
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
