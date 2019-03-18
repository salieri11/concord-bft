// Copyright 2019 VMware, all rights reserved
/**
 * Test public functions for RocksDBMetadataStorage class.
 */

#define USE_ROCKSDB 1

#include <log4cplus/configurator.h>
#include <log4cplus/hierarchy.h>
#include <log4cplus/loggingmacros.h>
#include "gtest/gtest.h"
#include "kvb/Comparators.h"
#include "kvb/HashDefs.h"
#include "kvb/RocksDBClient.h"
#include "kvb/RocksDBMetadataStorage.hpp"

using namespace std;
using namespace Blockchain;

namespace {

RocksDBMetadataStorage *metadataStorage = nullptr;
const ObjectId seqNumObjectId = RocksDBMetadataStorage::getSeqNumObjectId();
const ObjectId initialObjectId = 1;
const uint32_t initialObjDataSize = 80;
const uint64_t seqNum = 123456789;
const uint16_t objectsNum = 100;

//
// void verifyMultiGet(KeysVector &keys, Sliver inValues[blocksNum],
//                    KeysVector &outValues) {
//  ASSERT_TRUE(dbClient->multiGet(keys, outValues) == Status::OK());
//  ASSERT_TRUE(outValues.size() == blocksNum);
//  for (int i = 0; i < blocksNum; i++) {
//    ASSERT_TRUE(inValues[i] == outValues[i]);
//  }
//}
//
// void verifyMultiDel(KeysVector &keys) {
//  const Status expectedStatus = Status::NotFound("Not Found");
//  for (const auto &it : keys) {
//    Sliver outValue;
//    ASSERT_TRUE(dbClient->get(it, outValue) == expectedStatus);
//  }
//}
//
// void launchMultiPut(KeysVector &keys, Sliver inValues[blocksNum],
//                    SetOfKeyValuePairs &keyValueMap) {
//  for (auto i = 0; i < blocksNum; i++) {
//    keys[i] = Sliver(createAndFillBuf(keyLen), keyLen);
//    inValues[i] = Sliver(createAndFillBuf(valueLen), valueLen);
//    keyValueMap.insert(KeyValuePair(keys[i], inValues[i]));
//  }
//  ASSERT_TRUE(dbClient->multiPut(keyValueMap).isOK());
//}

uint8_t *fillBufByGivenData(const uint8_t *data, const uint32_t &sizeOfData) {
  auto *inBuf = new uint8_t[sizeOfData];
  memcpy(inBuf, data, sizeOfData);
  return inBuf;
}

void writeSeqNum() {
  uint8_t *data = fillBufByGivenData((uint8_t *)&seqNum, sizeof(seqNum));
  metadataStorage->atomicWrite(seqNumObjectId, (char *)data, sizeof(seqNum));
  delete[] data;
}

uint8_t *createAndFillBuf(size_t length) {
  auto *buffer = new uint8_t[length];
  srand(static_cast<uint>(time(nullptr)));
  for (auto i = 0; i < length; i++) {
    buffer[i] = static_cast<uint8_t>(rand() % 256);
  }
  return buffer;
}

uint8_t *writeRandomData(const ObjectId &objectId, const uint32_t &dataLen) {
  uint8_t *data = createAndFillBuf(dataLen);
  metadataStorage->atomicWrite(objectId, (char *)data, dataLen);
  return data;
}

uint8_t *writeInTransaction(const ObjectId &objectId, const uint32_t &dataLen) {
  uint8_t *data = createAndFillBuf(dataLen);
  metadataStorage->writeInTransaction(objectId, (char *)data, dataLen);
  return data;
}

bool is_match(const uint8_t *exp, const uint8_t *actual, const size_t len) {
  for (size_t i = 0; i < len; i++) {
    if (exp[i] != actual[i]) {
      return false;
    }
  }
  return true;
}

TEST(metadataStorage_test, get_seq_num) {
  writeSeqNum();
  uint64_t outValue = metadataStorage->getSeqNum();
  ASSERT_TRUE(seqNum == outValue);
}

TEST(metadataStorage_test, single_read) {
  auto *inBuf = writeRandomData(initialObjectId, initialObjDataSize);
  auto *outBuf = new uint8_t[initialObjDataSize];
  uint32_t realSize = 0;
  metadataStorage->read(initialObjectId, initialObjDataSize, (char *)outBuf,
                        realSize);
  ASSERT_TRUE(initialObjDataSize == realSize);
  ASSERT_TRUE(is_match(inBuf, outBuf, realSize));
  delete[] inBuf;
  delete[] outBuf;
}

TEST(metadataStorage_test, multi_write) {
  metadataStorage->beginAtomicWriteOnlyTransaction();
  uint8_t *inBuf[objectsNum];
  uint8_t *outBuf[objectsNum];
  uint32_t objectsDataSize[objectsNum] = {initialObjDataSize};
  for (auto i = 0; i < objectsNum; i++) {
    objectsDataSize[i] += i;
    inBuf[i] = writeInTransaction(initialObjectId + i, objectsDataSize[i]);
    outBuf[i] = new uint8_t[objectsDataSize[i]];
  }
  metadataStorage->commitAtomicWriteOnlyTransaction();
  uint32_t realSize = 0;
  for (ObjectId i = 0; i < objectsNum; i++) {
    metadataStorage->read(initialObjectId + i, objectsDataSize[i],
                          (char *)outBuf[i], realSize);
    ASSERT_TRUE(objectsDataSize[i] == realSize);
    ASSERT_TRUE(is_match(inBuf[i], outBuf[i], realSize));
    delete[] inBuf[i];
    delete[] outBuf[i];
  }
}

}  // end namespace

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  log4cplus::initialize();
  log4cplus::Hierarchy &hierarchy = log4cplus::Logger::getDefaultHierarchy();
  hierarchy.disableDebug();
  log4cplus::BasicConfigurator config(hierarchy, false);
  config.configure();
  const string dbPath = "./metadataStorage_test";
  RocksDBClient *dbClient = new RocksDBClient(dbPath, new RocksKeyComparator());
  dbClient->init();
  metadataStorage = new RocksDBMetadataStorage(dbClient);
  int res = RUN_ALL_TESTS();
  dbClient->close();
  return res;
}
