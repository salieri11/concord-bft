// Concord
//
// Copyright (c) 2019 VMware, Inc. All Rights Reserved.
//
// This product is licensed to you under the Apache 2.0 license (the
// "License").  You may not use this product except in compliance with the
// Apache 2.0 License.
//
// This product may include a number of subcomponents with separate copyright
// notices and license terms. Your use of these subcomponents is subject to the
// terms and conditions of the subcomponent's license, as noted in the LICENSE
// file.

#include "rocksdb_metadata_storage.h"

#include <exception>
#include "consensus/blockchain_db_adapter.h"
#include "consensus/hash_defs.h"

using namespace std;

namespace concord {
namespace consensus {

void RocksDBMetadataStorage::verifyOperation(uint32_t dataLen,
                                             const char *buffer) const {
  if (!dataLen || !buffer) {
    LOG4CPLUS_ERROR(logger_, WRONG_PARAMETER);
    throw runtime_error(WRONG_PARAMETER);
  }
}

void RocksDBMetadataStorage::initMaxSizeOfObjects(
    ObjectDesc *metadataObjectsArray, uint16_t metadataObjectsArrayLength) {
  objectsNum_ = metadataObjectsArrayLength;
  metadataObjectsArray_ = new ObjectDesc[objectsNum_];
  for (uint16_t i = 0; i < objectsNum_; ++i) {
    metadataObjectsArray_[i] = metadataObjectsArray[i];
    LOG4CPLUS_DEBUG(
        logger_, "initMaxSizeOfObjects i="
                     << i << " object data: id=" << metadataObjectsArray_[i].id
                     << ", maxSize=" << metadataObjectsArray_[i].maxSize);
  }
}

void RocksDBMetadataStorage::read(uint16_t objectId, uint32_t bufferSize,
                                  char *outBufferForObject,
                                  uint32_t &outActualObjectSize) {
  verifyOperation(bufferSize, outBufferForObject);
  lock_guard<mutex> lock(ioMutex_);
  Status status =
      dbClient_->get(KeyManipulator::generateMetadataKey(objectId),
                     outBufferForObject, bufferSize, outActualObjectSize);
  if (status.isNotFound()) {
    memset(outBufferForObject, 0, bufferSize);
    outActualObjectSize = 0;
    return;
  }
  if (!status.isOK()) {
    throw runtime_error("RocksDB get operation failed");
  }
}

void RocksDBMetadataStorage::atomicWrite(uint16_t objectId, char *data,
                                         uint32_t dataLength) {
  verifyOperation(dataLength, data);
  auto *dataCopy = new uint8_t[dataLength];
  memcpy(dataCopy, data, dataLength);
  lock_guard<mutex> lock(ioMutex_);
  Status status = dbClient_->put(KeyManipulator::generateMetadataKey(objectId),
                                 Sliver(dataCopy, dataLength));
  if (!status.isOK()) {
    throw runtime_error("RocksDB put operation failed");
  }
}

void RocksDBMetadataStorage::beginAtomicWriteOnlyTransaction() {
  LOG4CPLUS_DEBUG(logger_, "Begin atomic transaction");
  lock_guard<mutex> lock(ioMutex_);
  if (transaction_) {
    LOG4CPLUS_INFO(logger_, "Transaction has been opened before; ignoring");
    return;
  }
  transaction_ = new SetOfKeyValuePairs;
}

void RocksDBMetadataStorage::writeInTransaction(uint16_t objectId, char *data,
                                                uint32_t dataLength) {
  LOG4CPLUS_DEBUG(logger_,
                  "objectId=" << objectId << ", dataLength=" << dataLength);
  verifyOperation(dataLength, data);
  auto *dataCopy = new uint8_t[dataLength];
  memcpy(dataCopy, data, dataLength);
  lock_guard<mutex> lock(ioMutex_);
  if (!transaction_) {
    LOG4CPLUS_ERROR(logger_, WRONG_FLOW);
    throw runtime_error(WRONG_FLOW);
  }
  transaction_->insert(
      KeyValuePair(KeyManipulator::generateMetadataKey(objectId),
                   Sliver(dataCopy, dataLength)));
}

void RocksDBMetadataStorage::commitAtomicWriteOnlyTransaction() {
  LOG4CPLUS_DEBUG(logger_, "Commit atomic transaction");
  lock_guard<mutex> lock(ioMutex_);
  if (!transaction_) {
    LOG4CPLUS_ERROR(logger_, WRONG_FLOW);
    throw runtime_error(WRONG_FLOW);
  }
  Status status = dbClient_->multiPut(*transaction_);
  if (!status.isOK()) {
    throw runtime_error("RocksDB multiPut operation failed");
  }
  delete transaction_;
}

Status RocksDBMetadataStorage::multiDel(const ObjectIdsVector &objectIds) {
  size_t objectsNumber = objectIds.size();
  assert(objectsNum_ >= objectsNumber);
  LOG4CPLUS_DEBUG(logger_, "Going to perform multiple delete");
  KeysVector keysVec;
  for (size_t objectId = 0; objectId < objectsNumber; objectId++) {
    Key key = KeyManipulator::generateMetadataKey(objectId);
    keysVec.push_back(key);
    LOG4CPLUS_INFO(logger_,
                   "Deleted object id=" << objectId << ", key=" << key);
  }
  return dbClient_->multiDel(keysVec);
}

}  // namespace consensus
}  // namespace concord
