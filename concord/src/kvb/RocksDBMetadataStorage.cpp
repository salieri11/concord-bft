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

#include "RocksDBMetadataStorage.hpp"
#include <exception>
#include "BlockchainDBAdapter.h"
#include "HashDefs.h"

using namespace Blockchain;
using namespace std;

namespace Blockchain {

void RocksDBMetadataStorage::verifyOperation(uint32_t dataLen,
                                             char *buffer) const {
  if (!dataLen || !buffer) {
    LOG4CPLUS_ERROR(logger_, WRONG_PARAMETER);
    throw runtime_error(WRONG_PARAMETER);
  }
}

void RocksDBMetadataStorage::initMaxSizeOfObjects(
    ObjectDesc *metadataObjectsArray, uint16_t metadataObjectsArrayLength) {}

void RocksDBMetadataStorage::read(uint16_t objectId, uint32_t bufferSize,
                                  char *outBufferForObject,
                                  uint32_t &outActualObjectSize) {
  verifyOperation(bufferSize, outBufferForObject);
  lock_guard<mutex> lock(ioMutex_);
  Status status =
      dbClient_->get(KeyManipulator::generateMetadataKey(objectId),
                     outBufferForObject, bufferSize, outActualObjectSize);
  if (!status.isOK()) {
    throw runtime_error("RocksDB get failed");
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
    throw runtime_error("RocksDB put failed");
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
    throw runtime_error("RocksDB multiPut failed");
  }
  delete transaction_;
}

uint64_t RocksDBMetadataStorage::getSeqNum() {
  uint64_t seqNum = 0;
  const uint32_t sizeOfSeqNum = sizeof(seqNum);
  char seqNumBuf[sizeOfSeqNum];
  uint32_t outActualObjectSize = 0;
  try {
    read(seqNumObjectId_, sizeOfSeqNum, seqNumBuf, outActualObjectSize);
  } catch (runtime_error &ex) {
    LOG4CPLUS_DEBUG(logger_, "Failed to get a sequence number");
    throw runtime_error("Failed to get a sequence number");
  }
  memcpy(&seqNum, seqNumBuf, outActualObjectSize);
  return seqNum;
}

}  // namespace Blockchain
