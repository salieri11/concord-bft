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

#ifndef ROCKS_DB_METADATA_STORAGE_HPP
#define ROCKS_DB_METADATA_STORAGE_HPP

#ifdef USE_ROCKSDB

#include <log4cplus/loggingmacros.h>
#include <mutex>
#include "DatabaseInterface.h"
#include "MetadataStorage.hpp"

namespace Blockchain {

typedef std::vector<uint16_t> ObjectIdsVector;

class RocksDBMetadataStorage : public bftEngine::MetadataStorage {
 public:
  explicit RocksDBMetadataStorage(Blockchain::IDBClient *dbClient)
      : logger_(log4cplus::Logger::getInstance(
            "com.concord.vmware.metadatastorage")),
        dbClient_(dbClient) {}
  ~RocksDBMetadataStorage() { delete[] metadataObjectsArray_; }

  void initMaxSizeOfObjects(ObjectDesc *metadataObjectsArray,
                            uint16_t metadataObjectsArrayLength) override;
  void read(uint16_t objectId, uint32_t bufferSize, char *outBufferForObject,
            uint32_t &outActualObjectSize) override;
  void atomicWrite(uint16_t objectId, char *data, uint32_t dataLength) override;
  void beginAtomicWriteOnlyTransaction() override;
  void writeInTransaction(uint16_t objectId, char *data,
                          uint32_t dataLength) override;
  void commitAtomicWriteOnlyTransaction() override;
  Status multiDel(const ObjectIdsVector &objectIds);

 private:
  void verifyOperation(uint32_t dataLen, const char *buffer) const;

 private:
  const char *WRONG_FLOW =
      "beginAtomicWriteOnlyTransaction should be launched first";
  const char *WRONG_PARAMETER = "Wrong parameter value specified";

  log4cplus::Logger logger_;
  IDBClient *dbClient_ = nullptr;
  SetOfKeyValuePairs *transaction_ = nullptr;
  std::mutex ioMutex_;
  ObjectDesc *metadataObjectsArray_ = nullptr;
  uint16_t objectsNum_ = 0;
};

}  // namespace Blockchain

#endif  // ifdef USE_ROCKSDB
#endif
