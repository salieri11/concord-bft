// Concord
//
// Copyright (c) 2019 VMware, Inc. All Rights Reserved.
//
// This product is licensed to you under the Apache 2.0 license (the "License").
// You may not use this product except in compliance with the Apache 2.0
// License.
//
// This product may include a number of subcomponents with separate copyright
// notices and license terms. Your use of these subcomponents is subject to the
// terms and conditions of the subcomponent's license, as noted in the LICENSE
// file.
// This module provides an ability for offline DB modifications for a specific
// replica.

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fstream>
#include <sstream>
#include <string>
#include "commonKVBTests.hpp"

#define USE_ROCKSDB 1

#ifdef USE_LOG4CPP
#include <log4cplus/configurator.h>
#endif

#include "Logging.hpp"
#include "storage/comparators.h"
#include "storage/rocksdb_client.h"
#include "storage/rocksdb_metadata_storage.h"

using namespace bftEngine;

using concord::consensus::Status;
using concord::storage::ObjectIdsVector;
using concord::storage::RocksDBClient;
using concord::storage::RocksDBMetadataStorage;
using concord::storage::RocksKeyComparator;

std::stringstream dbPath;
RocksDBClient *dbClient = nullptr;
const uint16_t MAX_OBJECT_ID = 10;
const uint16_t MAX_OBJECT_SIZE = 200;
int numOfObjectsToAdd = MAX_OBJECT_ID;
RocksDBMetadataStorage *metadataStorage = nullptr;
ObjectIdsVector objectIdsVector;

enum DB_OPERATION {
  NO_OPERATION,
  GET_LAST_BLOCK_SEQ_NBR,
  ADD_STATE_METADATA_OBJECTS,
  DELETE_LAST_STATE_METADATA
};

DB_OPERATION dbOperation = NO_OPERATION;

auto logger = concordlogger::Logger::getLogger("skvbtest.db_editor");

void printUsageAndExit(char **argv) {
  LOG_ERROR(logger, "Wrong usage! \nRequired parameters: "
                        << argv[0] << " -p FULL_DB_PATH \n"
                        << "Optional parameters: -g or -d");
  exit(-1);
}

void setupDBEditorParams(int argc, char **argv) {
  std::string valStr;
  char argTempBuffer[PATH_MAX + 10];
  int o = 0;
  int tempNum = 0;
  while ((o = getopt(argc, argv, "gdp:a:")) != EOF) {
    switch (o) {
      case 'p':
        strncpy(argTempBuffer, optarg, sizeof(argTempBuffer) - 1);
        argTempBuffer[sizeof(argTempBuffer) - 1] = 0;
        dbPath << argTempBuffer;
        break;
      case 'g':
        dbOperation = GET_LAST_BLOCK_SEQ_NBR;
        break;
      case 'd':
        dbOperation = DELETE_LAST_STATE_METADATA;
        break;
      case 'a':
        strncpy(argTempBuffer, optarg, sizeof(argTempBuffer) - 1);
        argTempBuffer[sizeof(argTempBuffer) - 1] = 0;
        valStr = argTempBuffer;
        tempNum = std::stoi(valStr);
        if (tempNum >= 0 && tempNum < MAX_OBJECT_ID)
          numOfObjectsToAdd = tempNum;
        dbOperation = ADD_STATE_METADATA_OBJECTS;
        break;
      default:
        printUsageAndExit(argv);
    }
  }
}

void setupMetadataStorage() {
  MetadataStorage::ObjectDesc objectsDesc[MAX_OBJECT_ID];
  MetadataStorage::ObjectDesc objectDesc = {0, MAX_OBJECT_SIZE};
  for (uint16_t i = 0; i < MAX_OBJECT_ID; i++) {
    objectDesc.id = i;
    objectsDesc[i] = objectDesc;
    objectIdsVector.push_back(i);
  }
  metadataStorage = new RocksDBMetadataStorage(dbClient);
  metadataStorage->initMaxSizeOfObjects(objectsDesc, MAX_OBJECT_ID);
}

bool addStateMetadataObjcts() {
  char objectData[MAX_OBJECT_SIZE];
  memset(objectData, 0, MAX_OBJECT_SIZE);
  LOG_INFO(logger, "*** Going to add " << numOfObjectsToAdd << " objects");
  for (uint16_t objectId = 0; objectId < numOfObjectsToAdd; objectId++) {
    memcpy(objectData, &objectId, sizeof(objectId));
    metadataStorage->atomicWrite(objectId, objectData, MAX_OBJECT_SIZE);
    LOG_INFO(logger, "*** State metadata for object id "
                         << objectId << " written: " << objectData);
  }
  return true;
}

bool getLastStateMetadata() {
  uint32_t outActualObjectSize = 0;
  char outBufferForObject[MAX_OBJECT_SIZE];
  bool found = false;
  for (uint16_t i = 0; i < MAX_OBJECT_ID; i++) {
    metadataStorage->read(i, MAX_OBJECT_SIZE, outBufferForObject,
                          outActualObjectSize);
    if (outActualObjectSize) {
      found = true;
      LOG_INFO(logger, "*** State metadata for object id "
                           << i << " is " << outBufferForObject);
    }
  }
  if (!found) LOG_ERROR(logger, "No State Metadata objects found");
  return found;
}

bool deleteLastStateMetadata() {
  Status status = metadataStorage->multiDel(objectIdsVector);
  if (!status.isOK()) {
    LOG_ERROR(logger, "Failed to delete metadata keys");
    return false;
  }
  return true;
}

void verifyInputParams(char **argv) {
  if (dbPath.str().empty() || (dbOperation == NO_OPERATION))
    printUsageAndExit(argv);

  std::ifstream ifile(dbPath.str());
  if (ifile.good()) {
    return;
  }

  LOG_ERROR(logger, "Specified DB file " << dbPath.str() << " does not exist.");
  exit(-1);
}

int main(int argc, char **argv) {
#ifdef USE_LOG4CPP
  using namespace log4cplus;
  initialize();
  BasicConfigurator logConfig(Logger::getDefaultHierarchy(), false);
  logConfig.configure();
#endif

  setupDBEditorParams(argc, argv);
  verifyInputParams(argv);

  dbClient = new RocksDBClient(dbPath.str(), new RocksKeyComparator());
  dbClient->init();

  setupMetadataStorage();
  bool res = false;
  switch (dbOperation) {
    case GET_LAST_BLOCK_SEQ_NBR:
      res = getLastStateMetadata();
      break;
    case DELETE_LAST_STATE_METADATA:
      res = deleteLastStateMetadata();
      break;
    case ADD_STATE_METADATA_OBJECTS:
      res = addStateMetadataObjcts();
      break;
    default:;
  }

  std::string result = res ? "success" : "fail";
  LOG_INFO(logger, "*** Operation completed with result: " << result);
  dbClient->close();
  return res;
}
