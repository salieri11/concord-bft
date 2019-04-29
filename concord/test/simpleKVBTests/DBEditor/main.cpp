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

#include <stdio.h>
#include <stdlib.h>
#include <fstream>
#include <sstream>
#include "commonKVBTests.hpp"

#define USE_ROCKSDB 1

#ifdef USE_LOG4CPP
#include <log4cplus/configurator.h>
#include <log4cplus/hierarchy.h>
#endif

#include "Logging.hpp"
#include "consensus/kvb/BlockchainDBAdapter.h"
#include "consensus/kvb/Comparators.h"
#include "consensus/kvb/RocksDBClient.h"

using namespace Blockchain;

std::stringstream dbPath;
RocksDBClient *dbClient = nullptr;
BlockchainDBAdapter *bcDBAdapter = nullptr;

auto logger = concordlogger::Logger::getLogger("skvbtest.db_editor");

void setupDBEditorParams(int argc, char **argv) {
  string idStr;
  char argTempBuffer[PATH_MAX + 10];
  int o = 0;
  while ((o = getopt(argc, argv, "p:")) != EOF) {
    if (o == 'p') {
      strncpy(argTempBuffer, optarg, sizeof(argTempBuffer) - 1);
      argTempBuffer[sizeof(argTempBuffer) - 1] = 0;
      dbPath << argTempBuffer;
    }
  }
}

int modifyDB() {
  int res = 0;
  BlockId lastBlockId = bcDBAdapter->getLastReachableBlock();
  if (lastBlockId == BasicRandomTests::FIRST_KVB_BLOCK) {
    LOG_INFO(logger, "DB is empty; nothing changed.");
    return 0;
  }
  LOG_INFO(logger, "Current last reachable block-ID is " << lastBlockId);
  bcDBAdapter->deleteBlockAndItsKeys(lastBlockId);
  LOG_INFO(logger, "New last reachable block-ID is "
                       << bcDBAdapter->getLastReachableBlock());
  return res;
}

void verifyInputParams(char **argv) {
  if (dbPath.str().empty()) {
    LOG_ERROR(logger, "Wrong usage! Required parameters: "
                          << argv[0] << " -p FULL_DB_PATH");
    exit(-1);
  }
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
  Hierarchy &hierarchy = Logger::getDefaultHierarchy();
  hierarchy.disableDebug();
  BasicConfigurator logConfig(hierarchy, false);
  logConfig.configure();
#endif

  setupDBEditorParams(argc, argv);
  verifyInputParams(argv);

  dbClient = new RocksDBClient(dbPath.str(), new RocksKeyComparator());
  dbClient->init();
  bcDBAdapter = new BlockchainDBAdapter(dbClient);

  int res = modifyDB();
  string result = res ? "fail" : "success";
  LOG_INFO(logger, "Operation completed with result: " << result);

  dbClient->close();
  delete bcDBAdapter;
  return res;
}
