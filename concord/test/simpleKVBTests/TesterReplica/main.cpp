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
// This module creates an instance of Blockchain::ReplicaImp class using input
// parameters and launches it. ReplicaImp object serves requests received from
// Blockchain::ClientImp object via communication layer.

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sstream>
#include <thread>

#define USE_ROCKSDB 1

#include "commonKVBTests.hpp"
#include "config/configuration_manager.hpp"
#include "consensus/kvb/Comparators.h"
#include "consensus/kvb/ReplicaImp.h"
#include "consensus/kvb/RocksDBClient.h"
#include "consensus/replica_state_sync_imp.hpp"
#include "ethereum/eth_kvb_storage.hpp"
#include "internalCommandsHandler.hpp"
#include "test_comm_config.hpp"
#include "test_parameters.hpp"

#ifndef _WIN32
#include <sys/param.h>
#include <unistd.h>
#else
#include "winUtils.h"
#endif

using namespace Blockchain;
using namespace bftEngine;
using namespace concord::config;
using concord::ethereum::EthKvbStorage;

using ::TestCommConfig;
using std::string;

ReplicaImp *replica = nullptr;
ReplicaParams replicaParams;

auto logger = concordlogger::Logger::getLogger("skvbtest.replica");

void setupReplicaParams(int argc, char **argv) {
  string idStr;
  char argTempBuffer[PATH_MAX + 10];
  replicaParams.replicaId = UINT16_MAX;
  int o = 0;
  while ((o = getopt(argc, argv, "r:i:k:n:")) != EOF) {
    switch (o) {
      case 'i': {
        strncpy(argTempBuffer, optarg, sizeof(argTempBuffer) - 1);
        argTempBuffer[sizeof(argTempBuffer) - 1] = 0;
        idStr = argTempBuffer;
        int tempId = std::stoi(idStr);
        if (tempId >= 0 && tempId < UINT16_MAX)
          replicaParams.replicaId = (uint16_t)tempId;
      } break;

      case 'k': {
        strncpy(argTempBuffer, optarg, sizeof(argTempBuffer) - 1);
        argTempBuffer[sizeof(argTempBuffer) - 1] = 0;
        replicaParams.keysFilePrefix = argTempBuffer;
      } break;

      case 'n': {
        strncpy(argTempBuffer, optarg, sizeof(argTempBuffer) - 1);
        argTempBuffer[sizeof(argTempBuffer) - 1] = 0;
        replicaParams.configFileName = argTempBuffer;
      } break;

      default:
        break;
    }
  }
}

ReplicaConsensusConfig setupConsensusParams(ReplicaConfig &replicaConfig) {
  ReplicaConsensusConfig consensusConfig;
  consensusConfig.replicaId = replicaConfig.replicaId;
  consensusConfig.cVal = replicaConfig.cVal;
  consensusConfig.fVal = replicaConfig.fVal;
  consensusConfig.numOfClientProxies = replicaConfig.numOfClientProxies;
  consensusConfig.concurrencyLevel = replicaConfig.concurrencyLevel;
  consensusConfig.autoViewChangeEnabled = replicaConfig.autoViewChangeEnabled;
  consensusConfig.viewChangeTimerMillisec =
      replicaConfig.viewChangeTimerMillisec;
  consensusConfig.statusReportTimerMillisec =
      replicaConfig.statusReportTimerMillisec;
  consensusConfig.publicKeysOfReplicas = replicaConfig.publicKeysOfReplicas;
  consensusConfig.replicaPrivateKey = replicaConfig.replicaPrivateKey;

  consensusConfig.thresholdSignerForExecution =
      replicaConfig.thresholdSignerForExecution;
  consensusConfig.thresholdVerifierForExecution =
      replicaConfig.thresholdVerifierForExecution;
  consensusConfig.thresholdSignerForSlowPathCommit =
      replicaConfig.thresholdSignerForSlowPathCommit;
  consensusConfig.thresholdVerifierForSlowPathCommit =
      replicaConfig.thresholdVerifierForSlowPathCommit;
  consensusConfig.thresholdVerifierForCommit =
      replicaConfig.thresholdVerifierForCommit;
  consensusConfig.thresholdSignerForCommit =
      replicaConfig.thresholdSignerForCommit;
  consensusConfig.thresholdSignerForOptimisticCommit =
      replicaConfig.thresholdSignerForOptimisticCommit;
  consensusConfig.thresholdVerifierForOptimisticCommit =
      replicaConfig.thresholdVerifierForOptimisticCommit;

  return consensusConfig;
}

ReplicaConfig setupReplicaConfig() {
  ReplicaConfig replicaConfig;
  replicaConfig.autoViewChangeEnabled = replicaParams.viewChangeEnabled;
  replicaConfig.numOfClientProxies = replicaParams.numOfClients;
  replicaConfig.autoViewChangeEnabled = replicaParams.viewChangeEnabled;
  replicaConfig.viewChangeTimerMillisec = replicaParams.viewChangeTimeout;
  replicaConfig.numOfClientProxies = replicaParams.numOfClients;

  return replicaConfig;
}

CommConfig setupCommunicationParams(ReplicaConfig &replicaConfig) {
  CommConfig commParams;
  commParams.maxServerId = 0;
  // Used to get info from parsed keys file
  TestCommConfig testCommConfig(logger);
  testCommConfig.GetReplicaConfig(replicaParams.replicaId,
                                  replicaParams.keysFilePrefix, &replicaConfig);
  auto numOfReplicas =
      (uint16_t)(3 * replicaConfig.fVal + 2 * replicaConfig.cVal + 1);

#ifdef USE_COMM_PLAIN_TCP
  PlainTcpConfig commConfig = testCommConfig.GetTCPConfig(
      true, replicaParams.replicaId, replicaParams.numOfClients, numOfReplicas,
      replicaParams.configFileName);
  commParams.maxServerId = commConfig.maxServerId;
#elif USE_COMM_TLS_TCP
  TlsTcpConfig commConfig = testCommConfig.GetTlsTCPConfig(
      true, replicaParams.replicaId, replicaParams.numOfClients, numOfReplicas,
      replicaParams.configFileName);
  commParams.certificatesRootPath = commConfig.certificatesRootPath;
#else
  PlainUdpConfig commConfig = testCommConfig.GetUDPConfig(
      true, replicaParams.replicaId, replicaParams.numOfClients, numOfReplicas,
      replicaParams.configFileName);
#endif

  commParams.listenIp = commConfig.listenIp;
  commParams.listenPort = commConfig.listenPort;
  commParams.bufferLength = commConfig.bufferLength;
  commParams.nodes = commConfig.nodes;
  commParams.statusCallback = commConfig.statusCallback;
  commParams.selfId = commConfig.selfId;

  return commParams;
}

void createFirstBlock() {
  const ILocalKeyValueStorageReadOnly &storage = replica->getReadOnlyStorage();
  EthKvbStorage kvbStorage(storage, replica, 0);

  auto blockId = storage.getLastBlock();
  if (blockId == 0)
    kvbStorage.write_block(0x1111111111111111, 0x2222222222222222);
  else
    LOG_INFO(logger, "*** Last DB blockId=" << blockId);
}

void signalHandler(int signum) {
  if (replica) replica->stop();

  LOG_INFO(logger, "Replica " << replicaParams.replicaId << " stopped");
  exit(0);
}

int main(int argc, char **argv) {
#if defined(_WIN32)
  initWinSock();
#endif

#ifdef USE_LOG4CPP
#include <log4cplus/configurator.h>
  using namespace log4cplus;
  initialize();
  BasicConfigurator logConfig(Logger::getDefaultHierarchy(), false);
  logConfig.configure();
#endif

  signal(SIGABRT, signalHandler);
  signal(SIGTERM, signalHandler);

  setupReplicaParams(argc, argv);

  if (replicaParams.replicaId == UINT16_MAX ||
      replicaParams.keysFilePrefix.empty()) {
    LOG_ERROR(logger, "Wrong usage! Required parameters: "
                          << argv[0]
                          << " -k KEYS_FILE_PREFIX -i ID -n COMM_CONFIG_FILE");
    exit(-1);
  }

  ReplicaConfig replicaConfig = setupReplicaConfig();
  CommConfig commConfig = setupCommunicationParams(replicaConfig);
  ReplicaConsensusConfig consensusConfig = setupConsensusParams(replicaConfig);

  std::stringstream dbPath;
  dbPath << BasicRandomTests::DB_FILE_PREFIX << replicaParams.replicaId;
  auto dbClient = new RocksDBClient(dbPath.str(), new RocksKeyComparator());

  auto *replicaStateSync = new ReplicaStateSyncImp;
  replica = dynamic_cast<ReplicaImp *>(
      createReplica(commConfig, consensusConfig, dbClient, *replicaStateSync));

  InternalCommandsHandler cmdHandler(replica, replica, logger);
  replica->set_command_handler(&cmdHandler);

  createFirstBlock();

  replica->start();
  while (replica->isRunning())
    std::this_thread::sleep_for(std::chrono::seconds(3));

  dbClient->close();
}
