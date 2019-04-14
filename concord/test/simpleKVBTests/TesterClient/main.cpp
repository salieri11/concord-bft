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
#include <string.h>

#include "basicRandomTestsRunner.hpp"
#include "consensus/kvb/bft_configuration.hpp"
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
using namespace BasicRandomTests;

using std::string;

#if defined(_WIN32)
initWinSock();
#endif

ClientParams setupClientParams(int argc, char **argv) {
  ClientParams clientParams;
  clientParams.clientId = UINT16_MAX;
  clientParams.numOfFaulty = UINT16_MAX;
  clientParams.numOfSlow = UINT16_MAX;
  clientParams.numOfOperations = UINT16_MAX;
  char argTempBuffer[PATH_MAX + 10];
  int o = 0;
  while ((o = getopt(argc, argv, "i:f:c:p:n:")) != EOF) {
    switch (o) {
      case 'i': {
        strncpy(argTempBuffer, optarg, sizeof(argTempBuffer) - 1);
        argTempBuffer[sizeof(argTempBuffer) - 1] = 0;
        string idStr = argTempBuffer;
        int tempId = std::stoi(idStr);
        if (tempId >= 0 && tempId < UINT16_MAX)
          clientParams.clientId = (uint16_t)tempId;
      } break;

      case 'f': {
        strncpy(argTempBuffer, optarg, sizeof(argTempBuffer) - 1);
        argTempBuffer[sizeof(argTempBuffer) - 1] = 0;
        string fStr = argTempBuffer;
        int tempfVal = std::stoi(fStr);
        if (tempfVal >= 1 && tempfVal < UINT16_MAX)
          clientParams.numOfFaulty = (uint16_t)tempfVal;
      } break;

      case 'c': {
        strncpy(argTempBuffer, optarg, sizeof(argTempBuffer) - 1);
        argTempBuffer[sizeof(argTempBuffer) - 1] = 0;
        string cStr = argTempBuffer;
        int tempcVal = std::stoi(cStr);
        if (tempcVal >= 0 && tempcVal < UINT16_MAX)
          clientParams.numOfSlow = (uint16_t)tempcVal;
      } break;

      case 'p': {
        strncpy(argTempBuffer, optarg, sizeof(argTempBuffer) - 1);
        argTempBuffer[sizeof(argTempBuffer) - 1] = 0;
        string numOfOpsStr = argTempBuffer;
        int tempfVal = std::stoi(numOfOpsStr);
        if (tempfVal >= 1 && tempfVal < UINT32_MAX)
          clientParams.numOfOperations = (uint32_t)tempfVal;
      } break;

      case 'n': {
        strncpy(argTempBuffer, optarg, sizeof(argTempBuffer) - 1);
        argTempBuffer[sizeof(argTempBuffer) - 1] = 0;
        clientParams.configFileName = argTempBuffer;
      } break;

      default:
        break;
    }
  }
  return clientParams;
}

auto logger = concordlogger::Logger::getLogger("skvbtest.client");

CommConfig setupCommunicationParams(ClientParams &clientParams) {
  CommConfig commParams;
  commParams.maxServerId = 0;

  auto numOfReplicas = clientParams.get_numOfReplicas();
  TestCommConfig testCommConfig(logger);

#ifdef USE_COMM_PLAIN_TCP
  PlainTcpConfig commConfig = testCommConfig.GetTCPConfig(
      true, clientParams.clientId, clientParams.numOfClients, numOfReplicas,
      clientParams.configFileName);
  commParams.maxServerId = commConfig.maxServerId;
#elif USE_COMM_TLS_TCP
  TlsTcpConfig commConfig = testCommConfig.GetTlsTCPConfig(
      true, clientParams.clientId, clientParams.numOfClients, numOfReplicas,
      clientParams.configFileName);
  commParams.certificatesRootPath = commConfig.certificatesRootPath;
#else
  PlainUdpConfig commConfig = testCommConfig.GetUDPConfig(
      true, clientParams.clientId, clientParams.numOfClients, numOfReplicas,
      clientParams.configFileName);
#endif

  commParams.listenIp = commConfig.listenIp;
  commParams.listenPort = commConfig.listenPort;
  commParams.bufferLength = commConfig.bufferLength;
  commParams.nodes = commConfig.nodes;
  commParams.statusCallback = commConfig.statusCallback;
  commParams.selfId = commConfig.selfId;

  return commParams;
}

ClientConsensusConfig setupConsensusParams(ClientParams &clientParams) {
  ClientConsensusConfig consensusConfig;
  consensusConfig.clientId = clientParams.clientId;
  consensusConfig.maxFaulty = clientParams.numOfFaulty;
  consensusConfig.maxSlow = clientParams.numOfSlow;
  return consensusConfig;
}

int main(int argc, char **argv) {
#ifdef USE_LOG4CPP
#include <log4cplus/configurator.h>
  using namespace log4cplus;
  initialize();
  BasicConfigurator logConfig(Logger::getDefaultHierarchy(), false);
  logConfig.configure();
#endif

  ClientParams clientParams = setupClientParams(argc, argv);
  if (clientParams.clientId == UINT16_MAX ||
      clientParams.numOfFaulty == UINT16_MAX ||
      clientParams.numOfSlow == UINT16_MAX ||
      clientParams.numOfOperations == UINT32_MAX) {
    LOG_ERROR(logger, "Wrong usage! Required parameters: "
                          << argv[0] << " -f F -c C -p NUM_OPS -i ID");
    exit(-1);
  }

  ClientConsensusConfig consensusConfig = setupConsensusParams(clientParams);
  CommConfig commConfig = setupCommunicationParams(clientParams);
  IClient *client = createClient(commConfig, consensusConfig);
  BasicRandomTestsRunner testsRunner(logger, *client,
                                     clientParams.numOfOperations);
  testsRunner.run();
}
