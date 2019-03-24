// Concord
//
// Copyright (c) 2018 VMware, Inc. All Rights Reserved.
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
#include <sstream>
#include <signal.h>
#include <stdlib.h>
#include <thread>

#include "KVBCInterfaces.h"
#include "simpleKVBCTests.h"
#include "CommFactory.hpp"
#include "test_comm_config.hpp"
#include "test_parameters.hpp"

#ifndef _WIN32
#include <sys/param.h>
#include <unistd.h>
#else
#include "winUtils.h"
#endif

#ifdef USE_LOG4CPP
#include <log4cplus/configurator.h>
#endif

using namespace SimpleKVBC;
using namespace bftEngine;

using std::string;
using ::TestCommConfig;

IReplica* r = nullptr;
ReplicaParams rp;
concordlogger::Logger replicaLogger =
		concordlogger::Logger::getLogger("skvbctest.replica");

void signalHandler( int signum ) {
	if(r)
		r->stop();

	LOG_INFO(replicaLogger, "replica " << rp.replicaId << " stopped");
	exit(0);
}

int main(int argc, char **argv) {
#if defined(_WIN32)
	initWinSock();
#endif

#ifdef USE_LOG4CPP
  using namespace log4cplus;
  initialize();
  BasicConfigurator logConfig;
  logConfig.configure();
#endif
	rp.replicaId = UINT16_MAX;

	// allows to attach debugger
	if(rp.debug)
		std::this_thread::sleep_for(std::chrono::seconds(20));

	signal(SIGABRT, signalHandler);
	signal(SIGTERM, signalHandler);
	signal(SIGKILL, signalHandler);

	char argTempBuffer[PATH_MAX+10];
	string idStr;

	int o = 0;
	while ((o = getopt(argc, argv, "i:k:n:")) != EOF) {
		switch (o) {
		case 'i':
		{
			strncpy(argTempBuffer, optarg, sizeof(argTempBuffer) - 1);
			argTempBuffer[sizeof(argTempBuffer) - 1] = 0;
			idStr = argTempBuffer;
			int tempId = std::stoi(idStr);
			if (tempId >= 0 && tempId < UINT16_MAX) 
				rp.replicaId = (uint16_t)tempId;
			// TODO: check repId
		}
		break;

		case 'k':
		{
			strncpy(argTempBuffer, optarg, sizeof(argTempBuffer) - 1);
			argTempBuffer[sizeof(argTempBuffer) - 1] = 0;
			rp.keysFilePrefix = argTempBuffer;
			// TODO: check keysFilePrefix
		}
		break;

		case 'n':
        {
          strncpy(argTempBuffer, optarg, sizeof(argTempBuffer) - 1);
          argTempBuffer[sizeof(argTempBuffer) - 1] = 0;
          rp.configFileName = argTempBuffer;
        }
        break;

		default:
			// nop
			break;
		}
	}

	if(rp.replicaId == UINT16_MAX || rp.keysFilePrefix.empty())
	{
		fprintf(stderr, "%s -k KEYS_FILE_PREFIX -i ID -f COMM_CONFIG_FILE",argv[0]);
		exit(-1);
	}

	// TODO: check arguments

    //used to get info from parsing the key file
	bftEngine::ReplicaConfig replicaConfig;

    TestCommConfig testCommConfig(replicaLogger);
	testCommConfig.GetReplicaConfig(
			rp.replicaId, rp.keysFilePrefix, &replicaConfig);
	replicaConfig.numOfClientProxies = rp.numOfClients;
	replicaConfig.autoViewChangeEnabled = rp.viewChangeEnabled;
	replicaConfig.viewChangeTimerMillisec = rp.viewChangeTimeout;

#ifdef USE_COMM_PLAIN_TCP
	PlainTcpConfig conf = testCommConfig.GetTCPConfig(true, rp.replicaId,
                                                    rp.numOfClients,
                                                    rp.numOfReplicas,
                                                    rp.configFileName);
#elif USE_COMM_TLS_TCP
	TlsTcpConfig conf = testCommConfig.GetTlsTCPConfig(true, rp.replicaId,
                                                     rp.numOfClients,
                                                     rp.numOfReplicas,
                                                     rp.configFileName);
#else
	PlainUdpConfig conf = testCommConfig.GetUDPConfig(true, rp.replicaId,
                                                    rp.numOfClients,
                                                    rp.numOfReplicas,
                                                    rp.configFileName);
#endif
	//used to run tests. TODO(IG): use the standard config structs for all tests
	SimpleKVBC::ReplicaConfig c;

	ICommunication *comm = CommFactory::create(conf);

	c.pathOfKeysfile = rp.keysFilePrefix + std::to_string(rp.replicaId);
	c.replicaId = rp.replicaId;
	c.fVal = replicaConfig.fVal;
	c.cVal = replicaConfig.cVal;
	c.numOfClientProxies = rp.numOfClients;
	c.statusReportTimerMillisec = 20 * 1000;
	c.concurrencyLevel = 1;
	c.autoViewChangeEnabled = false;
	c.viewChangeTimerMillisec = 45 * 1000;
	c.maxBlockSize = 2 * 1024 * 1024;  // 2MB

	r = createReplica(c, comm, BasicRandomTests::commandsHandler());

	r->start();
	while (r->isRunning())
		std::this_thread::sleep_for(std::chrono::seconds(1));
}
