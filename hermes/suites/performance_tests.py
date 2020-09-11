#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Tests covering Performance.
# Converted to pytest for Jira:- BC-493
#########################################################################


#########################################################################
# Example executions
# ./main.py PerformanceTests
#########################################################################

import logging
import os
import traceback
import pytest
import subprocess
import collections
import util.json_helper
import util.hermes_logging

from fixtures.common_fixtures import fxProduct, fxBlockchain, fxConnection
from suites.case import describe
from rest.request import Request
from util.blockchain import eth as eth_helper

log = util.hermes_logging.getMainLogger()
LocalSetupFixture = collections.namedtuple(
    "LocalSetupFixture", "args, testLogDir, performanceSubModule, ethrpcApiUrl")

@pytest.fixture(scope="function")
@describe("fixture; local setup for given test suite")
def fxLocalSetup(request, fxBlockchain, fxHermesRunSettings, fxProduct, fxConnection):
    testName = fxConnection.request.testName
    args = fxHermesRunSettings["hermesCmdlineArgs"]
    testLogDir = os.path.join(
        fxHermesRunSettings["hermesTestLogDir"], testName)
    os.makedirs(testLogDir, exist_ok=True)
    performanceSubModuleDir = os.path.join(
        args.hermes_dir, '..', "performance")
    os.makedirs(performanceSubModuleDir, exist_ok=True)
    if args.ethrpcApiUrl:
        ethrpcApiUrl = args.ethrpcApiUrl
        log.debug("Url is getting retrieved from the args retrieved for the test")
    else:
        ethrpcApiUrl = eth_helper.getEthrpcApiUrl(
            fxConnection.request, fxBlockchain.blockchainId)
    return LocalSetupFixture(args=args, testLogDir=testLogDir,
                             performanceSubModule=performanceSubModuleDir, ethrpcApiUrl=ethrpcApiUrl)

@describe()
def test_performance(fxLocalSetup):
    log.debug("Validation for Performance repo path: {}".format(fxLocalSetup.performanceSubModule))
    assert os.path.isdir(fxLocalSetup.performanceSubModule), "Performance repo does not Exist"
    performanceJar = os.path.join(fxLocalSetup.performanceSubModule,
                                 "ballotApp", "target",
                                 "performance-1.0-SNAPSHOT-jar-with-dependencies.jar")
    log.debug("Validation for Performance jar file: {}".format(performanceJar))
    assert os.path.isfile(performanceJar), "Performance jar file does not Exist"
    concordIp = fxLocalSetup.ethrpcApiUrl.split("/")[2].split(":")[0]
    log.info("Concord IP {}".format(concordIp))
    numberOfVotes = str(fxLocalSetup.args.performanceVotes)
    log.info("No. of votes: {}".format(fxLocalSetup.args.performanceVotes))
    inputData = os.path.join(fxLocalSetup.performanceSubModule,
                            "data", "proposals")
    cmd = ["java", "-Xmx1g", "-jar", performanceJar,
         "--concord", concordIp, numberOfVotes, inputData, fxLocalSetup.testLogDir]
    log.info("Running benchmark tool: {}".format(" ".join(cmd)))
    completedProcess = subprocess.run(cmd,
                                    stdout=subprocess.PIPE,
                                    stderr=subprocess.STDOUT)
    psOutput = completedProcess.stdout.decode("UTF-8")
    log.info(psOutput)
    assert "Throughput: " in psOutput, "Failed to run Performance Test"