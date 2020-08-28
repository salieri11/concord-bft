#########################################################################
# Copyright 2018-2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Metadata persistency suite. This suite provides "hard" tests that simulates
# random replicas failures and expectation is to continue working as usual when the replica restarts
# without deleting the data
#
# replicas_random_restart - this test randomly kills 2 out of 4 replicas, except of primary
# and replica #4 (it serves as a endpoint for the requests)
# the system will not advance until at least 3 replicas are up, such that this test may require some time to finish
# eventually, all data should be exactly the same on all 4 replicas
#
# primary_down_viewchange_statetransfer - this test generates some state, then kills primary and triggers view change
# after the view has been changed, more state updates are conducted and then the former primary is turned back on
# and more data is sent to the system (currently with all 4 replicas up).
# the expectation is that former primary should use the the state transfer mechanism to fetch the missing data
# eventually, all data should be exactly the same on all 4 replicas
#########################################################################
import argparse
import collections
import json
import os
import time
import re
import random
import pytest
import subprocess
import traceback
from datetime import datetime
from threading import Thread
from suites.case import describe
from rpc.rpc_call import RPC
import util.json_helper
import util.numbers_strings
from fixtures.common_fixtures import fxHermesRunSettings, fxProduct, fxBlockchain, fxConnection

import util.hermes_logging
from util.blockchain import eth as eth_helper
log = util.hermes_logging.getMainLogger()

LocalSetupFixture = collections.namedtuple(
    "LocalSetupFixture", "request,rpc,product,fromAddr,funcPref,gas,blockchainId,testLogDir,userConfig,ethrpcApiUrl")

pytest.error = False
pytest.done = False
pytest.clientThreads = []
pytest.replicaCount = 0

# TODO - To move this function to a common utility file

# autouse will work once the invocation is done using pytest directly and not main.py


@pytest.fixture(scope="function", autouse=True)
@describe("fixture; local setup for given test suite")
def fxLocalSetup(request, fxBlockchain, fxHermesRunSettings, fxProduct, fxConnection):
    userConfig = fxHermesRunSettings["hermesUserConfig"]

    testLogDir = os.path.join(
        fxHermesRunSettings["hermesTestLogDir"], fxConnection.request.testName)

    if fxHermesRunSettings["hermesCmdlineArgs"].ethrpcApiUrl:
        ethrpcApiUrl = fxHermesRunSettings["hermesCmdlineArgs"].ethrpcApiUrl
    else:
        ethrpcApiUrl = eth_helper.getEthrpcApiUrl(
            fxConnection.request, fxBlockchain.blockchainId)

    pytest.replicaCount = fxHermesRunSettings["hermesCmdlineArgs"].numReplicas
    return LocalSetupFixture(request=fxConnection.request, rpc=fxConnection.rpc,
                             product=fxProduct.product, fromAddr="0x262c0d7ab5ffd4ede2199f6ea793f819e1abb019",
                             funcPref="0xe4b421f2000000000000000000000000000000000000000000000000000000000000",
                             gas="100000000", blockchainId=fxBlockchain.blockchainId,
                             testLogDir=testLogDir, userConfig=userConfig, ethrpcApiUrl=ethrpcApiUrl)


def getName():
    return "MetadataPersistencyTests"


def deploy_test_contract(localSetup):
    cFile = "resources/contracts/LargeBlockStorage.sol"
    cVersion = util.numbers_strings.random_string_generator()
    cId = util.numbers_strings.random_string_generator()
    cName = "LargeBlockStorage"
    res = util.blockchain.eth.upload_contract(localSetup.blockchainId, localSetup.request, cFile, cName,
                                              contractId=cId,
                                              contractVersion=cVersion,
                                              compilerVersion="v0.5.2+commit.1df8f40c",
                                              fromAddr=localSetup.fromAddr)

    result = localSetup.request.callContractAPI('/api/concord/contracts/' + cId
                                                + '/versions/' + cVersion, "")
    return result


def check_data(blockToStart, blocksCount):

    blocksData = []
    blocksDataLength = []

    log.info("Checking data from {} to {}".format(
        blockToStart, blockToStart+blocksCount))

    toolPath = "/concord/conc_rocksdb_adp"
    pathParam = "-path=/concord/rocksdbdata"
    opParam = "-op=getDigest"
    count = int(pytest.replicaCount) + 1

    for replicaId in range(1, count):
        pParam = "-p={0}:{1}".format(blockToStart, blockToStart + blocksCount)
        cmd = ' '.join([toolPath, pathParam, opParam, pParam])
        container = util.blockchain.eth.get_concord_container_name(replicaId)
        blockData = util.blockchain.eth.exec_in_concord_container(
            container, cmd)
        blocksData.append(blockData)
        length = int(str(blockData).split(":")[
                     1].replace("\\n", "").replace("'", ""))
        if length == 0:
            log.error("Blocks length 0")
            return False
        log.debug("Data length from replica {0} is {1} bytes".format(
            replicaId, length))
        blocksDataLength.append(length)

    if all(i == blocksDataLength[0] for i in blocksDataLength):
        return True if all(i == blocksData[0] for i in blocksData) else False
    else:
        return False


def send_data(localSetup, count, contractAddress, maxRetries):
    rpc = RPC(localSetup.testLogDir, getName(),
              localSetup.ethrpcApiUrl, localSetup.userConfig)
    retries = 0
    i = 0
    while i < count and not pytest.error:
        try:
            rand = "{:04x}".format(random.randint(100, 250))
            txres = rpc.sendTransaction(
                localSetup.fromAddr, localSetup.funcPref + rand, localSetup.gas, contractAddress)
            if txres:
                i += 1
                retries = 0
                log.debug(f"sent {i} transactions")
            else:
                raise
        except:
            log.debug(
                f"send_data fail, retries:{retries}, maxRetries:{maxRetries}")
            retries += 1
            if retries == maxRetries:
                raise


# This function creates number of threads (threadCount parameter) to send requests to concord
def _send_async(localSetup, transactions, contractAddress, maxRetries, threadCount):
    if transactions % threadCount != 0:
        raise Exception(
            f"transactions {transactions} % threadCount {threadCount} is not 0")
    txPerThread = int(transactions / threadCount)
    for i in range(0, threadCount):
        t = Thread(target=send_data, args=(
            localSetup, txPerThread, contractAddress, maxRetries))
        t.start()
        pytest.clientThreads.append(t)


# This function simulates random shutdown and restart of the replicas, which
# ids are in the replicas_id_list. The function runs until self.done is set to True
# by the calling function. If either stop or start fails after 3 retries - the test fails.
# These retries are essential because docker container sometimes doesn't start/stop on the first try
def stop_start_replicas_loop(localSetup, replicas_id_list):
    try:
        random.seed()
        replicas_start_times = []
        for replicaId in replicas_id_list:
            replicas_start_times.append(random.randint(10, 60) * -1)
        while not pytest.done:
            inx = 0
            for i in replicas_id_list:
                if replicas_start_times[inx] == 0:
                    counter = 0
                    while counter < 3:
                        res = localSetup.product.kill_concord_replica(i)
                        if not res:
                            log.error(f"Failed to kill concord replica {i}")
                            counter += 1
                        else:
                            counter = 0
                            break
                    if counter == 2:
                        pytest.error = True
                        raise

                    nextStart = time.time() + random.randint(60, 70) + i * random.randint(1, 10)
                    replicas_start_times[inx] = nextStart
                    log.debug(
                        f"replica {i} stopped for {nextStart - time.time()}")
                elif replicas_start_times[inx] > 0 and replicas_start_times[inx] < time.time():
                    log.debug(f"replica {i} starting")
                    counter = 0
                    while counter < 3:
                        res = localSetup.product.start_concord_replica(i)
                        if not res:
                            log.error(f"Failed to start concord replica {i}")
                            counter += 1
                        else:
                            counter = 0
                            break
                    if counter == 2:
                        pytest.error = True
                        raise

                    replicas_start_times[inx] = -1 * \
                        (random.randint(20, 60) + i * random.randint(1, 10))
                elif replicas_start_times[inx] < 0:
                    log.debug(f"replica {i} waiting for next stop")
                    replicas_start_times[inx] += 1
                else:
                    log.debug(f"replica {i} still down")
                inx += 1
            time.sleep(1)
    finally:
        inx = 0
        for replicaId in replicas_id_list:
            if replicas_start_times[inx] > 0:
                res = localSetup.product.start_concord_replica(i)
                log.debug(f"replica {i} started before leaving")
            inx += 1


def clean(localSetup):
    replicaCount = int(pytest.replicaCount)
    for i in range(1, replicaCount + 1):
        res = localSetup.product.kill_concord_replica(i)
        assert res, f"Failed to kill replica {i}"

    res = ""
    for i in range(1, replicaCount + 1):
        res = localSetup.product.cleanConcordDb(i)
        assert res, "Failed to clean RocksDb"
    path = res

    for i in range(1, replicaCount + 1):
        res = localSetup.product.start_concord_replica(i)
        assert res, f"Failed to start replica {i}"

    count = 0
    while count < (replicaCount + 2):
        contractInfo = deploy_test_contract(localSetup)
        if "address" not in contractInfo:
            count += 1
            time.sleep(10)
        else:
            count = 0
            break

    assert count <= 0, "Failed to deploy contract"
    contractAddress = contractInfo["address"]


def sleep_and_check(initSleepTime, step, maxSleepTime, transactions):
    res = False
    totalSleepTime = 0
    while not res and totalSleepTime < maxSleepTime:
        log.info(
            "Waiting for State Transfer to finish, estimated time {0} seconds".format(initSleepTime))
        time.sleep(initSleepTime)
        log.info("Checking data after State Transfer")
        res = check_data(0, transactions)
        totalSleepTime += initSleepTime
        initSleepTime = step
    return res


@describe()
def test_replicas_random_restart(fxLocalSetup):
    pytest.done = False
    pytest.error = False
    pytest.clientThreads = []
    transactions = 1000
    contractInfo = deploy_test_contract(fxLocalSetup)
    assert "address" in contractInfo, "Failed to deploy contract"
    contractAddress = contractInfo["address"]

    log.info(f"start sending {transactions} transactions")
    _send_async(fxLocalSetup, transactions, contractAddress, transactions, 4)
    t = Thread(target=stop_start_replicas_loop, args=(fxLocalSetup, [2, 3],))
    t.start()
    for thread in pytest.clientThreads:
        thread.join()

    pytest.done = True
    t.join()
    log.info(f"done sending {transactions} transactions")
    # there were containers start/stop problems
    assert not pytest.error, "Test didn't complete"

    log.info(f"Checking data")
    assert sleep_and_check(60, 30, 240, transactions), "Data check failed"


@describe()
def test_primary_down_viewchange_statetransfer(fxLocalSetup):
    pytest.done = False
    pytest.error = False
    pytest.clientThreads = []
    clean(fxLocalSetup)

    transactions = 1000
    contractInfo = deploy_test_contract(fxLocalSetup)
    assert "address" in contractInfo, "Failed to deploy contract"
    contractAddress = contractInfo["address"]

    log.info(f"start sending {transactions} transactions")
    _send_async(fxLocalSetup, transactions, contractAddress, transactions, 4)
    log.info(f"done sending {transactions} transactions")

    assert fxLocalSetup.product.kill_concord_replica(
        1), "Failed to kill replica 1"

    log.info(f"start sending {transactions} transactions")
    _send_async(fxLocalSetup, transactions, contractAddress, transactions, 4)
    log.info(f"done sending {transactions} transactions")

    assert fxLocalSetup.product.start_concord_replica(
        1), "Failed to start replica 1"

    log.info(f"start sending {transactions} transactions")
    _send_async(fxLocalSetup, transactions, contractAddress, transactions, 4)
    log.info(f"done sending {transactions} transactions")

    log.info(f"Checking data")
    assert sleep_and_check(60, 30, 240, transactions*3), "Data check failed"
