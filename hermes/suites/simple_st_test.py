#########################################################################
# Copyright 2018-2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# State transfer suite. Checks the ability of replica to fetch missing data.
# Since current Concord-bft implementation doesn't persist its metadata,the only
# way to test state transfer is to clean RocksDb after replica is down
# Replica can be paused instead of shut down - and if all the rest are in the
# same state - state transfer will work without cleaning RockdDb.

# kill_replica - create 1000 blocks with average size of 30k, shut down
# replica, clean its RocksDb, start replica again, wait for ST to complete and
# check blocks from all 4 replicas - both size and content must be equal.

# pause_replica - create 1000 blocks, suspend replica's process, create 400
# more blocks with only 3 replicas, resume replica, wait for ST to complete,
# check ALL 1400 blocks from all 4 replicas - both size and content must be
# equal.
#########################################################################

import pytest
from suites.case import describe, passed, failed
from fixtures.common_fixtures import fxBlockchain, fxConnection, fxInitializeOrgs, fxProduct
from util.blockchain import eth as eth_helper
from util import auth, csp, helper, infra, hermes_logging, numbers_strings, blockchain_ops, node_interruption_helper
import math

import collections
import json

import os
import time
import traceback
import re
import random

import subprocess
from threading import Thread

from rpc.rpc_call import RPC
from util.debug import pp as pp
from util.numbers_strings import trimHexIndicator, decToEvenHexNo0x
import util.json_helper
import util.numbers_strings
from rest.request import Request
from datetime import datetime

import util.hermes_logging
log = util.hermes_logging.getMainLogger()

SstSetupFixture = collections.namedtuple("SstSetupFixture", "to, funcPref, gas, existing_transactions")
RequestConnection = collections.namedtuple("RequestConnection", "request, rpc")

# def wait():
#     input('Press enter to continue...')

@pytest.fixture
@describe("fixture; Initial Setup")
def fxSstSetup(request):
     return SstSetupFixture(to="0x262c0d7ab5ffd4ede2199f6ea793f819e1abb019", 
                            funcPref="0xe4b421f2000000000000000000000000000000000000000000000000000000000000",
                            gas="100000000",
                            existing_transactions=2)

# set LogDir appropriately for each Test as required
def createRequest(hermesData, blockchainId, testName="" ):
   try:
      if testName:
         testLogDir = os.path.join(hermesData["hermesTestLogDir"], testName)
      else:
         testLogDir = hermesData["hermesTestLogDir"]

      req = Request(testLogDir,
                     getName(),
                     hermesData["hermesCmdlineArgs"].reverseProxyApiBaseUrl,
                     hermesData["hermesUserConfig"],
                     service=hermesData["hermesCmdlineArgs"].deploymentService)

      ethrpcUrl = eth_helper.getEthrpcApiUrl(req, blockchainId)
   
      # create rpc connection
      rpc = RPC(testLogDir,
               getName(),
               ethrpcUrl,
            hermesData["hermesUserConfig"])
      return RequestConnection(request=req, rpc=rpc)
   except:
      log.error("Failed in getting the Obj")
      raise


def getName():
   return "SimpleStateTransferTest"

def send_data(hermesData, sstParams, blockchainId, count, contractAddress, maxRetries ):
   request = createRequest(hermesData, blockchainId)

   retries = 0
   i = 0
   while i < count:
      try:
         rand = "{:04x}".format(random.randint(100, 250))
         
         txres = request.rpc.sendTransaction(sstParams.to, sstParams.funcPref + rand, sstParams.gas, contractAddress)
         if retries == 0:
            i += 1
         retries = 0
      except:
         log.debug("send_data fail, retries:{0}, maxRetries:{1}".format(retries, maxRetries))
         retries += 1
         if retries == maxRetries:
            raise

def check_data(path, blockToStart, blocksCount):
      try:
         originalCwd = os.getcwd()
         os.chdir(path)
         blocksData = []
         blocksDataLength = []
       
         log.info("Checking data from {} to {}".format(blockToStart, blockToStart+blocksCount))

         toolPath = "/concord/conc_rocksdb_adp"
         pathParam = "-path=/concord/rocksdbdata"
         opParam = "-op=getDigest"
         for replicaId in range(1,5):
            pParam = "-p={0}:{1}".format(blockToStart,blockToStart + blocksCount)
            cmd = ' '.join([toolPath, pathParam, opParam, pParam])
            container = util.blockchain.eth.get_concord_container_name(replicaId)
            blockData = util.blockchain.eth.exec_in_concord_container(container, cmd)

            blocksData.append(blockData)
            log.debug(json.dumps({"data":blockData}, indent=4, default=str))
            length = int(str(blockData).split(":")[1].replace("\\n", "").replace("'", ""))
            if length == 0:
               log.error("Blocks length 0")
               return False
            log.info("Data length from replica {0} is {1} bytes".format(replicaId, length))
            blocksDataLength.append(length)

         # is there better way to do it?
         os.chdir(originalCwd)
         if blocksDataLength[0] == blocksDataLength[1] == blocksDataLength[2] == blocksDataLength[3]:
            if blocksData[0] == blocksData[1] == blocksData[2] == blocksData[3]:
                return True
            else:
                return False
         else:
            return False
      except Exception as ex:
         t = "An exception of type {0} occurred. Arguments:\n{1!r}"
         message = t.format(type(ex).__name__, ex.args)
         stack = traceback.format_exc()
         log.error(f"{message}\n{stack}")
         return False

def deploy_test_contract(request, frmAddr):

      cdir = os.getcwd()
      log.debug(f'current dir: {cdir}')
      files = [f for f in os.listdir(cdir)]
      log.debug("folder content:")
      for f in files:
         log.debug(f)

      cFile = "resources/contracts/LargeBlockStorage.sol"
      cVersion = util.numbers_strings.random_string_generator()
      cId = util.numbers_strings.random_string_generator()
      cName = "LargeBlockStorage"
      blockchainId = request.getBlockchains()[0]["id"]
      res = util.blockchain.eth.upload_contract(blockchainId, request, cFile, cName,
                                                contractId = cId,
                                                contractVersion = cVersion,
                                                compilerVersion = "v0.5.2+commit.1df8f40c",
                                                fromAddr = frmAddr)

      result = request.callContractAPI('/api/concord/contracts/' + cId
                                      + '/versions/' + cVersion, "")
      return result

def send_async(hermesSettings, sstParams, blockchainId, transactions, contractAddress, maxRetries, threadCount = 10):
      log.info("Start sending {} transactions".format(transactions))
      threads = []
      txPerThread = math.ceil(transactions / threadCount)
      for i in range(0,threadCount):
         t = Thread(target=send_data, args=(hermesSettings, sstParams, blockchainId, txPerThread, contractAddress, maxRetries))
         t.start()
         threads.append(t)

      for t in threads:
         t.join()

      log.info("Done sending {} transactions".format(transactions))


def sleep_and_check(initSleepTime, step, maxSleepTime, transactions):
      res = False
      totalSleepTime = 0
      while not res and totalSleepTime < maxSleepTime:
         log.info(
            "Waiting for State Transfer to finish, estimated time {0} seconds".format(initSleepTime))
         time.sleep(initSleepTime)
         log.info("Checking data after State Transfer")
         res = check_data(path, 0, transactions)
         totalSleepTime += initSleepTime
         initSleepTime = step
      return res


@describe()
@pytest.mark.smoke
def test_kill_replica(request, fxSstSetup, fxHermesRunSettings, fxProduct, fxBlockchain, fxConnection):
      try:
         
         global path
         
         transactions = 1000
         hermesSettings = fxHermesRunSettings
         req = createRequest(hermesSettings, fxBlockchain.blockchainId, request.node.name )    
         
         contractInfo = deploy_test_contract(req.request, fxSstSetup.to)
     
         assert "address" in contractInfo, "Failed to deploy contract"
         contractAddress = contractInfo["address"]
         
         send_async(hermesSettings, fxSstSetup,fxBlockchain.blockchainId, transactions, contractAddress, 1)
   
         # Is this a alternate way of getting instance of aleady running product
         # product = Product(hermesData["hermesCmdlineArgs"],
         #                    hermesData["hermesUserConfig"])

         assert fxProduct.product.kill_concord_replica(2), "Failed to kill replica 2"

         path = fxProduct.product.cleanConcordDb(2)
         assert path, "Failed to clean RocksDb"
         assert fxProduct.product.start_concord_replica(2), "Failed to start replica 2"
         assert sleep_and_check(120, 30, 360, transactions + fxSstSetup.existing_transactions), "Data check failed"
         
         # return passed("Data checked")
      except Exception as ex:
         t = "An exception of type {0} occurred. Arguments:\n{1!r}"
         message = t.format(type(ex).__name__, ex.args)
         stack = traceback.format_exc()
         log.error(f"{message}\n{stack}")
         assert False, message
       
       
@describe()
@pytest.mark.smoke
def test_pause_replica(request, fxSstSetup, fxHermesRunSettings, fxBlockchain, fxProduct, fxConnection):
      try:

         global path
         hermesSettings = fxHermesRunSettings       

         assert fxProduct.product.kill_concord_replica(1), "Failed to kill replica 1"
         assert fxProduct.product.kill_concord_replica(2), "Failed to kill replica 2"
         assert fxProduct.product.kill_concord_replica(3), "Failed to kill replica 3"
         assert fxProduct.product.kill_concord_replica(4), "Failed to kill replica 4"
        
         p1 = fxProduct.product.cleanConcordDb(1)
         p2 = fxProduct.product.cleanConcordDb(2)
         p3 = fxProduct.product.cleanConcordDb(3)
         p4 = fxProduct.product.cleanConcordDb(4)
         
         assert p1 and p2 and p3 and p4, "Failed to clean RocksDb"
         path = p1

         assert fxProduct.product.start_concord_replica(1), "Failed to start replica 1"
         assert fxProduct.product.start_concord_replica(2), "Failed to start replica 2"
         assert fxProduct.product.start_concord_replica(3), "Failed to start replica 3"
         assert fxProduct.product.start_concord_replica(4), "Failed to start replica 4"

         time.sleep(40)

         req = createRequest(hermesSettings, fxBlockchain.blockchainId, request.node.name )
         contractInfo = deploy_test_contract(req.request, fxSstSetup.to)
 
         assert "address" in contractInfo, "Failed to deploy contract"
         contractAddress = contractInfo["address"]

         transactions_1 = 1000
         send_async(hermesSettings, fxSstSetup, fxBlockchain.blockchainId, transactions_1, contractAddress, 1)

         assert fxProduct.product.pause_concord_replica(3), "Failed to suspend replica"

         transactions_2 = 400
         #this batch will go slower since only 3 replicas are up
         send_async(hermesSettings, fxSstSetup, fxBlockchain.blockchainId, transactions_2, contractAddress, 3)

         assert fxProduct.product.resume_concord_replica(3), "Failed to resume replica"
         assert sleep_and_check(120, 30, 360, transactions_1 + transactions_2 + fxSstSetup.existing_transactions), "Data check failed"

         # check all blocks
         assert check_data(path, 0, transactions_1 + transactions_2 + fxSstSetup.existing_transactions), "Data check failed"

         # return passed("Data checked")
      except Exception as ex:
         t = "An exception of type {0} occurred. Arguments:\n{1!r}"
         message = t.format(type(ex).__name__, ex.args)
         stack = traceback.format_exc()
         log.error(f"{message}\n{stack}")
         assert False, message