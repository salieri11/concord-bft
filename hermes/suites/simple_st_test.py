#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
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
import argparse
import collections
import json
import logging
import os
import pprint
import tempfile
import time
import traceback
import re
import random
from shutil import rmtree
import subprocess
from . import helen_api_tests
from threading import Thread

from . import test_suite
from rpc.rpc_call import RPC
from util.debug import pp as pp
from util.numbers_strings import trimHexIndicator, decToEvenHexNo0x
from util.product import Product
import util.json_helper
from rest.request import Request
from datetime import datetime

log = logging.getLogger(__name__)

def wait():
    input('Press enter to continue...')

class SimpleStateTransferTest(test_suite.TestSuite):
   _apiServerUrl = None
   _helenApiTestInstance = None
   _baseUrl = None
   _to = "0x262c0d7ab5ffd4ede2199f6ea793f819e1abb019" #from genesis file
   _funcPref = "0xe4b421f2000000000000000000000000000000000000000000000000000000000000"
   _gas = "100000000"
   _product = None

   def __init__(self, passedArgs):
      super(SimpleStateTransferTest, self).__init__(passedArgs)
      self._baseUrl = passedArgs.baseUrl
      self._apiServerUrl = passedArgs.baseUrl + "/api/concord/eth/"
      self._helenApiTestInstance = helen_api_tests.HelenAPITests(passedArgs)
      self.existing_transactions = 2

   def getName(self):
      return "SimpleStateTransferTest"

   def send_data(self, count, contractAddress, maxRetries):
      rpc = RPC(self._testLogDir,
               self.getName(),
               self._apiServerUrl,
               self._userConfig)
      retries = 0
      i = 0
      while i < count:
         try:
            rand = "{:04x}".format(random.randint(100, 250))
            txres = rpc.sendTransaction(self._to, self._funcPref + rand, self._gas, contractAddress)
            if retries == 0:
               i += 1
            retries = 0
         except:
            log.debug("send_data fail, retries:{0}, maxRetries:{1}".format(retries, maxRetries))
            retries += 1
            if retries == maxRetries:
               raise

   def check_data(self, path, blockToStart, blocksCount):
      originalCwd = os.getcwd()
      os.chdir(path)
      blocksData = []
      blocksDataLength = []

      toolPath = os.path.abspath(os.path.join("..","build","tools", "conc_rocksdb_adp"))
      cmd = [toolPath]
      for replicaId in range(1,5):
         pathParam = "-path={0}{1}".format(os.path.join(path,"rocksdbdata"), replicaId)
         opParam = "-op=getDigest"
         pParam = "-p={0}:{1}".format(blockToStart,blockToStart + blocksCount)
         cmd.append(pathParam)
         cmd.append(opParam)
         cmd.append(pParam)
         blockData = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE).stdout.read()
         blocksData.append(blockData)
         length = int(str(blockData).split(":")[1].replace("\\n", "").replace("'", ""))
         if length == 0:
            log.error("Blocks length 0")
            return False
         log.debug("Data length from replica {0} is {1} bytes".format(replicaId, length))
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

   def deploy_test_contract(self):
      request = Request(self._testLogDir,
               self.getName(),
               self._baseUrl,
               self._userConfig)
      cCode = open("resources/contracts/LargeBlockStorage.sol", 'r').read()
      cVersion = self._helenApiTestInstance.random_string_generator()
      cId = self._helenApiTestInstance.random_string_generator()
      cName = "LargeBlockStorage"
      res = self._helenApiTestInstance.contract_upload_util_generic(request, cId, cVersion, cCode, self._to, cName, "")

      result = request.callContractAPI('/api/concord/contracts/' + cId
                                      + '/versions/' + cVersion, "")
      return result

   def _send_async(self, transactions, contractAddress, maxRetries, threadCount = 10):
      log.info("Start sending {} transactions".format(transactions))
      threads = []
      txPerThread = int(transactions / threadCount)
      for i in range(0,threadCount):
         t = Thread(target=self.send_data, args=(txPerThread, contractAddress, maxRetries))
         t.start()
         threads.append(t)

      for t in threads:
         t.join()

      log.info("Done sending {} transactions".format(transactions))

   def _run_kill_replica_test(self):
      global path
      transactions = 1000

      contractInfo = self.deploy_test_contract()
      if "address" not in contractInfo:
         return (False, "Failed to deploy contract")
      contractAddress = contractInfo["address"]

      self._send_async(transactions, contractAddress, 1)

      res = self._product.kill_concord_replica(2)
      if not res:
         return (False, "Failed to kill replica 2")

      path = self._product.cleanConcordDb(2)
      res = self._product.start_concord_replica(2)
      if not res:
         return (False, "Failed to start replica 2")

      sleepTime = int(120)
      log.info(
      "Waiting for State Transfer to finish, estimated time {0} seconds".format(sleepTime))
      time.sleep(sleepTime)

      log.info("Checking data after State Transfer")
      res = self.check_data(path, 0,transactions + self.existing_transactions)

      if not res:
         return (False, "Data check failed")

      return (True, "Data checked")

   def _run_pause_replica_test(self):
      global path

      res = self._product.kill_concord_replica(1)
      if not res:
         return (False, "Failed to kill replica 1")
      res = self._product.kill_concord_replica(2)
      if not res:
         return (False, "Failed to kill replica 2")
      res = self._product.kill_concord_replica(3)
      if not res:
         return (False, "Failed to kill replica 3")
      res = self._product.kill_concord_replica(4)
      if not res:
         return (False, "Failed to kill replica 4")

      p1 = self._product.cleanConcordDb(1)
      p2 = self._product.cleanConcordDb(2)
      p3 = self._product.cleanConcordDb(3)
      p4 = self._product.cleanConcordDb(4)
      if not p1 or not p2 or not p3 or not p4:
         return (False, "Failed to clean RocksDb")
      path = p1

      res = self._product.start_concord_replica(1)
      if not res:
         return (False, "Failed to start replica 1")
      res = self._product.start_concord_replica(2)
      if not res:
         return (False, "Failed to start replica 2")
      res = self._product.start_concord_replica(3)
      if not res:
         return (False, "Failed to start replica 3")
      res = self._product.start_concord_replica(4)
      if not res:
         return (False, "Failed to start replica 4")

      time.sleep(20)

      contractInfo = self.deploy_test_contract()
      if "address" not in contractInfo:
         return (False, "Failed to deploy contract")
      contractAddress = contractInfo["address"]

      transactions_1 = 1000
      self._send_async(transactions_1, contractAddress, 1)

      res = self._product.pause_concord_replica(3)
      if not res:
         return (False, "Failed to suspend replica")

      transactions_2 = 400
      #this batch will go slower since only 3 replicas are up
      self._send_async(transactions_2, contractAddress, 3)

      res = self._product.resume_concord_replica(3)
      if not res:
         return (False, "Failed to resume replica")

      # wait for ST to complete
      sleepTime = int(30)
      log.info(
      "Waiting for State Transfer to finish, estimated time {0} seconds".format(sleepTime))
      time.sleep(sleepTime)

      # check all blocks
      res = self.check_data(path, 0, transactions_1 + transactions_2 + self.existing_transactions)

      if not res:
         return (False, "Data check failed")

      return (True, "Data checked")

   def _get_tests(self):
      return [("kill_replica", self._run_kill_replica_test), \
      ("pause_replica", self._run_pause_replica_test)]

   def run(self):
      if self._productMode and not self._noLaunch:
         try:
            self._product = self.launchProduct(self._args,
                         self._apiServerUrl,
                         self._userConfig)
         except Exception as e:
            log.error(traceback.format_exc())
            return self._resultFile

      log.info("Starting tests")

      tests = self._get_tests()
      for (testName, testFunc) in tests:
         log.info("Starting test " + testName)
         res,info = testFunc()
         self.writeResult(testName, res, info)

      log.info("Tests are done")

      if self._productMode and not self._noLaunch:
         self._product.stopProduct()

      return self._resultFile
