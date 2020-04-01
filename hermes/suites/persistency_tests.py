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
from threading import Thread

from . import test_suite
from suites.cases import describe
from rpc.rpc_call import RPC
from util.debug import pp as pp
from util.numbers_strings import trimHexIndicator, decToEvenHexNo0x
from util.product import Product
import util.json_helper
import util.numbers_strings
from rest.request import Request
from datetime import datetime
from fixtures.common_fixtures import fxHermesRunSettings, fxProduct

import util.hermes_logging
log = util.hermes_logging.getMainLogger()

class MetadataPersistencyTests(test_suite.TestSuite):
   def __init__(self, passedArgs, product):
      super(MetadataPersistencyTests, self).__init__(passedArgs, product)
      self._done = False
      self._error = False
      self._to = "0x262c0d7ab5ffd4ede2199f6ea793f819e1abb019" #from genesis file
      self._funcPref = "0xe4b421f2000000000000000000000000000000000000000000000000000000000000"
      self._gas = "100000000"
      self._clientThreads = []

   def getName(self):
      return "MetadataPersistencyTests"

   def deploy_test_contract(self):
      request = Request(self._testLogDir,
               self.getName(),
               self.reverseProxyApiBaseUrl,
               self._userConfig)
      cFile = "resources/contracts/LargeBlockStorage.sol"
      cVersion = util.numbers_strings.random_string_generator()
      cId = util.numbers_strings.random_string_generator()
      cName = "LargeBlockStorage"
      blockchainId = request.getBlockchains()[0]["id"]
      res = util.blockchain.eth.upload_contract(blockchainId, request, cFile, cName,
                                                contractId = cId,
                                                contractVersion = cVersion,
                                                compilerVersion = "v0.5.2+commit.1df8f40c",
                                                fromAddr = self._to)

      result = request.callContractAPI('/api/concord/contracts/' + cId
                                      + '/versions/' + cVersion, "")
      return result

   def check_data(self, blockToStart, blocksCount, replicasIds):
      blocksData = []
      blocksDataLength = []

      log.info("Checking data from {} to {}".format(blockToStart, blockToStart+blocksCount))

      toolPath = "/concord/conc_rocksdb_adp"
      pathParam = "-path=/concord/rocksdbdata"
      opParam = "-op=getDigest"
      for replicaId in replicasIds:
         pParam = "-p={0}:{1}".format(blockToStart,blockToStart + blocksCount)
         cmd = ' '.join([toolPath, pathParam, opParam, pParam])
         container = util.blockchain.eth.get_concord_container_name(replicaId)
         blockData = util.blockchain.eth.exec_in_concord_container(container, cmd)
         blocksData.append(blockData)
         length = int(str(blockData).split(":")[1].replace("\\n", "").replace("'", ""))
         if length == 0:
            log.error("Blocks length 0")
            return False
         log.debug("Data length from replica {0} is {1} bytes".format(replicaId, length))
         blocksDataLength.append(length)

      # is there better way to do it?
      if blocksDataLength[0] == blocksDataLength[1] == blocksDataLength[2] == blocksDataLength[3]:
         if blocksData[0] == blocksData[1] == blocksData[2] == blocksData[3]:
             return True
         else:
             return False
      else:
         return False


   def send_data(self, count, contractAddress, maxRetries):
      rpc = RPC(self._testLogDir,
               self.getName(),
               self.ethrpcApiUrl,
               self._userConfig)
      retries = 0
      i = 0
      while i < count and not self._error:
         try:
            rand = "{:04x}".format(random.randint(100, 250))
            txres = rpc.sendTransaction(self._to, self._funcPref + rand, self._gas, contractAddress)
            if txres:
               i += 1
               retries = 0
               log.debug(f"sent {i} transactions")
            else:
               raise
         except:
            log.debug(f"send_data fail, retries:{retries}, maxRetries:{maxRetries}")
            retries += 1
            if retries == maxRetries:
               raise


   # This function creates number of threads (threadCount parameter) to send requests to concord
   def _send_async(self, transactions, contractAddress, maxRetries, threadCount):
      if transactions % threadCount != 0:
         raise Exception(f"transactions {transactions} % threadCount {threadCount} is not 0")
      txPerThread = int(transactions / threadCount)
      for i in range(0,threadCount):
         t = Thread(target=self.send_data, args=(txPerThread, contractAddress, maxRetries))
         t.start()
         self._clientThreads.append(t)


   # This function simulates random shutdown and restart of the replicas, which
   # ids are in the replicas_id_list. The function runs until self.done is set to True
   # by the calling function. If either stop or start fails after 3 retries - the test fails.
   # These retries are essential because docker container sometimes doesn't start/stop on the first try
   def stop_start_replicas_loop(self, replicas_id_list):
      try:
         random.seed()
         replicas_start_times = []
         for replicaId in replicas_id_list:
            replicas_start_times.append(random.randint(10, 60) * -1)
         while not self._done:
            inx = 0
            for i in replicas_id_list:
               if replicas_start_times[inx] == 0:
                  counter = 0
                  while counter < 3:
                     res = self.product.kill_concord_replica(i)
                     if not res:
                        log.error(f"Failed to kill concord replica {i}")
                        counter += 1
                     else:
                        counter = 0
                        break   
                  if counter == 2:
                     self._error = True
                     raise

                  nextStart = time.time() + random.randint(60, 70) + i * random.randint(1, 10)
                  replicas_start_times[inx] = nextStart
                  log.debug(f"replica {i} stopped for {nextStart - time.time()}")
               elif replicas_start_times[inx] > 0 and replicas_start_times[inx] < time.time():
                  log.debug(f"replica {i} starting")
                  counter = 0
                  while counter < 3:
                     res = self.product.start_concord_replica(i)
                     if not res:
                        log.error(f"Failed to start concord replica {i}")
                        counter += 1
                     else:
                        counter = 0
                        break   
                  if counter == 2:
                     self._error = True
                     raise

                  replicas_start_times[inx] = -1 * (random.randint(20, 60) + i * random.randint(1, 10))
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
               res = self.product.start_concord_replica(i)
               log.debug(f"replica {i} started before leaving")
            inx += 1   

   def start_all_replicas(self):
      for i in range(1,5):
         res = self.product.start_concord_replica(i)
         if not res:
             log.error(f"Failed to start concord replica {i}")
             return False
      return True

   @describe()
   def _test_replicas_random_restart(self):
      transactions = 1000
      contractInfo = self.deploy_test_contract()
      if "address" not in contractInfo:
         return (False, "Failed to deploy contract")
      contractAddress = contractInfo["address"]

      log.info(f"start sending {transactions} transactions")
      self._send_async(transactions, contractAddress, transactions, 1)
      t = Thread(target=self.stop_start_replicas_loop, args=([2,3],))
      t.start()

      for thread in self._clientThreads:
         thread.join()

      self._done = True
      t.join()
      log.info(f"done sending {transactions} transactions")
      # there were containers start/stop problems
      if self._error:
         return(False, "Test didn't complete")

      log.info("wait for 180 seconds for replica to get synchronized")
      time.sleep(180) #give replica2 enough time to sync state
      log.info(f"Checking data")
      res = self.check_data(0,transactions, [1,2,3,4])

      if not res:
          return(False, "Data check failed")
      return (True, "Data checked")
      
      
   def clean(self):
      for i in range(1,5):
         res = self.product.kill_concord_replica(i)
         if not res:
            return (False, f"Failed to kill replica {i}")
      
      res = ""
      for i in range(1,5):
         res = self.product.cleanConcordDb(i)
         if not res:
            return (False, "Failed to clean RocksDb")
      path = res

      for i in range(1,5):
         res = self.product.start_concord_replica(i)
         if not res:
            return (False, f"Failed to start replica {i}")

      count = 0
      while count < 6:
         contractInfo = self.deploy_test_contract()
         if "address" not in contractInfo:
            count += 1
            time.sleep(10)
         else:
            count = 0
            break

      if count > 0:
         return (False, "Failed to deploy contract")
      else:      
         contractAddress = contractInfo["address"]
                 
   
   @describe()
   def _test_primary_down_viewchange_statetransfer(self):
      self.clean()
        
      transactions = 1000
      contractInfo = self.deploy_test_contract()
      if "address" not in contractInfo:
         return (False, "Failed to deploy contract")
      contractAddress = contractInfo["address"]

      log.info(f"start sending {transactions} transactions")
      self._send_async(transactions, contractAddress, transactions, 4)
      for thread in self._clientThreads:
         thread.join()
      log.info(f"done sending {transactions} transactions")
      
      res = self.product.kill_concord_replica(1)
      if self._error:
         return(False, "Failed to kill replica 1")
               
      log.info(f"start sending {transactions} transactions")           
      self._send_async(transactions, contractAddress, transactions, 4)      
      for thread in self._clientThreads:
         thread.join()
      log.info(f"done sending {transactions} transactions")
      
      res = self.product.start_concord_replica(1)
      if self._error:
         return(False, "Failed to start replica 1")
         
      log.info(f"start sending {transactions} transactions")     
      self._send_async(transactions, contractAddress, transactions, 4)
      for thread in self._clientThreads:
         thread.join()
      log.info(f"done sending {transactions} transactions") 
         
      log.info("wait for 180 seconds for replica to get synchronized")
      time.sleep(180) #give replica2 enough time to sync state           
      
      log.info(f"Checking data")
      res = self.check_data(0,transactions * 3, [1,2,3,4])

      if not res:
          return(False, "Data check failed")
      return (True, "Data checked")         


   def _get_tests(self):
      return [("replicas_random_restart", self._test_replicas_random_restart),
      ("primary_down_viewchange_statetransfer", self._test_primary_down_viewchange_statetransfer)]

   def run(self):
      self.launchProduct()
      log.info("Starting tests")

      tests = self._get_tests()
      for (testName, testFunc) in tests:
         log.info("Starting test " + testName)
         testLogDir = os.path.join(self._testLogDir, testName)
         # When this suite is switched over to pytest, the request object
         # and the blockchain ID will be available from fixtures.  For now,
         # create a request object and derive the blockchain ID here.
         request = Request(testLogDir,
                           testName,
                           self._args.reverseProxyApiBaseUrl,
                           self._userConfig,
                           util.auth.internal_admin)
         blockchainId = request.getBlockchains()[0]["id"]
         self.setEthrpcNode(request, blockchainId)
         res,info = testFunc()
         self.writeResult(testName, res, info)

      log.info("Tests are done")
      return super().run()
