#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Test the parts of the Ethereum JSON RPC API beyond what the
# CoreVMTests cover. This checks things like web3_sha3,
# eth_clientVersion, eth_mining, ...
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

from . import test_suite
from rpc.rpc_call import RPC
from util.debug import pp as pp
from util.numbers_strings import trimHexIndicator, decToEvenHexNo0x
from util.product import Product
import util.json_helper
from rest.request import Request
from datetime import datetime

log = logging.getLogger(__name__)

class KVBTests(test_suite.TestSuite):
   _args = None
   _apiServerUrl = None
   _userConfig = None
   _ethereumMode = False
   _productMode = True
   _resultFile = None
   _unintentionallySkippedFile = None
   _userUnlocked = False
   p = None

   def __init__(self, passedArgs):
      super(KVBTests, self).__init__(passedArgs)

      if self._ethereumMode:
         log.debug("Running in ethereum mode")
         self._apiServerUrl = "http://localhost:8545"
      else:
         self._apiServerUrl = "https://localhost/blockchains/local/api/athena/eth/"

   def getName(self):
      return "KVBTests"

   def run(self):
      ''' Runs all of the tests. '''
      if self._productMode and not self._noLaunch:
         global p
         try:
            p = self.launchProduct(self._args,
                                   self._apiServerUrl,
                                   self._userConfig)
         except Exception as e:
            log.error(traceback.format_exc())
            return self._resultFile

      tests = self._getTests()

      for (testName, testFun) in tests:
         testLogDir = os.path.join(self._testLogDir, testName)

         try:
            result, info = self._runRpcTest(testName,
                                            testFun,
                                            testLogDir)
         except Exception as e:
            result = False
            info = str(e) + "\n" + traceback.format_exc()
            log.error("Exception running RPC test: '{}'".format(info))

         if info:
            info += "  "
         else:
            info = ""

         relativeLogDir = self.makeRelativeTestPath(testLogDir)
         info += "Log: <a href=\"{}\">{}</a>".format(relativeLogDir,
                                                     testLogDir)
         self.writeResult(testName, result, info)

      log.info("Tests are done.")

      if self._productMode and not self._noLaunch:
         p.stopProduct()

      return self._resultFile

   def _getTests(self):
      return [
              ("add_block", self._test_add_block), \
              ("disk_persistence", self._test_disk_persistence)]

   def _runRpcTest(self, testName, testFun, testLogDir):
      ''' Runs one test. '''
      log.info("Starting test '{}'".format(testName))
      rpc = RPC(testLogDir,
                testName,
                self._apiServerUrl,
                self._userConfig)
      return testFun(rpc)

   def _test_add_block(self, rpc):
      '''
      Check if blocks are getting added
      '''
      if self._productMode:
         pre = int(rpc.getBlockNumber(), 16)

         #Create a user. This should create a block
         random.seed(datetime.now())
         password = random.random()
         #print("using " + str(password) + " as password for creating new account")

         hash = rpc.newAccount(str(password))
         post = int(rpc.getBlockNumber(), 16)

         if post - pre != 1:
            return (False, "Blocks not created properly. "
            + str(post - pre) + " blocks getting created instead of 1")

         return (True, None)

      else:
         #Skip the test if running in Ethereum mode
         return (None, None)

   def _test_disk_persistence(self, rpc):
      '''
      Check if blocks persist to disk.
      Note: This test is not valid when Athena uses an in memory database.
      '''
      if self._productMode:
         pre = int(rpc.getBlockNumber(), 16)

         if pre <= 0:
            return (None, "No blocks to restore.")

         #Kill and reboot Athena
         global p
         p.stopProduct()

         try:
            self._args.keepAthenaDB = True
            p = self.launchProduct(self._args,
                                   self._apiServerUrl,
                                   self._userConfig)
         except Exception as e:
            log.error(traceback.format_exc())
            return self._resultFile

         post = int(rpc.getBlockNumber(), 16)

         if post < pre:
            return (False, "Blocks not persisted to disk correctly." +
              "Note: This test should fail if Athena uses an in memory database.")

      return (True, None)
