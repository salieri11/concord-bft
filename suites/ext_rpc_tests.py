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

from . import test_suite
from rpc.rpc_call import RPC
from util.debug import pp as pp
from util.numbers_strings import trimHexIndicator, decToEvenHexNo0x
from util.product import Product
import util.json_helper

log = logging.getLogger(__name__)

# The config file contains information aobut how to run things, as opposed to
# command line parameters, which are about running tests.
CONFIG_JSON = "resources/user_config.json"
TEST_LOG_DIR = "test_logs"

class ExtendedRPCTests(test_suite.TestSuite):
   _args = None
   _apiServerUrl = None
   _userConfig = None
   _ethereumMode = False
   _productMode = True
   _resultFile = None
   _unintentionallySkippedFile = None
   _userUnlocked = False

   def __init__(self, passedArgs):
      self._args = passedArgs
      self._ethereumMode = self._args.ethereumMode
      self._loadConfigFile()
      self._productMode = not self._ethereumMode
      self._resultFile = os.path.join(passedArgs.resultsDir,
                                     "extendedRPCTestResults.json")
      self._unintentionallySkippedFile = \
                                         os.path.join(passedArgs.resultsDir,
                                         "unintentionallySkippedTests.json")
      self._unintentionallySkippedTests = {}
      self._results = {
         "ExtendedRPCTests": {
            "result":"",
            "tests": collections.OrderedDict()
         }
      }

      if self._ethereumMode:
         log.debug("Running in ethereum mode")
         self._apiServerUrl = "http://localhost:8545"
      else:
         self._apiServerUrl = "http://localhost:8080/api/athena/eth/"

      with open(self._resultFile, "w") as f:
         f.write(json.dumps(self._results))

   def getName(self):
      return "ExtendedRPCTests"

   def run(self):
      ''' Runs all of the tests. '''
      if self._productMode:
         p = Product(self._args.resultsDir,
                     self._apiServerUrl,
                     self._userConfig["product"])
         p.launchProduct()
         if not p.waitForProductStartup():
            log.error("The product did not start.  Exiting.")
            exit(1)

      tests = self._getTests()

      for (testName, testFun) in tests:
         testLogDir = os.path.join(self._args.resultsDir, TEST_LOG_DIR, testName)

         try:
            result, info = self._runRpcTest(testName,
                                            testFun,
                                            testLogDir)
         except Exception as e:
            result = False
            info = str(e)
            traceback.print_tb(e.__traceback__)
            log.error("Exception running RPC test: '{}'".format(info))

         if info:
            info += "  "
         else:
            info = ""

         relativeLogDir = self._makeRelativeTestPath(testLogDir)
         info += "Log: <a href=\"{}\">{}</a>".format(relativeLogDir,
                                                     testLogDir)
         self._writeResult(testName, result, info)

      log.info("Tests are done.")

      if self._productMode:
         p.stopProduct()

      return self._resultFile

   def _makeRelativeTestPath(self, fullTestPath):
      '''
      Given the full test path (in the results directory), return the
      relative path.
      '''
      return fullTestPath[len(self._args.resultsDir)+1:len(fullTestPath)]

   def _writeUnintentionallySkippedTest(self, testName, info):
      tempFile = self._unintentionallySkippedFile + "_temp"
      realFile = self._unintentionallySkippedFile
      self._unintentionallySkippedTests[testName] = info

      with open(tempFile, "w") as f:
         f.write(json.dumps(self._unintentionallySkippedTests,
                            sort_keys=True, indent=4))

      os.rename(tempFile, realFile)

   def _writeResult(self, testName, result, info):
      '''
      We're going to write the full result or skipped test set to json for each
      test so that we have a valid result structure even if things die partway
      through.
      '''
      tempFile = self._resultFile + "_temp"
      realFile = self._resultFile

      if result:
         result = "PASS"
      elif result == False:
         result = "FAIL"
      else:
         result = "SKIPPED"

      log.info(result)

      if result == "SKIPPED":
         log.debug("Unintentionally skipped test '{}': '{}'".format(testName,
                                                                    info))
         self._writeUnintentionallySkippedTest(testName, info)

      # Never change the suite's result due to skips or if it has already
      # been set to "FAIL".
      if not result == "SKIPPED" and \
         not self._results["ExtendedRPCTests"]["result"] == "FAIL":
            self._results["ExtendedRPCTests"]["result"] = result

      self._results["ExtendedRPCTests"]["tests"][testName] = {
         "result": result,
         "info": info
      }

      with open(tempFile, "w") as f:
         f.write(json.dumps(self._results, indent=4))

      os.rename(tempFile, realFile)

   def _loadConfigFile(self):
      '''
      Loads the main config file.
      '''
      self._userConfig = util.json_helper.readJsonFile(CONFIG_JSON)

      if "ethereum" in self._userConfig and \
         "testRoot" in self._userConfig["ethereum"]:

         self._userConfig["ethereum"]["testRoot"] = \
            os.path.expanduser(self._userConfig["ethereum"]["testRoot"])

   def _getTests(self):
      return [("web3_sha3", self._test_web3_sha3), \
              ("web3_clientVersion", self._test_web3_clientVersion), \
              ("eth_mining", self._test_eth_mining), \
              ("rpc_modules", self._test_rpc_modules)]

   def _runRpcTest(self, testName, testFun, testLogDir):
      ''' Runs one test. '''
      log.info("Starting test '{}'".format(testName))
      rpc = RPC(testLogDir,
                testName,
                self._apiServerUrl)
      return testFun(rpc)

   def _test_web3_sha3(self, rpc):
      '''
      Check that hashing works as expected.
      '''
      # list of (data, expected hash) tuples
      datahashes = [("0x", "0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"), \
                    ("0x1234567890abcdef", "0xed8ab4fde4c4e2749641d9d89de3d920f9845e086abd71e6921319f41f0e784f")]

      for (d, h) in datahashes:
         result = rpc.sha3(d)
         if not result == h:
            return (False,
                    "Hash of '{}' did not match" \
                    "expected '{}': actual: '{}'".format(d, h, result))

      return (True, None)

   def _test_web3_clientVersion(self, rpc):
      '''
      Check that we return a valid version
      '''
      result = rpc.clientVersion()
      if not type(result) is str:
         return (False, "Client version should have been a string, " \
                 "but was '{}'".format(result))

      # Insisting version is
      # <name>/v<major>.<minor>.<patch><anything>/
      #    <os>/<language><major>.<minor>.<patch>
      version_re = re.compile("\\w+/v\\d+\\.\\d+\\.\\d+[^/]*/"\
                              "[-a-zA-Z0-9]+/\\w+\\d\\.\\d\\.\\d")
      if not version_re.match(result):
         return (False, "Client version doesn't match expected format: " \
                 "'{}'".format(result))

      return (True, None)

   def _test_eth_mining(self, rpc):
      '''
      Check that mining status is reported correctly
      '''
      result = rpc.mining()
      if self._ethereumMode and (not result == True):
         return (False, "Expected ethereumMode to be mining, " \
                 "but found '{}'".format(result))
      elif self._productMode and (not result == False):
         return (False, "Expected product to not be mining, " \
                 "buf found '{}'".format(result))

      return (True, None)

   def _test_rpc_modules(self, rpc):
      '''
      Check that available RPC modules are listed correctly
      '''
      result = rpc.modules()

      if not type(result) is collections.OrderedDict:
         return (False, "Reply should have been a dict.")

      # This means the test is invalid, but let's not blow up because of it
      if len(result) == 0:
         log.warn("No RPC modules returned from rpc request.")

      # Insisting the version is <number>.<number>
      version_re = re.compile("\\d+\\.\\d+")

      for k, v in result.items():
         if not k in ["admin", "eth", "miner", "net", "personal", "rpc", "web3"]:
            return (False,
                    "Response included unknown RPC module '{}'".format(k))
         if not version_re.match(v):
            return (False,
                    "Module version should be version like, " \
                    "but was '{}'".format(v))

      return (True, None)
