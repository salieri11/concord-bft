#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Tasks:
# - Launch geth when running in ethereumMode. (Low priority...dev mode)
# - Need to unlock the user. (Need API first.)
#########################################################################
import argparse
import json
import logging
import os
import tempfile
import time

from . import test_suite
from rpc.rpc_call import RPC
from util.product import Product

log = logging.getLogger(__name__)

# The config file contains information aobut how to run things, as opposed to
# command line parameters, which are about running tests.
CONFIG_JSON = "resources/user_config.json"
TEST_LOG_DIR = "test_logs"
TEST_SOURCE_CODE_SUFFIX = "Filler.json"

class CoreVMTests(test_suite.TestSuite):
   _args = None
   _apiServerUrl = None
   _userConfig = None
   _ethereumMode = False
   _productMode = True
   _resultFile = None

   def __init__(self, passedArgs):
      self._args = passedArgs
      self._ethereumMode = self._args.ethereumMode
      self._loadConfigFile()
      self._productMode = not self._ethereumMode

      if self._ethereumMode:
         log.debug("Running in ethereum mode")
         self._apiServerUrl = "http://localhost:8545"
      else:
         self._apiServerUrl = "http://localhost:8080/api/athena/eth/"

      self._resultFile = os.path.join(passedArgs.resultsDir,
                                     "coreVMTestResults.json")
      self._results = {
         "CoreVMTests": {
            "result":"N/A",
            "tests":{}
         }
      }

      with open(self._resultFile, "w") as f:
         f.write(json.dumps(self._results))

   def getName(self):
      return "CoreVMTests"

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

      for test in tests:
         if not self._isSourceCodeTestFile(test):
            testCompiled = self._loadCompiledTest(test)
            testSource = self._loadTestSource(testCompiled)
            testName = list(testCompiled.keys())[0]
            testLogDir = os.path.join(self._args.resultsDir, TEST_LOG_DIR, testName)
            result = self._runRpcTest(testSource, testCompiled, testLogDir)
            self._writeResult(testName, result, testLogDir)

      log.info("Tests are done.")

      if self._productMode:
         p.stopProduct()

      return self._resultFile

   def _writeResult(self, testName, result, info):
      '''
      We're going to write the full result set to json for each test so that
      we have a valid result structure even if things die partway through.

      We'll keep the structure in memory so at least we aren't reading the
      entire thing with every test case.
      '''
      result = "PASS" if result else "FAIL"
      log.info(result)

      self._results["CoreVMTests"]["tests"][testName] = {
         "result": result,
         "info": info
      }

      tempFile = self._resultFile + "_temp"

      with open(tempFile, "w") as f:
         f.write(json.dumps(self._results, sort_keys=True, indent=4))

      os.rename(tempFile, self._resultFile)

   def _loadConfigFile(self):
      '''
      Loads the main config file.
      '''
      try:
         with open(CONFIG_JSON) as f:
            self._userConfig = json.load(f)
      except IOError:
         log.error("The config file '{}' could not be read; it may be " \
                   "missing or have restricted permissions.".format(CONFIG_JSON))
      except json.JSONDecodeError as e:
         log.error("The config file '{}' could not be parsed as json. " \
                   "Error: '{}'" \
                   .format(CONFIG_JSON, e))

      if self._userConfig:
         if "ethereum" in self._userConfig and \
            "testRoot" in self._userConfig["ethereum"]:

            self._userConfig["ethereum"]["testRoot"] = \
               os.path.expanduser(self._userConfig["ethereum"]["testRoot"])

      else:
         exit(1)

   def _getTests(self):
      '''
      Returns a list of file names.  Each file is a test to run.
      '''
      ethTests = self._userConfig["ethereum"]["testRoot"]
      vmArithmeticTests = os.path.join(ethTests, "VMTests", "vmArithmeticTest")

      # The goal is to run them all.  Let's just start with the basics.
      return [
         os.path.join(vmArithmeticTests, "add0.json"),
         os.path.join(vmArithmeticTests, "add1.json")
      ]

   def _loadCompiledTest(self, test):
      '''
      Reads the compiled test from a file and returns it.
      '''
      testCompiled = None

      with open(test) as f:
         testCompiled = json.load(f)

      return testCompiled

   def _loadTestSource(self, testCompiled):
      '''
      Reads the test source code from a file and returns it.
      '''
      ethTestsSrc = self._userConfig["ethereum"]["testRoot"]
      testName = list(testCompiled.keys())[0]
      ethTestsSrc = os.path.join(ethTestsSrc, testCompiled[testName]["_info"]["source"])
      testSource = None

      with open(ethTestsSrc) as f:
         testSource = json.load(f)

      return testSource

   def _isSourceCodeTestFile(self, name):
      '''
      Determines if a test file name is source code or a real test.
      '''
      return TEST_SOURCE_CODE_SUFFIX in name

   def _getAUser(self):
      '''
      Gets a user hash from the user config file, based on whether we're using
      the product or Ethereum.
      '''
      if self._ethereumMode:
         return self._userConfig["ethereum"]["users"][0]["hash"]
      else:
         return self._userConfig["product"]["users"][0]["hash"]

   def _runRpcTest(self, testSource, testCompiled, testLogDir):
      ''' Runs one test. '''
      success = False
      testName = list(testSource.keys())[0]
      data = testCompiled[testName]["exec"]["code"]
      caller = self._getAUser()

      # For some tests, expected results are only in the "src" files which
      # are used to generate the tests.  The compiled files contain no
      # expected storage values.  The trend so far, from manual inspection, is
      # that this is when storage at 0x0 is expected to equal 0. I prefer to
      # have something more explicit, as is found in the src files.
      expectSection = testSource[testName]["expect"]
      expectedStorage = expectSection[list(expectSection.keys())[0]]["storage"]

      log.info("Starting test '{}'".format(testName))

      rpc = RPC(testLogDir,
                testName,
                self._apiServerUrl)
      txHash = rpc.sendTransaction(caller, data)

      if txHash:
         contractAddress = rpc.getContractAddrFromTxHash(txHash,
                                                         self._ethereumMode)

         if contractAddress:
            if (len(expectedStorage) < 1):
               log.debug("No expected storage detected.  Skipping")
            else:
               for storageLoc in expectedStorage:
                  actualValue = rpc.getStorageAt(contractAddress, storageLoc)
                  actualValue = int(actualValue, 16)
                  expectedValue = int(expectedStorage[storageLoc], 16)
                  log.debug("Expected value: '{}', actual value: '{}'". \
                            format(expectedValue, actualValue))
                  if expectedValue == actualValue:
                     success = True
                  else:
                     success = False
                     break
         else:
            log.debug("Never got a contract address.")

      return success
