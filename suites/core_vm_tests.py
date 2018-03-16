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
import util.json_helper

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
   _skippedFile = None
   _skippedTests = None

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
      self._skippedFile = os.path.join(passedArgs.resultsDir,
                                       "skippedTests.json")
      self._results = {
         "CoreVMTests": {
            "result":"N/A",
            "tests":{}
         }
      }
      self._skippedTests = {}

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

            if not testCompiled:
               self._writeResult(test, None, "Unable to load compiled test.")
               continue

            testSource = self._loadTestSource(testCompiled)

            if not testSource:
               self._writeResult(test, None, "Unable to load test source.")
               continue

            testName = list(testCompiled.keys())[0]
            testLogDir = os.path.join(self._args.resultsDir, TEST_LOG_DIR, testName)
            result, info = self._runRpcTest(testSource, testCompiled, testLogDir)

            if info:
               info += "  "
            else:
               info = ""

            info += "Log: {}".format(testLogDir)
            self._writeResult(testName, result, info)

      log.info("Tests are done.")

      if self._productMode:
         p.stopProduct()

      return self._resultFile

   def _writeResult(self, testName, result, info):
      '''
      We're going to write the full result or skipped test set to json for each
      test so that we have a valid result structure even if things die partway
      through.
      '''
      tempFile = None
      writeMe = None

      if result == None:
         log.debug("Skipping test '{}': '{}'".format(testName, info))
         tempFile = self._skippedFile + "_temp"
         realFile = self._skippedFile
         self._skippedTests[testName] = info
         writeMe = self._skippedTests
      else:
         log.info(result)
         tempFile = self._resultFile + "_temp"
         realFile = self._resultFile
         result = "PASS" if result else "FAIL"
         self._results["CoreVMTests"]["tests"][testName] = {
            "result": result,
            "info": info
         }
         writeMe = self._results

      with open(tempFile, "w") as f:
         f.write(json.dumps(writeMe, sort_keys=True, indent=4))

      os.rename(tempFile, realFile)

   def _loadConfigFile(self):
      '''
      Loads the main config file.
      '''
      self._userConfig = util.json_helper.readJsonFile(CONFIG_JSON)

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

      # These basic tests work reliably.
      # return [
      #    os.path.join(vmArithmeticTests, "add0.json"),
      #    os.path.join(vmArithmeticTests, "add1.json")
      # ]

      # This returns every test in vmArithmeticTests.
      tests = []
      for test in os.listdir(vmArithmeticTests):
         tests.append(os.path.join(vmArithmeticTests, test))

      return tests

      # This test and several like it are failing, but it worked when I
      # manually worked with it, so I'm missing something in the framework.
      # Keeping commented out here as a reminder for what to work on next.
      #return [os.path.join(vmArithmeticTests, "expPowerOf2_8.json")]

   def _loadCompiledTest(self, test):
      '''
      Reads the compiled test from a file and returns it.
      '''
      testCompiled = None
      testCompiled = util.json_helper.readJsonFile(test)
      return testCompiled

   def _loadTestSource(self, testCompiled):
      '''
      Reads the test source code from a file and returns it.
      '''
      ethTestsSrc = self._userConfig["ethereum"]["testRoot"]
      testName = list(testCompiled.keys())[0]
      ethTestsSrc = os.path.join(ethTestsSrc, testCompiled[testName] \
                                 ["_info"]["source"])
      testSource = util.json_helper.readJsonFile(ethTestsSrc)
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
      success = None
      info = None
      testName = list(testSource.keys())[0]
      data = testCompiled[testName]["exec"]["code"]
      caller = self._getAUser()

      # For some tests, expected results are only in the "src" files which
      # are used to generate the tests.  The compiled files contain no
      # expected storage values.  The trend so far, from manual inspection, is
      # that this is when storage at 0x0 is expected to equal 0. I prefer to
      # have something more explicit, as is found in the src files.
      if "expect" in testSource[testName]:
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
                  info = "No storage section."
               else:
                  for storageLoc in expectedStorage:
                     actualRawValue = rpc.getStorageAt(contractAddress, storageLoc)
                     actualValue = int(actualRawValue, 16)
                     expectedRawValue = expectedStorage[storageLoc]
                     expectedValue = int(expectedRawValue, 16)
                     log.debug("Expected raw value: '{}', actual raw value: '{}'". \
                               format(expectedRawValue, actualRawValue))
                     log.debug("Expected value: '{}', actual value: '{}'". \
                               format(expectedValue, actualValue))
                     if expectedValue == actualValue:
                        success = True
                     else:
                        success = False
                        info = "Expected '{}', received '{}'.". \
                               format(expectedValue, actualValue)
                        break
            else:
               info = "Never received contract address."
      else:
         info = "No 'expect' section."

      return (success, info)
