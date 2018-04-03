#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Tasks:
# - Launch geth when running in ethereumMode. (Low priority...dev mode)
# - Need to unlock the user. (Need API first.)
#########################################################################
import argparse
import collections
import json
import logging
import os
import pprint
import tempfile
import time

from . import test_suite
from rpc.rpc_call import RPC
from util.debug import pp as pp
from util.product import Product
import util.json_helper

log = logging.getLogger(__name__)

# The config file contains information aobut how to run things, as opposed to
# command line parameters, which are about running tests.
CONFIG_JSON = "resources/user_config.json"
INTENTIONALLY_SKIPPED_TESTS = "suites/skipped/core_vm_tests_to_skip.json"
TEST_LOG_DIR = "test_logs"
TEST_SOURCE_CODE_SUFFIX = "Filler.json"

class CoreVMTests(test_suite.TestSuite):
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
                                     "coreVMTestResults.json")
      self._unintentionallySkippedFile = \
                                         os.path.join(passedArgs.resultsDir,
                                         "unintentionallySkippedTests.json")
      self._unintentionallySkippedTests = {}
      self._results = {
         "CoreVMTests": {
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

            try:
               result, info = self._runRpcTest(test,
                                               testSource,
                                               testCompiled,
                                               testLogDir)
            except Exception as e:
               result = False
               info = str(e)
               log.error("Exception running RPC test: '{}'".format(info))

            if info:
               info += "  "
            else:
               info = ""

            info += "Log: {}".format(testLogDir)
            self._writeResult(test, result, info)

      log.info("Tests are done.")

      if self._productMode:
         p.stopProduct()

      return self._resultFile

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
         not self._results["CoreVMTests"]["result"] == "FAIL":
            self._results["CoreVMTests"]["result"] = result

      self._results["CoreVMTests"]["tests"][testName] = {
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

   def _removeSkippedTests(self, testList):
      '''
      Loads the skipped test file and removes skipped tests from the given
      test list.
      '''
      skippedConfig = util.json_helper.readJsonFile(INTENTIONALLY_SKIPPED_TESTS)
      for key in list(skippedConfig.keys()):
         skippedTest = os.path.join(self._userConfig["ethereum"]["testRoot"],
                                    key)
         if skippedTest in testList:
            log.info("Skipping: {}".format(key))
            testList.remove(skippedTest)

   def _getTests(self):
      '''
      Returns a list of file names.  Each file is a test to run.
      '''
      vmTests = os.path.join(self._userConfig["ethereum"]["testRoot"], "VMTests")
      tests = []
      testSubDirs = None

      if self._args.tests:
         fullPath = os.path.join(vmTests, self._args.tests)

         if os.path.exists(fullPath):
            if os.path.isfile(fullPath):
               tests.append(os.path.join(vmTests, fullPath))
            else:
               testSubDirs = [self._args.tests]
         else:
            log.error("File '{}' does not exist.".format(fullPath))
      else:
         testSubDirs = [
            "vmArithmeticTest",
            "vmBitwiseLogicOperation",
            "vmRandomTest",
            "vmPushDupSwapTest",
            "vmSha3Test",
            "vmBlockInfoTest",
            "vmEnvironmentalInfo",
            "vmIOandFlowOperations",
            "vmLogTest",
            "vmPerformance",
            "vmSystemOperations",
            "vmTests"
         ]

      if testSubDirs:
         for subdir in testSubDirs:
            for test in os.listdir(os.path.join(vmTests, subdir)):
               tests.append(os.path.join(vmTests, subdir, test))

      if tests:
         self._removeSkippedTests(tests)
         if not tests:
            log.error("All of the tests that would have run are in the " \
                      "skipped test list.")

      if not tests:
         log.error("There are no tests to run.")

      return tests

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
         return self._userConfig["ethereum"]["users"][0]
      else:
         return self._userConfig["product"]["users"][0]

   def _runRpcTest(self, testPath, testSource, testCompiled, testLogDir):
      ''' Runs one test. '''
      success = None
      info = None
      testName = list(testSource.keys())[0]
      data = testCompiled[testName]["exec"]["code"]

      # Gas levels provided in test cases may not be enough.  Just set it high.
      # Maye this should be a config file setting, since it depends on how the
      # user set up their Ethereum instance.
      # Note that VMware will not use gas.
      gas = "0x47e7c4" if self._ethereumMode else None
      user = self._getAUser()
      expectedStorage = self._getExpectedStorageResults(testSource,
                                                        testCompiled)
      rpc = RPC(testLogDir,
                testName,
                self._apiServerUrl)

      # Don't know if we'll have a personal_* interface in the product, but
      # we can at least handle Ethereum for now.
      if self._ethereumMode and not self._userUnlocked:
         log.debug("Unlocking account '{}'".format(user["hash"]))
         rpc.unlockAccount(user["hash"], user["password"])
         self._userUnlocked = True

      log.info("Starting test '{}'".format(testName))
      txHash = rpc.sendTransaction(user["hash"], data, gas)

      if txHash:
         txReceipt = rpc.getTransactionReceipt(txHash,
                                               self._ethereumMode)
         if txReceipt:
            expectTxSuccess = self._getExpectTxSuccess(testCompiled)
            success, info = self._checkResult(rpc,
                                              txReceipt,
                                              expectedStorage,
                                              expectTxSuccess)
         else:
            info = "Did not receive a transaction receipt."
      else:
         info = "Did not receive a transaction hash."

      return (success, info)

   def _getExpectedStorageResults(self, testSource, testCompiled):
      '''
      The ideal expected result is in the source file, since that is more
      detailed.  With some tests, there is only an expected result in the
      compiled file.  This function tries to figure out which "storage"
      section we want to use and returns it.  The storage section is a mapping
      of storage slot to expected value and looks like this:
         "storage" : {
                        "0x00" : "0x0100",
                        "0x01" : "0x80",
                        "0x02" : "0x0200"
                     }
      If there is a storage section, but it has no data, just fill it with
      zeroes.  Most test cases I've looked at in this situation create 0 at
      block 0.  One puts a 0 in location 3.
      If there is no storage section at all, returns None.
      '''
      storage = self._getStorageSection(testSource)
      if storage == None:
         storage = self._getStorageSection(testCompiled)

      if storage != None and not self._containsStorageData(storage):
         # Haven't seen a location > 3 yet, but check 10 just in case.
         for i in range(10):
            storage[hex(i)] = "0x0"

      return storage

   def _getExpectTxSuccess(self, testCompiled):
      '''
      Per http://ethereum-tests.readthedocs.io/en/latest/test_types/vm_tests.html

         It is generally expected that the test implementer will
         read env, exec and pre then check their results against
         gas, logs, out, post and callcreates. If an exception is
         expected, then latter sections are absent in the test.
      '''
      testName = list(testCompiled.keys())[0]
      keys = list(testCompiled[testName].keys())

      for indicator in ["gas", "logs", "out", "post", "callcreates"]:
         if indicator in keys:
            return True

      return False

   def _containsStorageData(self, storage):
      '''
      Given a storage section, determines if it contains data we can use to
      verify a result.  Data "we can use" means there is a k/v pair, and the
      value is a non-empty string.  Values are numbers encoded as a hex string
      starting with "0x".
      '''
      for key in storage:
         if storage[key]:
            return True

      return False

   def _getStorageSection(self, testData):
      '''
      Given the contents of a test json file, find and return the storage
      section.  It is under "expect" for a source test file and "post" for
      a compiled test file.  e.g.
      {
         "modByZero":
         {
            "expect":  <=== Either "expect" or "post"
            {
               "0x0f572e5295c57f15886f9b263e2f6d2d6c7b5ec6":  <== Varies
               {
                  "storage" : {    <=== Return this dict.
                                 "0x00" : "0x0100",
                                 "0x01" : "0x80",
                                 "0x02" : "0x0200"
                              }
               }
            }
         }
      }
      Returns None if it is not present.
      '''
      expectOrPostSection = None
      testName = list(testData.keys())[0]
      storageSection = None

      if "expect" in testData[testName]:
         expectOrPostSection = testData[testName]["expect"]
      elif "post" in testData[testName]:
         expectOrPostSection = testData[testName]["post"]

      if expectOrPostSection:
         blockAddress = list(expectOrPostSection.keys())[0]
         if "storage" in expectOrPostSection[blockAddress]:
            storageSection = expectOrPostSection[blockAddress]["storage"]

      return storageSection

   def _checkResult(self, rpc, txReceipt, expectedStorage, expectTxSuccess):
      '''
      Loops through the expected storage structure in the test case, comparing
      those values to the actual stored values in the block.
      Returns True if all match, False if any fail to match, and None if there
      are no values.  (It's possible that tests with no values are supposed to
      contains 0 at 0x0; need to investigate.)
      '''
      success = None
      info = None
      txStatusCorrect, info = self._checkTxStatus(rpc, txReceipt, expectTxSuccess)

      if txStatusCorrect:
         if not expectTxSuccess:
            # Failed as expected.
            success = True
         else:
            if expectedStorage:
               keys = sorted(expectedStorage)
               contractAddress = txReceipt["contractAddress"]

               for storageLoc in keys:
                  actualRawValue = rpc.getStorageAt(contractAddress, storageLoc)
                  log.debug("actualRawValue: '{}'".format(actualRawValue))
                  actualValue = int(actualRawValue, 16)
                  log.debug("actualValue: '{}'".format(actualValue))
                  expectedRawValue = expectedStorage[storageLoc]
                  log.debug("expectedRawValue: '{}'".format(expectedRawValue))
                  expectedValue = int(expectedRawValue, 16)
                  log.debug("expectedValue: '{}'".format(expectedValue))

                  if expectedValue == actualValue:
                     success = True
                  else:
                     success = False
                     log.debug("Expected storage:")
                     for storageLoc in keys:
                       log.debug("   {}: {}".format(storageLoc, expectedStorage[storageLoc]))

                     info = "Expected value does not match actual value:\n"
                     info += "Expected raw value: '{}', actual raw value: '{}'\n". \
                             format(expectedRawValue, actualRawValue)
                     info += "Expected value: '{}', actual value: '{}'". \
                             format(expectedValue, actualValue)
                     log.info(info)
                     break
            else:
               # The test may be checking gas, logs, out, or callcreates,
               # which we're not checking (yet). This test should be added to the
               # skip list so it is not run in the future.
               success = None
               info = "No expected storage found, and the test was expected " \
                      "to be successful. Test needs verification by some " \
                      "other method. Test will be marked skipped."
      else:
         # None or False.
         success = txStatusCorrect

      return success, info

   def _checkTxStatus(self, rpc, txReceipt, expectTxSuccess):
      '''
      Returns a tuple of whether the transaction's status is what is expected,
      and a message if it is not as expected.
      The tx status is 0 (failure) or 1 (success) per API doc:
      https://github.com/ethereum/wiki/wiki/JSON-RPC#returns-31
      If the tx status is unrecognized, the first item in the tuple is None,
      which tells the framework to skip evaluating the result.
      '''
      txStatus = rpc.getTransactionReceiptStatus(txReceipt)
      success = None
      info = None

      if not txStatus in ["0x0", "0x1"]:
         # Unexpected. We cannot evaluate this test's results.
         success = None
         info = "Unrecognized transaction status: '{}'".format(txStatus)
      else:
         if (expectTxSuccess and txStatus == "0x1") or \
            (not expectTxSuccess and txStatus == "0x0"):
            success = True
         else:
            success = False
            info = "Expected Tx success: '{}', but transaction status was: " \
                   "'{}'".format(expectTxSuccess, txStatus)

      return success, info
