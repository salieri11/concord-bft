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
INTENTIONALLY_SKIPPED_TESTS = "suites/skipped/core_vm_tests_to_skip.json"
SUPPLEMENTAL_RESULTS = "suites/supplementalResults/core_vm_supplemental_results.json"
TEST_LOG_DIR = "test_logs"
TEST_SOURCE_CODE_SUFFIX = "Filler.json"

class CoreVMTests(test_suite.TestSuite):
   _args = None
   _apiServerUrl = None
   _userConfig = None
   _ethereumMode = False
   _productMode = True
   _resultFile = None
   _supplementalResults = None
   _unintentionallySkippedFile = None

   def __init__(self, passedArgs):
      self._args = passedArgs
      self._ethereumMode = self._args.ethereumMode
      self._loadConfigFile()
      self._productMode = not self._ethereumMode
      self._resultFile = os.path.join(passedArgs.resultsDir,
                                     "coreVMTestResults.json")
      self._supplementalResults = \
                           util.json_helper.readJsonFile(SUPPLEMENTAL_RESULTS)
      self._unintentionallySkippedFile = \
                                         os.path.join(passedArgs.resultsDir,
                                         "unintentionallySkippedTests.json")
      self._unintentionallySkippedTests = {}
      self._results = {
         "CoreVMTests": {
            "result":"N/A",
            "tests":{}
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
            result, info = self._runRpcTest(test,
                                            testSource,
                                            testCompiled,
                                            testLogDir)

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
         tempFile = self._unintentionallySkippedFile + "_temp"
         realFile = self._unintentionallySkippedFile
         self._unintentionallySkippedTests[testName] = info
         writeMe = self._unintentionallySkippedTests
      else:
         tempFile = self._resultFile + "_temp"
         realFile = self._resultFile
         result = "PASS" if result else "FAIL"
         log.info(result)
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
      testSubDirs = [
         "vmArithmeticTest",
         "vmBitwiseLogicOperation"
      ]
      tests = []

      for subdir in testSubDirs:
         for test in os.listdir(os.path.join(vmTests, subdir)):
            tests.append(os.path.join(vmTests, subdir, test))

      self._removeSkippedTests(tests)
      return tests
      ############################################################
      # Special tests
      # The following are skipped or fail for various reasons
      # and are work in progress.
      ############################################################
      # For this, verify that 0xbadf000d is not stored in 0x11.
      # return [
      #    "/home/rvollmar/source/ethereum_tests/VMTests/vmArithmeticTest/signextend_Overflow_dj42Filler.json"
      # ]
      #
      # These tests do not specify what to look for and I am unsure what to look
      # for after reading the code:
      # return [
      #    "/home/rvollmar/source/ethereum_tests/VMTests/vmArithmeticTest/mulUnderFlow.json"
      #    "/home/rvollmar/source/ethereum_tests/VMTests/vmArithmeticTest/stop.json",
      # ]
      #
      # These tests require data to be passed in.
      # Tried a bunch of stuff...haven't figured out how to get CALLDATALOAD to work with these bytecode things.  Is it because these aren't full contracts?
      # return [
      #    "/home/rvollmar/source/ethereum_tests/VMTests/vmArithmeticTest/expXY_success.json"
      #    "/home/rvollmar/source/ethereum_tests/VMTests/vmArithmeticTest/expXY.json"
      # ]
      #
      # These tests do not specify what to look for, but after reading the code,
      # I think they should store 0 at 0x0.
      # return [
      #    "/home/rvollmar/source/ethereum_tests/VMTests/vmArithmeticTest/signextend_BigByte_0.json",
      #    "/home/rvollmar/source/ethereum_tests/VMTests/vmArithmeticTest/divByNonZero1.json"
      #    "/home/rvollmar/source/ethereum_tests/VMTests/vmArithmeticTest/signextend_00.json"
      # ]
      #
      # These tests end with MSTORE instead of SSTORE, so we have to get their
      # value via execution.
      # I am not sure why, but calling eth_call() results in nothing, while
      # calling eth_getCode evaluates the code and returns the result. However,
      # eth_getCode is supposed to return the code, not the value.  Maybe this is
      # because it is written in asm?
      # return [
      #    "/home/rvollmar/source/ethereum_tests/VMTests/vmArithmeticTest/fibbonacci_unrolled.json",
      #    "/home/rvollmar/source/ethereum_tests/VMTests/vmArithmeticTest/arith1.json",
      #    "/home/rvollmar/source/ethereum_tests/VMTests/vmArithmeticTest/not1.json",
      # ]


      #########################################################################
      # Special tests after this comment are done/working.
      #########################################################################
      # These have no "expect" section in the source test file, but they do have
      # a "post"["storage"] section in the compiled test file with specific data
      # about what to look for.  Have the test suite look for that if the expect
      # is missing.
      # return [
      #    "/home/rvollmar/source/ethereum_tests/VMTests/vmArithmeticTest/mulmod1_overflow2.json",
      #    "/home/rvollmar/source/ethereum_tests/VMTests/vmArithmeticTest/addmod1_overflow3.json",
      #    "/home/rvollmar/source/ethereum_tests/VMTests/vmArithmeticTest/addmod1_overflow4.json",
      #    "/home/rvollmar/source/ethereum_tests/VMTests/vmArithmeticTest/addmod1_overflowDiff.json",
      #    "/home/rvollmar/source/ethereum_tests/VMTests/vmArithmeticTest/addmodDivByZero3.json",
      #    "/home/rvollmar/source/ethereum_tests/VMTests/vmArithmeticTest/modByZero.json",
      #    "/home/rvollmar/source/ethereum_tests/VMTests/vmArithmeticTest/mulmoddivByZero3.json",
      #    "/home/rvollmar/source/ethereum_tests/VMTests/vmArithmeticTest/sdivByZero2.json",
      #    "/home/rvollmar/source/ethereum_tests/VMTests/vmArithmeticTest/smod6.json",
      #    "/home/rvollmar/source/ethereum_tests/VMTests/vmArithmeticTest/smod8_byZero.json"
      # ]

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

      caller = self._getAUser()
      expectedStorage = self._getExpectedStorageResults(testPath,
                                                        testSource,
                                                        testCompiled)

      if not expectedStorage:
        info = "Could not find expected storage results."
      else:
         log.info("Starting test '{}'".format(testName))

         rpc = RPC(testLogDir,
                   testName,
                   self._apiServerUrl)
         txHash = rpc.sendTransaction(caller, data, gas)

         if txHash:
            contractAddress = rpc.getContractAddrFromTxHash(txHash,
                                                            self._ethereumMode)
            if contractAddress:
               success, info = self._checkResult(rpc,
                                                 contractAddress,
                                                 expectedStorage)
            else:
               info = "Never received contract address."
         else:
            info = "Did not receive a transaction hash."

      return (success, info)

   def _getExpectedStorageResults(self, testPath, testSource, testCompiled):
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
      '''
      storage = self._getStorageSection(testSource)
      if storage and self._containsStorageData(storage):
         return storage

      storage = self._getStorageSection(testCompiled)
      if storage and self._containsStorageData(storage):
         return storage

      storage = self._getSupplementalStorageSection(testPath)
      return storage

   def _getSupplementalStorageSection(self, testPath):
      '''
      Given the full path of the test, look for a match in the supplemental
      results.  We do this because some test cases do not define the expected
      result.
      '''
      subPath = testPath.replace(self._userConfig["ethereum"]["testRoot"] + "/",
                                 "")
      if subPath in list(self._supplementalResults.keys()):
         return self._supplementalResults[subPath]

      return None

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

   def _checkResult(self, rpc, contractAddress, expectedStorage):
      '''
      Loops through the expected storage structure in the test case, comparing
      those values to the actual stored values in the block.
      Returns True if all match, False if any fail to match, and None if there
      are no values.  (It's possible that tests with no values are supposed to
      contains 0 at 0x0; need to investigate.)
      '''
      success = None
      info = None

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

      if success == None:
         info = "No expected storage results were found."

      return success, info
