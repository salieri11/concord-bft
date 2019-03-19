#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
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
import tempfile
import traceback
import time
import traceback

from . import test_suite
from rpc.rpc_call import RPC
from util.debug import pp as pp
from util.numbers_strings import trimHexIndicator, stringOnlyContains
from util.product import Product
import util.json_helper

log = logging.getLogger(__name__)

INTENTIONALLY_SKIPPED_TESTS = "suites/skipped/core_vm_tests_to_skip.json"
TEST_SOURCE_CODE_SUFFIX = "Filler.json"

class CoreVMTests(test_suite.TestSuite):
   _args = None
   _resultFile = None
   _unintentionallySkippedFile = None

   def __init__(self, passedArgs):
      super(CoreVMTests, self).__init__(passedArgs)

      if "ethereum" in self._userConfig and \
         "testRoot" in self._userConfig["ethereum"]:

         self._userConfig["ethereum"]["testRoot"] = \
            os.path.expanduser(self._userConfig["ethereum"]["testRoot"])

   def getName(self):
      return "CoreVMTests"

   def run(self):
      ''' Runs all of the tests. '''
      try:
         log.info("Launching product...")
         self.launchProduct(self._args,
                            self._userConfig)

      except Exception as e:
         log.error(traceback.format_exc())
         return self._resultFile

      tests = self._getTests()

      log.info("Launching tests...")
      time.sleep(5)
      for test in tests:
         self.setEthrpcNode()

         if not self._isSourceCodeTestFile(test):
            testCompiled = self._loadCompiledTest(test)

            if not testCompiled:
               self.writeResult(test, None, "Unable to load compiled test.")
               continue

            testSource = self._loadTestSource(testCompiled)

            if not testSource:
               self.writeResult(test, None, "Unable to load test source.")
               continue

            testName = os.path.basename(os.path.dirname(test)) + "_" + \
                       os.path.splitext(os.path.basename(test))[0]
            testLogDir = os.path.join(self._testLogDir, testName)

            try:
               result, info = self._runRpcTest(test,
                                               testSource,
                                               testCompiled,
                                               testLogDir)
            except Exception as e:
               result = False
               info = str(e) + "\n" + traceback.format_exc()

            if info:
               info += "  "
            else:
               info = ""

            relativeLogDir = self.makeRelativeTestPath(testLogDir)
            info += "Log: <a href=\"{}\">{}</a>".format(relativeLogDir,
                                                        testLogDir)
            self.writeResult(testName, result, info)
      log.info("Tests are done.")

      if self._shouldStop():
            self.product.stopProduct()

      return self._resultFile

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

   def _runRpcTest(self, testPath, testSource, testCompiled, testLogDir):
      ''' Runs one test. '''
      success = None
      info = None
      testName = list(testSource.keys())[0]
      testExecutionCode = testCompiled[testName]["exec"]["code"]
      expectTxSuccess = self._getExpectTxSuccess(testCompiled)
      if testCompiled[testName]["exec"]["origin"]:
         user = {"hash": testCompiled[testName]["exec"]["origin"]}
      else:
         user = self._getAUser()
      expectedStorage = self._getExpectedStorageResults(testSource, testCompiled)
      expectedOut = self._getExpectedOutResults(testCompiled)
      expectedLogs = self._getExpectedLogsResults(testCompiled)
      expectedAddress = testCompiled[testName]["exec"]["address"]
      rpc = RPC(testLogDir,
                testName,
                self.ethrpcApiUrl,
                self._userConfig)
      gas = self._getGas()
      testData = testCompiled[testName]["exec"]["data"]
      self._unlockUser(rpc, user)
      log.info("Starting test '{}'".format(testPath))

      if expectTxSuccess:
         testExecutionCode = self._addCodePrefix(testExecutionCode)
         txReceipt = self._createContract(user, rpc, testExecutionCode, gas)

         if txReceipt:
            contractAddress = RPC.searchResponse(txReceipt, ["contractAddress"])
            success, info = self._checkResult(user,
                                              rpc,
                                              contractAddress,
                                              testData,
                                              txReceipt,
                                              expectedStorage,
                                              expectedOut,
                                              expectedLogs,
                                              expectedAddress)
         else:
            info = "Did not receive a transaction receipt."
      else:
         # If the bytecode is meant to fail, create the contract without
         # prefix code so it will run immediately, and make sure it fails.
         # Other types of tests involve calling the contract from another
         # contract, and in that situation we can't get the status of the thing
         # that got called; we just get the status of the ability to make a call.
         txReceipt = self._createContract(user, rpc, testExecutionCode, gas)
         success, info = self._checkTxStatus(rpc, txReceipt, expectTxSuccess)

      return (success, info)

   def _getExpectedOutResults(self, testCompiled):
      '''
      Test cases have an "out" field defined in the compiled JSON file:
      {
        "indirect_jump3" : {
          ...,
          "out" : "0x0000...00000000000000000000000000000000000000000000000001",
          ...
        },
      This is the value returned by the code in the test case.
      Returns this value.  Returns None if not provided.
      '''
      ret = None
      testName = list(testCompiled.keys())[0]

      if "out" in list(testCompiled[testName]):
         ret = testCompiled[testName]["out"]
         if ret == "0x":
            ret = "0x0"

      return ret

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

   def _getExpectedLogsResults(self, testCompiled):
      '''
      Test cases have an "logs" field defined in the compiled JSON file:
      {
        "log0_emptyMem" : {
          ...,
          "logs" : "0xea63b4dbbdbca1bd985580a0c3b6f35a4955d4d4cf0b4d903003cdfc4c40ba1c",
          ...
        },
      This is the Keccak hash of the RLP encoded logs attached to the
      transaction sent by this test case.
      Returns this value.  Returns None if not provided.
      '''
      ret = None
      testName = list(testCompiled.keys())[0]

      if "logs" in list(testCompiled[testName]):
         ret = testCompiled[testName]["logs"]

      return ret

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

   def _checkResult(self,
                    user,
                    rpc,
                    contractAddress,
                    testData,
                    txReceipt,
                    expectedStorage,
                    expectedOut,
                    expectedLogs,
                    expectedAddress):
      '''
      Loops through the expected storage structure in the test case, comparing
      those values to the actual stored values in the block.
      Returns True if all match, False if any fail to match, and None if there
      are no values.  (It's possible that tests with no values are supposed to
      contains 0 at 0x0; need to investigate.)
      '''
      success = None
      info = None
      callerReceipt = self._callContractFromContract(user,
                                                     rpc,
                                                     contractAddress,
                                                     testData,
                                                     expectedOut)

      # This Tx status indicates whether we could make the contract-to-contract
      # call.  It does not indicate whether the callee experienced an error.
      success, info = self._checkTxStatus(rpc, callerReceipt, True)
      testVerified = False

      if success and expectedOut:
         callerAddress = RPC.searchResponse(callerReceipt,
                                            ["contractAddress"])
         success, info = self._checkExpectedOut(rpc,
                                                callerAddress,
                                                expectedOut)
         testVerified = True

      if success and expectedStorage:
         # Note: since DELEGATECALL is used to make the call, the
         # storage used is the caller contract, not the callee
         # contract, so callerAddress is passed instead of
         # contractAddress here.
         success, info = self._checkExpectedStorage(rpc,
                                                    callerAddress,
                                                    expectedStorage)
         testVerified = True

      if success and expectedLogs:
         logs = RPC.searchResponse(callerReceipt,
                                   ["logs"])
         success, info = self._checkExpectedLogs(rpc,
                                                 logs,
                                                 expectedLogs,
                                                 expectedAddress,
                                                 contractAddress)
         testVerified = True

      if not testVerified:
         # The test may be checking gas, logs, or callcreates, which
         # we're not checking (yet). This test should be added to the
         # skip list so it is not run in the future.
         success = None
         info = "No expected storage or logs found, and the test was " \
                "expected to be successful. Test needs verification by some " \
                "other method. Test will be marked skipped."

      return success, info

   def _callContractFromContract(self, user, rpc, contractAddress, testData,
                                 fullExpectedOut):
      '''
      Creates a new contract from which the contract under test is called.
      This contract is executed immediately (has no prefix code).
      '''
      if fullExpectedOut:
         trimmedExpectedOut = trimHexIndicator(fullExpectedOut)
         numBytesOut = int(len(trimHexIndicator(trimmedExpectedOut))/2)
      else:
         numBytesOut = 0

      # Note: DELEGATECALL is used intead of CALL, because the value
      # returned by the CALLER opcode must be the address used to send
      # the transaction. If CALL is used, CALLER returns the address
      # of the intermediate contract we create by sending this
      # transaction.
      invokeCallBytecode = self._createDELEGATECALLBytecode(contractAddress,
                                                            numBytesOut,
                                                            testData)
      log.debug("DELEGATECALL bytecode: {}".format(invokeCallBytecode))
      log.debug("Creating the contract which will invoke the test contract.")
      txHash = rpc.sendTransaction(user["hash"],
                                   invokeCallBytecode,
                                   self._getGas())
      if txHash:
         txReceipt = rpc.getTransactionReceipt(txHash, self._ethereumMode)
         return txReceipt
      else:
         Log.error("No transaction hash returned when creating the contract " \
                   "to call another contract.")

   def _checkExpectedOut(self, rpc, callerAddress, fullExpectedOut):
      '''
      Checks the expected out, which is the return value of the code under test
      and has already been stored in the caller's storage.
      Returns whether it was successful, and an informational message if not.
      '''
      log.debug("Checking expected out.")
      fullExpectedOut = trimHexIndicator(fullExpectedOut)
      lengthRetBytes = max(1, int(len(trimHexIndicator(fullExpectedOut))/2))
      success = False
      info = None
      storageSlots = self._countStorageSlotsForBytes(lengthRetBytes)
      expectedOutRemaining = fullExpectedOut

      storageStart = self._delegateStorageKey()
      storageEnd = storageStart + storageSlots

      for storageSlot in range(storageStart, storageEnd):
         actual = rpc.getStorageAt(callerAddress, hex(storageSlot))
         actual = trimHexIndicator(actual)
         expected = None

         if len(expectedOutRemaining) >= 64:
            expected = expectedOutRemaining[:64]
            expectedOutRemaining = expectedOutRemaining[64:]
         else:
            expected = expectedOutRemaining

         if not len(actual) == len(expected):
            actual, info = self._trimActualForExpected(actual, expected)

         if actual:
            success, info = self._compareActAndExpValues(actual, expected)

         if not success:
            break

      return success, info

   def _trimActualForExpected(self, actual, expected):
      '''
      Sometimes, actual is something like 0x3700000000000... and expected
      is just 0x37. Trim, display what we did, and return the trimmed
      actual.  Return False (test failure) and an info message if something
      goes wrong.
      '''
      removed = actual[len(expected):]
      newActual = actual[:len(expected)]
      log.debug("Actual received value '{}' was truncated to " \
                "'{}' for comparison to the expected value " \
                "'{}'.". \
                format(actual, newActual, expected))

      if stringOnlyContains(removed, "0"):
         return newActual, ""
      else:
         info = "The actual output, '{}', could not safely " \
                "be truncated to compare to the expected output, " \
                "'{}'. This is considered a mismatch and " \
                "test failure.".format(actual, expected)
         log.info(info)
         return False, info

   def _checkExpectedStorage(self, rpc, contractAddress, expectedStorage):
      '''
      Checks the expected storage.
      Returns whether it was successful, and an informational message if not.
      '''
      log.debug("Checking expected storage.")
      success = False
      info = None
      keys = sorted(expectedStorage)

      for storageLoc in keys:
         rawVal = rpc.getStorageAt(contractAddress, storageLoc)
         expectedVal = expectedStorage[storageLoc]
         success, info = self._compareActAndExpValues(rawVal, expectedVal)

         if not success:
            log.debug("Expected storage:")
            for storageLoc in keys:
               log.debug("   {}: {}".format(storageLoc,
                                            expectedStorage[storageLoc]))
            break

      return success, info

   def _compareActAndExpValues(self, actualRawValue, expectedRawValue):
      '''
      Given values in raw format, convert to integers and compare. We convert
      because the actual value may be "0x000000000000000000000000000...2" while
      the expected value is "2".
      Returns whether it was successful, and an informational message if not.
      '''
      success = False
      info = None

      log.debug("actualRawValue: '{}'".format(actualRawValue))
      log.debug("expectedRawValue: '{}'".format(expectedRawValue))

      actualValue = int(actualRawValue, 16)
      log.debug("actualValue: '{}'".format(actualValue))

      # Some test cases use "0x" for 0.  Python does not approve.
      if expectedRawValue == "0x":
         expectedRawValue = "0x0"

      expectedValue = int(expectedRawValue, 16)
      log.debug("expectedValue: '{}'".format(expectedValue))

      if expectedValue == actualValue:
         log.debug("Match")
         success = True
      else:
         success = False
         info = "Expected value does not match actual value."
         log.info(info)

      return success, info

   def _checkExpectedLogs(self, rpc, logs, expectedLogs, expectedAddress, actualAddress):
      '''
      Checks the expected logs, which is a keccak hash of the RLP encoding of
      the returned logs.
      Returns whether it was successful, and an informational message if not.
      '''
      log.debug("Checking expected logs.")

      rlpLogs = self._rlpEncodeLogs(logs, expectedAddress, actualAddress)
      hashLogs = rpc.sha3(rlpLogs)
      return self._compareActAndExpValues(hashLogs, expectedLogs)

   def _rlpEncodeLogs(self, logs, expectedAddress, actualAddress):
      '''
      Encodes the RLP for a list of logs:
        [
         [address, [topic, ...], data],
         ...
        ]
      '''
      rlp = ""

      for log in logs:
         rlp += self._rlpEncodeLog(log, expectedAddress, actualAddress)

      rlp = self._rlpListPrefix(len(rlp)) + rlp

      return rlp

   def _rlpEncodeLog(self, log, expectedAddress, actualAddress):
      '''
      Enocdes the RLP for one log:
        [address, [topic, ...], data]
      '''
      rlp = ""

      if "address" in log:
         trimmedAddress = log["address"][2:]
         # The ethereum tests expect the contract to be at a
         # particular address. We can't guarantee this until we are
         # running all of their tests. So, for now, as long as the log
         # came from our contract under test, pretend that it came
         # from the expected contract address, for the purposes of
         # hashing.
         if log["address"] == actualAddress:
            trimmedAddress = expectedAddress[2:]
         rlp += self._rlpStringPrefix(len(trimmedAddress)) + trimmedAddress
      else:
         rlp += "80"

      if "topics" in log:
         rlp += self._rlpEncodeTopics(log["topics"])

      if "data" in log:
         trimmedData = log["data"][2:]
         rlp += self._rlpStringPrefix(len(trimmedData)) + trimmedData

      rlp = self._rlpListPrefix(len(rlp)) + rlp

      return rlp

   def _rlpEncodeTopics(self, topics):
      '''
      Encodes the RLP for a log's topics:
        [topic, ...]
      '''
      rlp = ""

      for topic in topics:
         trimmedTopic = topic[2:]
         rlp += self._rlpStringPrefix(len(trimmedTopic)) + trimmedTopic

      rlp = self._rlpListPrefix(len(rlp)) + rlp

      return rlp

   def _rlpStringPrefix(self, length):
      return self._rlpLengthPrefix(length, 0x80, 0xb7)

   def _rlpListPrefix(self, length):
      return self._rlpLengthPrefix(length, 0xc0, 0xf7)

   def _rlpLengthPrefix(self, stringLength, shortPrefix, longPrefix):
      '''
      Computes the correct prefix for an RLP string of a given length.
      Length is in hex characters.
      The [2:] strips the "0x" from the front of hex's return.
      '''
      byteLength = stringLength // 2
      if byteLength <= 55:
         rlp = hex(shortPrefix + byteLength)[2:]
      else:
         rlp = ""
         while byteLength > 0:
            rlp += hex(byteLength & 0xff)[2:]
            byteLength >>= 8
         rlp = hex(longPrefix + (len(rlp) // 2))[2:] + rlp

      return rlp

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
