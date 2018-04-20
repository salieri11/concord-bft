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
import traceback
import time
import traceback

from . import test_suite
from rpc.rpc_call import RPC
from util.debug import pp as pp
from util.numbers_strings import trimHexIndicator, decToEvenHexNo0x, \
   stringOnlyContains
from util.product import Product
import util.json_helper

log = logging.getLogger(__name__)

INTENTIONALLY_SKIPPED_TESTS = "suites/skipped/core_vm_tests_to_skip.json"
TEST_SOURCE_CODE_SUFFIX = "Filler.json"
HIGH_GAS = "0x47e7c4"

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
      super(CoreVMTests, self).__init__(passedArgs)

      if "ethereum" in self._userConfig and \
         "testRoot" in self._userConfig["ethereum"]:

         self._userConfig["ethereum"]["testRoot"] = \
            os.path.expanduser(self._userConfig["ethereum"]["testRoot"])

      if self._ethereumMode:
         log.debug("Running in ethereum mode")
         self._apiServerUrl = "http://localhost:8545"
      else:
         self._apiServerUrl = "http://localhost:8080/api/athena/eth/"

   def getName(self):
      return "CoreVMTests"

   def run(self):
      ''' Runs all of the tests. '''
      if self._productMode:
         try:
            p = self.launchProduct(self._args.resultsDir,
                                   self._apiServerUrl,
                                   self._userConfig["product"])
         except Exception as e:
            log.error(traceback.format_exc())
            return self._resultFile

      tests = self._getTests()

      for test in tests:
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

      if self._productMode:
         p.stopProduct()

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

   def _getAUser(self):
      '''
      Gets a user hash from the user config file, based on whether we're using
      the product or Ethereum.
      '''
      if self._ethereumMode:
         return self._userConfig["ethereum"]["users"][0]
      else:
         return self._userConfig["product"]["users"][0]

   def _getGas(self):
      '''
      Gas levels provided in test cases may not be enough.  Just set it high.
      Maye this should be a config file setting, since it depends on how the
      user set up their Ethereum instance.
      Note that VMware will not use gas.
      '''
      return HIGH_GAS if self._ethereumMode else "0x00"

   def _runRpcTest(self, testPath, testSource, testCompiled, testLogDir):
      ''' Runs one test. '''
      success = None
      info = None
      testName = list(testSource.keys())[0]
      testExecutionCode = testCompiled[testName]["exec"]["code"]
      expectTxSuccess = self._getExpectTxSuccess(testCompiled)
      user = self._getAUser()
      expectedStorage = self._getExpectedStorageResults(testSource, testCompiled)
      expectedOut = self._getExpectedOutResults(testCompiled)
      rpc = RPC(testLogDir,
                testName,
                self._apiServerUrl)
      gas = self._getGas()

      testData = None
      if "data" in testCompiled[testName]["exec"]:
         testData = testCompiled[testName]["exec"]["data"]

      if testData:
         testExecutionCode = self._addCodePrefix(testExecutionCode)

      self._unlockUser(rpc, user)
      log.info("Starting test '{}'".format(testName))

      log.info("Creating contract for test {}".format(testName))
      txReceipt = self._createContract(user, rpc, testExecutionCode, gas)

      if txReceipt:
         if testData:
            contractAddress = RPC.searchResponse(txReceipt, ["contractAddress"])
            log.info("Invoking contract for test {}".format(testName))
            txReceipt = self._invokeContract(user, rpc, contractAddress,
                                             testData, gas)

         success, info = self._checkResult(rpc,
                                           contractAddress,
                                           txReceipt,
                                           expectedStorage,
                                           expectedOut,
                                           expectTxSuccess)
      else:
         info = "Did not receive a transaction receipt."

      return (success, info)

   def _addCodePrefix(self, code):
      '''
      When creating a contract with some code, that code will run the first time
      only.  If we want to invoke code in a contract after running it, the code
      needs to return the code to run.  For example, say we want to test code
      which adds 5 to storage every time it is invoked:

      PUSH1 00
      SLOAD
      PUSH1 05
      ADD
      PUSH1 00
      SSTORE

      600054600501600055

      As-is, 5 will be put in storage on contract creation, and further
      invocations of the contract will do nothing.  To invoke the code in
      subsequent calls, add code to return that code in front:

      Prefix                   Code we want to test
      600980600c6000396000f300 600054600501600055

      The prefix instructions are:
      60: PUSH1 (Could be PUSH2, PUSH3, etc... depending on the next nunber.
      09: Hex number of bytes of the code we want to test.
      80: DUP1 because we're going to use the above number once for CODECOPY,
          then again for RETURN.
      60: PUSH
      0c: Hex number of bytes offset where the code we want to test starts.
          (In other words, the length of this prefix so that it is skipped.)
      60: PUSH
      00: Destination offset for the upcoming CODECOPY.  It's zero because we're
          going to start writing at the beginning of memory.
      39: CODECOPY (destOffset, offset, length).  These are three of the numbers
          we put on the stack with the above code.
      60: PUSH
      00: Offset for the upcoming RETURN. (We want to return all of the code
          copied to memory by the CODECOPY.)
      f3: RETURN (offset, length).  The length param is the first PUSH.
      00: STOP
      '''
      code = trimHexIndicator(code)

      # Tests may have more than 0xff bytes (the byte1 test), so we may end
      # up with three digits, like 0x9b0. We must have an even number of digits
      # in the bytecode, so pad it if necessary.
      numCodeBytes = int(len(code)/2)
      numCodeBytesString = decToEvenHexNo0x(numCodeBytes)

      # Make sure we use the right PUSHX instruction to add the code
      # length to the stack.
      # 96 = 0x60 = PUSH1 = uint8
      # 97 = 0x61 = PUSH2 = uint16
      # 98 - 0x62 = PUSH3 = uint24
      # ...
      codeLengthPush = 96

      for i in range(1, 32):
         maxBytesSupportedByThisPush = 2 ** (i*8) - 1
         if numCodeBytes <= maxBytesSupportedByThisPush:
            break
         else:
            codeLengthPush += 1

      codeLengthPush = hex(codeLengthPush)
      codeLengthPush = trimHexIndicator(codeLengthPush)

      # Calculate the length of this prefix.  What makes it variable is the
      # length of numCodeBytesString.  The prefix is 11 bytes without it.
      prefixLength = 11 + int(len(numCodeBytesString)/2)
      prefixLength = decToEvenHexNo0x(prefixLength)

      prefix = "0x{}{}8060{}6000396000f300".format(codeLengthPush,
                                                   numCodeBytesString,
                                                   prefixLength)
      return prefix + code

   def _unlockUser(self, rpc, user):
      '''
      Don't know if we'll have a personal_* interface in the product, but
      we can at least handle Ethereum for now.
      '''
      if self._ethereumMode and not self._userUnlocked:
         log.debug("Unlocking account '{}'".format(user["hash"]))
         rpc.unlockAccount(user["hash"], user["password"])
         self._userUnlocked = True

   def _createContract(self, user, rpc, testExecutionCode, gas):
      '''
      Create a contract and return a transaction receipt.
      '''
      txReceipt = None
      txHash = rpc.sendTransaction(user["hash"], testExecutionCode, gas)

      if txHash:
         txReceipt = rpc.getTransactionReceipt(txHash, self._ethereumMode)

      return txReceipt

   def _invokeContract(self, user, rpc, contractAddress, testData, gas):
      '''
      Invoke the contract whose address is in txReceipt, passing in the
      given testData as "data".
      Returns the txReceipt of the call to the contract.
      '''
      txReceipt = None
      txHash = rpc.sendTransaction(user["hash"], testData, gas,
                                   contractAddress)

      if txHash:
         txReceipt = rpc.getTransactionReceipt(txHash, self._ethereumMode)

      return txReceipt

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
                    rpc,
                    contractAddress,
                    txReceipt,
                    expectedStorage,
                    expectedOut,
                    expectTxSuccess):
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
            testVerified = False

            if expectedStorage:
               success, info = self._checkExpectedStorage(rpc,
                                                          contractAddress,
                                                          expectedStorage)
               testVerified = True
            if expectedOut:
               success, info = self._checkExpectedOut(rpc,
                                                      contractAddress,
                                                      expectedOut)
               testVerified = True
            if not testVerified:
               # The test may be checking gas, logs, or callcreates, which
               # we're not checking (yet). This test should be added to the
               # skip list so it is not run in the future.
               success = None
               info = "No expected storage found, and the test was expected " \
                      "to be successful. Test needs verification by some " \
                      "other method. Test will be marked skipped."
      else:
         # None or False.
         success = txStatusCorrect

      return success, info

   def _createCALLBytecode(self, contractAddress, expectedOut):
      '''
      Returns the bytecode to create a new contract which:
      - Invokes CALL on the given contract address.
      - Stores the returned result of CALL into storage. That way, the test
        framework can retrieve the returned values.

      Instructions:
        PUSH1  {output size}
        PUSH1  {output offset}
        PUSH1  {input size}
        PUSH1  {input offset}
        PUSH1  {value}
        PUSH20 {address}
        PUSH3  {gas}
        CALL
      '''
      lengthRetBytesDec = max(1, int(len(trimHexIndicator(expectedOut))/2))

      # These are values pushed onto the stack for the CALL instruction.
      lengthRetBytesHex = decToEvenHexNo0x(lengthRetBytesDec)
      retOffset = "00"
      argsLength = "00"
      argsOffset = "00"
      value = "00"
      address = trimHexIndicator(contractAddress)
      gas = trimHexIndicator(self._getGas())

      if not len(gas) % 2 == 0:
         gas = "0" + gas

      invokeCallBytecode = "0x60{}60{}60{}60{}60{}73{}62{}f1". \
                           format(lengthRetBytesHex,
                                  retOffset,
                                  argsLength,
                                  argsOffset,
                                  value,
                                  address,
                                  gas)

      # MLOAD only gets 32 bytes at a time, and some tests return multiple
      # 32-byte values, so MLOAD and SSTORE 32-bytes at a time.
      # e.g. Given an expected result of 0x<thirty-two 1's><thirty-two 2's>
      # we will end up doing two SSTORE commands:
      #   Storage slot 1: 11111111111111111111111111111111
      #   Storage slot 2: 22222222222222222222222222222222
      # We also may have expected storage of, say, 8 bytes, so be sure we
      # always allocate at least one storage slot.
      storageSlots = self._countStorageSlotsForBytes(lengthRetBytesDec)

      for storageSlot in range(0, storageSlots):
         memoryOffsetDec = storageSlot * 32
         memoryOffsetHex = decToEvenHexNo0x(memoryOffsetDec)
         storageSlotHex = decToEvenHexNo0x(storageSlot)

         # Even if we were given 8 bytes as an expected return value, MLOAD
         # only reads 32 bytes, so the 8 byte value will be stored in 32 bytes
         # of storage.
         # Instructions:
         #   PUSH1  {memory offset}
         #   MLOAD
         #   PUSH1  {storage slot}
         #   SSTORE
         mloadStep = "60{}5160{}55".format(memoryOffsetHex, storageSlotHex)
         invokeCallBytecode += mloadStep

      return invokeCallBytecode

   def _countStorageSlotsForBytes(self, numBytes):
      storageSlots = int(numBytes/32)

      if numBytes % 32 > 0:
         storageSlots += 1

      return storageSlots

   def _checkExpectedOut(self, rpc, contractAddress, fullExpectedOut):
      '''
      Checks the expected out, which is the return value of the code under test.
      We do this by creating a new contract which invokes the contract under
      test and saves the return value in storage, then checking the storage
      storage of the new contract.
      Returns whether it was successful, and an informational message if not.
      '''
      log.debug("Checking expected out.")
      fullExpectedOut = trimHexIndicator(fullExpectedOut)
      lengthRetBytes = max(1, int(len(trimHexIndicator(fullExpectedOut))/2))
      success = False
      info = None
      invokeCallBytecode = self._createCALLBytecode(contractAddress,
                                                    fullExpectedOut)
      txHash = rpc.sendTransaction(self._getAUser()["hash"],
                                   invokeCallBytecode,
                                   self._getGas())
      if txHash:
         txReceipt = rpc.getTransactionReceipt(txHash, self._ethereumMode)
         contractAddress = RPC.searchResponse(txReceipt, ["contractAddress"])

         if contractAddress:
            storageSlots = self._countStorageSlotsForBytes(lengthRetBytes)
            expectedOutRemaining = fullExpectedOut

            for storageSlot in range(0, storageSlots):
               actual = rpc.getStorageAt(contractAddress, hex(storageSlot))
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
         else:
            info = "No contract address was in the transaction receipt when " \
                   "creating a contract to check the expected output."
      else:
         info = "No transaction hash received when creating a contract to " \
                "check the expected output"

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
      log.info("Actual received value '{}' was truncated to " \
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
