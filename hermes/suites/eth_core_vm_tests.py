#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import json
import logging
import pytest
import os
import time
import re
from urllib.parse import urlparse

# These are fixtures used by tests directly.
from fixtures.common_fixtures import fxBlockchain, fxConnection, fxHermesRunSettings

# These are fixtures that the fixtures above depend on.  The way pytest works,
# these must be imported into a test file even though they are in the same
# source code file.
from fixtures.common_fixtures import fxProduct

from rpc.rpc_call import RPC
from util.numbers_strings import trimHexIndicator, stringOnlyContains
import util
import util.blockchain.eth
import util.json_helper
from suites.cases import describe

import util.hermes_logging
log = util.hermes_logging.getMainLogger()
TEST_SOURCE_CODE_SUFFIX = "Filler.json"

def dynamicReportOverride_for_test_eth_core_vm_tests(fxEthCoreVmTests, fxHermesRunSettings, fxConnection, fxBlockchain):
  '''
    Since eth_core_vm_tests is generative and reused, describe decorator
    for this function will output 500+ cases to have the SAME case name.
    This report handler will inherit the test function arguments exactly,
    and will override the report parameters extracted from those argument
    uniquely for every invocation of the test function.
  '''
  if not isSourceCodeTestFile(fxEthCoreVmTests):
    testCompiled = loadCompiledTest(fxEthCoreVmTests)
    if not testCompiled: return
    testSource = loadTestSource(testCompiled, fxHermesRunSettings["hermesUserConfig"]["ethereum"]["testRoot"])
    if not testSource: return
    testFileName = os.path.basename(os.path.dirname(fxEthCoreVmTests)) # e.g. vmArithmeticTest
    testName = os.path.splitext(os.path.basename(fxEthCoreVmTests))[0] # e.g. addmodDivByZero
    caseFullName = testFileName[2:] + "_" + testName
    # [camelCase => spaced] description based on testName
    description = re.sub(r'((?<=[a-z])[A-Z]|(?<!\A)[A-Z](?=[a-z]))', r' \1', testName)
    return {
      # EthCoreVMTests have over 500+ cases; hard to scroll down.
      # prepend 'Z_' for Racetrack to show this suite last on the table
      "suiteName": "Z_EthCoreVmTests",
      "caseName": caseFullName,
      "description": description
    }


@describe(dynamicReportOverride=dynamicReportOverride_for_test_eth_core_vm_tests)
def test_eth_core_vm_tests(fxEthCoreVmTests, fxHermesRunSettings, fxConnection, fxBlockchain):
   '''
   The fxEthCoreVmTests fixture is set up dynamically with code in hermes/conftest.py.
   This function is invoked once per ethereum test file.  The path to the file is
   in the fxEthCoreVmTests variable for each iteration.
   '''
   ethrpcUrl = fxHermesRunSettings["hermesCmdlineArgs"].ethrpcApiUrl
   if not ethrpcUrl:
      blockchainLocation = fxHermesRunSettings["hermesCmdlineArgs"].blockchainLocation
      useITApprovedPort = False
      scheme = "https"

      # Hack for VMware firewall.
      if util.helper.blockchainIsRemote(fxHermesRunSettings["hermesCmdlineArgs"]) or \
         util.helper.helenIsRemote(fxHermesRunSettings["hermesCmdlineArgs"]):
         useITApprovedPort = True
         scheme = "http"

      ethrpcUrl = util.blockchain.eth.getEthrpcApiUrl(fxConnection.request,
                                                      fxBlockchain.blockchainId,
                                                      useITApprovedPort,
                                                      scheme=scheme)
   if not isSourceCodeTestFile(fxEthCoreVmTests):
      testCompiled = loadCompiledTest(fxEthCoreVmTests)
      assert testCompiled, "Unable to load compiled test."

      testSource = loadTestSource(testCompiled, fxHermesRunSettings["hermesUserConfig"]["ethereum"]["testRoot"])
      assert testSource, "Unable to load test source."

      testName = os.path.basename(os.path.dirname(fxEthCoreVmTests)) + "_" + \
                 os.path.splitext(os.path.basename(fxEthCoreVmTests))[0]
      testLogDir = os.path.join(fxHermesRunSettings["hermesCmdlineArgs"].resultsDir, "test_logs", testName)
      result, info = runRpcTest(fxEthCoreVmTests,
                                testSource,
                                testCompiled,
                                testLogDir,
                                ethrpcUrl,
                                fxHermesRunSettings["hermesUserConfig"],
                                fxConnection)
      if not result:
         supportBundleFile = fxHermesRunSettings["supportBundleFile"]
         urlObj = urlparse(ethrpcUrl)
         bundles = {}

         if os.path.isfile(supportBundleFile):
            with open(supportBundleFile, "r") as f:
               bundles = json.load(f)

         log.info("Existing bundles: {}".format(bundles))

         host = urlObj.hostname
         if not host in bundles:
            # This structure defined in PytestSuite's collectSupportBundles().
            bundles[host] = {
               "type": "ethereum"
            }

         log.debug("Writing {}".format(bundles))
         with open(supportBundleFile, "w") as f:
            f.write(json.dumps(bundles))

      assert result, info

def loadCompiledTest(test):
   '''
   Reads the compiled test from a file and returns it.
   '''
   testCompiled = None
   testCompiled = util.json_helper.readJsonFile(test)
   return testCompiled

def loadTestSource(testCompiled, ethTestsSrc):
      '''
      Reads the test source code from a file and returns it.
      '''
      testName = list(testCompiled.keys())[0]
      ethTestsSrc = os.path.join(ethTestsSrc, testCompiled[testName] \
                                 ["_info"]["source"])
      testSource = util.json_helper.readJsonFile(ethTestsSrc)
      return testSource

def isSourceCodeTestFile(name):
   '''
   Determines if a test file name is source code or a real test.
   '''
   return TEST_SOURCE_CODE_SUFFIX in name

def runRpcTest(testPath, testSource, testCompiled, testLogDir,
               ethrpcUrl, userConfig, fxConnection):
   ''' Runs one test. '''
   success = None
   info = None
   testName = list(testSource.keys())[0]
   testExecutionCode = testCompiled[testName]["exec"]["code"]
   expectTxSuccess = getExpectTxSuccess(testCompiled)
   if testCompiled[testName]["exec"]["origin"]:
      user = {"hash": testCompiled[testName]["exec"]["origin"]}
   else:
      user = userConfig["product"]["users"][0]

   expectedStorage = getExpectedStorageResults(testSource, testCompiled)
   expectedOut = getExpectedOutResults(testCompiled)
   expectedLogs = getExpectedLogsResults(testCompiled)
   expectedAddress = testCompiled[testName]["exec"]["address"]
   rpc = RPC(testLogDir,
             testName,
             ethrpcUrl,
             userConfig,
             tokenDescriptor=fxConnection.rpc.tokenDescriptor)
   gas = util.blockchain.eth.highGas
   testData = testCompiled[testName]["exec"]["data"]
   log.info("Starting test '{}'".format(testPath))


   if expectTxSuccess:
      testExecutionCode = util.blockchain.eth.addCodePrefix(testExecutionCode)
      txReceipt = util.blockchain.eth.createContract(user, rpc, testExecutionCode, gas)

      if txReceipt:
         contractAddress = RPC.searchResponse(txReceipt, ["contractAddress"])
         success, info = checkResult(user,
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
      txReceipt = util.blockchain.eth.createContract(user, rpc, testExecutionCode, gas)
      success, info = checkTxStatus(rpc, txReceipt, expectTxSuccess)

   return (success, info)

def getExpectedOutResults(testCompiled):
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

def getExpectedStorageResults(testSource, testCompiled):
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
   storage = getStorageSection(testSource)
   if storage == None:
      storage = getStorageSection(testCompiled)

   if storage != None and not containsStorageData(storage):
      # Haven't seen a location > 3 yet, but check 10 just in case.
      for i in range(10):
         storage[hex(i)] = "0x0"

   return storage

def getExpectedLogsResults(testCompiled):
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

def getExpectTxSuccess(testCompiled):
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

def containsStorageData(storage):
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

def getStorageSection(testData):
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

def checkResult(user,
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
   callerReceipt = callContractFromContract(user,
                                             rpc,
                                             contractAddress,
                                             testData,
                                             expectedOut)

   # This Tx status indicates whether we could make the contract-to-contract
   # call.  It does not indicate whether the callee experienced an error.
   success, info = checkTxStatus(rpc, callerReceipt, True)
   testVerified = False

   if success and expectedOut:
      callerAddress = RPC.searchResponse(callerReceipt,
                                         ["contractAddress"])
      success, info = checkExpectedOut(rpc,
                                        callerAddress,
                                        expectedOut)
      testVerified = True

   if success and expectedStorage:
      # Note: since DELEGATECALL is used to make the call, the
      # storage used is the caller contract, not the callee
      # contract, so callerAddress is passed instead of
      # contractAddress here.
      success, info = checkExpectedStorage(rpc,
                                            callerAddress,
                                            expectedStorage)
      testVerified = True

   if success and expectedLogs:
      logs = RPC.searchResponse(callerReceipt,
                                ["logs"])
      success, info = checkExpectedLogs(rpc,
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

def callContractFromContract(user, rpc, contractAddress, testData,
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
   invokeCallBytecode = util.blockchain.eth.createDELEGATECALLBytecode(contractAddress,
                                                                       numBytesOut,
                                                                       testData)
   log.debug("DELEGATECALL bytecode: {}".format(invokeCallBytecode))
   log.debug("Creating the contract which will invoke the test contract.")
   txHash = rpc.sendTransaction(user["hash"],
                                invokeCallBytecode,
                                util.blockchain.eth.highGas)
   if txHash:
      txReceipt = rpc.getTransactionReceipt(txHash)
      return txReceipt
   else:
      Log.error("No transaction hash returned when creating the contract " \
                "to call another contract.")

def checkExpectedOut(rpc, callerAddress, fullExpectedOut):
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
   storageSlots = util.blockchain.eth.countStorageSlotsForBytes(lengthRetBytes)
   expectedOutRemaining = fullExpectedOut

   storageStart = util.blockchain.eth.delegateStorageKey()
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
         actual, info = trimActualForExpected(actual, expected)

      if actual:
         success, info = compareActAndExpValues(actual, expected)

      if not success:
         break

   return success, info

def trimActualForExpected(actual, expected):
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

def checkExpectedStorage(rpc, contractAddress, expectedStorage):
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
      success, info = compareActAndExpValues(rawVal, expectedVal)

      if not success:
         log.debug("Expected storage:")
         for storageLoc in keys:
            log.debug("   {}: {}".format(storageLoc,
                                         expectedStorage[storageLoc]))
         break

   return success, info

def compareActAndExpValues(actualRawValue, expectedRawValue):
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

def checkExpectedLogs(rpc, logs, expectedLogs, expectedAddress, actualAddress):
   '''
   Checks the expected logs, which is a keccak hash of the RLP encoding of
   the returned logs.
   Returns whether it was successful, and an informational message if not.
   '''
   log.debug("Checking expected logs.")

   rlpLogs = rlpEncodeLogs(logs, expectedAddress, actualAddress)
   hashLogs = rpc.sha3(rlpLogs)
   return compareActAndExpValues(hashLogs, expectedLogs)

def rlpEncodeLogs(logs, expectedAddress, actualAddress):
   '''
   Encodes the RLP for a list of logs:
     [
      [address, [topic, ...], data],
      ...
     ]
   '''
   rlp = ""

   for log in logs:
      rlp += rlpEncodeLog(log, expectedAddress, actualAddress)

   rlp = rlpListPrefix(len(rlp)) + rlp

   return rlp

def rlpEncodeLog(log, expectedAddress, actualAddress):
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
      rlp += rlpStringPrefix(len(trimmedAddress)) + trimmedAddress
   else:
      rlp += "80"

   if "topics" in log:
      rlp += rlpEncodeTopics(log["topics"])

   if "data" in log:
      trimmedData = log["data"][2:]
      rlp += rlpStringPrefix(len(trimmedData)) + trimmedData

   rlp = rlpListPrefix(len(rlp)) + rlp

   return rlp

def rlpEncodeTopics(topics):
   '''
   Encodes the RLP for a log's topics:
     [topic, ...]
   '''
   rlp = ""

   for topic in topics:
      trimmedTopic = topic[2:]
      rlp += rlpStringPrefix(len(trimmedTopic)) + trimmedTopic

   rlp = rlpListPrefix(len(rlp)) + rlp

   return rlp

def rlpStringPrefix(length):
   return rlpLengthPrefix(length, 0x80, 0xb7)

def rlpListPrefix(length):
   return rlpLengthPrefix(length, 0xc0, 0xf7)

def rlpLengthPrefix(stringLength, shortPrefix, longPrefix):
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

def checkTxStatus(rpc, txReceipt, expectTxSuccess):
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
