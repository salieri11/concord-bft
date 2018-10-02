#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
from abc import ABC
from abc import abstractmethod
import collections

import json
import logging
import os
from util.bytecode import getPushInstruction, addBytePadding
import util.json_helper
from util.numbers_strings import trimHexIndicator, decToEvenHexNo0x
from util.product import Product

log = logging.getLogger(__name__)

# The config file contains information aobut how to run things, as opposed to
# command line parameters, which are about running tests.
CONFIG_JSON = "resources/user_config.json"
TEST_LOG_DIR = "test_logs"
HIGH_GAS = "0x47e7c4"

class TestSuite(ABC):
   @abstractmethod
   def getName(self): pass

   @abstractmethod
   def run(self): pass

   def __init__(self, passedArgs):
      self._args = passedArgs
      self._testLogDir = os.path.join(self._args.resultsDir, TEST_LOG_DIR)
      self._resultFile = os.path.join(passedArgs.resultsDir,
                                      self.getName() + ".json")
      self.loadConfigFile()
      self._ethereumMode = self._args.ethereumMode
      self._productMode = not self._ethereumMode
      self._noLaunch = self._args.noLaunch

      if self._ethereumMode:
         log.debug("Running in ethereum mode")
         self._apiServerUrl = "http://localhost:8545"
      else:
         self._apiServerUrl = "http://localhost:8080/api/athena/eth/"

      self._results = {
         self.getName(): {
            "result":"",
            "tests": collections.OrderedDict()
         }
      }
      with open(self._resultFile, "w") as f:
         f.write(json.dumps(self._results))
      self._unintentionallySkippedFile = \
                                         os.path.join(passedArgs.resultsDir,
                                         "unintentionallySkippedTests.json")
      self._unintentionallySkippedTests = {}
      self._userUnlocked = False

   def loadConfigFile(self):
      '''
      Loads the main config file.
      '''
      if self._args.config == None:
         self._userConfig = util.json_helper.readJsonFile(CONFIG_JSON)
      else:
         self._userConfig = util.json_helper.readJsonFile(self._args.config)

      if "ethereum" in self._userConfig and \
         "testRoot" in self._userConfig["ethereum"]:

         self._userConfig["ethereum"]["testRoot"] = \
            os.path.expanduser(self._userConfig["ethereum"]["testRoot"])

   def makeRelativeTestPath(self, fullTestPath):
      '''
      Given the full test path (in the results directory), return the
      relative path.
      '''
      return fullTestPath[len(self._args.resultsDir)+1:len(fullTestPath)]

   def writeResult(self, testName, result, info):
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
      if not result == "PASS":
         log.info("The test case returned the following information: '{}'". \
                  format(info))

      if testName and result == "SKIPPED":
         log.debug("Unintentionally skipped test '{}': '{}'".format(testName,
                                                                    info))
         self.writeUnintentionallySkippedTest(testName, info)

      # Never change the suite's result due to skips or if it has already
      # been set to "FAIL".
      if not result == "SKIPPED" and \
         not self._results[self.getName()]["result"] == "FAIL":
            self._results[self.getName()]["result"] = result

      if testName:
         self._results[self.getName()]["tests"][testName] = {
            "result": result,
            "info": info
         }

      with open(tempFile, "w") as f:
         f.write(json.dumps(self._results, indent=4))

      os.rename(tempFile, realFile)

   def writeUnintentionallySkippedTest(self, testName, info):
      tempFile = self._unintentionallySkippedFile + "_temp"
      realFile = self._unintentionallySkippedFile
      self._unintentionallySkippedTests[testName] = info

      with open(tempFile, "w") as f:
         f.write(json.dumps(self._unintentionallySkippedTests,
                            sort_keys=True, indent=4))

      os.rename(tempFile, realFile)

   def launchProduct(self, cmdlineArgs, url, userConfig):
      try:
         p = Product(cmdlineArgs, url, userConfig)
         p.launchProduct()
         return p
      except Exception as e:
         log.error(str(e))
         log.error("The product did not start")
         self.writeResult("All Tests", None, "The product did not start.")
         raise(e)

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
      # The product seems to be using gas for CALL.
      return HIGH_GAS #if self._ethereumMode else "0x00"

   def _unlockUser(self, rpc, user):
      '''
      Don't know if we'll have a personal_* interface in the product, but
      we can at least handle Ethereum for now.
      '''
      if self._ethereumMode and not self._userUnlocked:
         log.debug("Unlocking account '{}'".format(user["hash"]))
         rpc.unlockAccount(user["hash"], user["password"])
         self._userUnlocked = True

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
      codeLengthPush = getPushInstruction(numCodeBytes)
      codeLengthPush = trimHexIndicator(codeLengthPush)

      # Calculate the length of this prefix.  What makes it variable is the
      # length of numCodeBytesString.  The prefix is 11 bytes without it.
      prefixLength = 11 + int(len(numCodeBytesString)/2)
      prefixLength = decToEvenHexNo0x(prefixLength)

      prefix = "0x{}{}8060{}6000396000f300".format(codeLengthPush,
                                                   numCodeBytesString,
                                                   prefixLength)
      return prefix + code

   def _createContract(self, user, rpc, testExecutionCode, gas):
      '''
      Create a contract and return a transaction receipt.
      '''
      txReceipt = None
      txHash = rpc.sendTransaction(user["hash"], testExecutionCode, gas)

      if txHash:
         txReceipt = rpc.getTransactionReceipt(txHash, self._ethereumMode)

      return txReceipt

   def _createCALLBytecode(self, contractAddress, numBytesOut, arguments):
      '''
      Returns the bytecode to create a new contract which:
      - Invokes CALL on the given contract address.
      - Stores the returned result of CALL into storage. That way, the test
        framework can retrieve the returned values.
      '''
      arguments = trimHexIndicator(arguments)

      # Define memory in which to place the return bytes.
      lengthRetBytes = numBytesOut
      retBytesPushInstruction = getPushInstruction(lengthRetBytes)
      retOffset = 0
      retOffsetPushInstruction = getPushInstruction(retOffset)

      # Start writing args at the next 32-bit boundary after the memory for
      # the return value.
      argsOffset = lengthRetBytes + 32 - lengthRetBytes % 32
      argsOffsetPushInstruction = getPushInstruction(argsOffset)
      memoryBytecode = ""
      argsLength = 0
      byteOffset = argsOffset

      # Write one byte to memory at a time with MSTORE8 (53).  Test cases have
      # varying numbers of bytes to pass in.
      for i in range(0, len(arguments)-1, 2):
         memoryBytecode += "60" + arguments[i:i+2]
         memoryBytecode += trimHexIndicator(getPushInstruction(byteOffset))
         memoryBytecode += decToEvenHexNo0x(byteOffset)
         memoryBytecode += "53"
         byteOffset += 1
         argsLength += 1

      argsLenPushInstruction = trimHexIndicator(getPushInstruction(argsLength))

      # Define the rest of the fields for CALL.
      value = 0
      valuePushInstruction = getPushInstruction(value)
      gas = trimHexIndicator(self._getGas())

      if not len(gas) % 2 == 0:
         gas = "0" + gas

      gasPushInstruction = getPushInstruction(int(gas, 16))
      invokeCallBytecode = "0x" + memoryBytecode
      invokeCallBytecode += "{}{}{}{}{}{}{}{}{}{}73{}{}{}f1". \
                            format(trimHexIndicator(retBytesPushInstruction),
                                   decToEvenHexNo0x(lengthRetBytes),
                                   trimHexIndicator(retOffsetPushInstruction),
                                   decToEvenHexNo0x(retOffset),
                                   trimHexIndicator(argsLenPushInstruction),
                                   decToEvenHexNo0x(argsLength),
                                   trimHexIndicator(argsOffsetPushInstruction),
                                   decToEvenHexNo0x(argsOffset),
                                   trimHexIndicator(valuePushInstruction),
                                   decToEvenHexNo0x(value),
                                   trimHexIndicator(contractAddress),
                                   trimHexIndicator(gasPushInstruction),
                                   gas)

      # After the CALL has occurred, copy return memory to storage so it can
      # be retrieved by the testing framework for test verification.
      storageSlots = self._countStorageSlotsForBytes(lengthRetBytes)

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

   def requireFields(self, ob, fieldList):
      for f in fieldList:
         if not f in ob:
            return (False, f)
      return (True, None)
