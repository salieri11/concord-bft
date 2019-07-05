###############################################################################
# Copyright 2018-2019 VMware, Inc.  All rights reserved. -- VMware Confidential
###############################################################################
from abc import ABC
from abc import abstractmethod
import collections

import json
import logging
import os
import random
import web3
from web3 import Web3, HTTPProvider
from requests.auth import HTTPBasicAuth

import util.json_helper
import util.blockchain.eth
import rest.request
from util.bytecode import getPushInstruction, addBytePadding
from util.numbers_strings import trimHexIndicator, decToEvenHexNo0x
from util.product import Product
from util.auth import getAccessToken
# Suppress security warnings
import urllib3
urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

log = logging.getLogger(__name__)

# The config file contains information aobut how to run things, as opposed to
# command line parameters, which are about running tests.
CONFIG_JSON = "resources/user_config.json"
CONTRACTS_DIR = "resources/contracts"
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
      self.ethrpcNodes = None
      self.reverseProxyApiBaseUrl = passedArgs.reverseProxyApiBaseUrl
      self.inDockerReverseProxyApiBaseUrl = passedArgs.inDockerReverseProxyApiBaseUrl
      self.contractCompilerApiBaseUrl = passedArgs.contractCompilerApiBaseUrl

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

      if self._args.repeatSuiteRun > 1:
         self._repeatSuiteRun = True
      else:
         self._repeatSuiteRun = False
      self._hermes_home = self._args.hermes_dir


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

      if result == True:
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

   def launchProduct(self, cmdlineArgs, userConfig, force=False):
      '''
      Creates the test suite's Product object and, if appropriate,
      launches the product.  Passing in force=True means the
      product will always be launched.
      '''
      self.product = Product(cmdlineArgs, userConfig, self)

      if force or (self._productMode and not self._noLaunch):
         try:
            self.product.launchProduct()
         except Exception as e:
            log.error(str(e))
            self.writeResult("All Tests", False, "The product did not start.")
            raise(e)


   def validateLaunchConfig(self, dockerCfg):
      '''
      When the product is being launched, a Product can pass a docker config into
      a test suite to identify configuration issues before continuing with the launch.
      - If we're running tests which deploy a new blockchain (e.g. Helen invoking
        Persephone), be sure we have a docker config that includes Persephone.
      - Be sure replaceable values in the Persephone config.json file have been replaced
        with real values.
      '''
      if self._args.deployNewBlockchain:
         foundPersephoneDockerConfig = False

         for key in dockerCfg["services"].keys():
            if "deploymentservice" in key:
               foundPersephoneDockerConfig = True
               provisioningConfig = None
               provisioningConfigFile = "resources/persephone/provisioning/config.json"
               invalidValues = ["<VMC_API_TOKEN>", "<DOCKERHUB_REPO_READER_PASSWORD>"]

               with open(provisioningConfigFile, "r") as f:
                  provisioningConfig = f.read()

               for invalid in invalidValues:
                  if invalid in provisioningConfig:
                     msg = "Invalid Persephone provisioning value(s) in {}.".format(provisioningConfigFile)
                     msg += "  Check instance(s) of: {}".format(invalidValues)
                     raise Exception(msg)

         if not foundPersephoneDockerConfig:
            raise Exception("Deploying a new blockchain was requested, but there is no "
                            "docker config for Persephone. It is likely you should add "
                            "this to your command: '"
                            "--dockerComposeFile ../docker/docker-compose.yml ../docker/docker-compose-persephone.yml'")


   def launchPersephone(self, cmdlineArgs, userConfig, force=False):
      '''
      Creates the test suite's Product object and, if appropriate,
      launches the product.  Passing in force=True means the
      product will always be launched.
      '''
      self.product = Product(cmdlineArgs, userConfig)

      if force or (self._productMode and not self._noLaunch):
         try:
            self.product.launchPersephone()
         except Exception as e:
            log.error(str(e))
            self.writeResult("All Tests", False, "The product did not start.")
            raise(e)

   def getWeb3Instance(self):
      '''
      Connect the web3 framework to an ethrpc node
      '''
      accessToken = getAccessToken()
      authHeader = {'Authorization': 'Bearer {0}'.format(accessToken)}
      return Web3(HTTPProvider(
               self.ethrpcApiUrl,
               request_kwargs={
                  'headers': authHeader,
                  'verify': False
               }
            ))

   def loadContract(self, name):
      '''
      Return contract object to deploy and run queries on.

      Note: We assume that there is only one contract per file.

      Technically, the solidity file isn't required but should be there anyways
      for documentation.
      '''
      sol_path = "{}.sol".format(os.path.join(CONTRACTS_DIR, name))
      abi_path = "{}.abi".format(os.path.join(CONTRACTS_DIR, name))
      hex_path = "{}.hex".format(os.path.join(CONTRACTS_DIR, name))

      assert os.path.isfile(sol_path)
      assert os.path.isfile(abi_path)
      assert os.path.isfile(hex_path)

      with open(abi_path, "r") as f:
         abi = f.read()

      with open(hex_path, "r") as f:
         hex_str = f.read()

      return (abi.strip(), hex_str.strip())


   def setEthrpcNode(self, request=None, blockchainId=None):
      '''
      Changes the ethRpcNode that a test suite uses.  Expected usage
      is that this function gets called at the beginning of every
      test case to pick a fresh node.
      '''
      if self._args.ethrpcApiUrl:
         self.ethrpcApiUrl = self._args.ethrpcApiUrl
      else:
         self.ethrpcApiUrl = self._getEthrpcApiUrl(request, blockchainId)


   def _getEthrpcApiUrl(self, request=None, blockchainId=None):
      '''
      Fetches a random ethRpc node.  This uses the Product class
      which gets the nodes by using Helen's getMembers API.
      '''
      if not self.ethrpcNodes:
         if not request:
            request = rest.request.Request(self.product._productLogsDir,
                                           "getMembers",
                                           self._args.reverseProxyApiBaseUrl,
                                           self._userConfig)
         self.ethrpcNodes = util.blockchain.eth.getEthrpcNodes(request, blockchainId)

      if self.ethrpcNodes:
         node = random.choice(self.ethrpcNodes)
         url = util.blockchain.eth.getUrlFromEthrpcNode(node)
         return url
      else:
         raise Exception("Error: No ethrpc nodes were reported by Helen.")


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

   def _createContract(self, user, rpc, testExecutionCode, gas):
      '''
      Create a contract and return a transaction receipt.
      '''
      txReceipt = None
      txHash = rpc.sendTransaction(user["hash"], testExecutionCode, gas)

      if txHash:
         txReceipt = rpc.getTransactionReceipt(txHash, self._ethereumMode)

      return txReceipt

   def _delegateStorageKey(self):
      '''
      Returns the key that the bytecode generated by
      _createDELEGATECALLBytecode will use to store the return value
      of the called contract.
      '''
      return 0x6865726D6573 # "hermes" in ASCII

   def _createDELEGATECALLBytecode(self, contractAddress, numBytesOut, arguments):
      '''
      Returns the bytecode to create a new contract which:
      - Invokes DELEGATECALL on the given contract address.
      - Stores the returned result of DELEGATECALL into storage. That way, the
        test framework can retrieve the returned values.
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

      # Define the rest of the fields for DELEGATECALL.
      gas = trimHexIndicator(self._getGas())

      if not len(gas) % 2 == 0:
         gas = "0" + gas

      gasPushInstruction = getPushInstruction(int(gas, 16))
      invokeCallBytecode = "0x" + memoryBytecode
      invokeCallBytecode += "{}{}{}{}{}{}{}{}73{}{}{}f4". \
                            format(trimHexIndicator(retBytesPushInstruction),
                                   decToEvenHexNo0x(lengthRetBytes),
                                   trimHexIndicator(retOffsetPushInstruction),
                                   decToEvenHexNo0x(retOffset),
                                   trimHexIndicator(argsLenPushInstruction),
                                   decToEvenHexNo0x(argsLength),
                                   trimHexIndicator(argsOffsetPushInstruction),
                                   decToEvenHexNo0x(argsOffset),
                                   trimHexIndicator(contractAddress),
                                   trimHexIndicator(gasPushInstruction),
                                   gas)

      # After the DELEGATECALL has occurred, copy return memory to
      # storage so it can be retrieved by the testing framework for
      # test verification.
      storageSlots = self._countStorageSlotsForBytes(lengthRetBytes)

      storageStart = self._delegateStorageKey()
      for storageSlot in range(0, storageSlots):
         memoryOffsetDec = storageSlot * 32
         memoryOffsetHex = decToEvenHexNo0x(memoryOffsetDec)
         storageSlotHex = storageStart + storageSlot
         storageSlotPushInstruction = getPushInstruction(storageSlotHex)

         # Even if we were given 8 bytes as an expected return value, MLOAD
         # only reads 32 bytes, so the 8 byte value will be stored in 32 bytes
         # of storage.
         # Instructions:
         #   PUSH1  {memory offset}
         #   MLOAD
         #   PUSH1  {storage slot}
         #   SSTORE
         mloadStep = "60{}51{}{}55". \
                     format(memoryOffsetHex,
                            trimHexIndicator(storageSlotPushInstruction),
                            decToEvenHexNo0x(storageSlotHex))
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

   def _shouldStop(self):
      return self._productMode and not self._noLaunch and not self._repeatSuiteRun

   def _isDATA(self, value):
      # Hex-encoded string
      if not isinstance(value, str):
         return False
      # Leading 0x
      if not value.startswith("0x"):
         return False
      # Even number of chars because it represents bytes
      if (len(value) % 2) != 0:
         return False
      return True

   def requireDATAFields(self, ob, fieldList):
      for f in fieldList:
         if not self._isDATA(ob[f]):
            return (False, f)
      return (True, None)

   def _isQUANTITY(self, value):
      # Hex-encoded string
      if not isinstance(value, str):
         return False
      # Leading 0x
      if not value.startswith("0x"):
         return False
      # Valid number (will flag "0x")
      try:
         int(value, 16)
      except ValueError as e:
         return False
      # No leading 0s
      if len(value) > 3 and value[2] == "0":
         return False
      return True

   def requireQUANTITYFields(self, ob, fieldList):
      for f in fieldList:
         if not self._isQUANTITY(ob[f]):
            return (False, f)
      return (True, None)
