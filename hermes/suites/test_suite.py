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

import util.helper
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

CONTRACTS_DIR = "resources/contracts"
TEST_LOG_DIR = "test_logs"

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
      self._userConfig = util.helper.loadConfigFile(self._args)
      self._ethereumMode = self._args.ethereumMode
      self._productMode = not self._ethereumMode
      self._noLaunch = self._args.noLaunch
      self.ethrpcNodes = None
      self.reverseProxyApiBaseUrl = passedArgs.reverseProxyApiBaseUrl
      self.inDockerReverseProxyApiBaseUrl = passedArgs.inDockerReverseProxyApiBaseUrl
      self.contractCompilerApiBaseUrl = passedArgs.contractCompilerApiBaseUrl
      self.product = Product(self._args, self._userConfig)

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

      if result == "FAIL":
         self.product.testFailed = True

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
      if self._args.blockchainLocation != util.helper.LOCATION_LOCAL:
         foundPersephoneDockerConfig = False

         for key in dockerCfg["services"].keys():
            if "persephone-provisioning" in key:
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
                  'verify': False,
                  'timeout': 60
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
         self.ethrpcApiUrl = util.blockchain.eth.getEthrpcApiUrl(request, blockchainId)

   def _getAUser(self):
      '''
      Gets a user hash from the user config file, based on whether we're using
      the product or Ethereum.
      '''
      if self._ethereumMode:
         return self._userConfig["ethereum"]["users"][0]
      else:
         return self._userConfig["product"]["users"][0]

   def _unlockUser(self, rpc, user):
      '''
      Don't know if we'll have a personal_* interface in the product, but
      we can at least handle Ethereum for now.
      '''
      if self._ethereumMode and not self._userUnlocked:
         log.debug("Unlocking account '{}'".format(user["hash"]))
         rpc.unlockAccount(user["hash"], user["password"])
         self._userUnlocked = True

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
