#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Tests the special corner case scenarios which where discovered while
# running ethereum transactions on concord
#########################################################################
import argparse
import collections
import json
import logging
import os
import pprint
import tempfile
import time
import traceback
import re
import random

from . import test_suite
from rpc.rpc_call import RPC
from util.debug import pp as pp
from util.numbers_strings import trimHexIndicator, decToEvenHexNo0x
from util.product import Product
import util.json_helper
from rest.request import Request
from datetime import datetime

log = logging.getLogger(__name__)

class RegressionTests(test_suite.TestSuite):
   _args = None
   _apiServerUrl = None
   _userConfig = None
   _ethereumMode = False
   _productMode = True
   _resultFile = None
   _unintentionallySkippedFile = None
   _userUnlocked = False

   def __init__(self, passedArgs):
      super(RegressionTests, self).__init__(passedArgs)

      if self._ethereumMode:
         log.debug("Running in ethereum mode")
         self._apiServerUrl = "http://localhost:8545"
      else:
         self._apiServerUrl = passedArgs.baseUrl + "/api/concord/eth/"

   def getName(self):
      return "RegressionTests"

   def run(self):
      ''' Runs all of the tests. '''
      try:
         self.launchProduct(self._args,
                            self._apiServerUrl,
                            self._userConfig)
      except Exception as e:
         log.error(traceback.format_exc())
         return self._resultFile

      tests = self._getTests()

      for (testName, testFun) in tests:
         self.setEthRpcNode()
         testLogDir = os.path.join(self._testLogDir, testName)

         try:
            result, info = self._runRpcTest(testName,
                                            testFun,
                                            testLogDir)
         except Exception as e:
            result = False
            info = str(e) + "\n" + traceback.format_exc()
            log.error("Exception running RPC test: '{}'".format(info))

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

   def _getTests(self):
      return [("nested_contract_creation", self._test_nested_contract_creation), \
              ("invalid_addresses", self._test_invalid_addresses), \
              ("call_writer", self._test_call_writer)]

   def _runRpcTest(self, testName, testFun, testLogDir):
      ''' Runs one test. '''
      log.info("Starting test '{}'".format(testName))
      rpc = RPC(testLogDir,
                testName,
                self._apiServerUrl,
                self._userConfig)
      return testFun(rpc)

   def _test_nested_contract_creation(self, rpc):
      '''
      Submit a request to create a new contract which itself creates another contract
      '''
      if self._productMode:
         from_addr = "0x61c5e2a298f40dbb2adee3b27c584adad6833bac"
         data = ("60606040525b60405161015b806102a08339018090506040518091039060"
                 "00f0600160006101000a81548173ffffffffffffffffffffffffffffffff"
                 "ffffffff021916908302179055505b610247806100596000396000f30060"
                 "606040526000357c01000000000000000000000000000000000000000000"
                 "00000000000000900480632ef9db1314610044578063e376787614610071"
                 "57610042565b005b61005b6004803590602001803590602001506100ad56"
                 "5b6040518082815260200191505060405180910390f35b61008860048035"
                 "906020018035906020015061008a565b005b806000600050600084815260"
                 "2001908152602001600020600050819055505b5050565b60006000600084"
                 "846040518083815260200182815260200192505050604051809103902091"
                 "50610120600160009054906101000a900473ffffffffffffffffffffffff"
                 "ffffffffffffffff167f6164640000000000000000000000000000000000"
                 "000000000000000000000000846101e3565b905060016000905490610100"
                 "0a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffff"
                 "ffffffffffffffffffffffffffffffff1681868660405180807f61646400"
                 "000000000000000000000000000000000000000000000000000000008152"
                 "602001506020018481526020018381526020018281526020019350505050"
                 "6000604051808303816000866161da5a03f1915050506000600050600082"
                 "81526020019081526020016000206000505492506101db565b5050929150"
                 "50565b60004340848484604051808581526020018473ffffffffffffffff"
                 "ffffffffffffffffffffffff166c01000000000000000000000000028152"
                 "601401838152602001828152602001945050505050604051809103902090"
                 "50610240565b9392505050566060604052610148806100136000396000f3"
                 "0060606040526000357c0100000000000000000000000000000000000000"
                 "00000000000000000090048063471407e614610044578063e37678761461"
                 "007757610042565b005b6100616004803590602001803590602001803590"
                 "602001506100b3565b6040518082815260200191505060405180910390f3"
                 "5b61008e600480359060200180359060200150610090565b005b80600060"
                 "00506000848152602001908152602001600020600050819055505b505056"
                 "5b6000818301905080506100c684826100d5565b8090506100ce565b9392"
                 "505050565b3373ffffffffffffffffffffffffffffffffffffffff168282"
                 "60405180807f7265676973746572496e7400000000000000000000000000"
                 "000000000000000081526020015060200183815260200182815260200192"
                 "5050506000604051808303816000866161da5a03f1915050505b505056")
         gas = 0x47e7c4
         val = "0000000000000000000000000000000000000000000000000000000000000000"
         txHash = rpc.sendTransaction(from_addr, data, gas, value=val)
         if txHash:
            txReceipt = rpc.getTransactionReceipt(txHash)
            if (txReceipt['status'] == '0x1' and txReceipt['contractAddress']):
               return (True, None)
            else:
               return (False, "Transaction was not successful")
         return (False, "Transaction hash not received")


      else:
         #Skip the test if running in Ethereum mode
         return (None, None)

   def _test_invalid_addresses(self, rpc):
      '''
      Submit transactions using bad addresses: too long, too short.
      '''
      if self._productMode:
         valid_from = "0x1111111111111111111111111111111111111111"
         valid_to = "0x2222222222222222222222222222222222222222"

         long_from = valid_from + "33"
         long_to = valid_to + "44"

         short_from = valid_from[:len(valid_from)-2]
         short_to = valid_to[:len(valid_to)-2]

         bad_tests = [(valid_from, long_to),
                      (valid_from, short_to),
                      (long_from, valid_to),
                      (short_from, valid_to)]

         for (f, t) in bad_tests:
            try:
               rpc.sendTransaction(f, data="0x00", to=t, value="0x01")

               # This call should fail. If it gets here, we probably
               # silently discarded address bytes.
               return (False, "Invalid address allowed from=%s, to=%s" % (f, s))
            except:
               # Receiving an error message will arrive here. An error is
               # fine - we just need to make sure that concord is still up
               # afterward.
               pass

         # After all of that invalid stuff, a valid transaction should work
         txHash = rpc.sendTransaction(valid_from, data="0x00", to=valid_to, value="0x01")
         if txHash:
            # we don't actually care if that transaction worked - just
            # that concord was alive to give us the hash for it
            return (True, None)

         return (False, "No transaction hash was returned")

      else:
         #Skip the test if running in Ethereum mode
         return (None, None)

   def _test_call_writer(self, rpc):
      '''
      Submit an eth_call to a contract that modifies storage. concord
      should properly catch the exception, and stay up to handle
      requests afterward. In ATH-53, it was found that concord would
      exit in this case.
      '''
      if self._productMode:
         from_addr = "0x61c5e2a298f40dbb2adee3b27c584adad6833bac"

         # This creates a contract that just writes the length of the
         # input data to storage when called.
         data = "600480600c6000396000f30036600055"
         gas = 0x47e7c4
         txHash = rpc.sendTransaction(from_addr, data, gas)
         if txHash:
            txReceipt = rpc.getTransactionReceipt(txHash)
            if (txReceipt['status'] == '0x1' and txReceipt['contractAddress']):
               try:
                  callData = "0x01"
                  callResult = rpc.callContract(txReceipt['contractAddress'],
                                                callData)
                  # this call should actually fail, because the
                  # contract tried to write data
                  return (False, "Contract call was allowed to write data")
               except:
                  pass

               # Before ATH-53, the above call would crash concord, so
               # we now send a transaction to see if concord is still up

               sendData = "0x0102"
               sendHash = rpc.sendTransaction(from_addr,
                                              sendData,
                                              to=txReceipt['contractAddress'])
               if sendHash:
                  sendReceipt = rpc.getTransactionReceipt(sendHash)
                  if sendReceipt['status'] == '0x1':
                     value = rpc.getStorageAt(txReceipt['contractAddress'], "0x00")
                     # "2" == length of sendData
                     if (value == "0x0000000000000000000000000000000000000000000000000000000000000002"):
                        return (True, None)
                     else:
                        return (False, "Contract did not store correct data")
                  else:
                     return (False, "Contract send transaction failed")
               else:
                  return (False, "Contract send transaction failed")
            else:
               return (False, "Contract creation failed")
         return (False, "Contract transaction hash not received")

      else:
         #Skip the test if running in Ethereum mode
         return (None, None)
