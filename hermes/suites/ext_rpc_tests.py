#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Test the parts of the Ethereum JSON RPC API beyond what the
# CoreVMTests cover. This checks things like web3_sha3,
# eth_clientVersion, eth_mining, ...
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

from . import test_suite
from rpc.rpc_call import RPC
from rest.request import Request
from util.debug import pp as pp
from util.numbers_strings import trimHexIndicator, decToEvenHexNo0x
from util.product import Product
import util.json_helper

log = logging.getLogger(__name__)

class ExtendedRPCTests(test_suite.TestSuite):
   _args = None
   _apiBaseServerUrl = None
   _apiServerUrl = None
   _userConfig = None
   _ethereumMode = False
   _productMode = True
   _resultFile = None
   _unintentionallySkippedFile = None
   _userUnlocked = False

   def __init__(self, passedArgs):
      super(ExtendedRPCTests, self).__init__(passedArgs)

      if self._ethereumMode:
         log.debug("Running in ethereum mode")
         self._apiServerUrl = "http://localhost:8545"
      else:
         self._apiBaseServerUrl = "https://localhost/blockchains/local"
         self._apiServerUrl = self._apiBaseServerUrl+"/api/concord/eth/"

   def getName(self):
      return "ExtendedRPCTests"

   def run(self):
      ''' Runs all of the tests. '''
      if self._productMode and not self._noLaunch:
         try:
            p = self.launchProduct(self._args,
                                   self._apiServerUrl,
                                   self._userConfig)
         except Exception as e:
            log.error(traceback.format_exc())
            return self._resultFile

      tests = self._getTests()

      for (testName, testFun) in tests:
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

      if self._productMode and not self._noLaunch:
         p.stopProduct()

      return self._resultFile

   def _getTests(self):
      return [("web3_sha3", self._test_web3_sha3), \
              ("web3_clientVersion", self._test_web3_clientVersion), \
              ("eth_mining", self._test_eth_mining), \
              ("rpc_modules", self._test_rpc_modules), \
              ("eth_gasPrice", self._test_eth_gasPrice), \
              ("eth_syncing", self._test_eth_syncing), \
              ("eth_getTransactionCount", self._test_eth_getTransactionCount), \
              ("eth_sendRawTransaction", self._test_eth_sendRawTransaction), \
              ("eth_sendRawContract", self._test_eth_sendRawContract), \
              ("eth_getBlockByNumber", self._test_eth_getBlockByNumber),
              ("eth_getBalance", self._test_eth_getBalance),
              ("eth_getStorageAt", self._test_eth_getStorageAt),
              ("eth_getCode", self._test_eth_getCode)]

   def _runRpcTest(self, testName, testFun, testLogDir):
      ''' Runs one test. '''
      log.info("Starting test '{}'".format(testName))
      rpc = RPC(testLogDir,
                testName,
                self._apiServerUrl,
                self._userConfig)
      request = Request(testLogDir,
                        testName,
                        self._apiBaseServerUrl,
                        self._userConfig)
      return testFun(rpc, request)

   def _test_web3_sha3(self, rpc, request):
      '''
      Check that hashing works as expected.
      '''
      # list of (data, expected hash) tuples
      datahashes = [("0x", "0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"), \
                    ("0x1234567890abcdef", "0xed8ab4fde4c4e2749641d9d89de3d920f9845e086abd71e6921319f41f0e784f")]

      for (d, h) in datahashes:
         result = rpc.sha3(d)
         if not result == h:
            return (False,
                    "Hash of '{}' did not match" \
                    "expected '{}': actual: '{}'".format(d, h, result))

      return (True, None)

   def _test_web3_clientVersion(self, rpc, request):
      '''
      Check that we return a valid version
      '''
      result = rpc.clientVersion()
      if not type(result) is str:
         return (False, "Client version should have been a string, " \
                 "but was '{}'".format(result))

      # Insisting version is
      # <name>/v<major>.<minor>.<patch><anything>/
      #    <os>/<language><major>.<minor>.<patch>
      version_re = re.compile("\\w+/v\\d+\\.\\d+\\.\\d+[^/]*/"\
                              "[-a-zA-Z0-9]+/\\w+\\d\\.\\d\\.\\d")
      if not version_re.match(result):
         return (False, "Client version doesn't match expected format: " \
                 "'{}'".format(result))

      return (True, None)

   def _test_eth_mining(self, rpc, request):
      '''
      Check that mining status is reported correctly
      '''
      result = rpc.mining()
      if self._ethereumMode and (not result == True):
         return (False, "Expected ethereumMode to be mining, " \
                 "but found '{}'".format(result))
      elif self._productMode and (not result == False):
         return (False, "Expected product to not be mining, " \
                 "buf found '{}'".format(result))

      return (True, None)

   def _test_rpc_modules(self, rpc, request):
      '''
      Check that available RPC modules are listed correctly
      '''
      result = rpc.modules()

      if not type(result) is collections.OrderedDict:
         return (False, "Reply should have been a dict.")

      # This means the test is invalid, but let's not blow up because of it
      if len(result) == 0:
         log.warn("No RPC modules returned from rpc request.")

      # Insisting the version is <number>.<number>
      version_re = re.compile("\\d+\\.\\d+")

      for k, v in result.items():
         if not k in ["admin", "eth", "miner", "net", "personal", "rpc", "web3"]:
            return (False,
                    "Response included unknown RPC module '{}'".format(k))
         if not version_re.match(v):
            return (False,
                    "Module version should be version like, " \
                    "but was '{}'".format(v))

      return (True, None)

   def _test_eth_gasPrice(self, rpc, request):
      '''
      Check that gas price is reported correctly
      '''
      result = rpc.gasPrice()
      if self._ethereumMode and (not len(result) > 2):
         return (False, "Expected ethereumMode to have 0x... gas price, " \
                 "but found '{}'".format(result))
      elif self._productMode and (not result == "0x0"):
         # "0x0" is the default GasPrice in Helen's application.properties
         return (False, "Expected product to have zero gas price, " \
                 "but found '{}'".format(result))

      return (True, None)

   def _test_eth_syncing(self, rpc, request):
      '''
      Check that syncing state is reported correctly
      '''
      result = rpc.syncing()
      if result:
         return (False, "Expected node to not be syncing, " \
                 "but found '{}'".format(result))

      # TODO: non-false result is also allowed, and indicates that the
      # node knows that it is behind. We don't expect nodes to be
      # running behind in this test right now.

      return (True, None)

   def _test_eth_getTransactionCount(self, rpc, request):
      '''
      Check that transaction count is updated.
      '''
      newAccount = None
      for i in range(0,10):
         try:
            newAccount = rpc.newAccount("gettxcount{}".format(i))
            if newAccount:
               break
         except:
            pass

      if not newAccount:
         return (False, "Unable to create new account")

      previousBlockNumber = rpc.getBlockNumber()

      txResult = rpc.sendTransaction(newAccount,
                                     data = "0x00",
                                     gas = "0x01")

      startNonce = rpc.getTransactionCount(newAccount, previousBlockNumber)
      if not startNonce:
         return (False, "Unable to get starting nonce")

      if not startNonce == "0x0":
         return (False, "Start nonce was not zero (was {})".format(startNonce))

      if not txResult:
         return (False, "Transaction was not accepted")

      endNonce = rpc.getTransactionCount(newAccount)
      if not endNonce:
         return (False, "Unable to get ending nonce")

      if not endNonce == "0x1":
         return (False, "End nonce was not 1 (was {})".format(endNonce))

      return (True, None)

   def _test_eth_sendRawTransaction(self, rpc, request):
      '''
      Check that a raw transaction gets decoded correctly.
      '''

      # known transaction from public ethereum
      # https://etherscan.io/tx/0x6ab11d26df13bc3b2cb1c09c4d274bfce325906c617d2bc744b45fa39b7f8c68
      rawTransaction = "0xf86b19847735940082520894f6c3fff0b77efe806fcc10176b8cbf71c6dfe3be880429d069189e00008025a0141c8487e4db65457266978a7f8d856b777a51dd9863d31637ccdec8dea74397a07fd0e14d0e3e891882f13acbe68740f1c5bd82a1a254f898cdbec5e9cfa8cf38"
      expectedHash = "0x6ab11d26df13bc3b2cb1c09c4d274bfce325906c617d2bc744b45fa39b7f8c68"
      expectedFrom = "0x42c4f19a097955ff2a013ef8f014977f4e8516c3"
      expectedTo = "0xf6c3fff0b77efe806fcc10176b8cbf71c6dfe3be"
      expectedValue = "300000000000000000"

      txResult = rpc.sendRawTransaction(rawTransaction)
      if not txResult:
         return (False, "Transaction was not accepted")

      # if this test is re-run on a cluster, we'll see a different
      # hash (or an error once nonce tracking works); don't consider
      # it an error for now
      if not txResult == expectedHash:
         log.warn("Receipt hash != expected hash. Was this run on an empty cluster?")

      if not self._productMode:
         log.warn("No verification done in ethereum mode")
      else:
         tx = request.getTransaction(txResult)
         if not tx:
            return (False, "No transaction receipt found")

         # This is the important one: it tells whether signature address
         # recovery works.
         if not tx["from"] == expectedFrom:
            return (False, "Found from does not match expected from")

         # The rest of these are just checking parsing.
         if not tx["to"] == expectedTo:
            return (False, "Found to does not match expectd to")
         if not tx["value"] == expectedValue:
            return (False, "Found value does not match expected value")

      return (True, None)

   def _test_eth_sendRawContract(self, rpc, request):
      '''
      Check that a raw transaction can create a contract
      '''

      # TODO: lookup current account nonce, and sign transaction with
      # correct nonce. This currently uses a static nonce, and that
      # means the test has to be run against an empty blockchain (or
      # at least once that hasn't seen nonce 1 for this account
      # before).

      # Simple contract that returns 0x42 when called
      rawTransaction = "0xf860800101800197600b80600c6000396000f300604260005260206000f30025a0b9f688baaf66a51d4965a526803499fb1688d6b6720086270e64fcd67cde2921a02cecc55e0200c831f01b00d46865fe7a89e8891ae5906821eb86609b022f44b2"
      expectedHash = "0xa596153062e4d5bd7d51b797e0390b0caa310fd0af5c6f188981e7d98c4e2b63"
      expectedFrom = "0xaf4be85b32868c5b7c121115ad8cd93e0ad4f14e"
      expectedTo = "0xc2ddc84b30c43c090db2bd3a55a0fb0d8f0af208"
      expectedValue = "1"

      txResult = rpc.sendRawTransaction(rawTransaction)
      if not txResult:
         return (False, "Transaction was not accepted")

      # if this test is re-run on a cluster, we'll see a different
      # hash (or an error once nonce tracking works); don't consider
      # it an error for now
      if not txResult == expectedHash:
         log.warn("Receipt hash != expected hash. Was this run on an empty cluster?")

      if not self._productMode:
         log.warn("No verification done in ethereum mode")
      else:
         tx = request.getTransaction(txResult)
         if not tx:
            return (False, "No transaction receipt found")

         # This is the important one: it tells whether signature address
         # recovery works.
         if not tx["from"] == expectedFrom:
            return (False, "Found from does not match expected from")

         # The rest of these are just checking parsing.
         if not "contract_address" in tx:
            return (False, "No contract_address found. Was this run on an empty cluster?")
         if not tx["contract_address"] == expectedTo:
            return (False, "Found contract_address does not match expected contract_address")
         if not tx["value"] == expectedValue:
            return (False, "Found value does not match expected value")

         callResult = rpc.callContract(tx["contract_address"])

         if not callResult == "0x0000000000000000000000000000000000000000000000000000000000000042":
            return (False, "Contract did not return expected value")

      return (True, None)

   def _test_eth_getBlockByNumber(self, rpc, request):
      '''
      Check that blocks can be fetched by number.
      '''

      currentBlockNumber = rpc.getBlockNumber()

      latestBlock = rpc.getBlockByNumber("latest")

      (present, missing) = self.requireFields(
         latestBlock,
         ["number","hash","parentHash","timestamp"])
      if not present:
         return (False, "No '{}' field in block response.".format(missing))

      if not latestBlock["number"] == currentBlockNumber:
         return (False, "Latest block does not have current block number")

      currentBlock = rpc.getBlockByNumber(currentBlockNumber)

      if not currentBlock["number"] == currentBlockNumber:
         return (False, "Current block does not have current block number")

      futureBlockNumber = 1 + int(currentBlockNumber, 16)

      try:
         futureBlock = rpc.getBlockByNumber(futureBlockNumber)
         return (False,
                 "Expected an error for future block {}, " \
                 "but received block {}".format(futureBlockNumber,
                                                futureBlock["number"]))
      except:
         # requesting an uncommitted block should return an error
         pass

      return (True, None)

   def _test_eth_getStorageAt(self, rpc, request):
      '''
      here we use the Counter contract. We first deploy the Counter contract and
      then call subtract(). All the encoded transaction data
      (contractTransaction and decrementTx) is generated by web3j_3.5.0
      '''
      contractTransaction = "0xf901628085051f4d5c0083419ce08080b9010f608060405234801561001057600080fd5b506104d260005560ea806100256000396000f30060806040526004361060525763ffffffff7c01000000000000000000000000000000000000000000000000000000006000350416634f2be91f811460575780636deebae314606b5780638ada066e14607d575b600080fd5b348015606257600080fd5b50606960a1565b005b348015607657600080fd5b50606960ac565b348015608857600080fd5b50608f60b8565b60408051918252519081900360200190f35b600080546001019055565b60008054600019019055565b600054905600a165627a7a72305820b827241483c0f1a78e00de3ba4a4cb1e67a03bf6fb9f5ecc0491712f7e0aeb8000291ca04a8037443f6f4045acccda71496b9727311bac5ca8c6443c963137f1dadfc38ca056dc2e656776b082962e5452398090a0d0c3a671aca06b61253588334f44bb13"
      decrementTx = "0xf8690185051f4d5c0083419ce094cf98dacbe219c04942a876fff3dc657e731ae9ba80846deebae31ba0de2f19ce91d7abad46a21cb8a017da98f2dd96d32a1b9eb199e0587f89a78dffa024a2b103cd2203b85e1a96b18a153d51d3fd5e1f68b45d3ad31dd22f3de0237d"
      storageLocation = "0x0"
      expectedStartStorage = "0x00000000000000000000000000000000000000000000000000000000000004d2"
      expectedEndStorage = "0x00000000000000000000000000000000000000000000000000000000000004d1"

      txResult = rpc.sendRawTransaction(contractTransaction)
      startBlockNumber = rpc.getBlockNumber()
      if not txResult:
         return (False, "Transaction was not accepted")

      if not self._productMode:
         log.warn("No verification done in ethereum mode")
      else:
         tx = request.getTransaction(txResult)
         if not tx:
            return (False, "No transaction receipt found")

         if not "contract_address" in tx:
            return (False,
                    "No contract_address found. Was this run on an empty " \
                    "cluster?")

      contractAddress = tx["contract_address"]
      txResult = rpc.sendRawTransaction(decrementTx)
      if not txResult:
         return (False, "Transaction was not accepted")

      endBlockNumber = rpc.getBlockNumber()
      startStorage = rpc.getStorageAt(contractAddress, storageLocation,
                                      startBlockNumber)
      if not startStorage == expectedStartStorage:
         return (False, "start storage does not match expected")

      endStorage = rpc.getStorageAt(contractAddress, storageLocation,
                                    endBlockNumber)
      if not endStorage == expectedEndStorage:
         return (False, "end storage does not match expected")

      return (True, None)

   def _test_eth_getCode(self, rpc, request):
      contractTransaction = "0xf901628085051f4d5c0083419ce08080b9010f608060405234801561001057600080fd5b506104d260005560ea806100256000396000f30060806040526004361060525763ffffffff7c01000000000000000000000000000000000000000000000000000000006000350416634f2be91f811460575780636deebae314606b5780638ada066e14607d575b600080fd5b348015606257600080fd5b50606960a1565b005b348015607657600080fd5b50606960ac565b348015608857600080fd5b50608f60b8565b60408051918252519081900360200190f35b600080546001019055565b60008054600019019055565b600054905600a165627a7a72305820b827241483c0f1a78e00de3ba4a4cb1e67a03bf6fb9f5ecc0491712f7e0aeb8000291ca080c9884eefca39aece8d308136f3bc2b95e44bd812afc33a9d741fcacee2f874a0125e2c8cd8af9e32cdbabf525a2e69ba7ebdd16411314a9bfa1e6bf414db6122"
      expectedCode = "0x60806040526004361060525763ffffffff7c01000000000000000000000000000000000000000000000000000000006000350416634f2be91f811460575780636deebae314606b5780638ada066e14607d575b600080fd5b348015606257600080fd5b50606960a1565b005b348015607657600080fd5b50606960ac565b348015608857600080fd5b50608f60b8565b60408051918252519081900360200190f35b600080546001019055565b60008054600019019055565b600054905600a165627a7a72305820b827241483c0f1a78e00de3ba4a4cb1e67a03bf6fb9f5ecc0491712f7e0aeb800029"
      address = "0x7c29bd452fae057dfd4e44b746846e4293db5060"
      startBlockNumber = rpc.getBlockNumber()
      rpc.sendRawTransaction(contractTransaction)
      endingBlockNumber = rpc.getBlockNumber()

      try:
         txResult = rpc.getCode(address, startBlockNumber)
         return (False, "getCode at the block before the contract deployed " \
                        "should fail")
      except:
         pass

      txResult = rpc.getCode(address, endingBlockNumber)
      txResultLatest = rpc.getCode(address, "latest")

      if not txResult == expectedCode:
         return (False, "code does not match expected")
      if not txResultLatest == expectedCode:
         return (False, "code does not match expected")

      return (True, None)

   def _test_eth_getBalance(self, rpc, request):
      addrFrom = "0x262c0d7ab5ffd4ede2199f6ea793f819e1abb019"
      addrTo = "0x5bb088f57365907b1840e45984cae028a82af934"
      transferAmount = "1"

      previousBlockNumber = rpc.getBlockNumber()
      # data has to be set as None for transferring-fund kind of transaction
      txResult = rpc.sendTransaction(addrFrom,
                                     data=None,
                                     to=addrTo,
                                     value=transferAmount)

      currentBlockNumber = rpc.getBlockNumber()
      addrFromBalance = int(rpc.getBalance(addrFrom, previousBlockNumber), 16)
      addrToBalance = int(rpc.getBalance(addrTo, previousBlockNumber), 16)
      expectedAddrFromBalance = addrFromBalance - int(transferAmount)
      expectedAddrToBalance = addrToBalance + int(transferAmount)

      if not txResult:
         return (False, "Transaction was not accepted")

      if not self._productMode:
         log.warn("No verification done in ethereum mode")
      else:
         tx = request.getTransaction(txResult)
         if not tx:
            return (False, "No transaction receipt found")

         # This is the important one: it tells whether signature address
         # recovery works.
         if not tx["from"] == addrFrom:
            return (False, "Found from does not match expected from")

         # The rest of these are just checking parsing.
         if not tx["to"] == addrTo:
            return (False, "Found to does not match expectd to")
         if not tx["value"] == transferAmount:
            return (False, "Found value does not match expected value")
         if not expectedAddrFromBalance == int(
               rpc.getBalance(addrFrom, currentBlockNumber), 16):
            return (False, "sender balance does not match expected value")
         if not expectedAddrToBalance == int(
               rpc.getBalance(addrTo, currentBlockNumber), 16):
            return (False, "receiver balance does not match expected value")

      return (True, None)
