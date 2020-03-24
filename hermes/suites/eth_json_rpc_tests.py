#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Test the parts of the Ethereum JSON RPC API beyond what the
# EthCoreVmTests cover. This checks things like web3_sha3,
# eth_clientVersion, eth_mining, ...
#########################################################################
import argparse
import collections
import json
import logging
import os
import pprint
import random
import re
import tempfile
import time
import traceback

from . import test_suite
from rpc.rpc_call import RPC
from rest.request import Request
from util.debug import pp as pp
from util.numbers_strings import trimHexIndicator, decToEvenHexNo0x
from util.product import Product
import util.json_helper
import util.helper
from util.auth import getAccessToken
from requests.auth import HTTPBasicAuth

import web3
from web3 import Web3, HTTPProvider

import util.hermes_logging
log = util.hermes_logging.getMainLogger()

class EthJsonRpcTests(test_suite.TestSuite):
   _args = None
   _userConfig = None
   _ethereumMode = False
   _productMode = True
   _resultFile = None
   _unintentionallySkippedFile = None
   _userUnlocked = False
   _httpProvider = None
   _reverseProxyhttpProvider = None

   def __init__(self, passedArgs, product):
      accessToken = getAccessToken()
      authHeader = {'Authorization': 'Bearer {0}'.format(accessToken)}
      self._reverseProxyhttpProvider = HTTPProvider(
         "https://localhost/blockchains/local/api/concord/eth/",
         request_kwargs={
            'headers': authHeader,
            'verify': False
         }
      )

      self._userConfig = util.helper.loadConfigFile(passedArgs)
      super(EthJsonRpcTests, self).__init__(passedArgs, product)

   def getName(self):
      return "EthJsonRpcTests"

   def run(self):
      ''' Runs all of the tests. '''
      self.launchProduct()
      tests = self._getTests()

      for (testName, testFun) in tests:
         testLogDir = os.path.join(self._testLogDir, testName)
         # When this suite is switched over to pytest, the request object
         # and the blockchain ID will be available from fixtures.  For now,
         # create a request object and derive the blockchain ID here.
         request = Request(testLogDir,
                           testName,
                           self._args.reverseProxyApiBaseUrl,
                           self._userConfig,
                           util.auth.internal_admin)
         blockchainId = request.getBlockchains()[0]["id"]
         self.setEthrpcNode(request, blockchainId)

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
      return super().run()


   def _getTests(self):
      all_tests = [
         ("block_filter", self._test_block_filter),
         ("block_filter_independence", self._test_block_filter_independence),
         ("block_filter_uninstall", self._test_block_filter_uninstall),
         ("eth_estimateGas", self._test_eth_estimateGas),
         ("eth_fallback_function", self._test_fallback),
         ("eth_getBalance", self._test_eth_getBalance),
         ("eth_getBlockByNumber", self._test_eth_getBlockByNumber),
         ("eth_getCode", self._test_eth_getCode),
         ("eth_getLogs", self._test_eth_getLogs),
         ("eth_getLogs_addr", self._test_eth_getLogs_addr),
         ("eth_getLogs_block_range", self._test_eth_getLogs_block_range),
         ("eth_getLogs_topics", self._test_eth_getLogs_topics),
         ("eth_gasPrice", self._test_eth_gasPrice),
         ("eth_getStorageAt", self._test_eth_getStorageAt),
         ("eth_getTransactionByHash", self._test_eth_getTransactionByHash),
         ("eth_getTransactionCount", self._test_eth_getTransactionCount),
         ("eth_getTransactionReceipt", self._test_eth_getTransactionReceipt),
         ("eth_mining", self._test_eth_mining),
         ("eth_personal_newAccount", self._test_personal_newAccount),
         ("eth_replay_protection", self._test_replay_protection),
         ("eth_sendRawTransaction", self._test_eth_sendRawTransaction),
         ("eth_sendRawContract", self._test_eth_sendRawContract),
         ("eth_syncing", self._test_eth_syncing),
         ("rpc_modules", self._test_rpc_modules),
         ("web3_sha3", self._test_web3_sha3),
         ("web3_clientVersion", self._test_web3_clientVersion),
      ]
      if self._args.tests:
         return list(filter(lambda x: x[0] in self._args.tests, all_tests))
      return all_tests

   def _runRpcTest(self, testName, testFun, testLogDir):
      ''' Runs one test. '''
      log.info("Starting test '{}'".format(testName))
      rpc = RPC(testLogDir,
                testName,
                self.ethrpcApiUrl,
                self._userConfig)
      request = Request(testLogDir,
                        testName,
                        self.reverseProxyApiBaseUrl,
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

   def _test_eth_estimateGas(self, rpc, request):
      '''
      Check that gas price is reported correctly
      '''
      result = rpc.estimateGas()
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

   def _test_eth_getTransactionByHash(self, rpc, request):
      '''
      Make sure the API is available and all expected fields are present.
      A proper semantic test should be done in a larger e2e test.
      '''
      block = rpc.getBlockByNumber("latest")
      # The latest block may be a time-update with no transactions in
      # it. Look at an earlier block, if that's the case
      while not block["transactions"]:
         block = rpc.getBlockByNumber(hex(int(block["number"], 16)-1))
      txHash = random.choice(block["transactions"])

      tx = rpc.getTransactionByHash(txHash)
      if tx is None:
         return (False, "Failed to get transaction {}".format(txHash))

      dataFields = ["blockHash", "from", "hash", "input", "to", "r", "s"]
      quantityFields = ["blockNumber", "gas", "gasPrice", "nonce",
                        "transactionIndex", "value", "v"]
      expectedFields = dataFields + quantityFields

      (success, field) = util.helper.requireFields(tx, expectedFields)
      if not success:
         return (False, 'Field "{}" not found in getTransactionByHash'
                        .format(field))

      (success, field) = self.requireDATAFields(tx, dataFields)
      if not success:
         return (False, 'DATA expected for field "{}"'.format(field))

      (success, field) = self.requireQUANTITYFields(tx, quantityFields)
      if not success:
         return (False, 'QUANTITY expected for field "{}"'.format(field))

      # TODO: Better end-to-end test for semantic evaluation of transactions
      if block["hash"] != tx["blockHash"]:
         return (False, "Block hash is wrong in getTransactionByHash: {}:{}"
                        .format(block["hash"], tx["blockHash"]))

      if txHash != tx["hash"]:
         return (False, "Transaction hash is wrong in getTransactionByHash: {}:{}"
                        .format(txHash, tx["hash"]))

      return (True, None)

   def _test_eth_getTransactionCount(self, rpc, request):
      '''
      Check that transaction count is updated.
      '''
      caller = self.product.userProductConfig["users"][0]["hash"]
      previousBlockNumber = rpc.getBlockNumber()

      txResult = rpc.sendTransaction(caller,
                                     data = "0x00",
                                     gas = "0x01")

      startNonce = rpc.getTransactionCount(caller, previousBlockNumber)

      if not startNonce:
         return (False, "Unable to get starting nonce")

      if not txResult:
         return (False, "Transaction was not accepted")

      endNonce = rpc.getTransactionCount(caller)

      if not endNonce:
         return (False, "Unable to get ending nonce")

      if not int(endNonce, 16) - int(startNonce, 16) == 1:
         return (False, "End nonce '{}' should be exactly one greater than start "
                 "nonce {})".format(endNonce, startNonce))

      return (True, None)

   def _test_eth_getTransactionReceipt(self, rpc, request):
      '''
      Make sure the API is available and all expected fields are present.
      '''
      block = rpc.getBlockByNumber("latest")
      # The latest block may be a time-update with no transactions in
      # it. Look at an earlier block, if that's the case
      while not block["transactions"]:
         block = rpc.getBlockByNumber(hex(int(block["number"], 16)-1))
      txHash = random.choice(block["transactions"])

      tx = rpc.getTransactionReceipt(txHash)
      if tx is None:
         return (False, "Failed to get transaction {}".format(txHash))

      dataFields = ["transactionHash", "blockHash", "from", "to",
                    "contractAddress", "logsBloom"]
      quantityFields = ["transactionIndex", "blockNumber", "cumulativeGasUsed",
                        "gasUsed", "status"]
      expectedFields = dataFields + quantityFields + ["logs"]

      (success, field) = util.helper.requireFields(tx, expectedFields)
      if not success:
         return (False, 'Field "{}" not found in getTransactionByHash'
                        .format(field))

      (success, field) = self.requireDATAFields(tx, dataFields)
      if not success:
         # 'null' is allowed if the tx didn't create a contract
         if field != "contractAddress" or tx["contractAddress"] is not None:
            return (False, 'DATA expected for field "{}"'.format(field))

      (success, field) = self.requireQUANTITYFields(tx, quantityFields)
      if not success:
         return (False, 'QUANTITY expected for field "{}"'.format(field))

      if not isinstance(tx["logs"], list):
         return (False, 'Array expected for field "logs"')

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
      expectedValue = "0x429d069189e0000" # 300000000000000000 Wei

      txResult = rpc.sendRawTransaction(rawTransaction)
      if not txResult:
         return (False, "Transaction was not accepted")

      # if this test is re-run on a cluster, we'll see a different
      # hash (or an error once nonce tracking works); don't consider
      # it an error for now
      if not txResult == expectedHash:
         return (False, "Receipt hash != expected hash.")

      if not self._productMode:
         log.warn("No verification done in ethereum mode")
      else:
         blockchainId = request.getBlockchains()[0]["id"]
         tx = request.getTransaction(blockchainId, txResult)
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
      Check that a raw transaction can create a contract. For the contract in use,
      please refer resources/contracts/Counter.sol for the detail.
      '''

      # Compiled abi & bin from contract resources/contracts/Counter.sol
      contract_interface = {
         "abi": "[{\"constant\":false,\"inputs\":[{\"name\":\"x\",\"type\":\"int256\"}],\"name\":\"decrementCounter\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[{\"name\":\"x\",\"type\":\"int256\"}],\"name\":\"incrementCounter\",\"outputs\":[],\"payable\":true,\"stateMutability\":\"payable\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[],\"name\":\"getCount\",\"outputs\":[{\"name\":\"\",\"type\":\"int256\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"inputs\":[],\"payable\":true,\"stateMutability\":\"payable\",\"type\":\"constructor\"},{\"payable\":true,\"stateMutability\":\"payable\",\"type\":\"fallback\"}]",
         "bin": "60806040526000805560e7806100166000396000f30060806040526004361060525763ffffffff7c0100000000000000000000000000000000000000000000000000000000600035041663645962108114605a5780639867b4aa146071578063a87d942c14607a575b6103e8600055005b348015606557600080fd5b50606f600435609e565b005b606f60043560aa565b348015608557600080fd5b50608c60b5565b60408051918252519081900360200190f35b60008054919091039055565b600080549091019055565b600054905600a165627a7a72305820388f79153f456193bb5fb284fa52a73de823a1add68bbf8bf11023fc527ad60d0029"
      }

      wallet = {
         "address": "0000a12b3f3d6c9b0d3f126a83ec2dd3dad15f39",
         "id": "30e15474-1056-4316-b3d9-d2942a1397d6",
         "version": 3,
         "crypto": {
            "cipher": "aes-128-ctr",
            "ciphertext": "47a0f60dab255972bf5bf7f6c57ad119e6e0018df05a997b277b54335736ac21",
            "cipherparams": {"iv": "da55653a91e84b10043860bc3e995c47"},
            "kdf": "scrypt",
            "kdfparams": {"dklen": 32, "n": 262144, "p": 1, "r": 8,
                          "salt": "5aeee57524423766d08c643fca8d23da655614465895d352c57b506130a05ac9"},
            "mac": "4afa549ab91d0a3328df6b28ab62f723f30c699fa40848b1d75a15579b44aebc"
         }
      }
      # Password to decrypt the wallet
      password = "Test123456"
      # will invoke incrementCounter founction, which will increase the counter to 1234
      expectedCount = 1234
      # 10wei (creating) + 10wei(function call)
      expectedBalance = 20

      user = self._userConfig.get('product').get('db_users')[0]
      web3 = self.getWeb3Instance()

      Counter = web3.eth.contract(abi=contract_interface['abi'], bytecode=contract_interface['bin'])
      private_key = web3.eth.account.decrypt(wallet, password)
      account = web3.eth.account.privateKeyToAccount(private_key)

      # Currently, we can feed a contract when creating it
      contract_tx = Counter.constructor().buildTransaction({
         'from': account.address,
         'nonce': web3.eth.getTransactionCount(account.address),
         'gas': 2000000,
         'gasPrice': web3.eth.gasPrice,
         'value': 10
      })
      signed = web3.eth.account.signTransaction(contract_tx, private_key)
      txResult = web3.eth.sendRawTransaction(signed.rawTransaction)
      if not txResult:
         return (False, "Transaction was not accepted")

      if not self._productMode:
         log.warn("No verification done in ethereum mode")
      else:
         hexstring = txResult.hex()
         tx = web3.eth.getTransactionReceipt(hexstring)
         if not tx:
            return (False, "No transaction receipt found")

         if not "contractAddress" in tx:
            return (False, "Contract deployment failed.")

         # Test function in the contract, as incrementCounter is payable,
         # we could pass ether to it
         counter = web3.eth.contract(address=tx.contractAddress, abi=contract_interface["abi"])
         counter.functions.incrementCounter(1234).transact({
            'from': account.address,
            'value': 10
         })

         count = counter.functions.getCount().call()
         if not count == expectedCount:
            return (False, "incrementCounter does not work, which means contract did not get deployed properly")

         balance = web3.eth.getBalance(tx.contractAddress)
         if not balance == expectedBalance:
            return (False, "Ether balance is incorrect, which means contract did not get deployed properly")
      return (True, None)

   def _test_eth_getBlockByNumber(self, rpc, request):
      '''
      Check that blocks can be fetched by number.
      '''

      currentBlockNumber = rpc.getBlockNumber()

      latestBlock = rpc.getBlockByNumber("latest")

      dataFields = ["hash", "parentHash"]
      quantityFields = ["number","timestamp","gasLimit","gasUsed"]
      expectedFields = dataFields + quantityFields
      (present, missing) = util.helper.requireFields(latestBlock, expectedFields)
      if not present:
         return (False, "No '{}' field in block response.".format(missing))

      (success, field) = self.requireDATAFields(latestBlock, dataFields)
      if not success:
         return (False, 'DATA expected for field "{}"'.format(field))

      (success, field) = self.requireQUANTITYFields(latestBlock, quantityFields)
      if not success:
         return (False, 'QUANTITY expected for field "{}"'.format(field))

      if not int(latestBlock["number"], 16) >= int(currentBlockNumber, 16):
         return (False, "Latest block is before current block number")

      currentBlock = rpc.getBlockByNumber(currentBlockNumber)

      if not currentBlock["number"] == currentBlockNumber:
         return (False, "Current block does not have current block number")

      # this gasLimit value is exactly as specified by --gas_limit param
      # of concord CLI
      if not currentBlock["gasLimit"] == "0x989680":
         return (False, "Gas limit isn't 0x989680")

      # Reminder that if time service is running, new blocks might be
      # added at any time, so predict something semi-far future to
      # keep this test stable
      futureBlockNumber = 1000 + int(currentBlockNumber, 16)

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
         blockchainId = request.getBlockchains()[0]["id"]
         tx = request.getTransaction(blockchainId, txResult)
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

   def _test_eth_getLogs(self, rpc, request):
      w3 = self.getWeb3Instance()
      abi, bin = self.loadContract("SimpleEvent")
      contract = w3.eth.contract(abi=abi, bytecode=bin)
      tx_args = {"from":"0x09b86aa450c61A6ed96824021beFfD32680B8B64"}

      # Deploy contract
      contract_tx = contract.constructor().transact(tx_args)
      contract_txr = w3.eth.waitForTransactionReceipt(contract_tx)

      contract = w3.eth.contract(
         address=contract_txr.contractAddress, abi=abi)

      # Invoke function that generates an event which should be logged
      func = contract.get_function_by_name("foo")
      func_tx = func(w3.toInt(hexstr="0xdeadbeef")).transact(tx_args)
      func_txr = w3.eth.waitForTransactionReceipt(func_tx)

      # eth_getLogs()
      logsLatest = rpc.getLogs()

      # eth_getLogs(blockHash)
      logs = rpc.getLogs({"blockHash":func_txr.blockHash.hex()})

      # If the time service is publishing, the latest block might not
      # be the block that our transaction is in. If that's the case,
      # the logs will be empty, so skip this check.
      if logsLatest != [] and logsLatest != logs:
         return (False, "getLogs() != getLogs(blockHash)")

      for l in logs:
         # The first element is the event signature
         # The second is the first argument of the event
         if int(l["topics"][1], 16) == 0xdeadbeef:
            return (True, None)

      return (False, "Couldn't find log in block #" + str(func_txr.blockNumber))

   def _test_eth_getLogs_addr(self, rpc, request):
      w3 = self.getWeb3Instance()
      abi, bin = self.loadContract("SimpleEvent")
      contract = w3.eth.contract(abi=abi, bytecode=bin)
      tx_args = {"from":"0x09b86aa450c61A6ed96824021beFfD32680B8B64"}

      # Deploy the contract twice to get two different contract addresses
      contract_tx = contract.constructor().transact(tx_args)
      contract_txr = w3.eth.waitForTransactionReceipt(contract_tx)
      caddr1 = contract_txr.contractAddress
      contract1 = w3.eth.contract(address=caddr1, abi=abi)

      contract_tx = contract.constructor().transact(tx_args)
      contract_txr = w3.eth.waitForTransactionReceipt(contract_tx)
      caddr2 = contract_txr.contractAddress
      contract2 = w3.eth.contract(address=caddr2, abi=abi)

      # Invoke both contracts to trigger events and generate logs
      func = contract1.get_function_by_name("foo")
      func_tx = func(w3.toInt(hexstr="0x0ff1ce")).transact(tx_args)
      func_txr = w3.eth.waitForTransactionReceipt(func_tx)
      assert func_txr.status == 1

      func = contract1.get_function_by_name("foo")
      func_tx = func(w3.toInt(hexstr="0x0ff1cef001")).transact(tx_args)
      func_txr = w3.eth.waitForTransactionReceipt(func_tx)
      assert func_txr.status == 1

      func = contract2.get_function_by_name("foo")
      func_tx = func(w3.toInt(hexstr="0xba1d0ff1ce")).transact(tx_args)
      func_txr = w3.eth.waitForTransactionReceipt(func_tx)
      assert func_txr.status == 1

      # Get all logs for caddr1
      logs = rpc.getLogs({"fromBlock":"earliest", "address":caddr1})
      if len(logs) != 2:
         return (False, "Expected two log entries for addr " + caddr1)
      filter_out = list(filter(lambda l: w3.toChecksumAddress(l["address"]) == caddr1, logs))
      if not filter_out:
         return (False, "Couldn't find logs for addr " + caddr1)
      if len(filter_out) != len(logs):
         return (False, "Unexpected logs - only logs from {} expected".format(caddr1))

      # Get all logs for caddr2
      logs = rpc.getLogs({"fromBlock":"earliest", "address":caddr2})
      if len(logs) != 1:
         return (False, "Expected one log entries for addr " + caddr1)
      filter_out = list(filter(lambda l: w3.toChecksumAddress(l["address"]) == caddr2, logs))
      if not filter_out:
         return (False, "Couldn't find logs for addr " + caddr2)
      if len(filter_out) != len(logs):
         return (False, "Unexpected logs - only logs from {} expected".format(caddr2))

      return (True, None)

   def _test_eth_getLogs_block_range(self, rpc, request):
      w3 = self.getWeb3Instance()
      abi, bin = self.loadContract("SimpleEvent")
      contract = w3.eth.contract(abi=abi, bytecode=bin)
      tx_args = {"from":"0x09b86aa450c61A6ed96824021beFfD32680B8B64"}

      # Deploy contract
      contract_tx = contract.constructor().transact(tx_args)
      contract_txr = w3.eth.waitForTransactionReceipt(contract_tx)

      contract = w3.eth.contract(
         address=contract_txr.contractAddress, abi=abi)

      # Issue two transactions (which generate two blocks)
      func = contract.get_function_by_name("foo")
      func_tx = func(w3.toInt(hexstr="0xdeadbeef")).transact(tx_args)
      foo_func_txr = w3.eth.waitForTransactionReceipt(func_tx)
      func_tx = func(w3.toInt(hexstr="0xc0ffee")).transact(tx_args)
      w3.eth.waitForTransactionReceipt(func_tx)
      func_tx = func(w3.toInt(hexstr="0x0ddba11")).transact(tx_args)
      oddball_func_txr = w3.eth.waitForTransactionReceipt(func_tx)

      # Let's get all logs
      logs_all = rpc.getLogs({"fromBlock":"0x0"})
      logs_range = rpc.getLogs({"fromBlock":"earliest", "toBlock":"latest"})

      if logs_all != logs_range:
         # Specifying "toBlock" is optional
         return (False, "getLogs(0) != getLogs(earliest, latest)")

      expected = [0xdeadbeef, 0xc0ffee, 0x0ddba11]
      for l in logs_all:
         # The first element is the event signature
         # The second is the first argument of the event
         if len(l["topics"]) == 2:
            val = int(l["topics"][1], 16)
            if val in expected:
               expected.remove(val)

      if expected:
         return (False, "From all logs: Couldn't find " + str(expected))

      # Let's omit the "0x0ddba11" and don't get everything
      from_block = int(foo_func_txr["blockNumber"])
      to_block = int(oddball_func_txr["blockNumber"])-1
      logs = rpc.getLogs({"fromBlock": str(from_block), "toBlock": str(to_block)})

      expected = [0xdeadbeef, 0xc0ffee]
      for l in logs:
         if int(l["blockNumber"], 16) < from_block \
            or int(l["blockNumber"], 16) > to_block:
            return (False, "Unexpected block")
         if len(l["topics"]) == 2:
            val = int(l["topics"][1], 16)
            if val in expected:
               expected.remove(val)

      if expected:
         return (False, "From log range: Couldn't find " + str(expected))

      return (True, None)

   def _test_eth_getLogs_topics(self, rpc, request):
      w3 = self.getWeb3Instance()
      abi, bin = self.loadContract("SimpleEvent")
      contract = w3.eth.contract(abi=abi, bytecode=bin)
      tx_args = {"from":"0x0000A12B3F3d6c9B0d3F126a83Ec2dd3Dad15f39"}

      event_signature = w3.sha3(text="Event(int256)").hex()

      # Deploy the contract twice to get two different contract addresses
      contract_tx = contract.constructor().transact(tx_args)
      contract_txr = w3.eth.waitForTransactionReceipt(contract_tx)
      caddr1 = contract_txr.contractAddress
      contract1 = w3.eth.contract(address=caddr1, abi=abi)

      contract_tx = contract.constructor().transact(tx_args)
      contract_txr = w3.eth.waitForTransactionReceipt(contract_tx)
      caddr2 = contract_txr.contractAddress
      contract2 = w3.eth.contract(address=caddr2, abi=abi)

      # Invoke "twoEvents" to trigger two logs in one transaction
      func = contract1.get_function_by_name("twoEvents")
      func_tx = func(w3.toInt(hexstr="0xc0ffee")).transact(tx_args)
      func_txr = w3.eth.waitForTransactionReceipt(func_tx)
      assert func_txr.status == 1
      logs = contract1.events.Event().processReceipt(func_txr)
      assert logs[0].args.value == w3.toInt(hexstr="0xc0ffef")
      assert logs[1].args.value == w3.toInt(hexstr="0xc0fff0")

      # The latest block should contain our events
      logs = rpc.getLogs({"fromBlock": hex(func_txr["blockNumber"]), "topics":[event_signature]})
      if len(logs) != 2 \
         or logs[0]["topics"][0] != event_signature \
         or logs[1]["topics"][0] != event_signature:
         return (False, "Expected two logs in the transaction block with event sig " + event_signature)

      # Let's get only one event from the two
      # The "twoEvents" function emits two events one with x+1 and the other with x+2
      param = "0x" + 29 * "00" + "c0ffef"
      logs = rpc.getLogs({"fromBlock": hex(func_txr["blockNumber"]), "topics":[event_signature, param]})
      if len(logs) != 1 or logs[0]["topics"][1] != param:
         return (False, "Expected one log in the transaction block with 124")

      return (True, None)

   def _test_eth_getBalance(self, rpc, request):
      addrFrom = "0x262c0d7ab5ffd4ede2199f6ea793f819e1abb019"
      addrTo = "0x5bb088f57365907b1840e45984cae028a82af934"
      transferAmount = 1

      previousBlockNumber = rpc.getBlockNumber()
      # data has to be set as None for transferring-fund kind of transaction
      txResult = rpc.sendTransaction(addrFrom,
                                     data=None,
                                     to=addrTo,
                                     value=hex(transferAmount))

      currentBlockNumber = rpc.getBlockNumber()
      addrFromBalance = int(rpc.getBalance(addrFrom, previousBlockNumber), 16)
      addrToBalance = int(rpc.getBalance(addrTo, previousBlockNumber), 16)
      expectedAddrFromBalance = addrFromBalance - transferAmount
      expectedAddrToBalance = addrToBalance + transferAmount

      if not txResult:
         return (False, "Transaction was not accepted")

      if not self._productMode:
         log.warn("No verification done in ethereum mode")
      else:
         blockchainId = request.getBlockchains()[0]["id"]
         tx = request.getTransaction(blockchainId, txResult)
         if not tx:
            return (False, "No transaction receipt found")

         # This is the important one: it tells whether signature address
         # recovery works.
         if not tx["from"] == addrFrom:
            return (False, "Found from does not match expected from")

         # The rest of these are just checking parsing.
         if not tx["to"] == addrTo:
            return (False, "Found to does not match expectd to")
         if not tx["value"] == hex(transferAmount):
            return (False, "Found value does not match expected value")
         if not expectedAddrFromBalance == int(
               rpc.getBalance(addrFrom, currentBlockNumber), 16):
            return (False, "sender balance does not match expected value")
         if not expectedAddrToBalance == int(
               rpc.getBalance(addrTo, currentBlockNumber), 16):
            return (False, "receiver balance does not match expected value")

      return (True, None)

   def _createBlockFilterAndSendTransactions(self, rpc, txCount):
      '''
      Setup a new block filter, and send txCount transactions that it
      should catch. txCount is assumed to be at least 1.
      '''
      # These are just dummy addresses, that could be passed as
      # parameters if needed in a future version
      addrFrom = "0x262c0d7ab5ffd4ede2199f6ea793f819e1abb019"
      addrTo = "0x5bb088f57365907b1840e45984cae028a82af934"
      transferAmount = "1"

      # Ethereum apps all create filters after submitting the
      # transaction that they expect the filter to catch. Mimic that
      # here, to make sure the filter actually returns the block with
      # this transaction.
      #
      # WARNING: this will be flaky with the time service in play. If
      # a time update is added between this transaction and the
      # creation of the filter, this transaction will not show up.
      transactions = [rpc.sendTransaction(addrFrom,
                                          data=None,
                                          to=addrTo,
                                          value=transferAmount)]

      # create the block filter now, so it sees our transactions
      filter = rpc.newBlockFilter()

      # submit a bunch of tranasactions to create a bunch of blocks
      for x in range(1,txCount):
         # data has to be set as None for transferring-fund kind of transaction
         transactions.append(rpc.sendTransaction(addrFrom,
                                                 data=None,
                                                 to=addrTo,
                                                 value=transferAmount))

      return (filter, transactions)

   def _readBlockFilterToEnd(self, rpc, filter):
      '''
      Read all of the blocks a filter currently matches. Returns the
      block hashes found before the poll for changes returns an
      empty list.
      '''
      lastBlock = rpc.getBlockNumber()
      lastBlockHash = rpc.getBlockByNumber(number=lastBlock)["hash"]

      doubleEmpty = False
      blocksCaught = []
      # now read until the filter says there's nothing more
      while True:
         result = rpc.getFilterChanges(filter)
         if len(result) == 0:
            # web3 doesn't expect an immediate update to a filter, so
            # during the first poll we return "no new blocks" - don't
            # stop yet, try one more read
            if doubleEmpty:
               # no more blocks to read
               break
            doubleEmpty = True

         # list is reversed because we want to search in order of
         # transactions later, and filter response has latest block
         # first
         blocksCaught.extend(reversed(result))

         # Time Service may continue adding blocks, so we need to stop
         # checking the filter if we've seen the block that was at the
         # end before we started reading.
         if lastBlockHash in result:
            break

      return blocksCaught

   def _allTransactionBlocksCaught(self, request, transactions, blocks):
      '''
      Check that the blocks named by the transaction receipts are in the
      blocks list. This is used to verify that a list of blocks
      returned by a filter includes transactions we expect.
      '''

      # transactions and blocks should be in the same order, so we can
      # just iterate and skip blocks that aren't relatvant, instead of
      # having to start the search over again every time
      blockchainId = request.getBlockchains()[0]["id"]
      blocksCopy = blocks[:]
      for t in transactions[1:]:
         tx = request.getTransaction(blockchainId, t)

         found = False
         while blocksCopy:
            if tx["block_hash"] == blocksCopy.pop(0):
               found = True
               break

         if not found:
            return False

      # Because of timing updates, and the crazy way that most
      # ethereum clients create a filter *after* sending the
      # transaction that they want to catch in the filter, we may miss
      # the first transaction. This can't be helped, but let's warn
      # about it to find out how often it really happens.
      tx0 = request.getTransaction(blockchainId, transactions[0])
      if not tx0["block_hash"] in blocks:
         log.warn("First transaction missing from filter")

      return True

   def _test_block_filter(self, rpc, request):
      '''
      Check that a block filter sees updates
      '''
      testCount = 25

      (filter, transactions) = self._createBlockFilterAndSendTransactions(rpc, testCount)
      blocksCaught = self._readBlockFilterToEnd(rpc, filter)
      if self._allTransactionBlocksCaught(request, transactions, blocksCaught):
         return (True, None)
      else:
         return (False, "Expected %d blocks, but read %d from filter" %
                 (testCount, len(blocksCaught)))

   def _test_block_filter_independence(self, rpc, request):
      '''
      Check that two block filters see updates independently.
      '''
      testCount1 = 5
      testCount2 = 5

      (filter1, transactions1) = self._createBlockFilterAndSendTransactions(rpc, testCount1)
      (filter2, transactions2) = self._createBlockFilterAndSendTransactions(rpc, testCount2)

      blocksCaught1 = self._readBlockFilterToEnd(rpc, filter1)
      if not (self._allTransactionBlocksCaught(request, transactions1, blocksCaught1) and
              self._allTransactionBlocksCaught(request, transactions2, blocksCaught1)):
         return (False, "Expected %d blocks, but read %d from filter1" %
                 (testCount1 + testCount2, len(blocksCaught1)))

      blocksCaught2 = self._readBlockFilterToEnd(rpc, filter2)
      if (self._allTransactionBlocksCaught(request, transactions2, blocksCaught2) and
          not self._allTransactionBlocksCaught(request, transactions1, blocksCaught2)):
         return (True, None)
      else:
         return (False, "Expected %d blocks, but read %d from filter2" %
                 (testCount2, len(blocksCaught2)))

   def _test_block_filter_uninstall(self, rpc, request):
      '''
      Check that a filter can't be found after uninstalling it
      '''
      filter = rpc.newBlockFilter()
      result = rpc.getFilterChanges(filter)
      success = rpc.uninstallFilter(filter)
      try:
         rpc.getFilterChanges(filter)
         return (False, "Deleted filter should not be found")
      except:
         return (True, None)

   def _test_replay_protection(self, rpc, request):
      '''
      Check that transactions with incorrect chain IDs
      can't be replayed on blockchain
      '''
      user_id = request.getUsers()[0]['user_id']
      user = self._userConfig.get('product').get('db_users')[0]
      web3 = Web3(self._reverseProxyhttpProvider)
      password = "123456"
      address = web3.personal.newAccount(password)
      wallet = request.getWallet(user_id, address[2:].lower())
      private_key = web3.eth.account.decrypt(wallet, password)

      # Default VMware Blockchain ID is 1
      # By passing chain ID as 2, this transaction must fail
      # (https://github.com/ethereum/EIPs/blob/master/EIPS/eip-155.md#specification)
      try:
         transaction = {
         'to': '0xF0109fC8DF283027b6285cc889F5aA624EaC1F55',
         'value': 0,
         'gas': 0,
         'gasPrice': 0,
         'nonce': 0,
         'chainId': 2
         }
         signed = web3.eth.account.signTransaction(transaction, private_key)
         txResult = web3.eth.sendRawTransaction(signed.rawTransaction)
         log.debug("**** txResult: {}".format(txResult))
         return (False, "Transaction with incorrect chain ID was replayed")
      except:
         pass

      try:
         transaction = {
         'to': '0xF0109fC8DF283027b6285cc889F5aA624EaC1F55',
         'value': 0,
         'gas': 0,
         'gasPrice': 0,
         'nonce': 0,
         'chainId': 1
         }
         signed = web3.eth.account.signTransaction(transaction, private_key)
         txResult = web3.eth.sendRawTransaction(signed.rawTransaction)
      except:
         return (False, "Unsuccessful transaction with correct chain ID")

      return (True, "Tests successful")

   def _test_personal_newAccount(self, rpc, request):
      '''
      Check that account is created correctly
      :param password:
      :return:
      '''
      user_id = request.getUsers()[0]['user_id']
      user = self._userConfig.get('product').get('db_users')[0]
      web3 = Web3(self._reverseProxyhttpProvider)
      password = "123456"
      address = web3.personal.newAccount(password)
      wallet = request.getWallet(user_id, address[2:].lower())
      private_key = web3.eth.account.decrypt(wallet, password)
      transaction = {
         'to': '0xF0109fC8DF283027b6285cc889F5aA624EaC1F55',
         'value': 0,
         'gas': 0,
         'gasPrice': 0,
         'nonce': 0,
         'chainId': 1}
      signed = web3.eth.account.signTransaction(transaction, private_key)
      txResult = web3.eth.sendRawTransaction(signed.rawTransaction)
      if not txResult:
         return (False, "Transaction was not accepted")


      if not self._productMode:
         log.warn("No verification done in ethereum mode")
      else:
         hexstring = txResult.hex()
         blockchainId = request.getBlockchains()[0]["id"]
         tx = request.getTransaction(blockchainId, hexstring)
         if not tx:
            return (False, "No transaction receipt found")

         # Note that the there is no leading '0x' for address in wallet
         if not tx["from"][2:] == wallet['address']:
            return (False, "Found from does not match expected from")

      return (True, None)

   def _test_fallback(self, rpc, request):
      '''
      Check that a contract's fallback function is called if the data passed in a transaction
      does not match any of the other functions in the contract. For the contract in use,
      please refer resources/contracts/Counter.sol for the detail.
      '''

      contract_interface = {
         "abi": "[{\"constant\":false,\"inputs\":[{\"name\":\"x\",\"type\":\"int256\"}],\"name\":\"decrementCounter\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[{\"name\":\"x\",\"type\":\"int256\"}],\"name\":\"incrementCounter\",\"outputs\":[],\"payable\":true,\"stateMutability\":\"payable\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[],\"name\":\"getCount\",\"outputs\":[{\"name\":\"\",\"type\":\"int256\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"inputs\":[],\"payable\":true,\"stateMutability\":\"payable\",\"type\":\"constructor\"},{\"payable\":true,\"stateMutability\":\"payable\",\"type\":\"fallback\"}]",
         "bin": "60806040526000805560e7806100166000396000f30060806040526004361060525763ffffffff7c0100000000000000000000000000000000000000000000000000000000600035041663645962108114605a5780639867b4aa146071578063a87d942c14607a575b6103e8600055005b348015606557600080fd5b50606f600435609e565b005b606f60043560aa565b348015608557600080fd5b50608c60b5565b60408051918252519081900360200190f35b60008054919091039055565b600080549091019055565b600054905600a165627a7a72305820388f79153f456193bb5fb284fa52a73de823a1add68bbf8bf11023fc527ad60d0029"
      }

      wallet = {
         "address": "0000a12b3f3d6c9b0d3f126a83ec2dd3dad15f39",
         "id": "30e15474-1056-4316-b3d9-d2942a1397d6",
         "version": 3,
         "crypto": {
            "cipher": "aes-128-ctr",
            "ciphertext": "47a0f60dab255972bf5bf7f6c57ad119e6e0018df05a997b277b54335736ac21",
            "cipherparams": {"iv": "da55653a91e84b10043860bc3e995c47"},
            "kdf": "scrypt",
            "kdfparams": {"dklen": 32, "n": 262144, "p": 1, "r": 8,
                          "salt": "5aeee57524423766d08c643fca8d23da655614465895d352c57b506130a05ac9"},
            "mac": "4afa549ab91d0a3328df6b28ab62f723f30c699fa40848b1d75a15579b44aebc"
         }
      }
      # Password to decrypt the wallet
      password = "Test123456"
      # will trigger fallback function in this test, the fallback function will set the counter to 1000,
      # please refer resouces/contracts/Counter.sol for detail
      expectedCount = 1000

      user = self._userConfig.get('product').get('db_users')[0]
      web3 = self.getWeb3Instance()

      Counter = web3.eth.contract(abi=contract_interface['abi'], bytecode=contract_interface['bin'])
      private_key = web3.eth.account.decrypt(wallet, password)
      account = web3.eth.account.privateKeyToAccount(private_key)
      contract_tx = Counter.constructor().buildTransaction({
         'from': account.address,
         'nonce': web3.eth.getTransactionCount(account.address),
         'gas': 2000000,
         'gasPrice': web3.eth.gasPrice,
         'value': 10
      })
      signed = web3.eth.account.signTransaction(contract_tx, private_key)
      txResult = web3.eth.sendRawTransaction(signed.rawTransaction)
      if not txResult:
         return (False, "Transaction was not accepted")


      if not self._productMode:
         log.warn("No verification done in ethereum mode")
      else:
         hexstring = txResult.hex()
         tx = web3.eth.getTransactionReceipt(hexstring)
         if not tx:
            return (False, "No transaction receipt found")

         if not "contractAddress" in tx:
            return (False, "Contract deployment failed.")

         # Trigger the fallback function, the passed 'data' does not match any
         # function in the contract
         counter = web3.eth.contract(address=tx.contractAddress, abi=contract_interface["abi"])
         transaction = {
            'from': account.address,
            'to': tx.contractAddress,
            'value': 0,
            'gas': 2000000,
            'gasPrice': web3.eth.gasPrice,
            'nonce':  web3.eth.getTransactionCount(account.address),
            'data': '0xff'}
         signed = web3.eth.account.signTransaction(transaction, private_key)
         txResult = web3.eth.sendRawTransaction(signed.rawTransaction)
         if not txResult:
            return (False, "Transaction was not accepted")

         count = counter.functions.getCount().call()
         if not count == expectedCount:
            return (False, "Did not trigger fallback function")

      return (True, None)
