#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Test the parts of the Ethereum JSON RPC Websocket beyond what the
# EthCoreVmTests cover. This checks things like web3_sha3,
# eth_clientVersion, eth_mining, ...
#########################################################################
import json
from numbers import Number
import random
import logging
import time
import collections
import re
import os
import traceback
import websocket
from . import test_suite

import util.hermes_logging
log = util.hermes_logging.getMainLogger()


class WebSocketRPCTests(test_suite.TestSuite):
   _endpoint = "wss://localhost:8545/ws"
   _ws = websocket.WebSocket()

   def __init__(self, passedArgs, product):
      super(WebSocketRPCTests, self).__init__(passedArgs, product)

   def getName(self):
      return "WebSocketRPCTests"

   def run(self):
      ''' Runs all of the tests. '''
      self.launchProduct()
      tests = self._getTests()

      self._connect
      for (testName, testFun) in tests:
         log.info("Starting test '{}'".format(testName))
         testLogDir = os.path.join(self._testLogDir, testName)

         try:
            result, info = testFun()
         except Exception as e:
            result = False
            info = str(e) + "\n" + traceback.format_exc()
            log.error(
               "Exception running RPC Websocket test: '{}'".format(info))

         if info:
            info += "  "
         else:
            info = ""

         relativeLogDir = self.makeRelativeTestPath(testLogDir)
         info += "Log: <a href=\"{}\">{}</a>".format(relativeLogDir,
                                              testLogDir)
         self.writeResult(testName, result, info)

      self._close
      log.info("Websocket Tests are done.")
      return super().run()

   def _getTests(self):
      all_tests = [
         ("eth_estimateGas_ws", self._test_eth_estimateGas),
         ("eth_getBalance_ws", self._test_eth_getBalance),
         ("eth_getBlockByNumber_ws", self._test_eth_getBlockByNumber),
         ("eth_getCode_ws", self._test_eth_getCode),
         ("eth_gasPrice_ws", self._test_eth_gasPrice),
         ("eth_getTransactionByHash_ws", self._test_eth_getTransactionByHash),
         ("eth_getTransactionCount_ws", self._test_eth_getTransactionCount),
         ("eth_getTransactionReceipt_ws", self._test_eth_getTransactionReceipt),
         ("eth_mining_ws", self._test_eth_mining),
         ("eth_sendRawTransaction_ws", self._test_eth_sendRawTransaction),
         ("eth_syncing_ws", self._test_eth_syncing),
         ("rpc_modules_ws", self._test_rpc_modules),
         ("web3_sha3_ws", self._test_web3_sha3),
         ("web3_clientVersion", self._test_web3_clientVersion),
      ]
      if self._args.tests:
         return list(filter(lambda x: x[0] in self._args.tests, all_tests))
      return all_tests

   def _connect(self):
      _ws.connect(self._endpoint)

   def _close(self):
      _ws.close()

   def _test_eth_getBalance(self):
      addrFrom = "0x262c0d7ab5ffd4ede2199f6ea793f819e1abb019"
      addrTo = "0x5bb088f57365907b1840e45984cae028a82af934"
      transferAmount = 1

      previousBlockNumber = getBlockNumber()
      txResult = sendTransaction(
         addrFrom, None, None, addrTo, hex(transferAmount))
      currentBlockNumber = getBlockNumber()
      addrFromBalance = int(getBalance(addrFrom, previousBlockNumber), 16)
      addrToBalance = int(getBalance(addrTo, previousBlockNumber), 16)
      expectedAddrFromBalance = addrFromBalance - transferAmount
      expectedAddrToBalance = addrToBalance + transferAmount

      if not txResult:
         return (False, "Transaction was not accepted")

      if not expectedAddrFromBalance == int(
            getBalance(addrFrom, currentBlockNumber), 16):
         return (False, "sender balance does not match expected value")
      if not expectedAddrToBalance == int(
            getBalance(addrTo, currentBlockNumber), 16):
         return (False, "receiver balance does not match expected value")

      return (True, None)

   def _test_eth_estimateGas(self):
      '''
      Check that gas price is reported correctly
      '''
      _ws.send('{"jsonrpc":"2.0","id":0,"method":"eth_estimateGas","params":[]}')
      result = _ws.recv()
      resutl_dict = json.loads(result)
      if int(resutl_dict['result'], 16) == 0:
         return (True, None)

   def _test_eth_getBlockByNumber(self):
      currentBlockNumber = getBlockNumber()
      latestBlock = getBlockByNumber()

      if not int(latestBlock["number"], 16) >= int(currentBlockNumber, 16):
         return (False, "Latest block is before current block number")

      currentBlock = getBlockByNumber(currentBlockNumber)

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
         futureBlock = getBlockByNumber(futureBlockNumber)
         return (False,
               "Expected an error for future block {}, "
               "but received block {}".format(futureBlockNumber,
                                       futureBlock["number"]))
      except:
         # requesting an uncommitted block should return an error
         pass

      return (True, None)

   def _test_eth_getCode(self):
      contractTransaction = "0xf901628085051f4d5c0083419ce08080b9010f608060405234801561001057600080fd5b506104d260005560ea806100256000396000f30060806040526004361060525763ffffffff7c01000000000000000000000000000000000000000000000000000000006000350416634f2be91f811460575780636deebae314606b5780638ada066e14607d575b600080fd5b348015606257600080fd5b50606960a1565b005b348015607657600080fd5b50606960ac565b348015608857600080fd5b50608f60b8565b60408051918252519081900360200190f35b600080546001019055565b60008054600019019055565b600054905600a165627a7a72305820b827241483c0f1a78e00de3ba4a4cb1e67a03bf6fb9f5ecc0491712f7e0aeb8000291ca080c9884eefca39aece8d308136f3bc2b95e44bd812afc33a9d741fcacee2f874a0125e2c8cd8af9e32cdbabf525a2e69ba7ebdd16411314a9bfa1e6bf414db6122"
      expectedCode = "0x60806040526004361060525763ffffffff7c01000000000000000000000000000000000000000000000000000000006000350416634f2be91f811460575780636deebae314606b5780638ada066e14607d575b600080fd5b348015606257600080fd5b50606960a1565b005b348015607657600080fd5b50606960ac565b348015608857600080fd5b50608f60b8565b60408051918252519081900360200190f35b600080546001019055565b60008054600019019055565b600054905600a165627a7a72305820b827241483c0f1a78e00de3ba4a4cb1e67a03bf6fb9f5ecc0491712f7e0aeb800029"
      address = "0x7c29bd452fae057dfd4e44b746846e4293db5060"
      startBlockNumber = getBlockNumber()
      sendRawTransaction(contractTransaction)
      endingBlockNumber = getBlockNumber()
      try:
         txResult = getCode(address, startBlockNumber)
         if txResult:
            return (False, "getCode at the block before the contract deployed "
                           "should fail")
      except:
         pass
      txResult = getCode(address, endingBlockNumber)
      txResultLatest = getCode(address, "latest")
      if not txResult == expectedCode:
         return (False, "code does not match expected")
      if not txResultLatest == expectedCode:
         return (False, "code does not match expected")

      return (True, None)

   def _test_eth_gasPrice(self):
      '''
      Check that gas price is reported correctly
      '''
      result = gasPrice()
      if not result == "0x0":
         # "0x0" is the default GasPrice in Helen's application.properties
         return (False, "Expected product to have zero gas price, "
                        "but found '{}'".format(result))
      return (True, None)

   def _test_eth_getTransactionByHash(self):
      block = getBlockByNumber("latest")
      while not block["transactions"]:
         block = getBlockByNumber(hex(int(block["number"], 16)-1))

      txHash = random.choice(block["transactions"])
      tx = getTransactionByHash(txHash)

      if tx is None:
         return (False, "Failed to get transaction {}".format(txHash))
      if block["hash"] != tx["blockHash"]:
         return (False, "Block hash is wrong in getTransactionByHash: {}:{}"
                        .format(block["hash"], tx["blockHash"]))
      if txHash != tx["hash"]:
         return (False, "Transaction hash is wrong in getTransactionByHash: {}:{}"
                        .format(txHash, tx["hash"]))

      return (True, None)

   def _test_eth_getTransactionCount(self):
      caller = "0x262c0d7ab5ffd4ede2199f6ea793f819e1abb019"
      previousBlockNumber = getBlockNumber()

      txResult = sendTransaction(caller,
                           data="0x00",
                           gas="0x01")

      startNonce = getTransactionCount(caller, previousBlockNumber)

      if not startNonce:
         return (False, "Unable to get starting nonce")

      if not txResult:
         return (False, "Transaction was not accepted")

      endNonce = getTransactionCount(caller)

      if not endNonce:
         return (False, "Unable to get ending nonce")

      if not int(endNonce, 16) - int(startNonce, 16) == 1:
         return (False, "End nonce '{}' should be exactly one greater than start "
                        "nonce {})".format(endNonce, startNonce))

      return (True, None)

   def _test_eth_getTransactionReceipt(self):
      '''
      Make sure the API is available and all expected fields are present.
      '''
      block = getBlockByNumber("latest")
      # The latest block may be a time-update with no transactions in
      # it. Look at an earlier block, if that's the case
      while not block["transactions"]:
         block = getBlockByNumber(hex(int(block["number"], 16)-1))
      txHash = random.choice(block["transactions"])

      tx = getTransactionReceipt(txHash)
      if tx is None:
         return (False, "Failed to get transaction {}".format(txHash))

      if not isinstance(tx["logs"], list):
         return (False, 'Array expected for field "logs"')

      return (True, None)

   def _test_eth_mining(self):
      '''
      Check that mining status is reported correctly
      '''
      result = mining()
      if result == True:
         return (False, "code does not match expected")

      return (True, None)

   def _test_eth_sendRawTransaction(self):
      '''
      Check that a raw transaction gets decoded correctly.
      '''
      rawTransaction = "0xf86b19847735940082520894f6c3fff0b77efe806fcc10176b8cbf71c6dfe3be880429d069189e00008025a0141c8487e4db65457266978a7f8d856b777a51dd9863d31637ccdec8dea74397a07fd0e14d0e3e891882f13acbe68740f1c5bd82a1a254f898cdbec5e9cfa8cf38"
      expectedHash = "0x6ab11d26df13bc3b2cb1c09c4d274bfce325906c617d2bc744b45fa39b7f8c68"
      expectedFrom = "0x42c4f19a097955ff2a013ef8f014977f4e8516c3"
      expectedTo = "0xf6c3fff0b77efe806fcc10176b8cbf71c6dfe3be"
      expectedValue = "0x429d069189e0000"  # 300000000000000000 Wei

      txResult = sendRawTransaction(rawTransaction)
      if not txResult:
         return (False, "Transaction was not accepted")

      if not txResult == expectedHash:
         return (False, "Receipt hash != expected hash.")

      tx = _getTransactionReceipt(txResult)

      if not tx["from"] == expectedFrom:
         return (False, "Found from does not match expected from")

      if not tx["to"] == expectedTo:
         return (False, "Found to does not match expectd to")

      return (True, None)

   def _test_eth_syncing(self):
      '''
      Check that syncing state is reported correctly
      '''
      result = syncing()
      if result:
         return (False, "Expected node to not be syncing, "
                        "but found '{}'".format(result))

      return (True, None)

   def _test_rpc_modules(self):
      '''
      Check that available RPC modules are listed correctly
      '''
      result = modules()

      if not isinstance(result, dict):
         return (False, "Reply should have been a dict.")

      if len(result) == 0:
         log.warn("No RPC modules returned from rpc request.")

      version_re = re.compile("\\d+\\.\\d+")

      for k, v in result.items():
         if not k in ["admin", "eth", "miner", "net", "personal", "rpc", "web3"]:
            return (False,
                  "Response included unknown RPC module '{}'".format(k))
         if not version_re.match(v):
            return (False,
                  "Module version should be version like, "
                  "but was '{}'".format(v))

      return (True, None)

   def _test_web3_sha3(self):
      '''
      Check that hashing works as expected.
      '''
      # list of (data, expected hash) tuples
      datahashes = [("0x", "0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"),
                    ("0x1234567890abcdef", "0xed8ab4fde4c4e2749641d9d89de3d920f9845e086abd71e6921319f41f0e784f")]

      for (d, h) in datahashes:
         result = sha3(d)
         if not result == h:
            return (False,
                  "Hash of '{}' did not match"
                  "expected '{}': actual: '{}'".format(d, h, result))

      return (True, None)

   def _test_web3_clientVersion(self):
      '''
      Check that we return a valid version
      '''
      result = clientVersion()
      if not type(result) is str:
         return (False, "Client version should have been a string, "
                        "but was '{}'".format(result))

      # Insisting version is
      # <name>/v<major>.<minor>.<patch><anything>/
      #   <os>/<language><major>.<minor>.<patch>
      version_re = re.compile("\\w+/v\\d+\\.\\d+\\.\\d+[^/]*/"
                              "[-a-zA-Z0-9]+/\\w+\\d\\.\\d\\.\\d")
      if not version_re.match(result):
         return (False, "Client version doesn't match expected format: "
                        "'{}'".format(result))

      return (True, None)

   def sendTransaction(caller, data, gas=None, to=None, value=None):
      method = "eth_sendTransaction"
      params = [{
         "from": caller,
         "data": data
      }]
      if gas:
         params[0]["gas"] = gas
      if to:
         params[0]["to"] = to
      if value:
         params[0]["value"] = value

      jsonrpc = {"jsonrpc": "2.0", "id": 0,
                 "method": method, "params": params}
      _ws.send(json.dumps(jsonrpc))
      return getResultFromResponse(json.loads(_ws.recv()))

   def getResultFromResponse(receive):
      '''
      Given a JSON RPC receive, looks for the "result" field and returns it.
      '''
      if "result" in receive:
         return receive["result"]
      else:
         return None

   def getBalance(address, blockNumber=None):
      '''
      Returns the balance of the account of given address.
      :param address: 20 Bytes - address to check for balance
      :param blockNumber: integer block number, or the string "latest",
               "earliest" or "pending"
      :return: integer of the current balance in wei.
      '''
      method = "eth_getBalance"
      if blockNumber is None:
         blockNumber = "latest"
      params = [
         address,
         blockNumber
      ]
      jsonrpc = {"jsonrpc": "2.0", "id": 0,
                   "method": method, "params": params}
      _ws.send(json.dumps(jsonrpc))
      return getResultFromResponse(json.loads(_ws.recv()))

   def getBlockNumber():
      method = "eth_blockNumber"
      params = []
      jsonrpc = {"jsonrpc": "2.0", "id": 0,
                 "method": method, "params": params}
      _ws.send(json.dumps(jsonrpc))
      return getResultFromResponse(json.loads(_ws.recv()))

   def getBlockByNumber(number="latest", includeTransactions=False):
      method = "eth_getBlockByNumber"
      if isinstance(number, Number):
         number = hex(number)
      params = [number, includeTransactions]
      jsonrpc = {"jsonrpc": "2.0", "id": 0,
                 "method": method, "params": params}
      _ws.send(json.dumps(jsonrpc))
      return getResultFromResponse(json.loads(_ws.recv()))

   def sendRawTransaction(rawTx):
      method = "eth_sendRawTransaction"
      params = [rawTx]
      jsonrpc = {"jsonrpc": "2.0", "id": 0,
                 "method": method, "params": params}
      _ws.send(json.dumps(jsonrpc))
      return getResultFromResponse(json.loads(_ws.recv()))

   def getCode(address, blockNumber=None):
      method = "eth_getCode"
      if blockNumber is None:
         blockNumber = "latest"
      params = [address, blockNumber]
      jsonrpc = {"jsonrpc": "2.0", "id": 0,
                 "method": method, "params": params}
      _ws.send(json.dumps(jsonrpc))
      return getResultFromResponse(json.loads(_ws.recv()))

   def gasPrice():
      method = "eth_gasPrice"
      params = []
      jsonrpc = {"jsonrpc": "2.0", "id": 0,
                 "method": method, "params": params}
      _ws.send(json.dumps(jsonrpc))
      return getResultFromResponse(json.loads(_ws.recv()))

   def getTransactionByHash(hash):
      method = "eth_getTransactionByHash"
      params = [hash]
      jsonrpc = {"jsonrpc": "2.0", "id": 0,
                 "method": method, "params": params}
      _ws.send(json.dumps(jsonrpc))
      return getResultFromResponse(json.loads(_ws.recv()))

   def getTransactionCount(address, blockNumber=None):
      '''
      Get the number of transactions that the account named by `address`
      has sent.
      :param address: 20 Bytes - address.
      :param blockNumber: integer block number, or the string "latest",
                  "earliest" or "pending"
      :return: integer of the number of transactions send from this address
      '''
      method = "eth_getTransactionCount"
      if blockNumber is None:
         blockNumber = "latest"
      params = [address, blockNumber]
      jsonrpc = {"jsonrpc": "2.0", "id": 0,
                 "method": method, "params": params}
      _ws.send(json.dumps(jsonrpc))
      return getResultFromResponse(json.loads(_ws.recv()))

   def _getTransactionReceipt(txHash):
      '''
      NOTE: This is the bare RPC call.  You probably want to call
         getTransactionReceipt(), which wraps this in order to
         wait for mining.
      Given a transaction hash, returns the receipt for that transaction.
      '''
      method = "eth_getTransactionReceipt"
      params = [
         txHash
      ]
      jsonrpc = {"jsonrpc": "2.0", "id": 0,
                 "method": method, "params": params}
      _ws.send(json.dumps(jsonrpc))
      return getResultFromResponse(json.loads(_ws.recv()))

   def getTransactionReceipt(txHash, waitForMining=False):
      '''
      Given a transaction hash, gets the receipt.

      VMware will not implement mining.  waitForMining is available for running
      against official Ethereum in sort of a "test diagnostic" mode.
      Mining is going to be an issue.  It can take a long time to mine a
      contract.
      '''
      # Even with 200, it times out sometimes.
      attempts = 200 if waitForMining else 1
      ret = None

      while attempts > 0 and not ret:
         attempts -= 1
         txReceipt = _getTransactionReceipt(txHash)

         if txReceipt and "contractAddress" in txReceipt:
            ret = txReceipt
         elif not txReceipt:
            log.debug("No transaction receipt")
         else:
            log.debug("Unable to find 'contractAddress' in receipt '{}'".
                    format(txReceipt))

         if attempts > 0:
            time.sleep(1)

      return ret

   def mining():
      '''
      Ask if mining is in progress
      '''
      method = "eth_mining"
      params = []
      jsonrpc = {"jsonrpc": "2.0", "id": 0,
                 "method": method, "params": params}
      _ws.send(json.dumps(jsonrpc))
      return getResultFromResponse(json.loads(_ws.recv()))

   def syncing():
      '''
      Ask what the syncing state of the node is.
      '''
      method = "eth_syncing"
      params = []
      jsonrpc = {"jsonrpc": "2.0", "id": 0,
                 "method": method, "params": params}
      _ws.send(json.dumps(jsonrpc))
      return getResultFromResponse(json.loads(_ws.recv()))

   def modules():
      '''
      Ask what RPC modules are supported
      '''
      method = "rpc_modules"
      params = []
      jsonrpc = {"jsonrpc": "2.0", "id": 0,
                 "method": method, "params": params}
      _ws.send(json.dumps(jsonrpc))
      return getResultFromResponse(json.loads(_ws.recv()))

   def sha3(data):
      '''
      Ask to hash the given data
      '''
      method = "web3_sha3"
      params = [data]
      jsonrpc = {"jsonrpc": "2.0", "id": 0,
                 "method": method, "params": params}
      _ws.send(json.dumps(jsonrpc))
      return getResultFromResponse(json.loads(_ws.recv()))

   def clientVersion():
      '''
      Ask for the current version
      '''
      method = "web3_clientVersion"
      params = []
      jsonrpc = {"jsonrpc": "2.0", "id": 0,
                 "method": method, "params": params}
      _ws.send(json.dumps(jsonrpc))
      return getResultFromResponse(json.loads(_ws.recv()))

