#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import json
import os
import subprocess
from subprocess import CompletedProcess, PIPE
import time

TEST_LOG_DIR = "test_logs"

class RPC():
   # Class
   _idCounter = 1

   # Instance
   _rpcData = None
   _resultsDir = None
   _testName = None
   _responseFile = None
   _outputFile = None
   _url = None

   def __init__(self, resultsDir, testName, url):
      self._rpcData = {
         "jsonrpc": "2.0",
         "id": RPC._idCounter
      }

      self._resultsDir = resultsDir
      self._testName = testName
      self._url = url
      RPC._idCounter += 1

   def _call(self):
      '''
      Makes the actual RPC call by invoking curl.  Returns the raw json
      of the response.
      '''
      self._setUpOutput(self._rpcData["method"])
      curlCmd = ["curl",
                 "-H", "Content-Type: application/json",
                 "--data", json.dumps(self._rpcData),
                 self._url,
                 "--output", self._responseFile,
                 "--verbose"]

      with open (self._outputFile, "a") as f:
         curlProc = subprocess.run(curlCmd,
                                   stdout=f,
                                   stderr=subprocess.STDOUT)

      with open(self._responseFile, "r") as f:
         return json.load(f)

   def _setUpOutput(self, method):
      '''
      Creates the log directory and sets the response/output files for an
      RPC call.
      '''
      outputDir = os.path.join(self._resultsDir, TEST_LOG_DIR, self._testName)
      os.makedirs(outputDir, exist_ok=True)
      self._responseFile = os.path.join(outputDir, method + ".json")
      self._outputFile = os.path.join(outputDir, method + ".log")

   def getResultFromResponse(self, response):
      '''
      Given a JSON RPC response, looks for the "result" field and returns it.
      Logs a message and returns None if there is no "result" field.
      '''
      if "result" in response:
         return response["result"]
      else:
         print("Unable to find 'result' in response:", response)
         return None

   def getStorageAt(self, blockAddress, storageLocation):
      '''
      Given a block's address and storage location, returns the value from that
      location.
      '''
      self._rpcData["method"] = "eth_getStorageAt"
      self._rpcData["params"] = [
         blockAddress,
         storageLocation,
         "latest"
      ]
      response = self._call()
      return self.getResultFromResponse(response)

   def sendTransaction(self, caller, data):
      '''
      Given a blockchain user hash and some data (e.g. bytecode), submits
      it to the blockchain and returns the result field of the response, which
      is typically the transaction hash.
      '''
      self._rpcData["method"] = "eth_sendTransaction"
      self._rpcData["params"] = [{
         "from": caller,
         "data": data
      }]
      response = self._call()
      return self.getResultFromResponse(response)

   def getTransactionReceipt(self, txHash):
      '''
      Given a transaction hash, returns the receipt for that transaction.
      '''
      self._rpcData["method"] = "eth_getTransactionReceipt"
      self._rpcData["params"] = [
         txHash
      ]
      response = self._call()
      return self.getResultFromResponse(response)

   def getContractAddrFromTxHash(self, txHash, waitForMining=False):
      '''
      Given a transaction hash, returns the address of the contract that was
      created by it.

      VMware does not implement mining.  waitForMining is available for running
      against official Ethereum in sort of a "test diagnostic" mode.
      Mining is going to be an issue.  With one thread, it can take > 20
      seconds to mine a contract.  With two, it seems to be less than 5
      seconds.
      '''
      attempts = 20 if waitForMining else 1
      ret = None

      while attempts > 0 and not ret:
         attempts -= 1
         txReceipt = self.getTransactionReceipt(txHash)

         if txReceipt and "contractAddress" in txReceipt:
            ret = txReceipt["contractAddress"]
         elif not txReceipt:
            print("No transaction receipt")
         else:
            print("Unable to find 'contractAddress' in receipt:", txReceipt)

         if attempts > 0:
            time.sleep(1)

      return ret
