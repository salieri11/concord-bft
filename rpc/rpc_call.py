#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import json
import util.json_helper
import logging
import os
import subprocess
from subprocess import CompletedProcess, PIPE
import threading
import time

log = logging.getLogger(__name__)

class RPC():
   # Class
   # Incremented for every call, even across test cases, since this is included
   # in the RPC call that is sent.
   # Since it's unique, it is also prepended to the output file names for the
   # calls.
   _idCounter = 1

   # Instance
   _logDir = None
   _rpcData = None
   _testName = None
   _responseFile = None
   _outputFile = None
   _url = None

   def __init__(self, logDir, testName, url):
      self._logDir = logDir
      os.makedirs(self._logDir, exist_ok=True)

      self._rpcData = {
         "jsonrpc": "2.0"
      }

      self._testName = testName
      self._url = url

   def _call(self):
      '''
      Makes the actual RPC call by invoking curl.  Returns the raw json
      of the response.
      '''
      response = None

      # Protect use of RPC._idCounter.
      lock = threading.Lock()
      lock.acquire()
      self._rpcData["id"]= RPC._idCounter
      self._setUpOutput(self._rpcData["method"])
      RPC._idCounter += 1
      lock.release()

      curlCmd = ["curl",
                 "-H", "Content-Type: application/json",
                 "--data", json.dumps(self._rpcData),
                 self._url,
                 "--output", self._responseFile,
                 "--verbose"]

      with open (self._outputFile, "a") as f:
         # Make people's lives easier by printing a copy/pastable command.
         f.write("Command: \n'" + "' '".join(curlCmd) + "'\n\n")
         f.flush()
         curlProc = subprocess.run(curlCmd,
                                   stdout=f,
                                   stderr=subprocess.STDOUT)

      if os.path.isfile(self._responseFile):
         return util.json_helper.readJsonFile(self._responseFile)
      else:
         errorMsg = "No response for an RPC call was received.  Is the " \
                    "server running?"

         if os.path.isfile(self._outputFile):
            errorMsg += "  Details:\n"
            with open (self._outputFile, "r") as f:
               errorMsg += f.read()

            raise Exception(errorMsg)

      return response

   def _setUpOutput(self, method):
      '''
      Creates the log directory and sets the response/output files for an
      RPC call.
      '''
      fileRoot = os.path.join(self._logDir, str(RPC._idCounter) + "_" + method)
      self._responseFile = fileRoot + ".json"
      self._outputFile = fileRoot + ".log"

   def getResultFromResponse(self, response):
      '''
      Given a JSON RPC response, looks for the "result" field and returns it.
      Logs a message and returns None if there is no "result" field.
      '''
      if "result" in response:
         return response["result"]
      else:
         log.debug("Unable to find 'result' in response '{}'".format(response))
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

   def sendTransaction(self, caller, data, gas = None):
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

      if gas:
         self._rpcData["params"][0]["gas"] = gas


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
      attempts = 200 if waitForMining else 1
      ret = None

      while attempts > 0 and not ret:
         attempts -= 1
         txReceipt = self.getTransactionReceipt(txHash)

         if txReceipt and "contractAddress" in txReceipt:
            ret = txReceipt["contractAddress"]
         elif not txReceipt:
            log.debug("No transaction receipt")
         else:
            log.debug("Unable to find 'contractAddress' in receipt '{}'". \
                      format(txReceipt))

         if attempts > 0:
            time.sleep(1)

      return ret
