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
import traceback

from util.debug import pp as pp

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

   @staticmethod
   def searchResponse(searchMe, findMe):
      '''
      It is possible that a response from the server does not contain an
      expected field.  Instead of coding foo["bar"] and getting a  key error
      (which is not always helpful), or always checking "if 'bar' in foo", let's
      wrap that stuff up into a function and try to provide better feedback.

      Accepts a dict holding the server response, and a dict containing the
      nesting of keys to find.  e.g. To retrieve "bar" in:

      response = {
         "foo1": {
            "foo2": {
               "foo3": "bar"
            }
         }
      }

      searchResponse(response, ["foo1", "foo2", "foo3"])

      Raises an exception if a field is missing.  The exception shows what was
      being searched for at each level and where the failure occurred.  e.g.
         Exception: Unable to find key.
         Searching for foo3 in ['foo1', 'foo2', 'foo3'].
         Searching for foo3a_ in ['foo3a', 'foo3b'].
         foo3a_ not found in ['foo3a', 'foo3b'].
      '''
      return RPC._searchResponse(searchMe, findMe)

   @staticmethod
   def _searchResponse(searchMe, findMe, msg="", fullResponse=None):
      '''
      Don't call this one. This one is called by searchResponse.
      '''
      if isinstance(findMe, str):
         findMe = [findMe]

      if not fullResponse:
         fullResponse = searchMe

      keyToFind = findMe.pop(0)

      try:
         keysToSearch = list(searchMe.keys())
      except Exception as e:
         keysToSearch = []

      msg += "Searching for '{}' in '{}'.\n".format(keyToFind, keysToSearch)

      if keyToFind in keysToSearch:
         if len(findMe) == 0:
            return searchMe[keyToFind]
         else:
            return RPC._searchResponse(searchMe[keyToFind],
                                        findMe, msg, fullResponse)
      else:
         if not keyToFind:
            keyToFind = ""

         msg = "Unable to find key '{}'.\n{}" \
               "\nFull response was '{}'.".format(keyToFind, msg, fullResponse)
         log.debug(msg)
         raise Exception(msg)

   def _call(self):
      '''
      Makes the actual RPC call by invoking curl.  Returns the raw json
      of the response.
      '''
      response = None
      exception = None

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
         response = util.json_helper.readJsonFile(self._responseFile)

         if "error" in response:
            exception = "RPC response contained an error.\n" \
                        "Data sent: '{}'\n".format(self._rpcData) + \
                        "Response: '{}'".format(response)
      else:
         exception = "No response for an RPC call was received.  Is the " \
                     "server running?"

         if os.path.isfile(self._outputFile):
            exception += "  Details:\n"
            with open (self._outputFile, "r") as f:
               exception += f.read()

      if exception:
         raise Exception(exception)
      else:
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

   def sendTransaction(self, caller, data, gas = None, to = None):
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

      if to:
         self._rpcData["params"][0]["to"] = to

      response = self._call()
      return self.getResultFromResponse(response)

   def _getTransactionReceipt(self, txHash):
      '''
      NOTE: This is the bare RPC call.  You probably want to call
            getTransactionReceipt(), which wraps this in order to
            wait for mining.
      Given a transaction hash, returns the receipt for that transaction.
      '''
      self._rpcData["method"] = "eth_getTransactionReceipt"
      self._rpcData["params"] = [
         txHash
      ]
      response = self._call()
      return self.getResultFromResponse(response)

   def getTransactionReceipt(self, txHash, waitForMining=False):
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
         txReceipt = self._getTransactionReceipt(txHash)

         if txReceipt and "contractAddress" in txReceipt:
            ret = txReceipt
         elif not txReceipt:
            log.debug("No transaction receipt")
         else:
            log.debug("Unable to find 'contractAddress' in receipt '{}'". \
                      format(txReceipt))

         if attempts > 0:
            time.sleep(1)

      return ret

   def getTransactionReceiptStatus(self, receipt):
      '''
      Retrieve the status of a receipt.
      '''
      if not "status" in receipt:
         error = "No status was found in transaction receipt:\n" +\
                 pprint.PrettyPrinter().pprint(txReceipt) +\
                 "\nBe sure 'ByzantiumBlock' was set in your genesis.json " \
                 "file's config section. e.g. \"ByzantiumBlock\": 0"
         raise(error)

      return receipt["status"]

   def unlockAccount(self, hsh, password):
      '''
      Given a blockchain user hash and password, unlocks the account.
      '''
      self._rpcData["method"] = "personal_unlockAccount"
      self._rpcData["params"] = [hsh, password, 0]

      response = self._call()
