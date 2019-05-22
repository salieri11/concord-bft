#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Wrapper for ReST API calls to Helen.
#########################################################################
import json
import util.json_helper
import logging
import os
import subprocess
from subprocess import CompletedProcess, PIPE
import threading
import time

from util.debug import pp as pp

log = logging.getLogger(__name__)

class Request():
   # Class
   # Incremented for every call, even across test cases, to have
   # something unique to prepended to the output file names for the calls.
   _idCounter = 1

   # Instance
   logDir = None
   testName = None
   _endpointName = None
   _responseFile = None
   _outputFile = None
   _baseUrl = None
   _subPath = None
   _params = ""
   _data = None

   def __init__(self, logDir, testName, baseUrl, userConfig):
      self.logDir = logDir
      os.makedirs(self.logDir, exist_ok=True)

      self.testName = testName
      self._baseUrl = baseUrl
      self._subPath = ""
      self._params = ""
      self._userConfig = userConfig

   def _send(self, verb=None):
      '''
      Makes the actual ReST request by invoking curl.  Returns the raw json
      of the response.
      '''
      response = None
      exception = None

      # Protect use of Request._idCounter.
      lock = threading.Lock()
      lock.acquire()
      self._setUpOutput(self._endpointName)
      Request._idCounter += 1
      lock.release()
      user = self._userConfig.get('product').get('db_users')[0]
      username = user['username']
      password = user['password']
      url = self._baseUrl+self._subPath

      if self._params:
         if "?" in url:
            url += "&" + self._params
         else:
            url += "?" + self._params

      if verb is None:
         if self._data is None:
            curlCmd = ["curl",
                       "-H", "Accept: application/json",
                       "--user", "{0}:{1}".format(
                        username, password),
                       url,
                       "--output", self._responseFile,
                       "--verbose",
                       "--insecure"]
         else:
            curlCmd = ["curl",
                       "-H", "Accept: application/json",
                       "-H", "Content-Type: application/json",
                       "--user", "{0}:{1}".format(
                        username, password),
                       "--data", json.dumps(self._data),
                       url,
                       "--output", self._responseFile,
                       "--verbose",
                       "--insecure"]
      else:
         curlCmd = ["curl",
                    "--request", verb,
                    "-H", "Accept: application/json",
                    "-H", "Content-Type: application/json",
                    "--user", "{0}:{1}".format(
                        username, password),
                    "--data", json.dumps(self._data),
                    url,
                    "--output", self._responseFile,
                    "--verbose",
                    "--insecure"]

      log.debug("REST COMMAND: {}".format(" ".join(curlCmd)))

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
            exception = "ReST response contained an error.\n" \
                        "Response: '{}'".format(response)
      else:
         exception = "No response for a ReST request was received.  Is the " \
                     "server running?"

         if os.path.isfile(self._outputFile):
            exception += "  Details:\n"
            with open (self._outputFile, "r") as f:
               exception += f.read()

      if exception:
         raise Exception(exception)
      else:
         return response

   def _addParam(self, param):
      '''
      Adds the given parameter.  e.g. count=2
      '''
      if param:
         if self._params:
            self._params += "&"

         self._params += param

   def _setUpOutput(self, method):
      '''
      Creates the log directory and sets the response/output files for a
      ReST request.
      '''
      fileRoot = os.path.join(self.logDir, str(Request._idCounter) + "_" +
                              method)
      self._responseFile = fileRoot + ".json"
      self._outputFile = fileRoot + ".log"

   def getMemberList(self, blockchainId, certs=False):
      '''
      Get the list of nodes in the concord cluster
      '''
      self._subPath = "/api/blockchains/" + blockchainId + "/concord/members"
      if certs:
         self._params = "certs=true"
      else:
         self._params = ""
      self._endpointName = "members"

      return self._send()

   def getBlockchains(self, certs=False):
      '''
      Get the list of blockchains
      '''
      self._subPath = "/api/blockchains"
      if certs:
         self._params = "certs=true"
      else:
         self._params = ""
      self._endpointName = "blockchains"

      return self._send()

   def getABlockchainId(self):
      '''
      Returns the first blockchain.
      Will be enhanced when user/org/consortia work is done in Helen.
      '''
      blockchains = self.getBlockchains()
      return blockchains[0]["id"]

   def getBlockList(self, blockchainId, nextUrl=None, latest=None, count=None):
      '''
      Get the list of blocks for the passed in blockchain.
      '''
      self._subPath = nextUrl or "/api/blockchains/" + blockchainId + "/concord/blocks"
      self._params = ""

      if latest != None:
         self._addParam("latest=" + str(latest))

      if count != None:
         self._addParam("count=" + str(count))

      self._endpointName = "blocklist"

      return self._send()

   def getBlockByUrl(self, url):
      '''
      Get a specific block
      '''
      self._subPath = url
      self._params = ""
      self._endpointName = "block"

      return self._send()

   def getBlockByNumber(self, blockchainId, number):
      '''
      Get a specific block, by its number
      '''
      self._subPath = "/api/blockchains/" + blockchainId + "/concord/blocks/{}".format(number)
      self._params = ""
      self._endpointName = "block"

      return self._send()

   def getTransaction(self, blockchainId, txhash):
      '''
      Get a specific transaction
      '''
      self._subPath = "/api/blockchains/" + blockchainId + "/concord/transactions/" + txhash
      self._params = ""
      self._endpointName = "transaction"

      return self._send()

   def getNextTransactionList(self, nextUrl):
      '''
      Calls the given nextUrl to get list of next transactions
      '''
      self._subPath = nextUrl
      self._params = ""
      self._endpointName = "transactionList"
      return self._send()


   def getTransactionList(self, latest=None, count=None):
      '''
      Get a list of transactions
      '''
      self._subPath = '/api/concord/transactions/'
      if latest:
         self._addParam("latest=" + latest)

      if count:
         self._addParam("count={}".format(count))

      self._endpointName = "transactionList"

      return self._send()

   def getUsers(self):
       self._subPath = '/api/users'
       self._params = ""
       self._endpointName = "users"

       return self._send()

   def getWallet(self, userId, address):
      '''
      Get the wallet of the user & address
      :param userId:
      :param address:
      :return:
      '''

      self._subPath = '/api/users/{}'.format(userId)
      self._subPath += '/wallet/' + address
      self._params = ""
      self._endpointName = "wallet"

      return self._send()

   def uploadContract(self, blockchainId, data):
      '''
      Does an upload new contract POST request
      '''
      self._subPath = "/api/blockchains/" + blockchainId + "/concord/contracts"
      self._params = ""
      self._endpointName = "postContract"
      self._data = data

      return self._send()

   def getContracts(self, blockchainId):
      '''
      Returns a list of all contracts.  Does a GET request.
      '''
      self._subPath = "/api/blockchains/" + blockchainId + "/concord/contracts"
      self._params = ""
      self._endpointName = "getContract"
      self._data = None

      return self._send()

   def getAllContractVersions(self, blockchainId, contractId):
      '''
      Returns all versions of a contract.
      '''
      self._subPath = "/api/blockchains/" + blockchainId + "/concord/contracts/" \
                      + contractId
      self._params = ""
      self._endpointName = "getAllContractVersions"
      self._data = None

      return self._send()

   def getContractVersion(self, blockchainId, contractId, contractVersion):
      '''
      Returns a contract.
      '''
      self._subPath = "/api/blockchains/" + blockchainId + "/concord/contracts/" \
                      + contractId + "/versions/" + contractVersion
      self._params = ""
      self._endpointName = "getContractVersion"
      self._data = None

      return self._send()

   def callContractAPI(self, apiPath, params):
      '''
      Calls a contract management API. Does a GET request.
      '''
      self._subPath = apiPath
      self._params = params
      self._endpointName = "contractsManagement"
      self._data = None

      return self._send()

   def callUserAPI(self, apiPath, verb=None, params=None, data=None):
      '''
      Calls a user management API. Send the request based on verb value
      '''
      self._subPath = '/api' + apiPath
      self._endpointName = "userManagement"
      self._data = None
      self._params = ""
      if data:
         self._data = data
      if params:
         self._params = params
      return self._send(verb)

   def compileContract(self, data):
      '''
      Does a compile contract POST request
      '''
      self._subPath = '/contracts/compile'
      self._params = ""
      self._endpointName = "microserviceCompileContract"
      self._data = data

      return self._send()

   def verifyContract(self, data):
      '''
      Does a verify contract POST request
      '''
      self._subPath = '/contracts/verify'
      self._params = ""
      self._endpointName = "microserviceVerifyContract"
      self._data = data

      return self._send()
