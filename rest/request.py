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
   _logDir = None
   _testName = None
   _endpointName = None
   _responseFile = None
   _outputFile = None
   _baseUrl = None
   _subPath = None
   _params = ""
   _data = None

   def __init__(self, logDir, testName, baseUrl):
      self._logDir = logDir
      os.makedirs(self._logDir, exist_ok=True)

      self._testName = testName
      self._baseUrl = baseUrl
      self._subPath = ""
      self._params = ""


   def _send(self):
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

      if self._data is None:
         curlCmd = ["curl",
                    "-H", "Accept: application/json",
                    self._baseUrl+self._subPath+self._params,
                    "--output", self._responseFile,
                    "--verbose"]
      else:
         curlCmd = ["curl",
                    "-H", "Accept: application/json",
                    "--data", json.dumps(self._data),
                    self._baseUrl+self._subPath+self._params,
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

   def _setUpOutput(self, method):
      '''
      Creates the log directory and sets the response/output files for a
      ReST request.
      '''
      fileRoot = os.path.join(self._logDir, str(Request._idCounter) + "_" +
                              method)
      self._responseFile = fileRoot + ".json"
      self._outputFile = fileRoot + ".log"

   def getMemberList(self):
      '''
      Get the list of nodes in the Athena cluster
      '''
      self._subPath = "/api/athena/members"
      self._params = ""
      self._endpointName = "members"

      return self._send()

   def getSwaggerDefinition(self):
      '''
      Get the swagger definition
      '''
      self._subPath = "/api"
      self._params = ""
      self._endpointName = "swaggerdef"

      return self._send()

   def getBlockList(self, nextUrl=None):
      '''
      Get the list of blocks
      '''
      self._subPath = nextUrl or "/api/athena/blocks"
      self._params = ""
      self._endpointName = "blocklist"

      return self._send()

   def getBlock(self, url):
      '''
      Get a specific block
      '''
      self._subPath = url
      self._params = ""
      self._endpointName = "block"

      return self._send()

   def getTransaction(self, txhash):
      '''
      Get a specific transaction
      '''
      self._subPath = '/api/athena/transactions/'+txhash
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

      self._subPath = '/api/athena/transactions/'
      if latest:
         self._subPath += "?latest=" + latest

      if count:
         if latest:
            self._subPath += "&count={}".format(count)
         else:
            self._subPath += "?count={}".format(count)

      self._params = ""
      self._endpointName = "transactionList"

      return self._send()

   def uploadContract(self, data):
      '''
      Does an upload new contract POST request
      '''
      self._subPath = '/api/athena/contracts'
      self._params = ""
      self._endpointName = "contracts management"
      self._data = data

      return self._send()

   def callContractAPI(self, apiPath, params):
      '''
      Calls a contract management API. Does a GET request.
      '''
      self._subPath = apiPath
      self._params = params
      self._endpointName = "contracts management"
      self._data = None

      return self._send()
