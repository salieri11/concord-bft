#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Tests covering Helen's non-ethereum ReST API.
# (i.e. everything under /api/, excluding /api/athena/eth)
#########################################################################
import collections
import json
import logging
import os
import traceback

from . import test_suite
from util.product import Product
from rest.request import Request
import util.json_helper

log = logging.getLogger(__name__)
testContractId = "TestHelloWorld"
testContractVersion = "TestVersion1"

class HelenAPITests(test_suite.TestSuite):
   _args = None
   _apiBaseServerUrl = "http://localhost:8080"
   _userConfig = None
   _ethereumMode = False
   _productMode = True
   _resultFile = None

   def __init__(self, passedArgs):
      super(HelenAPITests, self).__init__(passedArgs)

   def getName(self):
      return "HelenAPITests"

   def run(self):
      ''' Runs all of the tests. '''
      if self._productMode:
         try:
            p = self.launchProduct(self._args.resultsDir,
                                   self._apiBaseServerUrl+"/api/athena/eth",
                                   self._userConfig["product"])
         except Exception as e:
            log.error(traceback.format_exc())
            return self._resultFile

      if self._ethereumMode:
         info = "HelenAPITests are not applicable to ethereumMode."
         log.warn(info)
         self.writeResult("All tests", None, info)
         return self._resultFile

      tests = self._getTests()

      for (testName, testFun) in tests:
         testLogDir = os.path.join(self._testLogDir, testName)

         try:
            result, info = self._runRestTest(testName,
                                             testFun,
                                             testLogDir)
         except Exception as e:
            result = False
            info = str(e)
            traceback.print_tb(e.__traceback__)
            log.error("Exception running ReST test: '{}'".format(info))

         if info:
            info += "  "
         else:
            info = ""

         relativeLogDir = self.makeRelativeTestPath(testLogDir)
         info += "Log: <a href=\"{}\">{}</a>".format(relativeLogDir,
                                                     testLogDir)
         self.writeResult(testName, result, info)

      log.info("Tests are done.")

      if self._productMode:
         p.stopProduct()

      return self._resultFile

   def _runRestTest(self, testName, testFun, testLogDir):
      log.info("Starting test '{}'".format(testName))
      request = Request(testLogDir, testName, self._apiBaseServerUrl)
      return testFun(request)

   def _getTests(self):
      return [("getMembers", self._test_getMembers), \
              # ("swaggerDef", self._test_getSwaggerDef), \
              # ("blockList", self._test_getBlockList), \
              # ("block", self._test_getBlocks), \
              # ("transaction", self._test_getTransactions), \
              ("contract_upload", self._test_contractUpload), \
              ("get_contracts", self._test_getAllContracts), \
              ("version_upload", self._test_versionUpload), \
              ("get_versions", self._test_getAllVersions), \
              ("get_version", self._test_getVersion), \
              ("duplicate_contract", self._test_duplicateContractUpload)]

   # Tests: expect one argument, a Request, and produce a 2-tuple
   # (bool success, string info)

   def _test_getMembers(self, request):
      result = request.getMemberList()

      if not type(result) is list:
         return (False, "Response was not a list")
      if len(result) < 1:
         return (False, "No members returned")

      for m in result:
         (present, missing) = self.requireFields(m, ["host", "status"])
         if not present:
            return (False, "No '{}' field in member entry.".format(missing))

         if not isinstance(m["host"], str):
            return (False, "'host' field in member entry is not a string")
         if not isinstance(m["status"], str):
            return (False, "'status' field in member entry is not a string")

      return (True, None)

   def _test_getSwaggerDef(self, request):
      result = request.getSwaggerDefinition()

      # How stable is comparing to OrderedDict?
      if not type(result) is collections.OrderedDict:
         return (False, "Response was not an OrderedDict".format(
            type(result).__name__))

      (present, missing) = self.requireFields(result, ["info", "paths"])
      if not present:
         return (False,
                 "No '{}' field in result; unlikely to be swagger.".format(
                    missing))

      if not "title" in result["info"]:
         return (False, "No 'title' in result['info']; unlikely to be swagger")
      if not result["info"]["title"] == "VMware Project Athena":
         return (False, "Wrong title in result; likely wrong swagger file")

      # Maybe we should just read the swagger file from the helen
      # install, and compare the result to it, but the above is a
      # pretty good indicator that we found what we're looking for
      return (True, None)

   def _test_getBlockList(self, request, latestBlock=None, nextUrl=None):
      result = request.getBlockList(nextUrl)

      # How stable is comparing to OrderedDict?
      if not type(result) is collections.OrderedDict:
         return (False, "Response was not an OrderedDict".format(
            type(result).__name__))

      if not "blocks" in result:
         return (False, "No 'blocks' field in result")
      if not type(result["blocks"]) is list:
         return (False, "'blocks' field is not a list")

      latestFound = None
      earliestFound = None
      for b in result["blocks"]:
         (present, missing) = self.requireFields(b, ["number", "hash", "url"])
         if not present:
            return (False, "No '{}' field in block.".format(missing))

         latestFound = max(latestFound, b["number"]) \
                       if latestFound else b["number"]
         earliestFound = min(earliestFound, b["number"]) \
                         if earliestFound else b["number"]

      if (latestFound-earliestFound) != len(result["blocks"])-1:
         return (False, "Range of block IDs does not equal length of list")
      if latestBlock and (not latestBlock-1 == latestFound):
         return (False, "Latest block in response is not immediately prior"
                 " to earliest block in previous response")

      # only follow one 'next' link per test
      if (not nextUrl):
         if "next" in result:
            return self._test_getBlockList(request, earliestFound,
                                           result['next'])
         elif not earliestFound == 0:
            return (False, "No 'next' URL, but not yet at block 0")
         else:
            log.warn("Not enough blocks to test 'next' link")
            return (True, "Warning: Not enough blocks to test 'next' link")
      else:
         return (True, None)

   def _test_getBlocks(self, request):
      result = request.getBlockList()

      # validation of the structure of the blocklist response is done
      # in _test_getBlockList

      # get each block and find out if it's valid
      for b in result["blocks"]:
         blockResult = request.getBlock(b["url"])

         (present, missing) = self.requireFields(
            blockResult,
            ["number", "hash", "parentHash", "nonce", "size", "transactions"])
         if not present:
            return (False, "No '{}' field in block response.".format(missing))

         if not b["number"] == blockResult["number"]:
            return (False, "Block number does not match request.")
         if not type(blockResult["transactions"]) is list:
            return (False, "'transactions' field is not a list.")

      return (True, None)

   def _test_getTransactions(self, request):
      result = request.getBlockList()
      blockResult = request.getBlock(result["blocks"][0]["url"])

      # get all of the transactions in the most recent block
      for t in blockResult["transactions"]:
         txResult = request.getTransaction(t["hash"])

         (present, missing) = self.requireFields(
            txResult,
            ["hash", "from", "to", "value", "input", "blockHash",
             "blockNumber", "transactionIndex", "nonce"])
         if not present:
            return (False, "No '{}' field in tx response.".format(missing))

         if not txResult["hash"] == t["hash"]:
            return (False, "'hash' field does not match requested hash.")

      return (True, None)

   def _test_contractUpload(self, request):
      hello_world_contract = open("resources/contracts/HelloWorld.sol", 'r')
      data = {
         "id": 1,
      };
      data["from"] = "0x1111111111111111111111111111111111111111"
      data["contract_id"] = testContractId
      data["version"] = testContractVersion
      data["sourcecode"] = hello_world_contract.read()
      result = request.uploadContract(data)["result"];
      if result[0]["error"] is None:
         return (True, None)
      else:
         return (False, "Contract upload failed with error '{}'".format(result["error"]));


   def _test_getAllContracts(self, request):
      result = request.callContractAPI('/api/athena/contracts', "")
      result = result["result"][0]
      if (result["contract_id"] == testContractId and
          result["url"] == "/api/athena/contracts/" + testContractId):
         return (True, None)
      else:
         return (False, "GET /api/athena/contracts did not return correct response")

   def _test_duplicateContractUpload(self, request):
      hello_world_contract = open("resources/contracts/HelloWorld.sol", 'r')
      data = {
         "id": 1,
      };
      data["from"] = "0x1111111111111111111111111111111111111111"
      data["contract_id"] = testContractId
      data["version"] = testContractVersion
      data["sourcecode"] = hello_world_contract.read()
      try:
         result = request.uploadContract(data);
      except Exception as e:
         print(e)
         return (True, None)
      return (False, "Duplicate contract should not be allowed")

   def _test_versionUpload(self, request):
      hello_world_contract = open("resources/contracts/HelloWorld.sol", 'r')
      data = {
         "id": 1,
      };
      data["from"] = "0x1111111111111111111111111111111111111111"
      data["contract_id"] = testContractId
      data["version"] = "TestVersion2"
      data["sourcecode"] = hello_world_contract.read()
      result = request.uploadContract(data)["result"];
      if result[0]["error"] is None:
         return (True, None)
      else:
         return (False,
                 "Contract upload failed with error '{}'".format(result["error"]));


   def _test_getAllVersions(self, request):
      uri = '/api/athena/contracts/' + testContractId
      total_versions = 2
      result = request.callContractAPI(uri, "")
      result = result["result"]
      if (result["contract_id"] == testContractId and
          len(result["versions"]) == total_versions):
         return (True, None)
      else:
         return (False, "GET /api/athena/contracts/" + testContractId \
                 + " did not return correct response")

   def _test_getVersion(self, request):
      uri = '/api/athena/contracts/' + testContractId \
            + '/versions/' + testContractVersion
      result = request.callContractAPI(uri, "")
      result = result["result"]
      if (result["contract_id"] == testContractId and
          result["version"] == testContractVersion):
         return (True, None)
      else:
         return (False, "GET /api/athena/contracts/" + testContractId \
                 + "/versions/" + testContractVersion \
                 + " did not return correct response")

   def requireFields(self, ob, fieldList):
      for f in fieldList:
         if not f in fieldList:
            return (False, f)
      return (True, None)
