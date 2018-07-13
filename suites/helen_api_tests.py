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
import string
import random
from time import sleep

from . import test_suite
from util.product import Product
from rest.request import Request
from rpc.rpc_call import RPC

import util.json_helper

log = logging.getLogger(__name__)


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
                                   self._apiBaseServerUrl + "/api/athena/eth",
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
              ("swaggerDef", self._test_getSwaggerDef), \
              ("blockList", self._test_getBlockList), \
              ("block", self._test_getBlocks), \
              ("transaction", self._test_getTransactions), \
              ("contract_upload", self._test_contractUpload), \
              ("get_contracts", self._test_getAllContracts), \
              ("version_upload", self._test_versionUpload), \
              ("get_versions", self._test_getAllVersions), \
              ("get_version", self._test_getVersion), \
              ("duplicate_contract", self._test_duplicateContractUpload), \
              ("block_hash", self._test_blockHash), \
              ("invalid_block_hash", self._test_invalidBlockHash), \
              ("get_transaction_list", self._test_getTransactionList), \
              ("get_transaction_list_max_size", self._test_getTransactionListMaxSize), \
              ("get_transaction_list_fields", self._test_transactionListFields), \
              ("get_transaction_list_invalid_latest", self._test_getTransactionListInvalidLatest), \
              ("get_transaction_list_next_url", self._test_getTransactionListNextUrl)]

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
                 "No '{}' field in result; unlikely to be swagger."
                 .format(missing))

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

      if (latestFound - earliestFound) != len(result["blocks"]) - 1:
         return (False, "Range of block IDs does not equal length of list")
      if latestBlock and (not latestBlock - 1 == latestFound):
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
            ["hash", "from", "value", "input", "nonce"])
         if not present:
            return (False, "No '{}' field in tx response.".format(missing))

         if not txResult["hash"] == t["hash"]:
            return (False, "'hash' field does not match requested hash.")

      return (True, None)

   def contract_upload_util(self, request, contractId,
                            contractVersion, sourceCode):
      '''
      A helper method to upload simple hello world contract.
      '''
      data = {};
      data["from"] = "0x1111111111111111111111111111111111111111"
      data["contract_id"] = contractId
      data["version"] = contractVersion
      data["sourcecode"] = sourceCode
      return request.uploadContract(data)

   def random_string_generator(self, size=6, chars=string.ascii_uppercase + string.digits):
      return ''.join(random.choice(chars) for _ in range(size))

   def upload_mock_contract(self, request, contractId=None,
                            contractVersion=None):
      if contractId is None:
         contractId = self.random_string_generator()
      if contractVersion is None:
         contractVersion = self.random_string_generator()

      contractFile = open("resources/contracts/HelloWorld.sol", 'r')
      result = self.contract_upload_util(request, contractId, contractVersion,
                                         contractFile.read())
      if "url" in result[0]:
         return (contractId, contractVersion)
      else:
         raise Exception("Contract upload failed with error '{}'".format(result[0]["error"]))

   def has_contract(self, request, contractId, contractVersion):
      result = request.callContractAPI('/api/athena/contracts/' + contractId
                                       + '/versions/' + contractVersion, "")
      try:
         if (result["contract_id"] == contractId and
             result["version"] == contractVersion):
            return True
      except Exception as e:
         print(e)
         return False

   def _test_contractUpload(self, request):
      contractId, contractVersion = self.upload_mock_contract(request)
      result = request.callContractAPI('/api/athena/contracts/' + contractId
                                       + '/versions/' + contractVersion, "")
      if self.has_contract(request, contractId, contractVersion):
         return (True, None)
      else:
         return (False, "Unable to retrieve uploaded contract")

   def _test_getAllContracts(self, request):
      result = request.callContractAPI('/api/athena/contracts', "")
      existingCount = len(result)
      contractId, contractVersion = self.upload_mock_contract(request)
      result = request.callContractAPI('/api/athena/contracts', "")
      newCount = len(result)
      result = result[0]
      if (self.has_contract(request, contractId, contractVersion) and
          existingCount + 1 == newCount):
         return (True, None)
      else:
         return (False,
                 "GET /api/athena/contracts did not return correct response." \
                 "Expected count to be {} but found {}".format(existingCount + 1,
                                                               newCount))

   def _test_duplicateContractUpload(self, request):
      contractId, contractVersion = self.upload_mock_contract(request)
      try:
         self.upload_mock_contract(request, contractId, contractVersion);
      except Exception as e:
         print(e)
         return (True, None)
      return (False, "Duplicate upload should not be allowed.")

   def _test_versionUpload(self, request):
      contractId, contractVersion = self.upload_mock_contract(request)
      _, newContractVersion = self.upload_mock_contract(request, contractId)
      if (self.has_contract(request, contractId, newContractVersion) and
          newContractVersion != contractVersion):
         return (True, None)
      else:
         return (False, "Contract upload failed.");

   def _test_getAllVersions(self, request):
      contractId, contractVersion = self.upload_mock_contract(request)
      _, newContractVersion = self.upload_mock_contract(request, contractId)
      uri = '/api/athena/contracts/' + contractId
      expectedVersionCount = 2
      result = request.callContractAPI(uri, "")
      if (result["contract_id"] == contractId and
          len(result["versions"]) == expectedVersionCount):
         return (True, None)
      else:
         return (False,
                 "GET /api/athena/contracts/{} did not return correct response" \
                 " expected {} versions".format(contractId, expectedVersionCount))

   def _test_getVersion(self, request):
      contractId, contractVersion = self.upload_mock_contract(request)
      uri = '/api/athena/contracts/' + contractId \
            + '/versions/' + contractVersion
      result = request.callContractAPI(uri, "")
      if (result["contract_id"] == contractId and
          result["version"] == contractVersion):
         return (True, None)
      else:
         return (False,
                 "GET /api/athena/contracts/{}/versions/{} did not return" \
                 " correct response".format(contractId, contractVersion))

   def _mock_transaction(self, request):
      rpc = RPC(request._logDir,
                self.getName(),
                self._apiServerUrl)
      # do a transaction so that we have some block
      caller = "0x1111111111111111111111111111111111111111"
      to = "0x2222222222222222222222222222222222222222"
      response = rpc.sendTransaction(caller, "0x00", "0xffff", to, "0x01")
      response = rpc._getTransactionReceipt(response)
      return response;


   def _test_blockHash(self, request):
      txReceipt = self._mock_transaction(request)
      blockNumber = txReceipt['blockNumber']
      blockHash = txReceipt['blockHash']
      # query same block with hash and number and compare results
      block1 = request.getBlock("/api/athena/blocks/{}".format(blockNumber))
      block2 = request.getBlock("/api/athena/blocks/{}".format(blockHash))
      if (block1 == block2):
         return (True, None)
      return (False, "Block returned with block hash API doesn't match with block returned by block Number")


   def _test_invalidBlockHash(self, request):
      try:
         block = request.getBlock("/api/athena/blocks/0xbadbeef")
      except Exception as e:
         return(True, None)
      return (False, "invalid block hash exception should be thrown")


   def _test_getTransactionList(self, request):
      txReceipt = self._mock_transaction(request)
      txList = request.getTransactionList(count=1)
      txList = txList['transactions']
      if (len(txList) == 1 and
          txList[0]['hash'] == txReceipt['transactionHash']):
         return (True, None)
      return (False, "Trasaction list response did not follow count & latest parameters")

   def _test_transactionListFields(self, request):
      tr_count = 10
      first_tr = self._mock_transaction(request)
      for i in range(1, tr_count):
         tr = self._mock_transaction(request)

      txList = request.getTransactionList(count=tr_count - 1)
      for i in range(tr_count - 1):
         (present, missing) = self.requireFields(txList['transactions'][i],
                                                 ["from", "to", "value", "nonce", "hash", "url"]);
         if not present:
            return (False, "{} field is missing from response".format(missing))
      if first_tr['transactionHash'] not in txList['next']:
         return (False, "next field does not refer to correct next transaction")
      return (True, None)


   def _test_getTransactionListMaxSize(self, request):
      txReceipt = self._mock_transaction(request)
      txList = request.getTransactionList(count=1000)
      txList = txList['transactions']
      if (len(txList) < 1000 and
          txList[0]['hash'] == txReceipt['transactionHash']):
         return (True, None)
      return (False, "Trasaction list response should limit maximum number of transactions returned")

   def _test_getTransactionListInvalidLatest(self, request):
      txReceipt = self._mock_transaction(request)
      try:
         txList = request.getTransactionList(latest="0xabq")
      except Exception as e:
         return (True, None)
      return (False, "Error should be returned on invalid latest value")

   def _test_getTransactionListNextUrl(self, request):
      sentTrList = []
      tr_count = 10
      for i in range(tr_count):
         tr = self._mock_transaction(request)
         sentTrList.append(tr)
      sentTrList = list(map(lambda x : x['transactionHash'], sentTrList))
      sentTrList.reverse()

      receivedTrList1 = request.getTransactionList(count=int((tr_count / 2)))
      nextUrl = receivedTrList1['next']
      receivedTrList1 = list(map(lambda x : x['hash'], receivedTrList1['transactions']))

      if sentTrList[:5] != receivedTrList1:
         return (False, "transaction list query did not return correct transactions")

      receivedTrList2 = request.getNextTransactionList(nextUrl)
      receivedTrList2 = list(map(lambda x : x['hash'], receivedTrList2['transactions']))

      if sentTrList[5:] != receivedTrList2[:5]:
         return (False, "transaction list query did not return correct transactions")
      return (True, None)

   def requireFields(self, ob, fieldList):
      for f in fieldList:
         if not f in ob:
            return (False, f)
      return (True, None)
