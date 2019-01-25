#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Tests covering Helen's non-ethereum ReST API.
# (i.e. everything under /api/, excluding /api/concord/eth)
#########################################################################
import collections
import json
import logging
import os
import traceback
import string
import random
import time
from time import sleep

from . import test_suite
from util.product import Product
from rest.request import Request
from rpc.rpc_call import RPC
from uuid import uuid4

import util.json_helper

log = logging.getLogger(__name__)


class HelenAPITests(test_suite.TestSuite):
   _args = None
   _apiBaseServerUrl = None
   _userConfig = None
   _ethereumMode = False
   _productMode = True
   _resultFile = None

   def __init__(self, passedArgs):
      super(HelenAPITests, self).__init__(passedArgs)
      self._apiBaseServerUrl = passedArgs.baseUrl

   def getName(self):
      return "HelenAPITests"

   def run(self):
      ''' Runs all of the tests. '''
      if self._productMode and not self._noLaunch:
         try:
            p = self.launchProduct(self._args,
                                   self._apiBaseServerUrl + "/api/concord/eth/",
                                   self._userConfig)
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

      if self._shouldStop():
         p.stopProduct()

      return self._resultFile

   def _runRestTest(self, testName, testFun, testLogDir):
      log.info("Starting test '{}'".format(testName))
      request = Request(testLogDir,
                        testName,
                        self._apiBaseServerUrl,
                        self._userConfig)
      return testFun(request)

   def _getTests(self):
      return [("getMembers", self._test_getMembers), \
              ("getCerts", self._test_getCerts), \
              ("blockList", self._test_getBlockList), \
              ("block", self._test_getBlocks), \
              ("transaction", self._test_getTransactions), \
              ("contract_upload", self._test_contractUpload), \
              ("multiple_contract_upload", self._test_multiContractUpload), \
              ("contract_tx", self._test_contractTx), \
              ("contract_call", self._test_contractCall), \
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
              ("get_transaction_list_next_url", self._test_getTransactionListNextUrl), \
              ("large_reply", self._test_largeReply), \
              ("get_multiple_users", self._test_get_multiple_users), \
              ("create_user", self._test_createUser), \
              ("get_non_existing_user", self._test_get_non_existing_user), \
              ("user_login", self._test_user_login), \
              ("user_patch", self._test_patch_user)]


   # Tests: expect one argument, a Request, and produce a 2-tuple
   # (bool success, string info)

   def _test_getMembers(self, request):
      result = request.getMemberList()

      if not type(result) is list:
         return (False, "Response was not a list")
      if len(result) < 1:
         return (False, "No members returned")

      for m in result:
         (present, missing) = self.requireFields(m, ["hostname", "status"])
         if not present:
            return (False, "No '{}' field in member entry.".format(missing))

         if not isinstance(m["hostname"], str):
            return (False, "'hostname' field in member entry is not a string")
         if not isinstance(m["status"], str):
            return (False, "'status' field in member entry is not a string")
         if not isinstance(m["address"], str):
            return (False, "'address' field in member entry is not a string")
         if not isinstance(m["millis_since_last_message"], int):
            return (False, "'millis_since_last_message' field in member entry is not a string")
         if not isinstance(m["millis_since_last_message_threshold"], int):
            return (False, "'millis_since_last_message_threshold' field in member entry is not a string")
         if not isinstance(m["rpc_url"], str):
            return (False, "'rpc_url' field in member entry is not a string")
         if m["rpc_url"] == "":
            return (False, "'rpc_url' field in member entry is empty string")
         if "rpc_cert" in m:
            return (False, "'rpc_cert' field should not be included if certs=true is not passed")

      return (True, None)

   def _test_getCerts(self, request):
      '''
      Test that if we pass "?certs=true" to the Members endpoint, we get at
      least one non-empty rpc_cert in the response.
      '''
      result = request.getMemberList(certs=True)

      if not type(result) is list:
         return (False, "Response was not a list")
      if len(result) < 1:
         return (False, "No members returned")

      foundACert = False
      for m in result:
         # rpc_cert must be present
         if not isinstance(m["rpc_cert"], str):
            return (False, "'rpc_cert' field in member entry is not a string")
         if m["rpc_cert"] != "":
            foundACert = True

      if foundACert:
         return (True, None)
      else:
         return (False, "no non-empty rpc_cert found in response")

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
            ["number", "hash", "parentHash", "nonce", "size", "transactions",
             "timestamp"])
         if not present:
            return (False, "No '{}' field in block response.".format(missing))

         if not b["number"] == blockResult["number"]:
            return (False, "Block number does not match request.")
         if not type(blockResult["transactions"]) is list:
            return (False, "'transactions' field is not a list.")

      # try to get an uncommitted block, and make sure an error comes back
      uncommitted = 1 + result["blocks"][0]["number"]
      try:
         uncomresult = request.getBlockByNumber(uncommitted)
         return (False,
                 "Expected an error for future block {}, " \
                 "but received block {}".format(uncommitted,
                                                uncomresult["number"]))
      except:
         # It is expected that requesting an uncommitted block will fail
         pass

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

   def contract_upload_util_generic(self,request,contractId,
    contractVersion,sourceCode, fromAddr, contractName, ctorParams):

      data = {}
      data["from"] = fromAddr
      data["contract_id"] = contractId
      data["version"] = contractVersion
      data["sourcecode"] = sourceCode
      data["contractName"] = contractName
      data["constructorParams"] = ctorParams
      return request.uploadContract(data)

   def contract_upload_util(self, request, contractId, contractVersion, sourceCode):
      '''
      A helper method to upload simple hello world contract.
      '''
      return self.contract_upload_util_generic(request, contractId, contractVersion, sourceCode,"0x1111111111111111111111111111111111111111", "HelloWorld", "")

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
      if "url" in result:
         return (contractId, contractVersion)
      else:
         raise Exception("Contract upload failed with error '{}'".format(result["error"]))

   def upload_mock_multiple_contract(self, request, contractId=None,
                            contractVersion=None):
      if contractId is None:
         contractId = self.random_string_generator()
      if contractVersion is None:
         contractVersion = self.random_string_generator()

      # This file contains two contract definitions, 'HelloWorld' and 'DummyContract'.
      # The contract_upload_util function sets the required contractName parameter to 'HelloWorld', thereby
      # specifying 'HelloWorld' as the contract to compile.
      contractFile = open("resources/contracts/HelloWorldMultiple.sol", 'r')
      result = self.contract_upload_util(request, contractId, contractVersion,
                                         contractFile.read())
      if "url" in result:
         return (contractId, contractVersion)
      else:
         raise Exception("Contract upload failed with error '{}'".format(result["error"]))

   def has_contract(self, request, contractId, contractVersion):
      result = request.callContractAPI('/api/concord/contracts/' + contractId
                                       + '/versions/' + contractVersion, "")
      try:
         if (result["contract_id"] == contractId and
             result["version"] == contractVersion):
            return True
      except Exception as e:
         print(e)
         return False

   def has_contract_with_bytecode(self, request, contractId, contractVersion, contractBytecode):
      result = request.callContractAPI('/api/concord/contracts/' + contractId
                                       + '/versions/' + contractVersion, "")
      # Remove swarm hash from deployed contract bytecode
      parsedBytecode = result["bytecode"].split("a165627a7a72305820")[0]
      try:
         if (result["contract_id"] == contractId and
             result["version"] == contractVersion and
             parsedBytecode == contractBytecode):
            return True
         else:
            return False
      except Exception as e:
         print(e)
         return False

   def _test_contractUpload(self, request):
      contractId, contractVersion = self.upload_mock_contract(request)
      result = request.callContractAPI('/api/concord/contracts/' + contractId
                                       + '/versions/' + contractVersion, "")
      if self.has_contract(request, contractId, contractVersion):
         return (True, None)
      else:
         return (False, "Unable to retrieve uploaded contract")

   def _test_multiContractUpload(self, request):
      contractBytecode_0_4_19 = ('608060405234801561001057600080fd5b5061013f806100'
      '206000396000f300608060405260043610610041576000357c0100000000000000000000000'
      '000000000000000000000000000000000900463ffffffff16806319ff1d2114610046575b60'
      '0080fd5b34801561005257600080fd5b5061005b6100d6565b6040518080602001828103825'
      '283818151815260200191508051906020019080838360005b8381101561009b578082015181'
      '840152602081019050610080565b50505050905090810190601f1680156100c857808203805'
      '16001836020036101000a031916815260200191505b509250505060405180910390f35b6060'
      '6040805190810160405280600d81526020017f48656c6c6f2c20576f726c642100000000000'
      '0000000000000000000000000008152509050905600')

      contractBytecode_0_5 = ('608060405234801561001057600080fd5b50610139806100206'
      '000396000f3fe60806040526004361061003b576000357c0100000000000000000000000000'
      '0000000000000000000000000000009004806319ff1d2114610040575b600080fd5b3480156'
      '1004c57600080fd5b506100556100d0565b6040518080602001828103825283818151815260'
      '200191508051906020019080838360005b83811015610095578082015181840152602081019'
      '05061007a565b50505050905090810190601f1680156100c257808203805160018360200361'
      '01000a031916815260200191505b509250505060405180910390f35b6060604080519081016'
      '0405280600d81526020017f48656c6c6f2c20576f726c642100000000000000000000000000'
      '00000000000081525090509056fe')

      contractBytecode_0_5_2 = ('608060405234801561001057600080fd5b506101398061002'
      '06000396000f3fe608060405234801561001057600080fd5b5060043610610048576000357c'
      '01000000000000000000000000000000000000000000000000000000009004806319ff1d211'
      '461004d575b600080fd5b6100556100d0565b60405180806020018281038252838181518152'
      '60200191508051906020019080838360005b838110156100955780820151818401526020810'
      '1905061007a565b50505050905090810190601f1680156100c2578082038051600183602003'
      '6101000a031916815260200191505b509250505060405180910390f35b60606040805190810'
      '160405280600d81526020017f48656c6c6f2c20576f726c6421000000000000000000000000'
      '0000000000000081525090509056fe')

      contractId, contractVersion = self.upload_mock_multiple_contract(request)

      result = request.callContractAPI('/api/concord/contracts/' + contractId
                                       + '/versions/' + contractVersion, "")
      if self.has_contract_with_bytecode(request, contractId, contractVersion, contractBytecode_0_4_19) \
         or self.has_contract_with_bytecode(request, contractId, contractVersion, contractBytecode_0_5) \
         or self.has_contract_with_bytecode(request, contractId, contractVersion, contractBytecode_0_5_2):
         return (True, None)
      else:
         return (False, "Unable to retrieve uploaded contract")

   def _test_contractTx(self, request):
      '''Sends a transaction to a contract that has been uploaded.

      This may look a little weird, because the function in the test
      contract is pure, can could therefor be called instead of sent
      to. This was created as a regression check
      (vmwathena/athena#122). The important thing the transaction does
      is cause evmjit to allocate some data that needs to be
      released. The pointer to this release function was being stored
      in a way that confused transaction hashing, and thus confused
      SBFT (responses from nodes didn't match).
      '''
      contractId, contractVersion = self.upload_mock_contract(request)
      result = request.callContractAPI('/api/concord/contracts/' + contractId
                                       + '/versions/' + contractVersion, "")
      rpc = RPC(request._logDir,
                self.getName(),
                self._apiServerUrl,
                self._userConfig)
      txres = rpc.sendTransaction("0x1111111111111111111111111111111111111111",
                                  "0x19ff1d21", # HelloWorld.sol's hello function
                                  to=result["address"])
      # do we have a better check for "got a transaction receipt"?
      if len(txres) == 66:
         return (True, None)
      else:
         return (False, "Transaction send to uploaded contract failed")

   def _test_contractCall(self, request):
      '''Calls a contract that has been uploaded.

      This test makes a *call* instead of sending a transaction.
      '''
      contractId, contractVersion = self.upload_mock_contract(request)
      result = request.callContractAPI('/api/concord/contracts/' + contractId
                                       + '/versions/' + contractVersion, "")
      rpc = RPC(request._logDir,
                self.getName(),
                self._apiServerUrl,
                self._userConfig)
      txres = rpc.callContract(result["address"],
                               "0x19ff1d21") # HelloWorld.sol's hello function

      # "Hello, World!" hex-encoded ASCII
      expected = "48656c6c6f2c20576f726c6421"

      # The actual result contains some prefix details, but just
      # assert that the expected content is in there somewhere.
      if txres.find(expected) >= 0:
         return (True, None)
      else:
         return (False, "Contract call failed")

   def _test_getAllContracts(self, request):
      result = request.callContractAPI('/api/concord/contracts', "")
      existingCount = len(result)
      contractId, contractVersion = self.upload_mock_contract(request)
      result = request.callContractAPI('/api/concord/contracts', "")
      newCount = len(result)
      result = result[0]
      if (self.has_contract(request, contractId, contractVersion) and
          existingCount + 1 == newCount):
         return (True, None)
      else:
         return (False,
                 "GET /api/concord/contracts did not return correct response." \
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
      uri = '/api/concord/contracts/' + contractId
      expectedVersionCount = 2
      result = request.callContractAPI(uri, "")
      if (result["contract_id"] == contractId and
          len(result["versions"]) == expectedVersionCount):
         return (True, None)
      else:
         return (False,
                 "GET /api/concord/contracts/{} did not return correct response" \
                 " expected {} versions".format(contractId, expectedVersionCount))

   def _test_getVersion(self, request):
      contractId, contractVersion = self.upload_mock_contract(request)
      uri = '/api/concord/contracts/' + contractId \
            + '/versions/' + contractVersion
      result = request.callContractAPI(uri, "")
      if (result["contract_id"] == contractId and
          result["version"] == contractVersion):
         return (True, None)
      else:
         return (False,
                 "GET /api/concord/contracts/{}/versions/{} did not return" \
                 " correct response".format(contractId, contractVersion))

   def _mock_transaction(self, request, data = "0x00"):
      rpc = RPC(request._logDir,
                self.getName(),
                self._apiServerUrl,
                self._userConfig)
      # do a transaction so that we have some block
      caller = "0x1111111111111111111111111111111111111111"
      to = "0x2222222222222222222222222222222222222222"
      response = rpc.sendTransaction(caller, data, "0xffff", to, "0x01")
      response = rpc._getTransactionReceipt(response)
      return response;


   def _test_blockHash(self, request):
      txReceipt = self._mock_transaction(request)
      blockNumber = txReceipt['blockNumber']
      blockHash = txReceipt['blockHash']
      # query same block with hash and number and compare results
      block1 = request.getBlock("/api/concord/blocks/{}".format(blockNumber))
      block2 = request.getBlock("/api/concord/blocks/{}".format(blockHash))
      if (block1 == block2):
         return (True, None)
      return (False, "Block returned with block hash API doesn't match with block returned by block Number")


   def _test_invalidBlockHash(self, request):
      try:
         block = request.getBlock("/api/concord/blocks/0xbadbeef")
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


   def email_generator(self, size=6, chars=string.ascii_uppercase + string.digits):
      return ''.join(random.choice(chars) for _ in range(size))


   def _get_mock_user_data(self):
      data = {}
      details = {}
      details['first_name'] = 'FirstName'
      details['last_name'] = 'LastName'
      data['name'] = 'Mock Name'
      data['email'] = self.email_generator() + '@email.com'
      data['details'] = details
      data['password'] = 'root'
      data['role'] = 'SYSTEM_ADMIN'
      return data


   def _create_mock_user(self, request, values=None):
      data = {}

      if values is None:
         values = {}
      mock_data = self._get_mock_user_data()

      # fill in the value from values dir if such key exists otherwise
      # fill in default value
      data['name'] = values.get('name', mock_data['name'])
      data['email'] = values.get('email', mock_data['email'])
      data['details'] = values.get('details', mock_data['details'])
      data['password'] = values.get('password', mock_data['password'])
      data['role'] = values.get('role', mock_data['role'])

      # TODO: We must also include organization ID and consortium ID
      # but until organization/consortium creation API is available helen will
      # create a default organization/consortium for us.
      return request.callUserAPI('/users/', data=data)


   def _get_user(self, request, user_id):
      return request.callUserAPI("/users/{}/".format(user_id))


   def _test_createUser(self, request):
      mock = self._get_mock_user_data()
      response = self._create_mock_user(request, mock)
      print(response)
      user_id = response['user_id']
      user = self._get_user(request, user_id)
      milliseconds_since_epoch = 0
      if (mock['name'] == user['name'] and mock['email'] == user['email']
          and mock['details']['first_name'] == user['details']['first_name']
          and mock['details']['last_name'] == user['details']['last_name']
          and mock['role'] == user['role'] and user['last_login'] == milliseconds_since_epoch):
         return (True, None)
      else:
         return (False, "Returned valeus don't match with mock values")

   def _test_get_non_existing_user(self, request):
      response = self._create_mock_user(request)
      user_id = response['user_id']
      response = self._get_user(request, str(uuid4()))
      if not response:
         return (True, None)
      return (False, "Incorrect response for invalid user")

   def _test_user_login(self, request):
      user = self._userConfig.get('product').get('db_users')[0]
      username = user['username']
      password = user['password']
      loginData = {}
      loginData['email'] = username
      loginData['password'] = password

      response = request.callUserAPI("/auth/login", data=loginData)
      after = int(round(time.time() * 1000))
      user = self._get_user(request, response['user_id'])
      message = ''
      # Newly created users last_login value will return 0
      # to signify they are a new user
      has_token = response.get('token', None) is not None
      has_refresh_token = response.get('refresh_token', None) is not None
      has_token_expires = response.get('token_expires', None) == 1800000
      last_login_correct = response['last_login'] == 0 and user['last_login'] < after

      if has_token == False:
        message += "Token isn't correct. "
      if not has_refresh_token:
        message += "Refresh token isn't correct. "
      if not has_token_expires:
        message += "Token expires isn't correct. "
      if not last_login_correct:
        message += "Last login timestamp not updated correctly. "

      if (has_token and has_refresh_token
            and has_token_expires and last_login_correct):
         return (True, None)

      return (False, message)

   def _test_patch_user(self, request):
      response = self._create_mock_user(request);
      user_id = response['user_id']
      patchData = {}
      patchData['email'] = 'patched@email.com'
      request.callUserAPI("/users/{}/".format(user_id), "PATCH", data=patchData)
      user = self._get_user(request, user_id)
      if user['email'] != 'patched@email.com':
         return (False, "Patch didn't update email")

      patchData = {}
      patchData['name'] = 'patchedName'
      request.callUserAPI("/users/{}/".format(user_id), "PATCH", data=patchData)
      user = self._get_user(request, user_id)
      if user['name'] != 'patchedName':
         return (False, "Patch didn't update name")

      patchData = {}
      patchData['role'] = 'ORG_USER'
      request.callUserAPI("/users/{}/".format(user_id), "PATCH", data=patchData)
      user = self._get_user(request, user_id)
      if user['role'] != 'ORG_USER':
         return (False, "Patch didn't update role")


      patchData = {}
      details = {}
      details['first_name'] = 'patchedFirstName'
      details['last_name'] = 'patchedLastName'
      patchData['details'] = details
      request.callUserAPI("/users/{}/".format(user_id), "PATCH", data=patchData)
      user = self._get_user(request, user_id)
      if (user['details']['first_name'] != 'patchedFirstName' and
         user['details']['last_name'] != 'patchedLastName'):
         return (False, "Patch didn't update details")

      return (True, None)

   def _test_get_multiple_users(self, request):
      created_user_id = []
      user_count = 5
      for i in range(1, user_count):
         response = self._create_mock_user(request);
         created_user_id.append(response['user_id'])
      user = self._get_user(request, created_user_id[0])
      params = "?consortium={}&organization={}".format(user['consortium']['consortium_id'],
                                                      user['organization']['organization_id'])
      user_list = request.callUserAPI("/users/", params=params)
      user_list = list(map(lambda u : u['user_id'], user_list))
      if not all(u in user_list for u in created_user_id):
         return (False, "All created users in consortium+organization not returned")

      # also try with no consortium and organization specified
      all_user_list = request.callUserAPI("/users/")
      all_user_list = list(map(lambda u : u['user_id'], all_user_list))
      if not all(u in all_user_list for u in created_user_id):
         return (False, "All created users not returned")

      return (True, None)


   def _test_largeReply(self, request):
      ### 1. Create three contracts, each 16kb in size
      ### 2. Request latest transaction list
      ### 3. Reply will be 48k+ in size
      ###
      ### This will require the highest bit in the size prefix of the
      ### concord->Helen response to be set, which makes the length look
      ### negative to Java's signed short type. If we get a response, then
      ### HEL-34 remains fixed.

      # Contract bytecode that is 16kb
      largeContract = "0x"+("aa" * 16384)

      sentTrList = []
      tr_count = 3
      for i in range(tr_count):
         tr = self._mock_transaction(request, data=largeContract)
         sentTrList.append(tr)
      sentTrList = list(map(lambda x : x['transactionHash'], sentTrList))
      sentTrList.reverse()

      receivedTrList = request.getTransactionList(count=tr_count)
      receivedTrHashes = list(map(lambda x : x['hash'], receivedTrList['transactions']))
      receivedDataSum = sum(len(x['input']) for x in receivedTrList['transactions'])

      if sentTrList != receivedTrHashes:
         return (False, "transaction list query did not return correct transactions")

      expectedDataSum = len(largeContract)*tr_count
      if receivedDataSum != expectedDataSum:
         return (False, "received only %d bytes, but expected %d" % (receviedDataSum, expectedDataSum))
      return (True, None)
