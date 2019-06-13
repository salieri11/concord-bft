#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Tests covering Helen's non-ethereum ReST API.
# (i.e. everything under /api/, excluding /api/concord/eth)
#########################################################################
import collections
import json
import logging
import os
import pickle
import pprint
import pytest
import traceback
import string
import subprocess
import random
import sys
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
   args = None
   _userConfig = None
   _ethereumMode = False
   _productMode = True
   _resultFile = None

   def __init__(self, passedArgs):
      super(HelenAPITests, self).__init__(passedArgs)

   def getName(self):
      return "HelenAPITests"

   def run(self):
      try:
         self.launchProduct(self._args,
                            self._userConfig)
      except Exception as e:
         log.error(traceback.format_exc())
         return self._resultFile

      if self._ethereumMode:
         info = "HelenAPITests are not applicable to ethereumMode."
         log.warn(info)
         self.writeResult("All tests", None, info)
         return self._resultFile

      os.environ["PYTHONDONTWRITEBYTECODE"] = "1"

      with open("pickled_helen_api_tests", "wb") as f:
         pickle.dump(self, f)

      # Notes on command line usage:
      # -m "performance and smoke" will run tests which are both performance and smoke.
      # -m performance -m smoke will run all peformance tests and all smoke tests.
      params = ["-s", "-v", "suites/helen", "--json", "report.json"]

      if self._args.tests:
         params += self._args.tests.split(" ")

      pytest.main(params)

      results = util.json_helper.readJsonFile("report.json")
      for testResult in results["report"]["tests"]:
         passed = None

         if testResult["outcome"] == "passed":
            passed = True
         elif testResult["outcome"] == "skipped":
            passed = "skipped"
         else:
            passed = False

         info = "" if passed else json.dumps(testResult, indent=2)
         testName = self.parsePytestTestName(testResult["name"])
         testLogDir = os.path.join(self._testLogDir, testName)
         relativeLogDir = self.makeRelativeTestPath(testLogDir)
         info += "\nLog: <a href=\"{}\">{}</a>".format(relativeLogDir,
                                                     testLogDir)
         self.writeResult(testResult["name"],
                          passed,
                          info)

      if self._shouldStop():
         self.product.stopProduct()

      return self._resultFile


   def validateLaunchConfig(self, dockerCfg):
      '''
      When the product is being launched, a Product can pass a docker config into
      a test suite to identify configuration issues before continuing with the launch.

      - If we're running tests which integrate Helen and Persephone, make sure we have
        a docker config that includes Persephone.
      - Be sure replaceable values in the Persephone config.json file have been replaced
        with real values.
      '''
      if self._args.deployNewBlockchain:
         foundPersephoneDockerConfig = False

         for key in dockerCfg["services"].keys():
            if "deploymentservice" in key:
               foundPersephoneDockerConfig = True
               provisioningConfig = None
               provisioningConfigFile = "resources/persephone/provisioning/config.json"
               invalidValues = ["<VMC_API_TOKEN>", "<DOCKERHUB_REPO_READER_PASSWORD>"]

               with open(provisioningConfigFile, "r") as f:
                  provisioningConfig = f.read()

               for invalid in invalidValues:
                  if invalid in provisioningConfig:
                     msg = "Invalid Persephone provisioning value(s) in {}.".format(provisioningConfigFile)
                     msg += "  Check instance(s) of: {}".format(invalidValues)
                     raise Exception(msg)

         if not foundPersephoneDockerConfig:
            raise Exception("Deploying a new blockchain was requested, but there is no "
                            "docker config for Persephone. It is likely you should add "
                            "this to your command: '"
                            "--dockerComposeFile ../docker/docker-compose.yml ../docker/docker-compose-persephone.yml'")


   def parsePytestTestName(self, parseMe):
      '''
      Pytest creates this long name; parse out the real name.
      If running the API tests from a parametrize, parseMe
      is a string such as "suites/helen/api_test.py::test_existing[user_login]".
      Otherwise, it's a string such as "suites/helen/api_test.py::test_blockchains_fields"
      '''
      return parseMe[parseMe.rindex(":")+1:]


   def _getTests(self):
      return [("getCerts", self._test_getCerts), \
              ("block_hash", self._test_blockHash), \
              ("invalid_block_hash", self._test_invalidBlockHash), \
              ("large_reply", self._test_largeReply), \
              ("get_multiple_users", self._test_get_multiple_users), \
              ("create_user", self._test_createUser), \
              ("get_non_existing_user", self._test_get_non_existing_user), \
              ("user_login", self._test_user_login), \
              ("user_patch", self._test_patch_user)
      ]


   # Tests: expect one argument, a Request, and produce a 2-tuple
   # (bool success, string info)
   def _resumeMembers(self, members):
      '''
      Given a list of items returned from the members call, unpauses them.
      Note that this currently assumes all members are running locally in
      docker.  We don't have multiple VM infra going yet, so we don't know
      how this method will have to change when that is in place.
      '''
      for m in members:
         concordIndex = int(m["hostname"][len("replica"):]) + 1
         self.product.resume_concord_replica(concordIndex)


   def _test_getCerts(self, fx, fxBlockchain):
      '''
      Test that if we pass "?certs=true" to the Members endpoint, we get at
      least one non-empty rpc_cert in the response.
      '''
      blockchains = fx.request.getBlockchains()
      result = fx.request.getMemberList(blockchains[0]["id"],certs=True)

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

   def random_string_generator(self, size=6, chars=string.ascii_uppercase + string.digits, mustNotMatch=None):
      while True:
         ret = ''.join(random.choice(chars) for _ in range(size))

         if ret != mustNotMatch:
            return ret

   def upload_contract(self, blockchainId, request, sourceFile, contractName,
                       contractId=None,
                       contractVersion=None,
                       fromAddr=None,
                       compilerVersion=None,
                       ctorParams=None,
                       optimize=None,
                       runs=None,
                       generateDefaults=True):
      '''
      Uploads a contract.
      request: A Hermes REST request object.
      sourceFile: Path to the source code.  e.g. "resources/contracts/Counter.sol"
      contractName: The name of the contract, located in sourceFile.
      contractId/Version: Id and version the contract will have in Helen.
      fromAddr: The contract owner.
      compilerVersion: Not sure where available strings come from.
      ctorParams: Contract constructor parameters
      isOptimize: Whether the compiler service should optimize
      runs: Helen passes this to the compiler service when optimizing. I don't know
         what it does or what valid values are.  Helen defaults to 200 if it is
         missing.
      generateDefaults: Whether to generate default values.  Set to False if, for
         example, you want to test sending a request with fields missing.
      '''
      data = {}

      if generateDefaults:
         contractId = contractId or self.random_string_generator()
         contractVersion = contractVersion or self.random_string_generator()
         fromAddr = fromAddr or "0x1111111111111111111111111111111111111111"
         compilerVersion = compilerVersion or "v0.5.2+commit.1df8f40c"
         ctorParams = ctorParams or ""
         optimize = optimize or False
         runs = runs or "200"

      if fromAddr != None:
         data["from"] = fromAddr
      if contractId != None:
         data["contract_id"] = contractId
      if contractVersion != None:
         data["version"] = contractVersion
      if contractName != None:
         data["contract_name"] = contractName
      if  ctorParams != None:
         data["constructor_params"] = ctorParams
      if compilerVersion != None:
         data["compiler_version"] = compilerVersion
      if optimize != None:
         data["is_optimize"] = optimize
      if runs != None:
         data["runs"] = runs

      if sourceFile:
         with open(sourceFile, 'r') as f:
            data["sourcecode"] = f.read()

      result = request.uploadContract(blockchainId, data)
      return result


   def upload_hello_contract(self, blockchainId, request):
      '''
      Uploads the HelloWorld contract with general settings. This is
      handy when you need blocks with contracts but don't really care
      about the fine details.
      Returns the contract ID and version.  Throws an exception if
      it fails.
      '''
      contractId = self.random_string_generator()
      contractVersion = self.random_string_generator()
      result = self.upload_contract(blockchainId, request,
                                    "resources/contracts/HelloWorld.sol",
                                    "HelloWorld",
                                    contractId = contractId,
                                    contractVersion = contractVersion,
                                    generateDefaults=True)
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

   # RV, May 8, 2019: Commenting out.  This is not testing the Helen API.
   # It should go somewhere, but not here.
   # def _test_contractTx(self, request):
   #    '''Sends a transaction to a contract that has been uploaded.

   #    This may look a little weird, because the function in the test
   #    contract is pure, can could therefor be called instead of sent
   #    to. This was created as a regression check
   #    (vmwathena/athena#122). The important thing the transaction does
   #    is cause evmjit to allocate some data that needs to be
   #    released. The pointer to this release function was being stored
   #    in a way that confused transaction hashing, and thus confused
   #    SBFT (responses from nodes didn't match).
   #    '''
   #    contractId, contractVersion = self.upload_mock_contract(request)
   #    result = request.callContractAPI('/api/concord/contracts/' + contractId
   #                                     + '/versions/' + contractVersion, "")
   #    rpc = RPC(request.logDir,
   #              self.getName(),
   #              self.ethrpcApiUrl,
   #              self._userConfig)
   #    txres = rpc.sendTransaction("0x1111111111111111111111111111111111111111",
   #                                "0x19ff1d21", # HelloWorld.sol's hello function
   #                                to=result["address"])
   #    # do we have a better check for "got a transaction receipt"?
   #    if len(txres) == 66:
   #       return (True, None)
   #    else:
   #       return (False, "Transaction send to uploaded contract failed")

   def _mock_transaction(self, request, data = "0x00", ethrpcNode = None, nonce = None):
      ethrpcApiUrl = ethrpcNode if ethrpcNode else self.ethrpcApiUrl
      rpc = RPC(request.logDir,
                self.getName(),
                ethrpcApiUrl,
                self._userConfig)
      # do a transaction so that we have some block
      caller = "0x1111111111111111111111111111111111111111"
      to = "0x2222222222222222222222222222222222222222"
      response = rpc.sendTransaction(caller, data, "0xffff", to, "0x01")
      response = rpc._getTransactionReceipt(response)
      return response;


   def _test_blockHash(self, fx, fxBlockchain):
      txReceipt = self._mock_transaction(fx.request)
      blockNumber = txReceipt['blockNumber']
      blockHash = txReceipt['blockHash']
      # query same block with hash and number and compare results
      block1 = fx.request.getBlockByUrl("/api/concord/blocks/{}".format(int(blockNumber, 16)))
      block2 = fx.request.getBlockByUrl("/api/concord/blocks/{}".format(blockHash))
      if (block1 == block2):
         return (True, None)
      return (False, "Block returned with block hash API doesn't match with block returned by block Number")


   def _test_invalidBlockHash(self, fx, fxBlockchain):
      try:
         block = fx.request.getBlockByUrl("/api/concord/blocks/0xbadbeef")
      except Exception as e:
         return(True, None)
      return (False, "invalid block hash exception should be thrown")

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
      data['role'] = 'vmbc-system:admin'
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


   def _test_createUser(self, fx, fxBlockchain):
      mock = self._get_mock_user_data()
      response = self._create_mock_user(fx.request, mock)
      print(response)
      user_id = response['user_id']
      user = self._get_user(fx.request, user_id)
      milliseconds_since_epoch = 0
      if (mock['name'] == user['name'] and mock['email'] == user['email']
          and mock['details']['first_name'] == user['details']['first_name']
          and mock['details']['last_name'] == user['details']['last_name']
          and mock['role'] == user['role'] and user['last_login'] == milliseconds_since_epoch):
         return (True, None)
      else:
         return (False, "Returned valeus don't match with mock values")

   def _test_get_non_existing_user(self, fx, fxBlockchain):
      response = self._create_mock_user(fx.request)
      user_id = response['user_id']
      response = self._get_user(fx.request, str(uuid4()))
      if "error_message" in response:
         return (True, None)
      return (False, "Incorrect response for invalid user")

   def _test_user_login(self, fx, fxBlockchain):
      user = self._userConfig.get('product').get('db_users')[0]
      username = user['username']
      password = user['password']
      loginData = {}
      loginData['email'] = username
      loginData['password'] = password

      response = fx.request.callUserAPI("/auth/login", data=loginData)
      after = int(round(time.time() * 1000))
      user = self._get_user(fx.request, response['user_id'])
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

   def _test_patch_user(self, fx, fxBlockchain):
      response = self._create_mock_user(fx.request);
      user_id = response['user_id']
      patchData = {}
      patchData['email'] = 'patched@email.com'
      fx.request.callUserAPI("/users/{}/".format(user_id), "PATCH", data=patchData)
      user = self._get_user(fx.request, user_id)
      if user['email'] != 'patched@email.com':
         return (False, "Patch didn't update email")

      patchData = {}
      patchData['name'] = 'patchedName'
      fx.request.callUserAPI("/users/{}/".format(user_id), "PATCH", data=patchData)
      user = self._get_user(fx.request, user_id)
      if user['name'] != 'patchedName':
         return (False, "Patch didn't update name")

      patchData = {}
      patchData['role'] = 'vmbc-org:user'
      fx.request.callUserAPI("/users/{}/".format(user_id), "PATCH", data=patchData)
      user = self._get_user(fx.request, user_id)
      if user['role'] != 'vmbc-org:user':
         return (False, "Patch didn't update role")


      patchData = {}
      details = {}
      details['first_name'] = 'patchedFirstName'
      details['last_name'] = 'patchedLastName'
      patchData['details'] = details
      fx.request.callUserAPI("/users/{}/".format(user_id), "PATCH", data=patchData)
      user = self._get_user(fx.request, user_id)
      if (user['details']['first_name'] != 'patchedFirstName' and
         user['details']['last_name'] != 'patchedLastName'):
         return (False, "Patch didn't update details")

      return (True, None)

   def _test_get_multiple_users(self, fx, fxBlockchain):
      created_user_id = []
      user_count = 5
      for i in range(1, user_count):
         response = self._create_mock_user(fx.request);
         created_user_id.append(response['user_id'])
      user = self._get_user(fx.request, created_user_id[0])
      params = "?consortium={}&organization={}".format(user['consortium']['consortium_id'],
                                                      user['organization']['organization_id'])
      user_list = fx.request.callUserAPI("/users/", params=params)
      user_list = list(map(lambda u : u['user_id'], user_list))
      if not all(u in user_list for u in created_user_id):
         return (False, "All created users in consortium+organization not returned")

      # also try with no consortium and organization specified
      all_user_list = fx.request.callUserAPI("/users/")
      all_user_list = list(map(lambda u : u['user_id'], all_user_list))
      if not all(u in all_user_list for u in created_user_id):
         return (False, "All created users not returned")

      return (True, None)


   def _test_largeReply(self, fx, fxBlockchain):
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
         tr = self._mock_transaction(fx.request, data=largeContract)
         sentTrList.append(tr)
      sentTrList = list(map(lambda x : x['transactionHash'], sentTrList))
      sentTrList.reverse()

      receivedTrList = fx.request.getTransactionList(fxBlockchain.blockchainId, count=tr_count)
      receivedTrHashes = list(map(lambda x : x['hash'], receivedTrList['transactions']))
      receivedDataSum = sum(len(x['input']) for x in receivedTrList['transactions'])

      if sentTrList != receivedTrHashes:
         return (False, "transaction list query did not return correct transactions")

      expectedDataSum = len(largeContract)*tr_count
      if receivedDataSum != expectedDataSum:
         return (False, "received only %d bytes, but expected %d" % (receviedDataSum, expectedDataSum))
      return (True, None)
