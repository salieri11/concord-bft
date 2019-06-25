#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Tests covering Helen's non-ethereum ReST API.
# (i.e. everything under /api/, excluding /api/concord/eth)
#########################################################################
import json
import logging
import os
import pickle
import pytest
import traceback
import string
import subprocess
import random
import util.json_helper

from . import test_suite
from rpc.rpc_call import RPC

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


   # Tests: expect one argument, a Request, and produce a 2-tuple
   # (bool success, string info)
   def resumeMembers(self, members):
      '''
      Given a list of items returned from the members call, unpauses them.
      Note that this currently assumes all members are running locally in
      docker.  We don't have multiple VM infra going yet, so we don't know
      how this method will have to change when that is in place.
      '''
      for m in members:
         concordIndex = int(m["hostname"][len("replica"):]) + 1
         self.product.resume_concord_replica(concordIndex)


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

   def mock_transaction(self, rpc, data = "0x00"):
      # Do a transaction so that we have some blocks
      caller = "0x1111111111111111111111111111111111111111"
      to = "0x2222222222222222222222222222222222222222"
      response = rpc.sendTransaction(caller, data, "0xffff", to, "0x01")
      response = rpc._getTransactionReceipt(response)
      return response;
