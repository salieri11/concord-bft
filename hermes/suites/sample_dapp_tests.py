#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Tests covering Sample DApp compatibility.
#########################################################################


#########################################################################
# Example executions
# 1) Passing an endpoint runs tests in --noLaunch mode
# ./main.py SampleDAppTests --endpoint='https://mgmt.blockchain.vmware.com/
# blockchains/c3e4c911-9f9d-4899-9c92-6ced72d3ded3/api/concord/eth'
# --user='admin@blockchain.local' --password='Passw0rd!'
# 2) Passing no endpoint launches the product
# ./main.py SampleDAppTests
# 3) You can also pass username and/or password, skipping the endpoint;
# this runs tests on the locally launched product
#########################################################################

import logging
import os
import traceback
import subprocess
import requests

from . import test_suite
from util.auth import tokens

log = logging.getLogger(__name__)

class SampleDAppTests(test_suite.TestSuite):
   # Set in init based on whether an endpoint was passed in.
   _apiServerUrl = None

   _args = None
   _subPath = "/api/concord/eth"
   _userConfig = None
   _ethereumMode = False
   _productMode = True
   _resultFile = None
   _user = None
   _password = None

   def __init__(self, passedArgs, product):
      super(SampleDAppTests, self).__init__(passedArgs, product)
      self._args = passedArgs

      user = self._userConfig.get('product').get('db_users')[0]
      username = user['username']
      self._password = tokens["blockchain_service_dev"]["admin-blockchain-dev"]["all_roles"]["api_key"]

      if self._args.user != None:
         self._user = self._args.user
      else:
         self._user = username

      # Test does not launch the product if a URL is passed to it
      if self._args.endpoint != None:
         self._apiServerUrl = self._args.endpoint
         self._noLaunch = True
      else:
         self._apiServerUrl = self.inDockerReverseProxyApiBaseUrl + self._subPath

      if self._ethereumMode:
         self._noLaunch = True


   def getName(self):
      return "SampleDAppTests"


   def _runTest(self, testName, testFun, testLogDir):
      log.info("Starting test '{}'".format(testName))
      fileRoot = os.path.join(self._testLogDir, testName);
      return testFun(fileRoot)


   def run(self):
      ''' Runs all of the tests. '''
      log.info("Launching product...")
      self.launchProduct(self._args,
                         self._userConfig)
      self._cleanUp()
      tests = self._getTests()

      for (testName, testFun) in tests:
         testLogDir = os.path.join(self._testLogDir, testName)
         try:
            result, info = self._runTest(testName,
                                         testFun,
                                         testLogDir)
         except Exception as e:
            result = False
            info = str(e)
            traceback.print_tb(e.__traceback__)
            log.error("Exception running test: '{}'".format(info))

         if info:
            info += "  "
         else:
            info = ""

         relativeLogDir = self.makeRelativeTestPath(testLogDir)
         info += "Log: <a href=\"{}\">{}</a>".format(relativeLogDir,
                                                     testLogDir)
         self.writeResult(testName, result, info)

      log.info("Tests are done.")
      self._cleanUp()
      return super().run()


   def _getTests(self):
      return [("asset_transfer", self._test_asset_transfer),
              ("test_verify_contracts", self._test_supply_chain_and_verify_contracts)]

   def _executeInContainer(self, command):
      '''
          Execute command; check for errors and return (output, error) pair
      '''
      p = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE)
      data, err = p.communicate()
      out = data.strip().decode('utf-8')
      if err != None:
         return (None, err)
      elif 'Error' in out or 'error' in out:
         return (None, out)
      return (out, None)

   def _concatenatedExecuteInContainer(self, command1, command2):
      ''' Equivalent to the following steps :
             1. Execute command2
             2. If errors in 1., exit
             3. If non-empty output in 1., execute command1 with the output as STDIN
      '''
      out1, err1 =self. _executeInContainer(command2)
      if out1 != None and out1 != '':
         return self._executeInContainer(command1 + " " + out1)
      return (out1, err1)

   def _cleanUp(self):
      ''' Cleaning up the Docker changes the test(s) caused '''
      log.info("Cleaning up")
      self._concatenatedExecuteInContainer("docker stop","docker ps | grep asset_transfer | sed 's/|/ /' | awk '{print $1}'")
      self._concatenatedExecuteInContainer("docker rm -f", "docker ps -a | grep asset_transfer | sed 's/|/ /' | awk '{print $1}'")



   def _test_asset_transfer(self, fileRoot):
      ''' Tests if AssetTransfer can be deployed using the docker container '''

      contracts_before = requests.get(url = "http://" + self._user + ":" + self._password + "@localhost/api/concord/contracts").content

      env = self.product.docker_env

      asset_transfer_repo = env["asset_transfer_repo"]
      asset_transfer_tag = env["asset_transfer_tag"]
      cmd = "docker run --rm --name asset_transfer-test --network docker_default -td {0}:{1}".format(asset_transfer_repo, asset_transfer_tag)
      out, err = self._executeInContainer(cmd)
      if err != None:
         return (False, err)

      if self._apiServerUrl != '':
         pass_endpoint = self._apiServerUrl.replace('/','\/');

         # Edit placeholders with actual values inside the container
         log.debug("Replacing ADDRESS_PLACEHOLDER with {} in test/test_AssetTransfer.js.".format(pass_endpoint))
         comm = 'docker exec asset_transfer-test sed -i -e \'s/ADDRESS_PLACEHOLDER/' + pass_endpoint + '/g\' test/test_AssetTransfer.js'
         out1, err1 = self._executeInContainer(comm)
         if err1 != None:
            return (False, err1)

         log.debug("Replacing USER_PLACEHOLDER with {} in test/test_AssetTransfer.js.".format(self._user))
         out2, err2 = self._executeInContainer("docker exec asset_transfer-test sed -i -e 's/USER_PLACEHOLDER/" + self._user + "/g' test/test_AssetTransfer.js")
         if err2 != None:
            return (False, err2)

         log.debug("Replacing PASSWORD_PLACEHOLDER with {} in test/test_AssetTransfer.js.".format(self._password))
         out3, err3 = self._executeInContainer("docker exec asset_transfer-test sed -i -e 's/PASSWORD_PLACEHOLDER/" + self._password + "/g' test/test_AssetTransfer.js")
         if err3 != None:
            return (False, err3)

      # Run the test script(s)
      log.debug("Running 'docker exec asset_transfer-test mocha'")
      out, err = self._executeInContainer("docker exec asset_transfer-test mocha")
      log.debug("Done running 'docker exec asset_transfer-test mocha'.  err: '{}', out: '{}'".format(err, out))
      if err != None or out == "":
         return (False, err)

      contracts_after = requests.get(url = "http://" + self._user + ":" + self._password + "@localhost/api/concord/contracts").content

      if contracts_after == contracts_before:
         return (False, "Contracts have not changed after asset transfer deployment.")

      log.debug("Contracts changed after asset transfer deployment, as expected.")
      log.info(out)

      return (True, None)

   def _test_supply_chain_and_verify_contracts(self, fileRoot):
      # Cloning the github repo
      os.environ["NODE_TLS_REJECT_UNAUTHORIZED"] = "0"
      BC_URL = "http://helen:8080"

      contracts_before = requests.get(url = "http://" + self._user + ":" + self._password + "@localhost/api/concord/contracts").content

      # Changing the truffle-config.js file
      with open("../vmware-blockchain-samples/supply-chain/truffle-config.js", "r+") as file:
         lines = file.read()
         auth_str = "http://" + self._user + ":" + self._password + "@helen:8080/api/concord/eth"
         lines = lines.replace("http://<username>:<password>@<url>", auth_str)
         file.truncate(0)
         file.seek(0)
         file.write(lines)


      # Changing the docker-compose.yml file
      with open("../vmware-blockchain-samples/supply-chain/docker-compose.yml", "r+") as file:
         lines = file.read()
         lines = lines.replace("command: \"npm run start:vmware\"",
            "command: \"npm config set registry http://build-artifactory.eng.vmware.com:80/artifactory/api/npm/npm && npm run start:vmware\"")
         lines = lines.replace("<change-me>", BC_URL)
         file.truncate(0)
         file.seek(0)
         file.write(lines)

      # Changing the verify.js file
      with open("../vmware-blockchain-samples/supply-chain/verify/verify.js", "r+") as file:
         lines = file.read()
         lines = lines.replace("<username>", self._user)
         lines = lines.replace("<password>", self._password)
         lines = lines.replace("localhost", "helen")
         lines = lines.replace("443", "8080")
         lines = lines.replace("https", "http")
         lines = lines.replace("/blockchains/local/api/concord/contracts/", "/api/concord/contracts/")
         file.truncate(0)
         file.seek(0)
         file.write(lines)


      os.system("cd ../vmware-blockchain-samples/supply-chain && docker-compose -f docker-compose.yml -f docker-compose-local-network.yml up -d")
      output = subprocess.check_output("cd ../vmware-blockchain-samples/supply-chain && docker-compose -f docker-compose.yml -f docker-compose-local-network.yml run supply-chain npm run deploy_and_verify:vmware", shell = True, universal_newlines=True)

      contracts_after = requests.get(url = "http://" + self._user + ":" + self._password + "@localhost/api/concord/contracts").content

      if output.split("\n")[-4:-1] != ["statusCode: 200"] * 3:
         return (False, "Failure in npm run deploy_and_verify:vmware")

      if contracts_before == contracts_after:
         return (False, "Contracts haven't changed after supply chain deployment.")

      log.info(output)

      return (True, None)
