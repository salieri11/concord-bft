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

import pytest
import util.hermes_logging
import collections

from fixtures.common_fixtures import fxHermesRunSettings, fxProduct, fxConnection,fxBlockchain
from suites.case import describe
from util.auth import tokens

import util.hermes_logging

log = util.hermes_logging.getMainLogger()

LocalSetupfixture = collections.namedtuple("LocalSetupfixture",["user","password","apiServerUrl","testName","testLogDir","docker_env"])

@pytest.fixture
@describe("fixture; Initial Setup")
def fxLocalSetup(fxHermesRunSettings, fxProduct, fxConnection,fxBlockchain):
   testName = fxConnection.request.testName
   args = fxHermesRunSettings["hermesCmdlineArgs"]
   userConfig = fxHermesRunSettings["hermesUserConfig"]
   subpath="/api/concord/eth"
   password = tokens["blockchain_service_dev"]["admin-blockchain-dev"]["all_roles"]["api_key"]

   if args.user == None:
      user=userConfig.get('product').get('db_users')[0].get('username')
   else:
      user=args.user

   if args.endpoint != None:
      apiServerUrl = args.endpoint 
   else:
      apiServerUrl = args.inDockerReverseProxyApiBaseUrl + subpath

   testLogDir = os.path.join(fxHermesRunSettings["hermesTestLogDir"], testName)

   docker_env = fxProduct.product.docker_env
   
   return LocalSetupfixture(user=user,password=password,apiServerUrl=apiServerUrl,testName=testName,testLogDir=testLogDir,docker_env=docker_env)



def executeInContainer(command):
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



def concatenatedExecuteInContainer(command1, command2):
   ''' Equivalent to the following steps :
            1. Execute command2
            2. If errors in 1., exit
            3. If non-empty output in 1., execute command1 with the output as STDIN
   '''
   out1, err1 = executeInContainer(command2)
   if out1 != None and out1 != '':
      return executeInContainer(command1 + " " + out1)
   return (out1, err1)

def cleanUp():
   ''' Cleaning up the Docker changes the test(s) caused '''
   log.info("Cleaning up")
   concatenatedExecuteInContainer("docker stop","docker ps | grep asset_transfer | sed 's/|/ /' | awk '{print $1}'")
   concatenatedExecuteInContainer("docker rm -f", "docker ps -a | grep asset_transfer | sed 's/|/ /' | awk '{print $1}'")


@describe()
@pytest.mark.skip 
def test_asset_transfer(fxLocalSetup):
   ''' Tests if AssetTransfer can be deployed using the docker container '''

   contracts_before = requests.get(url = "http://" + fxLocalSetup.user + ":" + fxLocalSetup.password + "@localhost/api/concord/contracts").content

   env = fxLocalSetup.docker_env

   asset_transfer_repo = env["asset_transfer_repo"]
   asset_transfer_tag = env["asset_transfer_tag"]
   cmd = "docker run --rm --name asset_transfer-test --network docker_default -td {0}:{1}".format(asset_transfer_repo, asset_transfer_tag)
   out, err = executeInContainer(cmd)

   assert err == None, err 


   if fxLocalSetup.apiServerUrl != '':
      pass_endpoint = fxLocalSetup.apiServerUrl.replace('/','\/')

      # Edit placeholders with actual values inside the container
      log.debug("Replacing ADDRESS_PLACEHOLDER with {} in test/test_AssetTransfer.js.".format(pass_endpoint))
      comm = 'docker exec asset_transfer-test sed -i -e \'s/ADDRESS_PLACEHOLDER/' + pass_endpoint + '/g\' test/test_AssetTransfer.js'
      out1, err1 = executeInContainer(comm)
      assert err1 == None, err1


      log.debug("Replacing USER_PLACEHOLDER with {} in test/test_AssetTransfer.js.".format(fxLocalSetup.user))
      out2, err2 = executeInContainer("docker exec asset_transfer-test sed -i -e 's/USER_PLACEHOLDER/" + fxLocalSetup.user + "/g' test/test_AssetTransfer.js")
      assert err2 == None, err2 

      log.debug("Replacing PASSWORD_PLACEHOLDER with {} in test/test_AssetTransfer.js.".format(fxLocalSetup.password))
      out3, err3 = executeInContainer("docker exec asset_transfer-test sed -i -e 's/PASSWORD_PLACEHOLDER/" + fxLocalSetup.password + "/g' test/test_AssetTransfer.js")
      
      assert err3 == None, err3

   # Run the test script(s)
   log.debug("Running 'docker exec asset_transfer-test mocha'")
   out, err = executeInContainer("docker exec asset_transfer-test mocha")
   log.debug("Done running 'docker exec asset_transfer-test mocha'.  err: '{}', out: '{}'".format(err, out))
   
   assert err == None and out != "", err

   contracts_after = requests.get(url = "http://" + fxLocalSetup.user + ":" + fxLocalSetup.password + "@localhost/api/concord/contracts").content

   
   assert contracts_after != contracts_before,"Contracts have not changed after asset transfer deployment."

   log.debug("Contracts changed after asset transfer deployment, as expected.")
   log.info(out)


@describe()
def test_supply_chain_and_verify_contracts(fxLocalSetup):
   # Cloning the github repo
   os.environ["NODE_TLS_REJECT_UNAUTHORIZED"] = "0"
   BC_URL = "http://helen:8080"

   contracts_before = requests.get(url = "http://" + fxLocalSetup.user + ":" + fxLocalSetup.password + "@localhost/api/concord/contracts").content

   # Changing the truffle-config.js file
   with open("../vmware-blockchain-samples/supply-chain/truffle-config.js", "r+") as file:
      lines = file.read()
      auth_str = "http://" + fxLocalSetup.user + ":" + fxLocalSetup.password + "@helen:8080/api/concord/eth"
      lines = lines.replace("http://<username>:<password>@<url>", auth_str)
      file.truncate(0)
      file.seek(0)
      file.write(lines)


   # Changing the docker-compose.yml file
   with open("../vmware-blockchain-samples/supply-chain/docker-compose.yml", "r+") as file:
      lines = file.read()
      lines = lines.replace("command: \"npm run start:vmware\"",
         "command: \"npm config set registry https://build-artifactory.eng.vmware.com/artifactory/api/npm/npm && npm run start:vmware\"")
      lines = lines.replace("<change-me>", BC_URL)
      file.truncate(0)
      file.seek(0)
      file.write(lines)

   # Changing the verify.js file
   with open("../vmware-blockchain-samples/supply-chain/verify/verify.js", "r+") as file:
      lines = file.read()
      lines = lines.replace("<username>", fxLocalSetup.user)
      lines = lines.replace("<password>", fxLocalSetup.password)
      lines = lines.replace("localhost", "helen")
      lines = lines.replace("443", "8080")
      lines = lines.replace("https", "http")
      lines = lines.replace("/blockchains/local/api/concord/contracts/", "/api/concord/contracts/")
      file.truncate(0)
      file.seek(0)
      file.write(lines)


   os.system("cd ../vmware-blockchain-samples/supply-chain && docker-compose -f docker-compose.yml -f docker-compose-local-network.yml up -d")
   output = subprocess.check_output("cd ../vmware-blockchain-samples/supply-chain && docker-compose -f docker-compose.yml -f docker-compose-local-network.yml run supply-chain npm run deploy_and_verify:vmware", shell = True, universal_newlines=True)

   contracts_after = requests.get(url = "http://" + fxLocalSetup.user + ":" + fxLocalSetup.password + "@localhost/api/concord/contracts").content

   assert output.split("\n")[-4:-1] == ["statusCode: 200"] * 3 , "Failure in npm run deploy_and_verify:vmware"
   assert contracts_before != contracts_after , "Contracts haven't changed after supply chain deployment."
   cleanUp()
   log.info(output)
 

