#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Tests covering BeerWars DApp compatibility.
#########################################################################


#########################################################################
# Example executions
# 1) Passing an endpoint runs tests in --noLaunch mode
# ./main.py BeerWarsTests --endpoint='https://mgmt.blockchain.vmware.com/
# blockchains/c3e4c911-9f9d-4899-9c92-6ced72d3ded3/api/concord/eth'
# --user='admin@blockchain.local' --password='Passw0rd!'
# 2) Passing no endpoint launches the product
# ./main.py BeerWarsTests
# 3) You can also pass username and/or password, skipping the endpoint;
# this runs tests on the locally launched product
#########################################################################

import logging
import os
import traceback
import subprocess

from . import test_suite

log = logging.getLogger(__name__)

class BeerWarsTests(test_suite.TestSuite):
   _args = None
   _apiBaseServerUrl = "https://reverse-proxy/blockchains/local"
   _subPath = "/api/concord/eth"
   _userConfig = None
   _ethereumMode = False
   _productMode = True
   _resultFile = None
   _user = None
   _password = None

   def __init__(self, passedArgs):
      super(BeerWarsTests, self).__init__(passedArgs)
      self._args = passedArgs

      user = self._userConfig.get('product').get('db_users')[0]
      username = user['username']
      password = user['password']

      if self._args.user != None:
         self._user = self._args.user
      else:
         self._user = username

      if self._args.password != None:
         self._password = self._args.password
      else:
         self._password = password

      # Test does not launch the product if a URL is passed to it
      if self._args.endpoint != None:
         self._apiServerUrl = self._args.endpoint
         self._noLaunch = True
      else:
         self._apiServerUrl = self._apiBaseServerUrl + self._subPath

      if self._ethereumMode:
         self._noLaunch = True


   def getName(self):
      return "BeerWarsTests"


   def _runTest(self, testName, testFun, testLogDir):
      log.info("Starting test '{}'".format(testName))
      fileRoot = os.path.join(self._testLogDir, testName);
      return testFun(fileRoot)


   def run(self):
      ''' Runs all of the tests. '''
      try:
         self.launchProduct(self._args,
                            "https://localhost/blockchains/local/api/concord/eth",
                            self._userConfig)
      except Exception as e:
         log.error(traceback.format_exc())
         return self._resultFile

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
      return self._resultFile


   def _getTests(self):
      return [("beerwars", self._test_beerwars)]

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
      self._concatenatedExecuteInContainer("docker stop","docker ps | grep beerwars | sed 's/|/ /' | awk '{print $1}'")
      self._concatenatedExecuteInContainer("docker rm -f", "docker ps -a | grep beerwars | sed 's/|/ /' | awk '{print $1}'")
      self._concatenatedExecuteInContainer("docker rmi -f", "docker images | grep beerwars | sed 's/|/ /' | awk '{print $3}'")


   def _test_beerwars(self, fileRoot):
      ''' Tests if BeerWars can be deployed using the docker container '''
      ### (TODO: Transfer the container image to VMware DockerHub and update here)
      # out, err = self._executeInContainer("docker run --rm --name beerwars-test --network docker_default -td vmwblockchain/beer-wars:1.0.4")
      out, err = self._executeInContainer("docker run --rm --name beerwars-test --network docker_default -td mmukundram/beerwars:1.0.4")
      if err != None:
         return (False, err)

      if self._apiServerUrl != '':
         pass_endpoint = self._apiServerUrl.replace('/','\/');

         # Edit placeholders with actual values inside the container
         comm = 'docker exec beerwars-test sed -i -e \'s/ADDRESS_PLACEHOLDER/' + pass_endpoint + '/g\' test/test_BeerWars.js'
         out1, err1 = self._executeInContainer(comm)
         if err1 != None:
            return (False, err1)

         out2, err2 = self._executeInContainer("docker exec beerwars-test sed -i -e 's/USER_PLACEHOLDER/" + self._user + "/g' test/test_BeerWars.js")
         if err2 != None:
            return (False, err2)

         out3, err3 = self._executeInContainer("docker exec beerwars-test sed -i -e 's/PASSWORD_PLACEHOLDER/" + self._password + "/g' test/test_BeerWars.js")
         if err3 != None:
            return (False, err3)

      # Run the test script(s)
      out, err = self._executeInContainer("docker exec beerwars-test mocha")
      if err != None:
         return (False, err)

      log.info(out)

      return (True, None)
