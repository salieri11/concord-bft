#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Tests covering BeerWars DApp compatibility.
#########################################################################


#########################################################################
# Example executions
# 1)
# ./main.py BeerWarsTests --endpoint='https://mgmt.blockchain.vmware.com/
# blockchains/c3e4c911-9f9d-4899-9c92-6ced72d3ded3/api/
# concord/eth' --user='admin@blockchain.local' --password='Passw0rd!'
# 2)
# ./main.py BeerWarsTests
#########################################################################

import collections
import json
import logging
import os
import traceback
import string
import random
import time
import subprocess
from time import sleep

from . import test_suite
from util.product import Product
from rest.request import Request
from rpc.rpc_call import RPC

import util.json_helper

log = logging.getLogger(__name__)

class BeerWarsTests(test_suite.TestSuite):
   _args = None
   _apiBaseServerUrl = "https://localhost/blockchains/local"
   _userConfig = None
   _ethereumMode = False
   _productMode = True
   _resultFile = None
   _user = None
   _password = None

   def __init__(self, passedArgs):
      super(BeerWarsTests, self).__init__(passedArgs)
      self._args = passedArgs

      if self._args.user != None:
         self._user = self._args.user
      else:
         self._user = "admin@blockchain.local"

      if self._args.password != None:
         self._password = self._args.password
      else:
         self._password = "Passw0rd!"

      if self._args.endpoint != None:
         self._apiServerUrl = self._args.endpoint
         self._noLaunch = True
      else:
         self._apiServerUrl = "http://URL_PLACEHOLDER:8080/api/concord/eth"

      if self._ethereumMode:
         self._noLaunch = True


   def getName(self):
      return "BeerWarsTests"


   def _runTest(self, testName, testFun, testLogDir):
      log.info("Starting test '{}'".format(testName))
      fileRoot = os.path.join(self._testLogDir, testName);
      return testFun(fileRoot)


   def run(self):
      if self._productMode and not self._noLaunch:
         try:
            p = self.launchProduct(self._args,
                                   self._apiBaseServerUrl + "/api/concord/eth",
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


   def _cleanUp(self):
      log.info("Cleaning up")
      os.system("docker stop $(docker ps | grep beerwars | sed 's/|/ /' | awk '{print $1}')")
      os.system("docker rm $(docker ps -a | grep beerwars | sed 's/|/ /' | awk '{print $1}')")
      os.system("docker rmi $(docker images | grep beerwars | sed 's/|/ /' | awk '{print $3}')")


   def _test_beerwars(self, fileRoot):
      status_docker_run = os.system('docker run --name beerwars-test --network="host" -td mmukundram/beerwars:latest')
      if os.WEXITSTATUS(status_docker_run):
         return (False, "Could not run docker container")

      p1 = subprocess.Popen('ifconfig docker | grep "inet addr" | cut -d: -f2 | cut -d " " -f1 | awk "{print $1}"', shell=True, stdout=subprocess.PIPE)
      out1, err1 = p1.communicate()
      self._apiServerUrl = self._apiServerUrl.replace("URL_PLACEHOLDER", out1.strip().decode('utf-8'))

      if self._apiServerUrl != '':
         pass_endpoint = self._apiServerUrl.replace('/','\/');
         comm = 'docker exec beerwars-test sed -i -e \'s/ADDRESS_PLACEHOLDER/' + pass_endpoint + '/g\' test/test_BeerWars.js'
         os.system(comm);
         status_exec_1 = os.system('docker exec beerwars-test sed -i -e \'s/USER_PLACEHOLDER/' + self._user + '/g\' test/test_BeerWars.js');
         status_exec_2 = os.system('docker exec beerwars-test sed -i -e \'s/PASSWORD_PLACEHOLDER/' + self._password + '/g\' test/test_BeerWars.js');
      if os.WEXITSTATUS(status_exec_1) or os.WEXITSTATUS(status_exec_2):
         return (False, "Could not run commands on docker container")
      status_mocha = os.system('docker exec beerwars-test mocha')
      if os.WEXITSTATUS(status_mocha):
         return (False, "Mocha tests failed")
      return (True, None)
