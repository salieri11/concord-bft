#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Tests covering Performance.
#########################################################################


#########################################################################
# Example executions
# ./main.py PerformanceTests
#########################################################################

import logging
import os
import traceback
import subprocess

from . import test_suite
from rest.request import Request
import util.json_helper

log = logging.getLogger(__name__)

class PerformanceTests(test_suite.TestSuite):
   _args = None
   _userConfig = None
   _resultFile = None

   def __init__(self, passedArgs, product):
      super(PerformanceTests, self).__init__(passedArgs, product)
      self._performance_submodule = os.path.join(self._hermes_home,
                                            '..', 'performance')

   def getName(self):
      return "PerformanceTests"

   def _runTest(self, testName, testFun, testLogDir):
      log.info("Starting test '{}'".format(testName))
      fileRoot = os.path.join(self._testLogDir, testName);
      os.makedirs(fileRoot, exist_ok=True)
      return testFun(fileRoot)


   def run(self):
      ''' Runs all of the tests. '''
      self.launchProduct(self._args,
                         self._userConfig)
      tests = self._getTests()

      for (testName, testFun) in tests:
         testLogDir = os.path.join(self._testLogDir, testName)
         try:
            testLogDir = os.path.join(self._testLogDir, testName)
            # When this suite is switched over to pytest, the request object
            # and the blockchain ID will be available from fixtures.  For now,
            # create a request object and derive the blockchain ID here.
            request = Request(testLogDir,
                              testName,
                              self._args.reverseProxyApiBaseUrl,
                              self._userConfig,
                              util.auth.internal_admin)
            blockchainId = request.getBlockchains()[0]["id"]
            self.setEthrpcNode(request, blockchainId)
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
      return super().run()


   def _getTests(self):
      return [("performance_test", self._test_performance)]


   def _test_performance(self, fileRoot):
      if not os.path.isdir(self._performance_submodule):
         return (False, "Performance repo {} does not Exist"
                 .format(self._performance_submodule))

      performance_jar = os.path.join(self._performance_submodule,
                                     "ballotApp", "target",
                                     "performance-1.0-SNAPSHOT-jar-with-dependencies.jar")
      if not os.path.isfile(performance_jar):
         return (False, "Performance jar file {} does not Exist".
                 format(performance_jar))

      concord_ip = self.ethrpcApiUrl.split("/")[2].split(":")[0]
      number_of_votes = str(self._args.performanceVotes)
      log.info("No. of votes: {}".format(self._args.performanceVotes))
      input_data = os.path.join(self._performance_submodule,
                                "data", "proposals")

      cmd = ["java", "-Xmx1g", "-jar", performance_jar,
             "--concord", concord_ip, number_of_votes, input_data, fileRoot]

      log.info("Running benchmark tool: {}".format(" ".join(cmd)))

      completedProcess = subprocess.run(cmd,
                                        stdout=subprocess.PIPE,
                                        stderr=subprocess.STDOUT)
      psOutput = completedProcess.stdout.decode("UTF-8")
      log.info(psOutput)
      if 'Throughput: ' not in psOutput:
         return (False, "Failed to run Performance Test")

      return (True, None)
