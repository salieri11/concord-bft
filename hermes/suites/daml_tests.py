# Copyright 2019 VMware, Inc.  All rights reserved. -- VMware Confidential

# In order to run DAML tests we need a special docker-compose and concord
# configuration file.
#
# main.py --runConcordConfigurationGeneration
#         --concordConfigurationInput /concord/config/dockerConfigurationInput-daml.yaml
#         --dockerComposeFile=../docker/docker-compose-daml.yml
#         DamlTests

import logging
import os
import traceback
import subprocess
import time

from . import test_suite
from util.product import Product

log = logging.getLogger(__name__)

class DamlProduct(Product):
   def _waitForProductStartup(self):
      """Wait for DAML ledger api server to be up and running.
      """
      LOG_READY = "listening on localhost:6865"
      LOG_FILE = self._createLogPath('daml_ledgerapi_1')

      with open(LOG_FILE) as f:
         timeout = 0
         while timeout < 60:
            if any(filter(lambda x: LOG_READY in x, f.readlines())):
               return True
            time.sleep(1)
            timeout += 1

      log.error("DAML ledger api service didn't start in time. %s",
                self._createLogPath('daml_ledgerapi_1'))
      return False

class DamlTests(test_suite.TestSuite):

   def __init__(self, passedArgs):
      super(DamlTests, self).__init__(passedArgs)

   def getName(self):
      return "DamlTests"

   def launchProduct(self, cmdlineArgs, userConfig, force=False):
      """Launch the DAML product.
      """
      self.product = DamlProduct(cmdlineArgs, userConfig)

      if force or (self._productMode and not self._noLaunch):
         try:
            self.product.launchProduct()
         except Exception as e:
            log.error(str(e))
            self.writeResult("All Tests", False, "The product did not start.")
            raise

   def run(self):
      """Runs all tests
      """
      try:
         log.info("Launching product...")
         self.launchProduct(self._args,
                            self._userConfig)
      except Exception as e:
         log.error(traceback.format_exc())
         return self._resultFile

      for (testName, testFun) in self._getTests():
         testLogDir = os.path.join(self._testLogDir, testName)
         log.info("Starting test '{}'".format(testName))
         try:
            result, info = testFun()
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

      log.info("DamlTests are done.")
      return self._resultFile

   def _getTests(self):
      return [("semantic_test_runner", self._test_semantic_test_runner)]

   def _test_semantic_test_runner(self):
      """Run semantic test runner

      See daml-on-vmware repo README.md
      """
      cmd = "docker exec -t docker_daml_ledgerapi_1 " \
            "bin/bazel run @com_github_digital_asset_daml//" \
            "ledger/ledger-api-integration-tests:semantic-test-runner" \
            " -- /daml-on-vmware/quickstart.dar"
      try:
         c = subprocess.run(cmd.split(), check=True, timeout=300, stdout=subprocess.PIPE)
      except subprocess.TimeoutExpired as e:
         log.error("Semantic test runner timed out: %s", str(e))
         return (False, str(e))
      except subprocess.CalledProcessError as e:
         log.error("Semantic test runner returned error code: %s", str(e))
         return (False, str(e))

      log.info("Semantic test runner passed.")
      return (True, None)
