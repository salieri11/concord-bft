#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
from abc import ABC
from abc import abstractmethod
import collections

import json
import logging
import os
import util.json_helper
from util.product import Product

log = logging.getLogger(__name__)

# The config file contains information aobut how to run things, as opposed to
# command line parameters, which are about running tests.
CONFIG_JSON = "resources/user_config.json"
TEST_LOG_DIR = "test_logs"

class TestSuite(ABC):
   @abstractmethod
   def getName(self): pass

   @abstractmethod
   def run(self): pass

   def __init__(self, passedArgs):
      self._args = passedArgs
      self._testLogDir = os.path.join(self._args.resultsDir, TEST_LOG_DIR)
      self.loadConfigFile()
      self._ethereumMode = self._args.ethereumMode
      self._productMode = not self._ethereumMode
      self._resultFile = os.path.join(passedArgs.resultsDir,
                                     self.getName() + ".json")
      self._results = {
         self.getName(): {
            "result":"",
            "tests": collections.OrderedDict()
         }
      }
      with open(self._resultFile, "w") as f:
         f.write(json.dumps(self._results))
      self._unintentionallySkippedFile = \
                                         os.path.join(passedArgs.resultsDir,
                                         "unintentionallySkippedTests.json")
      self._unintentionallySkippedTests = {}

   def loadConfigFile(self):
      '''
      Loads the main config file.
      '''
      self._userConfig = util.json_helper.readJsonFile(CONFIG_JSON)

   def makeRelativeTestPath(self, fullTestPath):
      '''
      Given the full test path (in the results directory), return the
      relative path.
      '''
      return fullTestPath[len(self._args.resultsDir)+1:len(fullTestPath)]

   def writeResult(self, testName, result, info):
      '''
      We're going to write the full result or skipped test set to json for each
      test so that we have a valid result structure even if things die partway
      through.
      '''
      tempFile = self._resultFile + "_temp"
      realFile = self._resultFile

      if result:
         result = "PASS"
      elif result == False:
         result = "FAIL"
      else:
         result = "SKIPPED"

      log.info(result)

      if testName and result == "SKIPPED":
         log.debug("Unintentionally skipped test '{}': '{}'".format(testName,
                                                                    info))
         self.writeUnintentionallySkippedTest(testName, info)

      # Never change the suite's result due to skips or if it has already
      # been set to "FAIL".
      if not result == "SKIPPED" and \
         not self._results[self.getName()]["result"] == "FAIL":
            self._results[self.getName()]["result"] = result

      if testName:
         self._results[self.getName()]["tests"][testName] = {
            "result": result,
            "info": info
         }

      with open(tempFile, "w") as f:
         f.write(json.dumps(self._results, indent=4))

      os.rename(tempFile, realFile)

   def writeUnintentionallySkippedTest(self, testName, info):
      tempFile = self._unintentionallySkippedFile + "_temp"
      realFile = self._unintentionallySkippedFile
      self._unintentionallySkippedTests[testName] = info

      with open(tempFile, "w") as f:
         f.write(json.dumps(self._unintentionallySkippedTests,
                            sort_keys=True, indent=4))

      os.rename(tempFile, realFile)

   def launchProduct(self, resultsDir, url, userConfig):
      try:
         p = Product(resultsDir, url, userConfig)
         p.launchProduct()
         return p
      except Exception as e:
         log.error(str(e))
         log.error("The product did not start")
         self.writeResult("All Tests", None, "The product did not start.")
         raise(e)
