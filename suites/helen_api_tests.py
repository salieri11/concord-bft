#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Tests covering Helen's non-ethereum ReST API.
# (i.e. everything under /api/, excluding /api/athena/eth)
#########################################################################
import collections
import json
import logging
import os
import traceback

from . import test_suite
from util.product import Product
from rest.request import Request
import util.json_helper

log = logging.getLogger(__name__)

# The config file contains information aobut how to run things, as opposed to
# command line parameters, which are about running tests.
CONFIG_JSON = "resources/user_config.json"
TEST_LOG_DIR = "test_logs"

class HelenAPITests(test_suite.TestSuite):
   _args = None
   _apiBaseServerUrl = "http://localhost:8080/api"
   _userConfig = None
   _ethereumMode = False
   _productMode = True
   _resultFile = None

   def __init__(self, passedArgs):
      self._args = passedArgs
      self._ethereumMode = self._args.ethereumMode
      self._loadConfigFile()
      self._productMode = not self._ethereumMode
      self._resultFile = os.path.join(passedArgs.resultsDir,
                                     "helenAPITestResults.json")
      self._results = {
         "HelenAPITests": {
            "result":"",
            "tests": collections.OrderedDict()
         }
      }
      with open(self._resultFile, "w") as f:
         f.write(json.dumps(self._results))

   def getName(self):
      return "HelenAPITests"

   def run(self):
      ''' Runs all of the tests. '''
      if self._productMode:
         p = Product(self._args.resultsDir,
                     self._apiBaseServerUrl+"/athena/eth",
                     self._userConfig["product"])
         p.launchProduct()
         if not p.waitForProductStartup():
            log.error("The product did not start.  Exiting.")
            self._writeResult(None, None, "The product did not start.")
            return
      else:
         log.warn("HelenAPITests are not applicable to ethereumMode.")
         self._writeResult(None, None, "Not applicable to ethereumMode.")
         return

      tests = self._getTests()

      for (testName, testFun) in tests:
         testLogDir = os.path.join(self._args.resultsDir, TEST_LOG_DIR, testName)

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

         relativeLogDir = self._makeRelativeTestPath(testLogDir)
         info += "Log: <a href=\"{}\">{}</a>".format(relativeLogDir,
                                                     testLogDir)
         self._writeResult(testName, result, info)

      log.info("Tests are done.")

      if self._productMode:
         p.stopProduct()

      return self._resultFile

   def _makeRelativeTestPath(self, fullTestPath):
      '''
      Given the full test path (in the results directory), return the
      relative path.
      '''
      return fullTestPath[len(self._args.resultsDir)+1:len(fullTestPath)]


   def _loadConfigFile(self):
      '''
      Loads the main config file.
      '''
      self._userConfig = util.json_helper.readJsonFile(CONFIG_JSON)


   def _writeResult(self, testName, result, info):
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
         self._writeUnintentionallySkippedTest(testName, info)

      # Never change the suite's result due to skips or if it has already
      # been set to "FAIL".
      if not result == "SKIPPED" and \
         not self._results["HelenAPITests"]["result"] == "FAIL":
            self._results["HelenAPITests"]["result"] = result

      if testName:
         self._results["HelenAPITests"]["tests"][testName] = {
            "result": result,
            "info": info
         }

      with open(tempFile, "w") as f:
         f.write(json.dumps(self._results, indent=4))

      os.rename(tempFile, realFile)

   def _runRestTest(self, testName, testFun, testLogDir):
      log.info("Starting test '{}'".format(testName))
      request = Request(testLogDir, testName, self._apiBaseServerUrl)
      return testFun(request)

   def _getTests(self):
      return [("getMembers", self._test_getMembers), \
              ("swaggerDef", self._test_getSwaggerDef)]

   # Tests: expect one argument, a Request, and produce a 2-tuple
   # (bool success, string info)

   def _test_getMembers(self, request):
      result = request.getMemberList()

      if not type(result) is list:
         return (False, "Response was not a list")
      if len(result) < 1:
         return (False, "No members returned")

      for m in result:
         if not "host" in m:
            return (False, "No 'host' entry in member entry")
         if not isinstance(m["host"], str):
            return (False, "'host' field in member entry is not a string")
         if not "status" in m:
            return (False, "No 'status' entry in member entry")
         if not isinstance(m["status"], str):
            return (False, "'status' field in member entry is not a string")

      return (True, None)

   def _test_getSwaggerDef(self, request):
      result = request.getSwaggerDefinition()

      # How stable is comparing to OrderedDict?
      if not type(result) is collections.OrderedDict:
         return (False, "Response was not an OrderedDict".format(
            type(result).__name__))

      if not "info" in result:
         return (False, "No 'info' field in result; unlikely to be swagger")
      if not "title" in result["info"]:
         return (False, "No 'title' in result['info']; unlikely to be swagger")
      if not result["info"]["title"] == "VMware Project Athena":
         return (False, "Wrong title in result; likely wrong swagger file")
      if not "paths" in result:
         return (False, "No 'paths' field in result; unlikely to be swagger")

      # Maybe we should just read the swagger file from the helen
      # install, and compare the result to it, but the above is a
      # pretty good indicator that we found what we're looking for
      return (True, None)
