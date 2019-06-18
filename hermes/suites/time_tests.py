#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Tests covering the Time Service.
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


class TimeTests(test_suite.TestSuite):
   args = None
   _userConfig = None
   _ethereumMode = False
   _productMode = True
   _resultFile = None

   def __init__(self, passedArgs):
      super(TimeTests, self).__init__(passedArgs)

   def getName(self):
      return "TimeTests"

   def run(self):
      try:
         self.launchProduct(self._args,
                            self._userConfig)
      except Exception as e:
         log.error(traceback.format_exc())
         return self._resultFile

      if self._ethereumMode:
         info = "TimeTests are not applicable to ethereumMode."
         log.warn(info)
         self.writeResult("All tests", None, info)
         return self._resultFile

      os.environ["PYTHONDONTWRITEBYTECODE"] = "1"

      with open("pickled_time_service_basic_tests", "wb") as f:
         pickle.dump(self, f)

      # Notes on command line usage:
      # -m "performance and smoke" will run tests which are both performance and smoke.
      # -m performance -m smoke will run all peformance tests and all smoke tests.
      params = ["-s", "-v", "suites/time_service", "--json", "report.json"]

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


   def parsePytestTestName(self, parseMe):
      '''
      Pytest creates this long name; parse out the real name.
      If running the API tests from a parametrize, parseMe
      is a string such as "suites/helen/api_test.py::test_existing[user_login]".
      Otherwise, it's a string such as "suites/helen/api_test.py::test_blockchains_fields"
      '''
      return parseMe[parseMe.rindex(":")+1:]
