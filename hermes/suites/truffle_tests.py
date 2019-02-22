#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Utility to test the performance of the Helen+concord ecosystem
#########################################################################
import logging
import os
import pathlib
import traceback
import subprocess

from . import test_suite

log = logging.getLogger(__name__)


class TruffleTests(test_suite.TestSuite):
    _args = None
    _userConfig = None
    _productMode = True
    _resultFile = None
    _testCaseDir = None

    def __init__(self, passedArgs):
        super(TruffleTests, self).__init__(passedArgs)

    def getName(self):
        return "TruffleTests"

    def run(self):
        try:
           self.launchProduct(self._args,
                              self._userConfig,)
        except Exception as e:
           log.error(traceback.format_exc())
           return self._resultFile

        tests = self._getTests()

        for (testName, testFun) in tests:
            result = False
            self._testCaseDir = os.path.join(self._testLogDir, testName)
            pathlib.Path(self._testCaseDir).mkdir(parents=True, exist_ok=True)

            try:
                result, info = getattr(self, "_test_{}".format(testName))()
            except Exception as e:
                result = False
                info = str(e) + "\n" + traceback.format_exc()
                log.error("Exception running Truffle test: '{}'".format(info))

            self.writeResult(testName, result, info)

        log.info("Truffle tests are done.")

        if self._shouldStop():
            self.product.stopProduct()

        return self._resultFile

    def _getTests(self):
        return [("truffle4CallContract", self._test_truffle4CallContract)]

    def _test_truffle4CallContract(self):
        truffleDir = os.path.join("suites", "truffle4_test")
        truffleScript = "./callContractTest.sh"
        cmd = [truffleScript]
        logFilePath = os.path.join(self._testCaseDir, "stdout.log")
        logText = None

        with open(logFilePath, "wb+") as logFile:
            procOutput = subprocess.run(cmd,
                                         stdout=logFile,
                                         stderr=subprocess.STDOUT,
                                         cwd=truffleDir)
        if procOutput.returncode != 0:
            with open(logFilePath, "r") as logFile:
                logText = logFile.read()

            return False, "The truffleCallContract test failed.  Output: {}". \
                format(logText)

        return True, "The truffleCallContract test passed."
