###############################################################################
# Copyright 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# This is the parent class for Hermes test suites which use PyTest.
###############################################################################
import json
import logging
import os
import pytest
import traceback
import collections

import util.json_helper
import util.helper
from . import test_suite
from suites.case import describe, getStackInfo

from util.product import Product, ProductLaunchException

import util.hermes_logging
log = util.hermes_logging.getMainLogger()

CONTRACTS_DIR = "resources/contracts"
TEST_LOG_DIR = "test_logs"
SUPPORT_BUNDLE = "support_bundles.json"
UNINTENTIONALLY_SKIPPED = "unintentionallySkippedTests.json"
REPORT = "report.json"
ALLURE_DIR = "allure_results"
# HTML_REPORT_DIR = "html-report"
# HTML_REPORT = "test_report.html"


class PytestSuite():

    def __init__(self, passedArgs, suiteName, testFile, product):
        self._suiteName = suiteName
        self._testFile = testFile
        self._args = passedArgs
        self._testLogDir = os.path.join(self._args.resultsDir, TEST_LOG_DIR)
        self._reportFile = os.path.join(self._testLogDir, REPORT)
        self._testLogFile = os.path.join(
            self._args.resultsDir, self._suiteName + ".log")
        self._resultFile = os.path.join(passedArgs.resultsDir,
                                        self._suiteName + ".json")
        self._userConfig = util.helper.loadConfigFile(self._args)
        self._zoneConfig = util.helper.loadZoneConfig(self._args)
        self._ethereumMode = self._args.ethereumMode
        self._productMode = not self._ethereumMode
        self._noLaunch = self._args.noLaunch
        self.ethrpcNodes = None

        self.reverseProxyApiBaseUrl = self._args.reverseProxyApiBaseUrl
        self.inDockerReverseProxyApiBaseUrl = self._args.inDockerReverseProxyApiBaseUrl
        self.contractCompilerApiBaseUrl = self._args.contractCompilerApiBaseUrl

        self._supportBundleFile = os.path.join(
            self._testLogDir, SUPPORT_BUNDLE)
        self._logHandler = util.hermes_logging.addFileHandler(
            self._testLogFile, self._args.logLevel)
        
        self.allure_report_dir = os.path.join(passedArgs.allureDir,
                                              ALLURE_DIR)
        
        os.makedirs(self.allure_report_dir, exist_ok=True)                                
        
        # self.html_report = os.path.join(
        #     passedArgs.resultsDir, HTML_REPORT)
        # If running multiple suites, just keep the same product object.
        self.product = product if product else Product(
            self._args, self._userConfig)

        self._results = {
            self.getName(): {
                "result": "",
                "tests": collections.OrderedDict()
            }
        }

        with open(self._resultFile, "w") as f:
            f.write(json.dumps(self._results))
        self._unintentionallySkippedFile = \
            os.path.join(passedArgs.resultsDir,
                         UNINTENTIONALLY_SKIPPED
                         )
        self._unintentionallySkippedTests = {}
        self._userUnlocked = False

        if self._args.repeatSuiteRun > 1:
            self._repeatSuiteRun = True
        else:
            self._repeatSuiteRun = False

        self._hermes_home = self._args.hermes_dir

    def getName(self):
        return self._suiteName

    def run(self):
        '''
        Runs the tests passed in, parses PyTest's json output and rewrites
        it in Hermes's format, and returns the path to that file.
        '''
        os.environ["PYTHONDONTWRITEBYTECODE"] = "1"

        # Notes on PyTest command line usage:
        # -m "performance and smoke" will run tests which are both performance and smoke.
        # -m performance -m smoke will run all peformance tests and all smoke tests.

        cmdlineArgsJson = json.dumps(vars(self._args))
        userConfigJson = json.dumps(self._userConfig)

        zoneConfigJson = json.dumps(self._zoneConfig)
        params = ["--capture=no", "--verbose", "--json", self._reportFile,
                  "--alluredir", self.allure_report_dir,
                  "--hermesCmdlineArgs", cmdlineArgsJson,
                  "--hermesUserConfig", userConfigJson,
                  "--hermesZoneConfig", zoneConfigJson,
                  "--hermesTestLogDir", self._testLogDir,
                  "--supportBundleFile", self._supportBundleFile,
                  "--log-cli-level", logging.getLevelName(log.level),
                  self._testFile]

        if util.helper.thisHermesIsFromJenkins():  # on Jenkins pipeline,
            params.insert(0, "-x")  # fail fast; exit on first failure.

        if self._args.tests:
            params += self._args.tests.split(" ")

        pytest.main(params)
        util.helper.collectSupportBundles(self._supportBundleFile, self._testLogDir)
        self.parsePytestResults(self._reportFile, self._testLogDir)

        log.removeHandler(self._logHandler)
        return self._resultFile, self.product

    # def collectSupportBundles(self):
    #     '''
    #     Collect support bundles found in support_bundles.json.  The structure must be:
    #     {
    #       "<host>": {
    #         "type": "daml | ethereum",  (mandatory)
    #         "dir": "<directory>"        (optional, defaults to the suite's _testLogDir)
    #       }
    #     }
    #     '''
    #     if os.path.isfile(self._supportBundleFile):
    #         with open(self._supportBundleFile, "r") as f:
    #             bundles = json.load(f)

    #         log.info("bundles: {}".format(bundles))

    #         for bundleHost in bundles:
    #             if "dir" in bundles[bundleHost]:
    #                 logDir = bundles[bundleHost]["dir"]
    #             else:
    #                 logDir = self._testLogDir

    #             util.helper.create_concord_support_bundle(
    #                 [bundleHost], bundles[bundleHost]["type"], logDir)

    def parsePytestResults(self, report_file, log_dir):
        '''
        Convert PyTest's json format to the Hermes format for Hermes
        to parse later.
        '''
        results = util.json_helper.readJsonFile(report_file)
        stackInfo = getStackInfo()
        for testResult in results["report"]["tests"]:
            testPassed = None

            if testResult["outcome"] == "passed" or testResult["outcome"] == "xfailed":
                testPassed = True
            elif testResult["outcome"] == "skipped":
                testPassed = "skipped"
            else:
                testPassed = False

            info = "" if testPassed else json.dumps(
                testResult, indent=2, default=str)
            stackInfo = getStackInfo()
            testName = util.helper.parsePytestTestName(testResult["name"])
            testLogDir = os.path.join(log_dir, testName)
            relativeLogDir = util.helper.makeRelativeTestPath(log_dir, testLogDir)
            info += "\nLog: <a href=\"{}\">{}</a>".format(relativeLogDir,
                                                          testLogDir)
            self.writeResult(testResult["name"],
                             testPassed,
                             info,
                             stackInfo)

    # def parsePytestTestName(self, parseMe):
    #     '''
    #     Returns a condensed name, just used to reduce noise.
    #     '''
    #     return parseMe[parseMe.rindex(":")+1:]

    # TODO: Remove after direct invocation of pytest, used in main.py
    def getResultFile(self):
        '''
        Typically, a suite will return its file.  However, in case
        dies, let's provide a way for the framework to try to get it.
        '''
        return self._resultFile

    # TODO: Remove after direct invocation of pytest, used in main.py
    def getTestLogDir(self):
        return self._testLogDir

    # TODO: To be deleted after porting of all testSuites to pytest
    # def makeRelativeTestPath(self, fullTestPath):
    #     '''
    #     Given the full test path (in the results directory), return the
    #     relative path.
    #     '''
    #     return fullTestPath[len(self._args.resultsDir)+1:len(fullTestPath)]

    def writeResult(self, testName, result, info, stackInfo=None):
        '''
        We're going to write the full result or skipped test set to json for each
        test so that we have a valid result structure even if things die partway
        through.
        '''
        tempFile = self._resultFile + "_temp"
        realFile = self._resultFile
        if stackInfo is None:
            stackInfo = getStackInfo()

        if result == True:
            result = "PASS"
        elif result == False:
            result = "FAIL"
        else:
            result = "SKIPPED"

        log.info(result)

        if result != "PASS":
            log.info("The test case returned the following information: '{}'".
                     format(info))

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
            f.write(json.dumps(self._results, indent=4, default=str))

        os.rename(tempFile, realFile)

        if result == "FAIL":
            self.product.testFailed = True

    def writeUnintentionallySkippedTest(self, testName, info):
        tempFile = self._unintentionallySkippedFile + "_temp"
        realFile = self._unintentionallySkippedFile
        self._unintentionallySkippedTests[testName] = info

        with open(tempFile, "w") as f:
            f.write(json.dumps(self._unintentionallySkippedTests,
                               sort_keys=True, indent=4))

        os.rename(tempFile, realFile)

    def validateLaunchConfig(self, dockerCfg):
        '''
        When the product is being launched, a Product can pass a docker config into
        a test suite to identify configuration issues before continuing with the launch.
        - If we're running tests which deploy a new blockchain (e.g. Helen invoking
          Persephone), be sure we have a docker config that includes Persephone.
        - Be sure replaceable values in the Persephone application-test.properties file have been replaced
          with real values.
        '''
        if self._args.blockchainLocation == util.helper.LOCATION_LOCAL:
            return

        foundPersephoneDockerConfig = False

        for key in dockerCfg["services"].keys():
            if "persephone-provisioning" in key:
                foundPersephoneDockerConfig = True
                provisioningConfig = None
                provisioningConfigFile = "resources/persephone/provisioning/application-test.properties"
                invalidValues = ["<VMC_API_TOKEN>",
                                 "<DOCKERHUB_REPO_READER_PASSWORD>"]

                with open(provisioningConfigFile, "r") as f:
                    provisioningConfig = f.read()

                for invalid in invalidValues:
                    if invalid in provisioningConfig:
                        msg = "Invalid Persephone provisioning value(s) in {}.".format(
                            provisioningConfigFile)
                        msg += "  Check instance(s) of: {}".format(invalidValues)
                        raise Exception(msg)

        if not foundPersephoneDockerConfig:
            raise Exception("Deploying a new blockchain was requested, but there is no "
                            "docker config for Persephone. It is likely you should add "
                            "this to your command: '"
                            "--dockerComposeFile ../docker/docker-compose.yml ../docker/docker-compose-persephone.yml'")

    # TODO: Check and  remove. not used
    def _shouldStop(self):
        return self._productMode and not self._noLaunch and not self._repeatSuiteRun
