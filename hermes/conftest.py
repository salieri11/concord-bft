#################################################################################
# Copyright 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Thisc file allows one to customize aspects of PyTest.
#################################################################################
import json
import logging
import os
import pytest
import util

import util.hermes_logging
log = util.hermes_logging.getMainLogger()
INTENTIONALLY_SKIPPED_TESTS = "suites/skipped/eth_core_vm_tests_to_skip.json"

def pytest_addoption(parser):
    parser.addoption(
        "--hermesCmdlineArgs",
        action="store",
        default="",
        help="Hermes command line options stored as json. POPULATED BY HERMES.")
    parser.addoption(
        "--hermesUserConfig",
        action="store",
        default="",
        help="Hermes userConfig object stored as json. POPULATED BY HERMES.")
    parser.addoption(
        "--hermesTestLogDir",
        action="store",
        default="",
        help="Hermes testLogDir. POPULATED BY HERMES.")
    parser.addoption(
        "--supportBundleFile",
        action="store",
        default="",
        help="Path to a file a test suite should create to have the framework create support "
            "bundles. POPULATED BY HERMES.")

def pytest_generate_tests(metafunc):
    '''
    Within a PyTest test file, parametrize runs before fixtures are available, so
    one cannot use things like command line parameters (which are fixtures) to
    feed a parametrize.
    PyTest provides this pytest_generate_tests() hook which allows us to use
    our fixtures to create a new fixture which sets up a parametrize.
    '''
    if "fxEthCoreVmTests" in metafunc.fixturenames:
        metafunc.parametrize("fxEthCoreVmTests", getEthCoreVmTests(metafunc))

def getEthCoreVmTests(metafunc):
    '''
    Returns a list of file names.  Each file is a test to run.
    '''
    tests = []
    testSubDirs = None
    hermesCmdlineArgs = json.loads(metafunc.config.option.hermesCmdlineArgs)
    ethereumTestRoot = json.loads(metafunc.config.option.hermesUserConfig)["ethereum"]["testRoot"]
    vmTests = os.path.join(ethereumTestRoot, "VMTests")

    if "tests" in hermesCmdlineArgs and hermesCmdlineArgs["tests"]:
        # Remove the "-k".
        log.debug("hermesCmdlineArgs['tests']: {}".format(hermesCmdlineArgs["tests"]))
        userRequestedTest = hermesCmdlineArgs["tests"].split(" ")[1]
        fullPath = os.path.join(vmTests, userRequestedTest)

        if os.path.exists(fullPath):
            if os.path.isfile(fullPath):
                tests.append(fullPath)
            else:
                testSubDirs = [userRequestedTest]
        else:
            log.error("File '{}' does not exist.".format(fullPath))
    else:
        testSubDirs = [
            "vmArithmeticTest",
            "vmBitwiseLogicOperation",
            "vmRandomTest",
            "vmPushDupSwapTest",
            "vmSha3Test",
            "vmBlockInfoTest",
            "vmEnvironmentalInfo",
            "vmIOandFlowOperations",
            "vmLogTest",
            "vmPerformance",
            "vmSystemOperations",
            "vmTests" # Yes, vmTests is a subdirectory of VMTests.
        ]

    if testSubDirs:
        for subdir in testSubDirs:
            for test in os.listdir(os.path.join(vmTests, subdir)):
                tests.append(os.path.join(vmTests, subdir, test))

    if tests:
        removeSkippedTests(tests, ethereumTestRoot)
        if not tests:
            log.error("All of the tests that would have run are in the " \
                      "skipped test list.")

    if not tests:
        log.error("There are no tests to run.")

    log.debug("EthCoreVmTests is running: {}".format(tests))
    return tests

def removeSkippedTests(testList, ethereumTestRoot):
    '''
    Loads the skipped test file and removes skipped tests from the given
    test list.
    '''
    skippedConfig = util.json_helper.readJsonFile(INTENTIONALLY_SKIPPED_TESTS)
    for key in list(skippedConfig.keys()):
        skippedTest = os.path.join(ethereumTestRoot, key)
        if skippedTest in testList:
            log.info("Skipping: {}".format(key))
            testList.remove(skippedTest)
