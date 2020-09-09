#################################################################################
# Copyright 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# This file allows one to customize aspects of PyTest.
#################################################################################
import json
import os
import types
import pytest
import util
from suites.case import describe

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
        "--hermesZoneConfig",
        action="store",
        default="",
        help="Hermes zoneConfig object stored as json. POPULATED BY HERMES.")
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


@pytest.fixture(scope="module")
@describe("fixture; run settings")
def hermes_settings(request):
    """
    Given a PyTest fixture's request object, returns a dictionary of various
    pieces of Hermes info that has been passed to PyTest via custom PyTest
    command line parameters.
    cmdline_args: The argparse object containing arguments passed to Hermes.
    user_config: The dictionary containing the contents of user_config.json.
    zone_config: The dictionary containing the contents of zone_config.json.
    log_dir: The log directory path, as a string.
    support_bundle_file: The support bundle file path.
    """
    cmdline_args_dict = json.loads(request.config.getoption("--hermesCmdlineArgs"))
    cmdline_args = types.SimpleNamespace(**cmdline_args_dict)
    user_config = json.loads(request.config.getoption("--hermesUserConfig"))
    zone_config = json.loads(request.config.getoption("--hermesZoneConfig"))
    log_dir = request.config.getoption("--hermesTestLogDir")
    support_bundle_file = request.config.getoption("--supportBundleFile")

    return {
        "cmdline_args": cmdline_args,
        "user_config": user_config,
        "zone_config": zone_config,
        "log_dir": log_dir,
        "support_bundle_file": support_bundle_file
    }
