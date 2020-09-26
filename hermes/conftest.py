#################################################################################
# Copyright 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# This file allows one to customize aspects of PyTest.
#################################################################################
import json
import os
import types
import pytest
import tempfile
import collections
import util
from suites.case import describe
from time import strftime, localtime
import util.chessplus.chessplus_helper as chessplus_helper
from util import auth, csp, helper, hermes_logging, html, json_helper, numbers_strings, generate_grpc_bindings

import util.hermes_logging
log = util.hermes_logging.getMainLogger()
INTENTIONALLY_SKIPPED_TESTS = "suites/skipped/eth_core_vm_tests_to_skip.json"

TEST_LOGDIR = "test_logs"
HERMES_LOGDIR = "hermes_logs"


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
    ethereumTestRoot = json.loads(metafunc.config.option.hermesUserConfig)[
        "ethereum"]["testRoot"]
    vmTests = os.path.join(ethereumTestRoot, "VMTests")

    if "tests" in hermesCmdlineArgs and hermesCmdlineArgs["tests"]:
        # Remove the "-k".
        log.debug("hermesCmdlineArgs['tests']: {}".format(
            hermesCmdlineArgs["tests"]))
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
            "vmTests"  # Yes, vmTests is a subdirectory of VMTests.
        ]

    if testSubDirs:
        for subdir in testSubDirs:
            for test in os.listdir(os.path.join(vmTests, subdir)):
                tests.append(os.path.join(vmTests, subdir, test))

    if tests:
        removeSkippedTests(tests, ethereumTestRoot)
        if not tests:
            log.error("All of the tests that would have run are in the "
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


@pytest.fixture(scope="session", autouse=True)
@describe("fixture; Set commandline argument dict")
def hermes_info(request):
    '''
    Session level fixture to read in commandline arguments and 
    prepares a dictionary of the arguments. 
    '''
    log.info("Setting up Hermes Commandline Arguments for the session...")

    if (request.config.getoption("--hermesCmdlineArgs")):
        cmdline_args_dict = json.loads(
            request.config.getoption("--hermesCmdlineArgs"))
        cmdline_args = types.SimpleNamespace(**cmdline_args_dict)
        user_config = json.loads(
            request.config.getoption("--hermesUserConfig"))
        zone_config = json.loads(
            request.config.getoption("--hermesZoneConfig"))
        log_dir = request.config.getoption("--hermesTestLogDir")
        support_bundle_file = request.config.getoption("--supportBundleFile")
        cmdline_args.fromMain = 'y'
    else:
        cmdline_args_dict = _getArgs(request)
        cmdline_args = types.SimpleNamespace(**cmdline_args_dict)

        hermes_log_dir = HERMES_LOGDIR + "_" + \
            strftime("%Y%m%d_%H%M%S", localtime())
        log_dir = os.path.join(
            cmdline_args.resultsDir, hermes_log_dir)

        os.makedirs(log_dir)
        cmdline_args.resultsDir = log_dir

        generate_grpc_bindings.generate_bindings(helper.PERSEPHONE_GRPC_BINDINGS_SRC_PATH,
                                                 helper.PERSEPHONE_GRPC_BINDINGS_DEST_PATH)

        if cmdline_args.su:
            helper.WITH_JENKINS_INJECTED_CREDENTIALS = True
            user_config = helper.getUserConfig()
            zone_config = helper.getZoneConfig()
        else:
            helper.WITH_JENKINS_INJECTED_CREDENTIALS = False
            user_config = helper.loadConfigFile(cmdline_args)
            zone_config = helper.loadZoneConfig(cmdline_args)

        if cmdline_args.zoneOverride:
            import util.infra
            folder = cmdline_args.zoneOverrideFolder if hasattr(
                cmdline_args, "zoneOverrideFolder") else None
            util.infra.overrideDefaultZones(cmdline_args.zoneOverride, folder)

        support_bundle_file = cmdline_args.supportBundleFile

        # Set appropriate  log level
        hermes_logging.setUpLogging(cmdline_args)
        helper.CMDLINE_ARGS = vars(cmdline_args)

    return {
        "hermesCmdlineArgs": cmdline_args,
        "hermesUserConfig": user_config,
        "hermesZoneConfig": zone_config,
        "hermesTestLogDir": log_dir,
        "supportBundleFile": support_bundle_file
    }


@pytest.fixture(scope="module", autouse=True)
@describe("fixture; Hermes run settings")
def set_hermes_info(request, hermes_info):
    '''
    Hermes setup at module level
    '''

    log.info("Setting up Hermes run time settings for {} Test Suite".format(
        request.module.__name__))

    resultsDir = _createResultsDir(request, hermes_info["hermesCmdlineArgs"])
    hermes_info["hermesTestLogDir"] = resultsDir
    hermes_info["hermesModuleLogDir"] = resultsDir

    # Set some helpers - update for each module
    helper.CURRENT_SUITE_NAME = request.module.__name__
    helper.CURRENT_SUITE_LOG_FILE = os.path.join(
        resultsDir, request.module.__name__, ".log")

    # Set Real Name same as Current Name
    hermes_info["hermesCmdlineArgs"].suitesRealname = request.module.__name__

    return hermes_info


@pytest.fixture(scope="function", autouse=True)
@describe("fixture; run settings")
def fxHermesRunSettings(request, set_hermes_info):
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
    log.info("Setting up Hermes Settings for test case {0} of {1} Test Suite".format(
        request.node.name, request.module.__name__))

    results_dir = os.path.join(set_hermes_info["hermesModuleLogDir"], request.cls.__name__) \
        if request.cls else set_hermes_info["hermesModuleLogDir"]

    set_hermes_info["hermesTestLogDir"] = os.path.join(
        results_dir, request.node.name)
    os.makedirs(set_hermes_info["hermesTestLogDir"])

    return set_hermes_info


def pytest_collection_modifyitems(config, items):
    skip_the_test = pytest.mark.skip(
        reason="Execution of this test requires --executeAvoid option")
    for item in items:
        if "avoid" in item.keywords:
            item.add_marker(skip_the_test)


def _createResultsDir(request, cmdline_args):
    '''
    Creates parent results dir by appending module name in result_dir
    <parent_results_dir>/ContractCompilerTests/ContractCompilerTests_20200305_140416
    <parent_results_dir>/ContractCompilerTests/ContractCompilerTests.log
    '''
    parent_results_dir = cmdline_args.resultsDir
    logDir = request.module.__name__ + "_" + \
        strftime("%Y%m%d_%H%M%S", localtime())
    results_dir = os.path.join(parent_results_dir, logDir, TEST_LOGDIR)
    os.makedirs(results_dir)
    return results_dir


def pytest_addoption(parser):

    parser.addoption("--ethereumMode",
                     help="Run tests against Ethereum",
                     default=False,
                     action="store_true")
    parser.addoption("--logLevel",
                     help="Set the log level.  Valid values:"
                     "'DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL'",
                     default="INFO")
    parser.addoption("--resultsDir",
                     default=tempfile.gettempdir(),
                     help="Results directory")
    parser.addoption("--eventsFile",
                     help="File to receive timing events.")
    parser.addoption("--tests",
                     help="Run specific tests. Details depend on the suite "
                     "being run. For EthCoreVmTests, this is a directory or "
                     "specific file relative to the VMTests directory. e.g. "
                     "'--tests vmArithmeticTest' or "
                     "'--tests vmArithmeticTest/add0.json'")
    parser.addoption("--config",
                     help="User config file to be considered.")
    parser.addoption("--zoneConfig",
                     help="Zone config file to load zones from",
                     default=helper.CONFIG_ZONE_FILE)
    parser.addoption("--zoneOverride",
                     help="override specific cloud/onprems zone segments. "
                     "e.g. 'sddc1.mr.*, sddc4.mr.*' "
                     "(to use sddc1 & sddc4 MR segs for both cloud/onprem)",
                     default="")
    parser.addoption("--zoneOverrideFolder",
                     help="override deployments to a specific folder",
                     default=None)
    parser.addoption("--dockerComposeFile",
                     help="REQUIRES SUDO. Accepts a docker compose file "
                     "which starts concord and helen.  The product will "
                     "be launched in docker images instead of on the "
                     "command line.  May be a space-separated list of files, "
                     "in the order in which the files should be applied.",
                     default=["../docker/docker-compose.yml"],
                     nargs="*")
    parser.addoption("--noLaunch",
                     default=False,
                     action='store_true',
                     help="Will not launch the product, assuming it is "
                     "already running")
    parser.addoption("--productLaunchAttempts",
                     help="Number of times to attempt to launch the product "
                     "before failing.  Used to work around intermittent bugs "
                     "with product startup.",
                     default=1,
                     type=int)
    parser.addoption("--keepconcordDB",
                     help="Keep and re-use the existing concord database files.",
                     default=False,
                     action='store_true')
    parser.addoption("--repeatSuiteRun",
                     default=1,
                     type=int,
                     help="Number of times to repeat test runs")
    parser.addoption("--endpoint",
                     help="Endpoint for Sample DApp tests")
    parser.addoption("--user",
                     help="User name for Sample DApp tests")
    parser.addoption("--password",
                     help="Password for Sample DApp tests")
    parser.addoption("--deploymentComponents",
                     help="Optional set of docker images required for "
                     "Persephone Tests to bypass the default components "
                     "defined in user_config.json "
                     "e.g. vmwblockchain/concord-core:e7cb6c3,"
                     "vmwblockchain/ethrpc:e7cb6c3,"
                     "vmwblockchain/agent:e7cb6c3, etc...",
                     default=helper.getDefaultDeploymentComponents())
    parser.addoption("--helenDeploymentComponentsVersion",
                     help="Optional version number of components to deploy.  e.g. 0.0.0.123"
                     "Defaults to concord_tag in .env>",
                     default=None)
    parser.addoption("--useLocalConfigService",
                     default=False,
                     action='store_true',
                     help="Optional parameter to use local config-service container")
    parser.addoption("--externalProvisioningServiceEndpoint",
                     default=None,
                     help="External Persephone provisioning service Endpoint. "
                     "Example: provisioningservice-vmbc.vdp.vmware.com:9002")
    parser.addoption("--performanceVotes",
                     default=10,
                     type=int,
                     help="Number of votes in Ballot App for Performance Testrun")
    parser.addoption("--reverseProxyApiBaseUrl",
                     default="https://localhost/blockchains/local",
                     help="Base URL for Helen REST API calls. Test cases drill "
                     "down further into the API with values such as '/api/users', "
                     "'/api/concord/blocks', '/api/concord/eth', etc...).")
    parser.addoption("--inDockerReverseProxyApiBaseUrl",
                     default="https://reverse-proxy/blockchains/local",
                     help="Base URL for accessing the reverse proxy server from within "
                     "the docker environment.")
    parser.addoption("--ethrpcApiUrl",
                     default=None,
                     help="By default, Helen's getMembers API is used to fetch ethrpc nodes, "
                     "and test cases randomly select nodes from that pool.  To force use "
                     "of one node, or to use an official Ethereum setup, specify its "
                     "url here.  e.g. 'http://localhost:8545'. NOTE: VMware IT only allows "
                     "https traffic over port 443 if you are in the 'vmware' network. "
                     "Use a different network (e.g. 'vmwareguest') if you must use a different "
                     "port and the replicas will be outside of VMware's network.")
    parser.addoption("--contractCompilerApiBaseUrl",
                     default="http://localhost:3000/api/v1",
                     help="Base URL for the contract compiler microservice")
    parser.addoption("--suitesRealname",
                     default="",
                     help="Comma-separated list of real names for supplied suites argument")
    parser.addoption("--su",
                     action="store_true",
                     default=False,
                     help="Super user privilege with all Jenkins injected credentials available.")
    parser.addoption("--spiderImageTag", default=None,
                     help="Spider image tag, optional. If not passed in, an appropriate one will be determined.")
    parser.addoption("--marketFlavor", default=chessplus_helper.DEFAULT_MARKET_FLAVOR,
                     help="market flavor (sample, cde7, etc)")
    parser.addoption("--concurrency", default=chessplus_helper.DEFAULT_CONCURRENCY,
                     help="Concurrency")
    parser.addoption("--noOfRequests", default=chessplus_helper.DEFAULT_NO_OF_REQUESTS,
                     help="No. of requests from chess+")
    parser.addoption("--dockerHubUser", default="blockchainrepositoryreader",
                     help="DockerHub user which has read access to the digitalasset private repos. "
                     "Only needed if the DAML SDK version is not one of {}.".format(list(chessplus_helper.KNOWN_SDK_SPIDER_VERSION_MAPPINGS.keys())))
    parser.addoption("--dockerHubPassword",
                     help="DockerHub password which has read access to the digitalasset private repos. "
                     "Only needed if the DAML SDK version is not in {}.".format(list(chessplus_helper.KNOWN_SDK_SPIDER_VERSION_MAPPINGS.keys())))

    concordConfig = parser.getgroup(
        "Concord configuration", "Concord Options:")
    concordConfig.addoption("--runConcordConfigurationGeneration",
                            help="Run Concord configuration generation for the test  cluster before "
                            "launching and launch with the newly generated configuration files. "
                            "If this option is not given, then configuration generation will be "
                            "skipped and the currently existing configuration files will be used.",
                            default=False,
                            action='store_true')
    concordConfig.addoption("--concordConfigurationInput",
                            help="The input file to the configuration generation utility. "
                            "Note: --runConcordConfigurationGeneration has to be set. "
                            "Note: The path specified is the absolute path within a Concord container",
                            default="/concord/config/dockerConfigurationInput.yaml")

    nonLocalDeployConfig = parser.getgroup(
        "SDDC Deployment Parameters", "SDDC Deployment Parameters:")
    nonLocalDeployConfig.addoption("--blockchainLocation",
                                   help="Location of the blockchain being tested.  Values: "
                                   "{} (default), {}, {}. {} not implemented."
                                   .format(helper.LOCATION_LOCAL,
                                           helper.LOCATION_SDDC,
                                           helper.LOCATION_ONPREM,
                                           helper.LOCATION_ONPREM),
                                   default=helper.LOCATION_LOCAL)
    nonLocalDeployConfig.addoption("--blockchainType",
                                   help="Type of blockchain to deploy if --blockchainLocation is not 'local'.  Values: "
                                   "{} (default), {}, {}, {}"
                                   .format(helper.TYPE_ETHEREUM,
                                           helper.TYPE_DAML,
                                           helper.TYPE_HLF,
                                           helper.TYPE_TEE),
                                   default=helper.TYPE_ETHEREUM)
    nonLocalDeployConfig.addoption("--numReplicas",
                                   help="The number of blockchain replicas to deploy. The 'f' value will be "
                                   "calculated automatically using f = (numReplicas - 1)/3. If Helen does not "
                                   "like the combination of the replica count and f value, deployment will fail.",
                                   default=4)
    nonLocalDeployConfig.addoption("--keepBlockchains",
                                   help="Whether to keep the blockchain(s) deployed by this run. "
                                   "Valid values: {}.  Default: '{}'".format([helper.KEEP_BLOCKCHAINS_ALWAYS,
                                                                              helper.KEEP_BLOCKCHAINS_ON_FAILURE,
                                                                              helper.KEEP_BLOCKCHAINS_NEVER],
                                                                             helper.KEEP_BLOCKCHAINS_NEVER),
                                   default=helper.KEEP_BLOCKCHAINS_NEVER)
    nonLocalDeployConfig.addoption("--numParticipants",
                                   help="The number of participant/client nodes to deploy.",
                                   default=1)
    nonLocalDeployConfig.addoption("--migrationFile",
                                   help="Helen Flyway migration file to write generated configurations into, "
                                   "before launching Helen",
                                   default=helper.MIGRATION_FILE)
    nonLocalDeployConfig.addoption("--damlParticipantIP",
                                   help="Public IP of the DAML participant to upload DAR and run tests",
                                   default='localhost')
    nonLocalDeployConfig.addoption("--replicasConfig",
                                   help="Replicas config file obtained after a helen/persephone deployment. "
                                   'Sample format: { "daml_committer": [ { "ip": "10.73.232.56", ... }, ... ], "daml_participant": [ { "ip": "10.73.232.65", ... } ] }',
                                   default=None)
    nonLocalDeployConfig.addoption("--deploymentOrg",
                                   help="Org to use for the deployment for long running tests. An org can specify details such as the Concord version to deploy and feature flags. "
                                   "Note that this org must first be created in CSP, with vmbc_test_con_admin@csp.local having the consortium admin role. "
                                   "Also, an API key with deployment permissions must be created and added to hermes/util/auth.py.",
                                   default=None)
    nonLocalDeployConfig.addoption("--deploymentService",
                                   help="The blockchain service to use for long running tests. (Feel free to adopt for other tests.) Valid values: "
                                   "A url or the word 'staging'. Defaults to https://localhost/blockchains/local (same as reverseProxyApiBaseUrl)".
                                   format(auth.SERVICE_STAGING),
                                   default="https://localhost/blockchains/local")
    nonLocalDeployConfig.addoption("--numGroups",
                                   help="The number of groups for the client node grouping.",
                                   default=1)
    parser.addoption("--executeAvoid",
                     help="Required to execute test cases which are marked as 'avoid'")
    parser.addoption(
        "--supportBundleFile",
        action="store",
        default="support_bundles.json",
        help="Path to a file a test suite should create to have the framework create support "
        "bundles. POPULATED BY HERMES.")

    #### Required to invoke pytest from main program #######
    #### This is a stop gap arrangement till main.py is deprecated. ####
    #### Below mentioned arguments to be deleted  when main.py is deprecated ####

    parser.addoption(
        "--hermesCmdlineArgs",
        action="store",
        default=None,
        help="Hermes command line options stored as json. POPULATED BY HERMES.")
    parser.addoption(
        "--hermesUserConfig",
        action="store",
        default=None,
        help="Hermes userConfig object stored as json. POPULATED BY HERMES.")
    parser.addoption(
        "--hermesZoneConfig",
        action="store",
        default=None,
        help="Hermes zoneConfig object stored as json. POPULATED BY HERMES.")
    parser.addoption(
        "--hermesTestLogDir",
        action="store",
        default=None,
        help="Hermes testLogDir. POPULATED BY HERMES.")


def _getArgs(request):

    cmdArgs = {}
    cmdArgs["ethereumMode"] = request.config.getoption("--ethereumMode")
    cmdArgs["logLevel"] = hermes_logging.logStringToInt(
        request.config.getoption("--logLevel"))
    cmdArgs["resultsDir"] = request.config.getoption("--resultsDir")
    cmdArgs["eventsFile"] = request.config.getoption("--eventsFile")
    cmdArgs["tests"] = request.config.getoption("--tests")
    cmdArgs["config"] = request.config.getoption("--config")
    cmdArgs["zoneConfig"] = request.config.getoption("--zoneConfig")
    cmdArgs["zoneOverride"] = request.config.getoption("--zoneOverride")
    cmdArgs["zoneOverrideFolder"] = request.config.getoption(
        "--zoneOverrideFolder")
    cmdArgs["dockerComposeFile"] = request.config.getoption(
        "--dockerComposeFile")
    cmdArgs["noLaunch"] = request.config.getoption("--noLaunch")
    cmdArgs["productLaunchAttempts"] = request.config.getoption(
        "--productLaunchAttempts")
    cmdArgs["keepconcordDB"] = request.config.getoption("--keepconcordDB")
    cmdArgs["repeatSuiteRun"] = request.config.getoption("--repeatSuiteRun")
    cmdArgs["endpoint"] = request.config.getoption("--endpoint")
    cmdArgs["user"] = request.config.getoption("--user")
    cmdArgs["password"] = request.config.getoption("--password")
    cmdArgs["deploymentComponents"] = request.config.getoption(
        "--deploymentComponents")
    cmdArgs["helenDeploymentComponentsVersion"] = request.config.getoption(
        "--helenDeploymentComponentsVersion")
    cmdArgs["useLocalConfigService"] = request.config.getoption(
        "--useLocalConfigService")
    cmdArgs["externalProvisioningServiceEndpoint"] = request.config.getoption(
        "--externalProvisioningServiceEndpoint")
    cmdArgs["performanceVotes"] = request.config.getoption(
        "--performanceVotes")
    cmdArgs["reverseProxyApiBaseUrl"] = request.config.getoption(
        "--reverseProxyApiBaseUrl")
    cmdArgs["inDockerReverseProxyApiBaseUrl"] = request.config.getoption(
        "--inDockerReverseProxyApiBaseUrl")
    cmdArgs["ethrpcApiUrl"] = request.config.getoption("--ethrpcApiUrl")
    cmdArgs["contractCompilerApiBaseUrl"] = request.config.getoption(
        "--contractCompilerApiBaseUrl")
    cmdArgs["suitesRealname"] = request.config.getoption("--suitesRealname")
    cmdArgs["su"] = request.config.getoption("--su")
    cmdArgs["spiderImageTag"] = request.config.getoption("--spiderImageTag")
    cmdArgs["marketFlavor"] = request.config.getoption("--marketFlavor")
    cmdArgs["concurrency"] = request.config.getoption("--concurrency")
    cmdArgs["noOfRequests"] = request.config.getoption("--noOfRequests")
    cmdArgs["dockerHubUser"] = request.config.getoption("--dockerHubUser")
    cmdArgs["dockerHubPassword"] = request.config.getoption(
        "--dockerHubPassword")
    cmdArgs["runConcordConfigurationGeneration"] = request.config.getoption(
        "--runConcordConfigurationGeneration")
    cmdArgs["concordConfigurationInput"] = request.config.getoption(
        "--concordConfigurationInput")
    cmdArgs["blockchainLocation"] = request.config.getoption(
        "--blockchainLocation")
    cmdArgs["blockchainType"] = request.config.getoption("--blockchainType")
    cmdArgs["numReplicas"] = request.config.getoption("--numReplicas")
    cmdArgs["keepBlockchains"] = request.config.getoption("--keepBlockchains")
    cmdArgs["numParticipants"] = request.config.getoption("--numParticipants")
    cmdArgs["migrationFile"] = request.config.getoption("--migrationFile")
    cmdArgs["damlParticipantIP"] = request.config.getoption(
        "--damlParticipantIP")
    cmdArgs["replicasConfig"] = request.config.getoption("--replicasConfig")
    cmdArgs["deploymentOrg"] = request.config.getoption("--deploymentOrg")
    cmdArgs["deploymentService"] = auth.SERVICE_STAGING if request.config.getoption(
        "--deploymentService") == "staging" else request.config.getoption("--deploymentService")
    cmdArgs["numGroups"] = request.config.getoption("--numGroups")
    cmdArgs["supportBundleFile"] = request.config.getoption(
        "--supportBundleFile")
    cmdArgs["hermes_dir"] = os.path.dirname(os.path.realpath(__file__))
    cmdArgs["executeAvoid"] = request.config.getoption("--executeAvoid")
    cmdArgs["hermesCmdlineArgs"] = request.config.getoption(
        "--hermesCmdlineArgs"),
    cmdArgs["hermesUserConfig"] = request.config.getoption(
        "--hermesUserConfig"),
    cmdArgs["hermesZoneConfig"] = request.config.getoption(
        "--hermesZoneConfig"),
    cmdArgs["hermesTestLogDir"] = request.config.getoption(
        "--hermesTestLogDir"),

    return cmdArgs
