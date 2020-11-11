#################################################################################
# Copyright 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# This file allows one to customize aspects of PyTest.
#################################################################################
import json
import os
import sys
import types
import pprint
import pytest
import tempfile
import collections
import util
from suites.case import describe, getStackInfo
from time import strftime, localtime
import util.chessplus.chessplus_helper as chessplus_helper
from util import auth, csp, helper, hermes_logging, html, json_helper, node_creator,\
    numbers_strings, generate_grpc_bindings, pipeline
from util.stats_gatherer import StatsGatherer

import event_recorder
import util.hermes_logging

local_modules = [os.path.join(".", "lib", "persephone")]
for path in local_modules:
    sys.path.append(path)

log = hermes_logging.getMainLogger()
INTENTIONALLY_SKIPPED_TESTS = "suites/skipped/eth_core_vm_tests_to_skip.json"

TEST_LOGDIR = "test_logs"
SUPPORT_BUNDLE = "support_bundles.json"
REPORT = "execution_results.json"
TESTSTATUS = "test_status.pass"

# Started and terminated by pytest hooks.
stats_gatherer = None

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

    if metafunc.config.option.repeatSuiteRun > 1:
        repeatSuiteRun = metafunc.config.option.repeatSuiteRun
        metafunc.fixturenames.append('repeat_cnt')
        metafunc.parametrize('repeat_cnt', range(repeatSuiteRun))


def getEthCoreVmTests(metafunc):
    '''
    Returns a list of file names.  Each file is a test to run.
    '''
    tests = []
    testSubDirs = None
    hermes_tests = None

    if metafunc.config.option.tests:
        hermes_tests = metafunc.config.option.tests 

    cmdline_args = metafunc.config.option
    user_config = helper.getUserConfig(
    ) if cmdline_args.su else helper.loadConfigFile(cmdline_args)
    ethereumTestRoot = user_config["ethereum"]["testRoot"]

    vmTests = os.path.join(ethereumTestRoot, "VMTests")

    if hermes_tests:
        # Remove the "-k".
        log.debug("Hermes option --tests: {}".format(
            hermes_tests))
        userRequestedTest = hermes_tests.split(" ")[1]
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
    skippedConfig = json_helper.readJsonFile(INTENTIONALLY_SKIPPED_TESTS)
    for key in list(skippedConfig.keys()):
        skippedTest = os.path.join(ethereumTestRoot, key)
        if skippedTest in testList:
            log.info("Skipping: {}".format(key))
            testList.remove(skippedTest)


def pytest_configure(config):
    '''
    Read in all provided options and set required options
    '''
    global cmdLineArguments
    cmdLineArguments = config.option
    cmdLineArguments.hermes_dir = os.path.dirname(
        os.path.realpath(__file__))
    cmdLineArguments.logLevel = hermes_logging.logStringToInt(
        config.option.logLevel)
    if config.option.deploymentService == "staging":
        cmdLineArguments.deploymentService = auth.SERVICE_STAGING


@pytest.hookimpl(hookwrapper=True, tryfirst=True)
def pytest_runtest_makereport(item, call):
    '''
    Set item attribute for  item result at setup, call and teardown
    '''
    outcome = yield
    report = outcome.get_result()
    setattr(item, "rep_" + report.when, report)
    if call.when == 'call' or call.when == 'setup':
        # Mark outcome xfailed if when.call executed and  outcome is skipped
        # and test is marked as xfailed
        if call.when == 'call' and report.outcome == 'skipped' \
            and 'xfail' in list(report.keywords.keys()):
            report.outcome = 'xfailed'

        # Mark outcome xpassed if when.call executed and  outcome is passed
        # and test is marked as xfailed
        if call.when == 'call' and report.outcome == 'passed' \
            and 'xfail' in list(report.keywords.keys()):
            report.outcome = 'xpassed'

        setattr(item, "result", {report.nodeid: {
                'result': report.outcome, 'duration': report.duration}})


@pytest.fixture(scope="session", autouse=True)
@describe("fixture; Set commandline argument dict")
def hermes_info(request):
    '''
    Session level fixture to read in commandline arguments and
    prepares a dictionary of the arguments.
    '''
    log.info("Setting up Hermes Commandline Arguments for the session...")

    cmdline_args = cmdLineArguments
    log_dir = cmdline_args.resultsDir
    helper.CURRENT_SUITE_NAME = "HermesSetup"
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

    support_bundle_file = os.path.join(
        log_dir, SUPPORT_BUNDLE)

    # Set appropriate  log level
    hermes_logging.setUpLogging(cmdline_args)
    helper.CMDLINE_ARGS = vars(cmdline_args)

    def post_session_processing():
        log.info("Teardown session...")
        complete_success = False
        complete_success = prepare_report(
            request.session.items, cmdline_args.resultsDir)
        log.info("Complete Success? ({})".format(complete_success))

        if not complete_success:
            all_replicas_and_type = None
            if cmdline_args.replicasConfig:
                all_replicas_and_type = helper.parseReplicasConfig(
                    cmdline_args.replicasConfig)
            elif cmdline_args.damlParticipantIP != "localhost":
                all_replicas_and_type = {
                    helper.TYPE_DAML_PARTICIPANT: [cmdline_args.damlParticipantIP]}

            if all_replicas_and_type:
                log.info("*************************************")
                log.info("Collecting support bundle(s)...")
                for blockchain_type, replica_ips in all_replicas_and_type.items():
                    helper.create_concord_support_bundle(
                        replica_ips, blockchain_type, log_dir)
                log.info("*************************************")

    request.addfinalizer(post_session_processing)
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
    Hermes module level setup
    '''
    short_name = _get_suite_short_name(request.module.__name__)
    log.info("Setting up Hermes run time settings for {0} Test Suite".format(
        short_name))

    if cmdLineArguments.eventsFile:
        event_recorder.record_event(
            short_name, "Start", cmdLineArguments.eventsFile)
    resultsDir = _createResultsDir(
        short_name, cmdLineArguments.resultsDir)
    hermes_info["hermesTestLogDir"] = resultsDir
    hermes_info["hermesModuleLogDir"] = resultsDir

    # Set some helpers - update for each module
    short_name = _get_suite_short_name(request.module.__name__)
    helper.map_run_id_to_this_run(cmdLineArguments.runID,
                                    cmdLineArguments.resultsDir,
                                    resultsDir)
    # Set Real Name same as Current Name
    helper.CURRENT_SUITE_NAME = cmdLineArguments.suitesRealname if cmdLineArguments.suitesRealname else short_name

    helper.CURRENT_SUITE_LOG_FILE = os.path.join(
        resultsDir, helper.CURRENT_SUITE_NAME + ".log")

    if pipeline.isDryRun():
        # CI dry run might not have to run the suite
        # if invocation signatures didn't change
        if pipeline.isQualifiedToSkip():
            pipeline.dryRunSaveInvocation()
        else:
            pipeline.dryRunSaveInvocation(runSuite=True)
    elif helper.thisHermesIsFromJenkins():
        pipeline.saveInvocationSignature(runSuite=True)

    # at this point, the suite is going to run; mark as executed
    if helper.thisHermesIsFromJenkins():
        pipeline.markInvocationAsExecuted()

    logHandler = hermes_logging.addFileHandler(
        helper.CURRENT_SUITE_LOG_FILE, cmdLineArguments.logLevel)

    support_bundle_file = os.path.join(
        resultsDir, SUPPORT_BUNDLE)
    hermes_info["supportBundleFile"] = support_bundle_file

    def post_module_processing():
        log.info("Teardown module...")

        helper.collectSupportBundles(
            hermes_info["supportBundleFile"], resultsDir)

        if cmdLineArguments.eventsFile:
            event_recorder.record_event(
                short_name, "End",  cmdLineArguments.eventsFile)

        log.removeHandler(logHandler)

    request.addfinalizer(post_module_processing)
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
    log.info("Setting up Hermes for '{0}' of '{1}'".format(
        request.node.name, helper.CURRENT_SUITE_NAME))

    results_dir = os.path.join(set_hermes_info["hermesModuleLogDir"], request.cls.__name__) \
        if request.cls else set_hermes_info["hermesModuleLogDir"]

    set_hermes_info["hermesTestLogDir"] = os.path.join(
        results_dir, request.node.name)
    os.makedirs(set_hermes_info["hermesTestLogDir"], exist_ok=True)

    return set_hermes_info


def _createResultsDir(module_name, resultsDir):
    '''
    Creates parent results dir by appending module name in result_dir
    <parent_results_dir>/ContractCompilerTests/ContractCompilerTests_20200305_140416
    <parent_results_dir>/ContractCompilerTests/ContractCompilerTests.log
    '''
    parent_results_dir = resultsDir
    logDir = module_name + "_" + \
        strftime("%Y%m%d_%H%M%S", localtime())
    results_dir = os.path.join(parent_results_dir, logDir, TEST_LOGDIR)
    log.info("Results directory: {}".format(results_dir))
    os.makedirs(results_dir)

    global stats_gatherer
    stats_gatherer.set_results_file(results_dir)

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
    parser.addoption("--runID",
                     default=helper.get_time_now_in_milliseconds(),
                     help="Unique ID to differentiate runs")
    parser.addoption("--runDuration",
                     type=int,
                     default=6,
                     help="No. of hrs to monitor replicas (default 6 hrs)")
    parser.addoption("--loadInterval",
                     type=int,
                     default=60,
                     help="Minutes to wait between monitors (default 60 mins)")
    parser.addoption("--testset",
                     default="basic_tests",
                     help="Set of test sets to be picked up from testlist file.  e.g. "
                     "'basic_tests'")
    parser.addoption("--testlistFile",
                     help="json file containing the list of tests",
                     default=helper.LONG_RUN_TEST_FILE)
    parser.addoption("--notifyTarget",
                     help="Slack channel name or email address, default will skip notification",
                     default=None)
    parser.addoption("--notifyJobName",
                     help="Shortened job name running this monitoring script",
                     default=None)

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
    nonLocalDeployConfig.addoption("--clientSize",
                                   help="Size of client nodes, must match the SaaS api",
                                   choices=node_creator.NodeCreator.SAAS_SIZES,
                                   default=node_creator.NodeCreator.SAAS_SIZES[0])
    nonLocalDeployConfig.addoption("--clientMemory",
                                   help="Ability to override the client memory value provided by SaaS.",
                                   default=None)
    nonLocalDeployConfig.addoption("--clientCpu",
                                   help="Ability to override the client cpu value provided by SaaS.",
                                   default=None)
    nonLocalDeployConfig.addoption("--clientStorage",
                                   help="Ability to override the client storage value provided by SaaS.",
                                   default=None)
    nonLocalDeployConfig.addoption("--replicaSize",
                                   help="Size of replica nodes, must match the SaaS api.",
                                   choices=node_creator.NodeCreator.SAAS_SIZES,
                                   default=node_creator.NodeCreator.SAAS_SIZES[0])
    nonLocalDeployConfig.addoption("--replicaMemory",
                                   help="Ability to override the replica memory value provided by SaaS.",
                                   default=None)
    nonLocalDeployConfig.addoption("--replicaCpu",
                                   help="Ability to override the replica cpu value provided by SaaS.",
                                   default=None)
    nonLocalDeployConfig.addoption("--replicaStorage",
                                   help="Ability to override the replica storage value provided by SaaS.",
                                   default=None)

    nonLocalDeployConfig.addoption("--propertiesString",
                                   help="The string containing comma seperated key value pairs for deployment properties.",
                                   default="")

    parser.addoption(
        "--supportBundleFile",
        action="store",
        default="support_bundles.json",
        help="Path to a file a test suite should create to have the framework create support "
        "bundles. POPULATED BY HERMES.")


def _get_suite_short_name(module_name):
    '''
    This function returns short name of the Test Suite from it's module name
    '''

    suite_list = {
        "CastorDeploymentTests": "hermes.suites.castor_deployment_tests",
        "CastorDeploymentSystemTests": "hermes.suites.sys_test_castor_deployment_tests",
        "ChessPlusTests": "hermes.suites.chess_plus_tests",
        "ClientGroupTests": "hermes.suites.sys_test_client_group_tests",
        "EthCoreVmTests": "hermes.suites.eth_core_vm_tests",
        "DamlTests": "hermes.suites.daml_tests",
        "DamlRegressionTests": "hermes.suites.daml_regression_tests",
        "ClientPoolDamlTests": "hermes.suites.daml_tests",
        "HelenAPITests": "api_test",
        "HelenBlockTests": "block_test",
        "HelenBlockchainTests": "blockchain_test",
        "HelenClientTests": "client_test",
        "HelenConsortiumTests": "consortium_test",
        "HelenContractTests": "contract_test",
        "HelenMemberTests": "members_test",
        "HelenOrganizationTests": "organization_test",
        "HelenReplicaTests": "replica_test",
        "HelenZoneTests": "zone_test",
        "HelenRoleTests": "roles",
        "NodeInterruptionTests": "hermes.suites.node_interruption_tests",
        "LongRunningTests": "hermes.suites.long_running_tests",
        "LoggingTests": "hermes.suites.logging_tests",
        "LogInsightTests": "hermes.suites.log_insight_tests",
        "PersephoneTestsNew": "hermes.suites.persephone_tests_new",
        "SampleSuite": "hermes.suites.sample_suite",
        "ThinReplicaServerTests": "hermes.suites.thin_replica_server_tests",
        "TimeTests": "hermes.suites.time_service.basic_test",
        "EvilTimeTests": "hermes.suites.time_service.evil_test",
        "PrivacyTeeTests": "hermes.suites.privacy_tee_tests",
        "ApolloBftTests": "hermes.suites.apollo_bft_tests",
        "SkvbcViewchangeTests": "hermes.suites.skvbc_viewchange_tests",
        "RoReplicaTests": "hermes.suites.ro_replica_tests",
        "SkvbcLinearizabilityTests": "hermes.suites.skvbc_linearizability_tests",
        "SkvbcLinearizabilityWithCrashesTests": "hermes.suites.skvbc_linearizability_with_crashes_tests",
        "SkvbcStateTransferTests": "hermes.suites.skvbc_state_transfer_tests",
        "SkvbcPreexecutionTests": "hermes.suites.skvbc_preexecution_tests",
        "DamlPreexecutionTests": "hermes.suites.daml_tests",
        "SimpleStateTransferTest": "hermes.suites.simple_st_test",
        "ContractCompilerTests": "hermes.suites.contract_compiler_tests",
        "SampleDAppTests": "hermes.suites.sample_dapp_tests",
        "EthJsonRpcTests": "hermes.suites.eth_jsonrpc_tests",
        "EthRegressionTests": "hermes.suites.eth_regression_tests",
        "PerformanceTests": "hermes.suites.performance_tests",
        "UiTests": "hermes.suites.ui_tests",
        "DeployDamlTests": "hermes.suites.ui_e2e_deploy_daml",
        "MetadataPersistencyTests": "hermes.suites.persistency_tests",
        "HelenNodeSizeTemplateTests": "nodesize_test",
        "ReconfigurationTests": "hermes.suites.reconfiguration_tests",
        "PreExecutionTests": "hermes.suites.sys_test_preexecution_tests",
        "SkvbcViewchangeTests": "hermes.suites.skvbc_viewchange_tests",
        "WavefrontTests": "hermes.suites.wavefront_tests",
        "HelenVMSizeTests": "vmsize_test"
    }

    short_name = list(suite_list.keys())[list(
        suite_list.values()).index(module_name)]

    return short_name


def prepare_report(items, results_dir):
    complete_success = True
    result_summary = {'succeeded': 0, 'expected-failed': 0,
                      'skipped': 0, 'failed': 0,
                      'un-expected-pass': 0,
                      'not-executed': 0, 'total': len(items)}
    
    fail = "\033[1;91m"
    ok = "\033[1;92m"
    yellow = "\033[1;33m"
    reset = "\033[0m"
    blue = "\033[1;94m"
    cyan = '\033[96m'

    msg = ""
    jobj = {}
    session_results = {'report': {'result': result_summary, 'tests': jobj}}
    for item in items:
        if hasattr(item, 'result'):
            res = item.result
            k = list(res.keys())
            suite = k[0].split('.')[0]
            test = k[0].split(':')[2]
            if suite in jobj.keys():
                jobj.get(suite)[test] = list(res.values())[0]
            else:
                jobj[suite] = {test: list(res.values())[0]}

        if hasattr(item, 'rep_call'):
            if item.rep_call.outcome == 'failed':
                msg = "Test Result: Not a Complete Success...."
                complete_success = False
                result_summary['failed'] += 1
            elif item.rep_call.outcome == 'xfailed':
                # Test is marked as xfail
                # In this case, fail result is equivalent to passed test
                result_summary['expected-failed'] += 1
            elif item.rep_call.outcome == 'xpassed':
                # Deliberately not marking complete_success=False
                # and reporting 'unexpected pass' similar to  'pass'
                # Should there be need to report xfail marked test as failed
                # when actual test result is pass, 
                # Suggest to use strict=True with xfail 
                result_summary['un-expected-pass'] += 1
            else:
                result_summary['succeeded'] += 1
        elif hasattr(item, 'rep_setup') and item.rep_setup.outcome == 'skipped':
            result_summary['skipped'] += 1
        else:
            complete_success = False
            result_summary['not-executed'] += 1

    if complete_success:
        msg = "Test Result: Successfully completed test session..."
        test_status_result = os.path.join(
            results_dir, TESTSTATUS)
        open(test_status_result, 'a').close()

    result_file = os.path.join(
        results_dir, REPORT)
    with open(result_file, "w") as f:
        f.write(json.dumps(session_results))
    log.info("Result file location: {}".format(result_file))
    log.info("Result Summary: {0}{1}{2}".format(cyan, result_summary, reset))

    log.info(msg)
    log.info("Summary: {0}[\u2714] tests succeeded {1}, {2}[\u2717] tests failed {3}, "
             "{4}tests skipped {5}, {0}[\u2714] tests expected-failed {6}, "
             "{2}[\u2717] tests un-expected-pass {7}, "
             "tests not-executed {8}, {9}Total Tests: {10}{11}"
             .format(ok, result_summary['succeeded'], fail, result_summary['failed'],
                     yellow, result_summary['skipped'],
                     result_summary['expected-failed'], 
                     result_summary['un-expected-pass'],
                     result_summary['not-executed'],
                     blue, result_summary['total'], reset))
    return complete_success


def start_stats_gatherer(session):
    '''
    Start the stats gathering tool.
    session: Created and passed to us by Pytest.
    '''
    global stats_gatherer
    stats_gatherer = StatsGatherer(replicas_config=session.config.option.replicasConfig,
                                   interval=10, overwrite=False)
    stats_gatherer.start()


def pytest_sessionstart(session):
    '''
    Pytest hook: https://docs.pytest.org/en/stable/reference.html#initialization-hooks
    '''
    start_stats_gatherer(session)


def pytest_sessionfinish(session, exitstatus):
    '''
    Pytest hook: https://docs.pytest.org/en/stable/reference.html#initialization-hooks
    '''
    stats_gatherer.request_stop()
