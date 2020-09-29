#!/usr/bin/env python3

#########################################################################
# Copyright 2018 - 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import argparse
import json
import os
import sys
import tempfile
import traceback
from time import strftime, localtime, sleep

import event_recorder
from suites import (
    eth_core_vm_tests,
    eth_regression_tests,
    performance_tests,
    persistency_tests,
    pytest_suite,
    sample_dapp_tests
)
from suites.case import summarizeExceptions, addExceptionToSummary
from util import (auth, csp, helper, hermes_logging, html, json_helper,
                 numbers_strings, generate_grpc_bindings, pipeline)
from util.product import ProductLaunchException
import util.chessplus.chessplus_helper as chessplus_helper

sys.path.append("lib/persephone")

log = None
# Add new test suite in suiteList.

suiteList = {
   "CastorDeploymentTests" : "suites/castor_deployment_tests.py",
   "ChessPlusTests": "suites/chess_plus_tests.py",
   "ClientGroupTests": "suites/st_client_group_tests.py",
   "EthCoreVmTests": "suites/eth_core_vm_tests.py",
   "DamlTests": "suites/daml_tests.py",
   "DamlRegressionTests": "suites/daml_regression_tests.py",
   "ClientPoolDamlTests": "suites/daml_tests.py",
   "HelenAPITests": "suites/helen/api_test.py",
   "HelenBlockTests": "suites/helen/block_test.py",
   "HelenBlockchainTests": "suites/helen/blockchain_test.py",
   "HelenClientTests": "suites/helen/client_test.py",
   "HelenConsortiumTests": "suites/helen/consortium_test.py",
   "HelenContractTests": "suites/helen/contract_test.py",
   "HelenMemberTests": "suites/helen/members_test.py",
   "HelenOrganizationTests": "suites/helen/organization_test.py",
   "HelenReplicaTests": "suites/helen/replica_test.py",
   "HelenZoneTests": "suites/helen/zone_test.py",
   "HelenRoleTests": "suites/helen/roles.py",
   "NodeInterruptionTests": "suites/node_interruption_tests.py",
   "LoggingTests": "suites/logging_tests.py",
   "PersephoneTestsNew": "suites/persephone_tests_new.py",
   "SampleSuite": "suites/sample_suite.py",
   "ThinReplicaServerTests": "suites/thin_replica_server_tests.py",
   "TimeTests": "suites/time_service/basic_test.py",
   "EvilTimeTests": "suites/time_service/evil_test.py",
   "PrivacyTeeTests": "suites/privacy_tee_tests.py",
   "ApolloBftTests": "suites/apollo_bft_tests.py",
   "SkvbcLinearizabilityTests": "suites/skvbc_linearizability_tests.py",
   "SkvbcLinearizabilityWithCrashesTests": "suites/skvbc_linearizability_with_crashes_tests.py",
   "SkvbcStateTransferTests": "suites/skvbc_state_transfer_tests.py",
   "SkvbcPreexecutionTests": "suites/skvbc_preexecution_tests.py",
   "DamlPreexecutionTests": "suites/daml_tests.py",
   "SimpleStateTransferTest": "suites/simple_st_test.py",
   "ContractCompilerTests": "suites/contract_compiler_tests.py",
   "SampleDAppTests":"suites/sample_dapp_tests.py",
   "EthJsonRpcTests": "suites/eth_jsonrpc_tests.py",
   "EthRegressionTests": "suites/eth_regression_tests.py",
   "PerformanceTests": "suites/performance_tests.py",
   "UiTests": "suites/ui_tests.py",
   "DeployDamlTests": "suites/ui_e2e_deploy_daml.py",
   "MetadataPersistencyTests": "suites/persistency_tests.py",
   "HelenNodeSizeTemplateTests": "suites/helen/nodesize_test.py"
}

local_modules = [os.path.join(".", "lib", "persephone")]


def initialize():
    '''
    Perform necessary initialization to setup the runtime environment.
    '''
    # For any Python modules that is only available "locally" with respect to
    # this Hermes installation, initialize the sys.path such that the module
    # can be imported.
    for path in local_modules:
        sys.path.append(path)


@summarizeExceptions
def main():
   suite = None

   # Initialize the runtime environment for this instance.
   initialize()

   parser = argparse.ArgumentParser()
   parser.add_argument("suites", help="Comma delimited list of test suites. " \
                       "Available suites: {}".format([*suiteList]))
   parser.add_argument("--ethereumMode",
                       help="Run tests against Ethereum",
                       default=False,
                       action='store_true')
   parser.add_argument("--logLevel",
                       help="Set the log level.  Valid values:"
                       "'DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL'",
                       default="INFO")
   parser.add_argument("--resultsDir",
                       default=tempfile.gettempdir(),
                       help="Results directory")
   parser.add_argument("--eventsFile",
                       help="File to receive timing events.")
   parser.add_argument("--tests",
                       help="Run specific tests. Details depend on the suite " \
                       "being run. For EthCoreVmTests, this is a directory or " \
                       "specific file relative to the VMTests directory. e.g. " \
                       "'--tests vmArithmeticTest' or " \
                       "'--tests vmArithmeticTest/add0.json'")
   parser.add_argument("--config",
                       help="User config file to be considered.")
   parser.add_argument("--zoneConfig",
                       help="Zone config file to load zones from",
                       default=helper.CONFIG_ZONE_FILE)
   parser.add_argument("--zoneOverride",
                       help="override specific cloud/onprems zone segments. " \
                            "e.g. 'sddc1.mr.*, sddc4.mr.*' " \
                            "(to use sddc1 & sddc4 MR segs for both cloud/onprem)",
                       default="")
   parser.add_argument("--zoneOverrideFolder",
                       help="override deployments to a specific folder",
                       default=None)
   parser.add_argument("--dockerComposeFile",
                       help="REQUIRES SUDO. Accepts a docker compose file " \
                       "which starts concord and helen.  The product will " \
                       "be launched in docker images instead of on the " \
                       "command line.  May be a space-separated list of files, " \
                       "in the order in which the files should be applied.",
                       default=["../docker/docker-compose.yml"],
                       nargs="*")
   parser.add_argument("--noLaunch",
                       default=False,
                       action='store_true',
                       help="Will not launch the product, assuming it is "
                            "already running")
   parser.add_argument("--productLaunchAttempts",
                       help="Number of times to attempt to launch the product " \
                       "before failing.  Used to work around intermittent bugs " \
                       "with product startup.",
                       default=1,
                       type=int)
   parser.add_argument("--keepconcordDB",
                       help="Keep and re-use the existing concord database files.",
                       default=False,
                       action='store_true')
   parser.add_argument("--repeatSuiteRun",
                       default=1,
                       type=int,
                       help="Number of times to repeat test runs")
   parser.add_argument("--endpoint",
                       help="Endpoint for Sample DApp tests")
   parser.add_argument("--user",
                       help="User name for Sample DApp tests")
   parser.add_argument("--password",
                       help="Password for Sample DApp tests")
   parser.add_argument("--deploymentComponents",
                       help="Optional set of docker images required for "
                            "Persephone Tests to bypass the default components "
                            "defined in user_config.json "
                            "e.g. vmwblockchain/concord-core:e7cb6c3,"
                            "vmwblockchain/ethrpc:e7cb6c3,"
                            "vmwblockchain/agent:e7cb6c3, etc...",
                       default=helper.getDefaultDeploymentComponents())
   parser.add_argument("--helenDeploymentComponentsVersion",
                       help="Optional version number of components to deploy.  e.g. 0.0.0.123" \
                       "Defaults to concord_tag in .env>",
                       default=None)
   parser.add_argument("--useLocalConfigService",
                       default=False,
                       action='store_true',
                       help="Optional parameter to use local config-service container")
   parser.add_argument("--externalProvisioningServiceEndpoint",
                       default=None,
                       help="External Persephone provisioning service Endpoint. "
                            "Example: provisioningservice-vmbc.vdp.vmware.com:9002")
   parser.add_argument("--performanceVotes",
                       default=10,
                       type=int,
                       help="Number of votes in Ballot App for Performance Testrun")
   parser.add_argument("--reverseProxyApiBaseUrl",
                       default="https://localhost/blockchains/local",
                       help="Base URL for Helen REST API calls. Test cases drill "
                            "down further into the API with values such as '/api/users', "
                            "'/api/concord/blocks', '/api/concord/eth', etc...).")
   parser.add_argument("--inDockerReverseProxyApiBaseUrl",
                       default="https://reverse-proxy/blockchains/local",
                       help="Base URL for accessing the reverse proxy server from within "
                            "the docker environment.")
   parser.add_argument("--ethrpcApiUrl",
                       default=None,
                       help="By default, Helen's getMembers API is used to fetch ethrpc nodes, "
                            "and test cases randomly select nodes from that pool.  To force use "
                            "of one node, or to use an official Ethereum setup, specify its "
                            "url here.  e.g. 'http://localhost:8545'. NOTE: VMware IT only allows "
                            "https traffic over port 443 if you are in the 'vmware' network. "
                            "Use a different network (e.g. 'vmwareguest') if you must use a different "
                            "port and the replicas will be outside of VMware's network.")
   parser.add_argument("--contractCompilerApiBaseUrl",
                       default="http://localhost:3000/api/v1",
                       help="Base URL for the contract compiler microservice")
   parser.add_argument("--suitesRealname",
                       default="",
                       help="Comma-separated list of real names for supplied suites argument")
   parser.add_argument("--su",
                       action="store_true", # implies default=False
                       help="Super user privilege with all Jenkins injected credentials available.")
   parser.add_argument("--spiderImageTag", default=None,
                       help="Spider image tag, optional. If not passed in, an appropriate one will be determined.")
   parser.add_argument("--marketFlavor", default=chessplus_helper.DEFAULT_MARKET_FLAVOR,
                       help="market flavor (sample, cde7, etc)")
   parser.add_argument("--concurrency", default=chessplus_helper.DEFAULT_CONCURRENCY,
                       help="Concurrency")
   parser.add_argument("--noOfRequests", default=chessplus_helper.DEFAULT_NO_OF_REQUESTS,
                       help="No. of requests from chess+")
   parser.add_argument("--dockerHubUser", default="blockchainrepositoryreader",
                       help="DockerHub user which has read access to the digitalasset private repos. " \
                       "Only needed if the DAML SDK version is not one of {}.".format(list(chessplus_helper.KNOWN_SDK_SPIDER_VERSION_MAPPINGS.keys())))
   parser.add_argument("--dockerHubPassword",
                       help="DockerHub password which has read access to the digitalasset private repos. " \
                       "Only needed if the DAML SDK version is not in {}.".format(list(chessplus_helper.KNOWN_SDK_SPIDER_VERSION_MAPPINGS.keys())))
   parser.add_argument("--runID", default=helper.get_time_now_in_milliseconds(),
                       help="Unique ID to differentiate runs")
   parser.add_argument("--allureDir",
                       default=tempfile.gettempdir(),
                       help="Location for storing data for Allure reports")



   concordConfig = parser.add_argument_group("Concord configuration")
   concordConfig.add_argument("--runConcordConfigurationGeneration",
      help="Run Concord configuration generation for the test  cluster before "
           "launching and launch with the newly generated configuration files. "
           "If this option is not given, then configuration generation will be "
           "skipped and the currently existing configuration files will be used.",
      default=False,
      action='store_true')
   concordConfig.add_argument("--concordConfigurationInput",
      help="The input file to the configuration generation utility. "
           "Note: --runConcordConfigurationGeneration has to be set. "
           "Note: The path specified is the absolute path within a Concord container",
      default="/concord/config/dockerConfigurationInput.yaml")

   nonLocalDeployConfig = parser.add_argument_group("SDDC Deployment Parameters")
   nonLocalDeployConfig.add_argument("--blockchainLocation",
                       help="Location of the blockchain being tested.  Values: " \
                            "{} (default), {}, {}. {} not implemented." \
                       .format(helper.LOCATION_LOCAL,
                               helper.LOCATION_SDDC,
                               helper.LOCATION_ONPREM,
                               helper.LOCATION_ONPREM),
                       default=helper.LOCATION_LOCAL)
   nonLocalDeployConfig.add_argument("--blockchainType",
                       help="Type of blockchain to deploy if --blockchainLocation is not 'local'.  Values: " \
                            "{} (default), {}, {}, {}" \
                       .format(helper.TYPE_ETHEREUM,
                               helper.TYPE_DAML,
                               helper.TYPE_HLF,
                               helper.TYPE_TEE),
                       default=helper.TYPE_ETHEREUM)
   nonLocalDeployConfig.add_argument("--numReplicas",
                       help="The number of blockchain replicas to deploy. The 'f' value will be " \
                       "calculated automatically using f = (numReplicas - 1)/3. If Helen does not " \
                       "like the combination of the replica count and f value, deployment will fail.",
                       default=4)
   nonLocalDeployConfig.add_argument("--keepBlockchains",
                       help="Whether to keep the blockchain(s) deployed by this run. " \
                            "Valid values: {}.  Default: '{}'".format([helper.KEEP_BLOCKCHAINS_ALWAYS,
                                                                       helper.KEEP_BLOCKCHAINS_ON_FAILURE,
                                                                       helper.KEEP_BLOCKCHAINS_NEVER],
                                                                      helper.KEEP_BLOCKCHAINS_NEVER),
                       default=helper.KEEP_BLOCKCHAINS_NEVER)
   nonLocalDeployConfig.add_argument("--numParticipants",
                                     help="The number of participant/client nodes to deploy.",
                                     default=1)
   nonLocalDeployConfig.add_argument("--migrationFile",
                                     help="Helen Flyway migration file to write generated configurations into, "
                                          "before launching Helen",
                                     default=helper.MIGRATION_FILE)
   nonLocalDeployConfig.add_argument("--damlParticipantIP",
                                     help="Public IP of the DAML participant to upload DAR and run tests",
                                     default='localhost')
   nonLocalDeployConfig.add_argument("--replicasConfig",
                                     help="Replicas config file obtained after a helen/persephone deployment. "
                                     'Sample format: { "daml_committer": [ { "ip": "10.73.232.56", ... }, ... ], "daml_participant": [ { "ip": "10.73.232.65", ... } ] }',
                                     default=None)
   nonLocalDeployConfig.add_argument("--deploymentOrg",
                                     help="Org to use for the deployment for long running tests. An org can specify details such as the Concord version to deploy and feature flags. "
                                          "Note that this org must first be created in CSP, with vmbc_test_con_admin@csp.local having the consortium admin role. "
                                          "Also, an API key with deployment permissions must be created and added to hermes/util/auth.py.",
                                     default=None)
   nonLocalDeployConfig.add_argument("--deploymentService",
                                     help="The blockchain service to use for long running tests. (Feel free to adopt for other tests.) Valid values: " \
                                     "A url or the word 'staging'. Defaults to https://localhost/blockchains/local (same as reverseProxyApiBaseUrl)". \
                                     format(auth.SERVICE_STAGING),
                                     default="https://localhost/blockchains/local")
   nonLocalDeployConfig.add_argument("--numGroups",
                                     help="The number of groups for the client node grouping.",
                                     default=1)

   args = parser.parse_args()

   # Don't make people memorize the staging url.
   if args.deploymentService == "staging":
      args.deploymentService = auth.SERVICE_STAGING

   parent_results_dir = args.resultsDir

   args.allureDir = parent_results_dir if args.allureDir is tempfile.gettempdir() else args.allureDir
   
   
   # In future, if the location of main.py changes from hermes/,
   # update args.hermes_dir accordingly
   dir_path = os.path.dirname(os.path.realpath(__file__))
   args.hermes_dir = dir_path

   global log
   import util.hermes_logging
   log = util.hermes_logging.getMainLogger()
   args.logLevel = hermes_logging.logStringToInt(args.logLevel)
   hermes_logging.setUpLogging(args)
   helper.CMDLINE_ARGS = vars(args)
   log.info("Args: {}".format(args))
   product = None
   resultFile = None
   totalSuccess = True
   allResults = {}
   log.info("Suites to run: {}".format(args.suites.split(",")))
   log.info("Allure Reports folder: {0}, parent results dir: {1}".format(args.allureDir, parent_results_dir))

   suitesRealname = args.suitesRealname.split(",") if args.suites else []
   if args.su:
      helper.WITH_JENKINS_INJECTED_CREDENTIALS = True
      userConfig = helper.getUserConfig()
      zoneConfig = helper.getZoneConfig()
   if args.zoneOverride:
      import util.infra
      folder = args.zoneOverrideFolder if hasattr(args, "zoneOverrideFolder") else None
      util.infra.overrideDefaultZones(args.zoneOverride, folder)

   #Valid Test Suite List?
   allSuitesValid = True
   unKnownSuites = []
   for i, suiteName in enumerate(args.suites.split(",")):
      if suiteName not in suiteList:
         allSuitesValid = (allSuitesValid and False)
         unKnownSuites.append(suiteName)
      else:
         allSuitesValid = (allSuitesValid and True)

   if not allSuitesValid :
      log.error("Unknown Suites found: {}".format(unKnownSuites))
      log.info("Available Test Suites {}".format([*suiteList]))
      try: raise Exception("Unknown test suite")
      except Exception as e: log.error(e); addExceptionToSummary(e)
      exit(3)

   # Generate gRPC bindings for Hermes
   log.info("Generating Persephone gRPC bindings - source: {} ; destination: {}"
            .format(helper.PERSEPHONE_GRPC_BINDINGS_SRC_PATH, helper.PERSEPHONE_GRPC_BINDINGS_DEST_PATH))
   generate_grpc_bindings.generate_bindings(helper.PERSEPHONE_GRPC_BINDINGS_SRC_PATH,
                                            helper.PERSEPHONE_GRPC_BINDINGS_DEST_PATH)

   for i, suiteName in enumerate(args.suites.split(",")):
      if args.eventsFile:
         event_recorder.record_event(suiteName, "Start", args.eventsFile)

      for run_count in range(1, args.repeatSuiteRun+1):
         log.info("\nTestrun: {0}/{1}".format(run_count, args.repeatSuiteRun))
         args.resultsDir = createResultsDir(suiteName,
                                            parent_results_dir=parent_results_dir)
         log.info("Results directory: {}".format(args.resultsDir))
         suite = pytest_suite.PytestSuite(args, suiteName, suiteList[suiteName], product)
         map_run_id_to_this_run(args, parent_results_dir)
         if suite is None:
            try: raise Exception("Unknown test suite")
            except Exception as e: log.error(e); addExceptionToSummary(e)
            exit(3)

         helper.CURRENT_SUITE_NAME = suiteName
         if i < len(suitesRealname) and suitesRealname[i]:
            helper.CURRENT_SUITE_NAME = suitesRealname[i]
         helper.CURRENT_SUITE_LOG_FILE = suite._testLogFile

         if pipeline.isDryRun():
            # CI dry run might not have to run the suite
            # if invocation signatures didn't change
            if pipeline.isQualifiedToSkip():
               pipeline.dryRunSaveInvocation()
               continue
            else:
               pipeline.dryRunSaveInvocation(runSuite=True)
         elif helper.thisHermesIsFromJenkins():
            pipeline.saveInvocationSignature(runSuite=True)
         
         # at this point, the suite is going to run; mark as executed
         if helper.thisHermesIsFromJenkins():
            pipeline.markInvocationAsExecuted()

         try:
            log.info("Running {}".format(suiteName))
            resultFile, product = suite.run()
            log.info("Finished running {}".format(suiteName))
         except ProductLaunchException as e:
            addExceptionToSummary(e)
            log.error("Product launch exception with suite {}".format(suiteName))
            traceback.print_exc()
            resultFile = e.logFile
            product = None
         except Exception as e:
            addExceptionToSummary(e)
            log.error(
               "Uncaught test exception in suite {}".format(suiteName))
            log.error(e)
            traceback.print_exc()
            resultFile = suite.getResultFile()

         try:
            suiteSuccess, suiteSuccessMessage = processResults(resultFile)

            # TODO Add support for localhost logs too
            all_replicas_and_type = None
            if args.replicasConfig:
               all_replicas_and_type = helper.parseReplicasConfig(
                  args.replicasConfig)
            elif args.damlParticipantIP != "localhost":
               all_replicas_and_type = {
                  helper.TYPE_DAML_PARTICIPANT: [args.damlParticipantIP]}

            if not suiteSuccess and all_replicas_and_type:
               log.info("*************************************")
               log.info("Collecting support bundle(s)...")
               for blockchain_type, replica_ips in all_replicas_and_type.items():
                  helper.create_concord_support_bundle(
                        replica_ips, blockchain_type, args.resultsDir)
               log.info("*************************************")
         except Exception as e:
               addExceptionToSummary(e)
               suiteSuccess = False
               suiteSuccessMessage = "Log {} for suite {} could not be processed.".format(
                  resultFile, suiteName)

         allResults[suiteName] = {
               "message": suiteSuccessMessage,
               "logs": suite.getTestLogDir()
         }

         totalSuccess = totalSuccess and suiteSuccess

         if not totalSuccess:
               update_repeated_suite_run_result(
                  parent_results_dir, "fail", args.repeatSuiteRun)
               break

      if args.eventsFile:
            event_recorder.record_event(suiteName, "End", args.eventsFile)

   # CI dry run does not have to actually run the suites
   if pipeline.isDryRun() and pipeline.isQualifiedToSkip():
     pipeline.markInvocationAsNotExecuted()
     helper.hermesPreexitWrapUp()
     exit(0)

   update_repeated_suite_run_result(parent_results_dir, "pass", args.repeatSuiteRun)
   printAllResults(allResults)
   helper.hermesPreexitWrapUp()

   if not totalSuccess:
      try: raise Exception("Not a total success for suite '{}'".format(helper.CURRENT_SUITE_NAME))
      except Exception as e: log.error(e); addExceptionToSummary(e)
      exit(2)


def printAllResults(allResults):
    '''
    Summarize the suite results on the console.
    '''
    longestName = 0
    longestMessage = 0
    longestDir = 0

    for suiteName in allResults:
        longestName = len(suiteName) if len(
            suiteName) > longestName else longestName

        messageLength = len(numbers_strings.stripEscapes(
            allResults[suiteName]["message"]))
        longestMessage = messageLength if messageLength > longestMessage else longestMessage

        logPath = allResults[suiteName]["logs"]
        longestDir = len(logPath) if len(logPath) > longestDir else longestDir

    longestValues = [longestName, longestMessage, longestDir]
    header = numbers_strings.createLogRow(
        ["Suite", "Result", "Logs"], longestValues)
    log.info(header)

    for suiteName in allResults:
        row = numbers_strings.createLogRow([suiteName,
                                            allResults[suiteName]["message"],
                                            allResults[suiteName]["logs"]],
                                           longestValues)
        log.info(row)


def update_repeated_suite_run_result(parent_results_dir, result, no_of_runs):
    if no_of_runs > 1:
        result_file = os.path.join(parent_results_dir,
                                   'test_status.{0}'.format(result))
        log.info("Repeated Suite run result: {0} [{1}]".format(
            result, result_file))
        open(result_file, 'a').close()


def createResultsDir(suiteName, parent_results_dir=tempfile.gettempdir()):
   '''
   Create
   <parent_results_dir>/ContractCompilerTests/ContractCompilerTests_20200305_140416
   <parent_results_dir>/ContractCompilerTests/ContractCompilerTests.log
   '''
   prefix = suiteName + "_" + strftime("%Y%m%d_%H%M%S", localtime())
   results_dir = os.path.join(suiteName, parent_results_dir, prefix)
   if not os.path.exists(results_dir): os.makedirs(results_dir)
   return results_dir

def processResults(resultFile):
    '''
    Process a result file, outputting an html file.  If we ever need to output
    a file in another format (such as a CI tool), do that here.
    '''
    results = json_helper.readJsonFile(resultFile)
    testCount, passCount, failCount, skippedCount = tallyResults(results)
    fileContents = html.createResultHeader(results,
                                           testCount,
                                           passCount,
                                           failCount,
                                           skippedCount)
    fileContents += html.createResultTable(results)
    fileContents += html.createHtmlFooter()
    fileLocation = os.path.join(os.path.dirname(resultFile), "results.html")

    with open(fileLocation, "w") as f:
        f.write(fileContents)

    log.info("Results written to '{}'".format(fileLocation))

    suiteName = list(results.keys())[0]
    red = "\033[1;31m"
    green = "\033[1;32m"
    yellow = "\033[1;33m"
    reset = "\033[0m"

    msg = "{color}{symbol} " + suiteName + " {tests}" + reset

    # If this output changes, make sure vars/gitlabBuildSteps.groovy, runTests(),
    # will still be able to find it.
    if failCount > 0 or passCount <= 0:
        msg = msg.format(color=red, symbol="\u2717",
                         tests="{} tests failed".format(failCount))
    else:
        msg = msg.format(color=green, symbol="\u2714",
                         tests="{} tests succeeded".format(passCount))

    if skippedCount > 0:
        msg += " {}{} tests skipped{}".format(yellow, skippedCount, reset)

    log.info(msg)

    return failCount == 0 and passCount != 0, msg


def tallyResults(results):
    '''
    Given the results structure, returns:
    - Total number of tests run.
    - Passes
    - Failures
    - Skipped
    '''
    suiteName = list(results.keys())[0]
    testCases = results[suiteName]["tests"]
    passCount = 0
    failCount = 0
    skippedCount = 0
    totalCount = 0

    for test in testCases:
        totalCount += 1

        if testCases[test]["result"] == "PASS":
            passCount += 1
        elif testCases[test]["result"] == "FAIL":
            failCount += 1
        elif testCases[test]["result"] == "SKIPPED":
            skippedCount += 1

    return totalCount, passCount, failCount, skippedCount

def map_run_id_to_this_run(args, parent_results_dir):
   '''
   Map a run ID for each run. This helps mapping iterations and log directorey
   in long running test runs
   :param args: all command line args
   :param parent_results_dir: base results diir
   '''
   hermes_testrun_info_file = os.path.join(parent_results_dir,
                                           helper.hermes_testrun_info_filename)
   data = {
      helper.testrun_info_results_dir_key_name: args.resultsDir
   }

   hermes_testrun_info_lock_file = "{}.lock".format(hermes_testrun_info_file)
   max_tries = 5
   count = 0
   while count < max_tries: # avoid deadlock if a parallel run is writing into the file
      if os.path.exists(hermes_testrun_info_lock_file):
         sleep(1)
      else:
         break
      count += 1

   if not os.path.exists(hermes_testrun_info_lock_file):
      try:
         # create lock file if we have parallel runs in future
         with open(hermes_testrun_info_lock_file, 'x') as f:
            pass

         if os.path.exists(hermes_testrun_info_file):
            with open(hermes_testrun_info_file) as json_fp:
               json_data = json.load(json_fp)
         else:
            json_data = {}
         json_data[args.runID] = data

         with open(hermes_testrun_info_file, "w") as fp:
            json.dump(json_data, fp, indent=2)

         # delete lock file
         os.remove(hermes_testrun_info_lock_file)
      except FileExistsError:
         pass


main()
