#!/usr/bin/python3

#########################################################################
# Copyright 2018 - 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import argparse
import datetime
import logging
import os
import sys
import tempfile
import traceback
from time import strftime, localtime, sleep

import event_recorder
from suites import (contract_compiler_tests, core_vm_tests,
                    ext_rpc_tests, ui_e2e_deploy_daml, hlf_tests, performance_tests, persephone_tests,
                    pytest_suite, regression_tests, sample_dapp_tests, simple_st_test,
                    ui_tests, websocket_rpc_tests, persistency_tests)
from util import helper, hermes_logging, html, json_helper, numbers_strings
from util.product import ProductLaunchException

sys.path.append("lib/persephone")

log = None
suiteList = [
   "ContractCompilerTests",
   "CoreVMTests",
   "DamlTests",
   "EvilTimeTests",
   "ExtendedRPCTests",
   "HelenAPITests",
   "HelenRoleTests",
   "HlfTests",
   "LintTests",
   "LoggingTests",
   "PerformanceTests",
   "PersephoneTests",
   "RegressionTests",
   "SampleDAppTests",
   "SampleSuite",
   "SimpleStateTransferTest",
   "ThinReplicaTests",
   "TimeTests",
   "TruffleTests",
   "UiTests",
   "WebSocketRPCTests",
   "MetadataPersistencyTests"
   "PrivacyTeeTests",
   "ApolloBftTests"
]
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


def main():
   suite = None

   # Initialize the runtime environment for this instance.
   initialize()

   parser = argparse.ArgumentParser()
   parser.add_argument("suites", help="Comma delimited list of test suites. " \
                       "Available suites: {}".format(suiteList))
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
                       "being run. For CoreVMTests, this is a directory or " \
                       "specific file relative to the VMTests directory. e.g. " \
                       "'--tests vmArithmeticTest' or " \
                       "'--tests vmArithmeticTest/add0.json'")
   parser.add_argument("--config",
                       help="User config file to be considered.")
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
                            "vmwblockchain/agent:e7cb6c3, etc...")
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

   args = parser.parse_args()
   parent_results_dir = args.resultsDir

   # In future, if the location of main.py changes from hermes/,
   # update args.hermes_dir accordingly
   dir_path = os.path.dirname(os.path.realpath(__file__))
   args.hermes_dir = dir_path

   global log
   import util.hermes_logging
   log = util.hermes_logging.getMainLogger()
   args.logLevel = hermes_logging.logStringToInt(args.logLevel)
   hermes_logging.setUpLogging(args)
   log.info("Args: {}".format(args))
   product = None
   resultFile = None
   totalSuccess = True
   allResults = {}
   log.info("Suites to run: {}".format(args.suites.split(",")))

   for suiteName in args.suites.split(","):
      if args.eventsFile:
         event_recorder.record_event(suiteName, "Start", args.eventsFile)

      for run_count in range(1, args.repeatSuiteRun+1):
         log.info("\nTestrun: {0}/{1}".format(run_count, args.repeatSuiteRun))
         args.resultsDir = createResultsDir(suiteName,
                                            parent_results_dir=parent_results_dir)
         log.info("Results directory: {}".format(args.resultsDir))
         suite = createTestSuite(args, suiteName, product)
         if suite is None:
            log.error("Unknown test suite")
            exit(3)

         try:
            log.info("Running {}".format(suiteName))
            resultFile, product = suite.run()
            log.info("Finished running {}".format(suiteName))
         except ProductLaunchException as e:
            log.error("Product launch exception with suite {}".format(suiteName))
            traceback.print_exc()
            resultFile = e.logFile
            product = None
         except Exception as e:
            log.error("Uncaught test exception in suite {}".format(suiteName))
            log.error(e)
            traceback.print_exc()
            resultFile = suite.getResultFile()

         try:
            suiteSuccess, suiteSuccessMessage = processResults(resultFile)
         except Exception:
            suiteSuccess = False
            suiteSuccessMessage = "Log {} for suite {} could not be processed.".format(resultFile, suiteName)

         allResults[suiteName] = {
            "message": suiteSuccessMessage,
            "logs": suite.getTestLogDir()
         }

         totalSuccess = totalSuccess and suiteSuccess

         if not totalSuccess:
            update_repeated_suite_run_result(parent_results_dir, "fail", args.repeatSuiteRun)
            break

      if args.eventsFile:
         event_recorder.record_event(suiteName, "End", args.eventsFile)

   update_repeated_suite_run_result(parent_results_dir, "pass", args.repeatSuiteRun)
   printAllResults(allResults)

   if not totalSuccess:
      exit(2)


def printAllResults(allResults):
   '''
   Summarize the suite results on the console.
   '''
   longestName = 0
   longestMessage = 0
   longestDir = 0

   for suiteName in allResults:
      longestName = len(suiteName) if len(suiteName) > longestName else longestName

      messageLength = len(numbers_strings.stripEscapes(allResults[suiteName]["message"]))
      longestMessage = messageLength if messageLength > longestMessage else longestMessage

      logPath = allResults[suiteName]["logs"]
      longestDir = len(logPath) if len(logPath) > longestDir else longestDir

   longestValues = [longestName, longestMessage, longestDir]
   header = numbers_strings.createLogRow(["Suite", "Result", "Logs"], longestValues)
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
      log.info("Repeated Suite run result: {0} [{1}]".format(result, result_file))
      open(result_file, 'a').close()


def createTestSuite(args, suiteName, product):
   if (suiteName == "SampleDAppTests"):
      return sample_dapp_tests.SampleDAppTests(args, product)
   elif (suiteName == "ContractCompilerTests"):
       return contract_compiler_tests.ContractCompilerTests(args, product)
   elif (suiteName == "CoreVMTests"):
      return pytest_suite.PytestSuite(args, "suites/core_vm_tests.py", product)
   elif (suiteName == "HelenAPITests"):
      return pytest_suite.PytestSuite(args, "suites/helen/api_test.py", product)
   elif (suiteName == "HelenRoleTests"):
      return pytest_suite.PytestSuite(args, "suites/helen/roles.py", product)
   elif (suiteName == "ExtendedRPCTests"):
      return ext_rpc_tests.ExtendedRPCTests(args, product)
   elif (suiteName == "WebSocketRPCTests"):
      return websocket_rpc_tests.WebSocketRPCTests(args, product)
   elif (suiteName == "PerformanceTests"):
      return performance_tests.PerformanceTests(args, product)
   elif (suiteName == "PersephoneTests"):
      return persephone_tests.PersephoneTests(args, product)
   elif (suiteName == "RegressionTests"):
      return regression_tests.RegressionTests(args, product)
   elif (suiteName == "SampleSuite"):
      return pytest_suite.PytestSuite(args, "suites/sample_suite.py", product)
   elif (suiteName == "SimpleStateTransferTest"):
      return simple_st_test.SimpleStateTransferTest(args, product)
   elif (suiteName == "TimeTests"):
      return pytest_suite.PytestSuite(args, "suites/time_service/basic_test.py", product)
   elif (suiteName == "EvilTimeTests"):
      return pytest_suite.PytestSuite(args, "suites/time_service/evil_test.py", product)
   elif (suiteName == "TruffleTests"):
      return truffle_tests.TruffleTests(args, product)
   elif (suiteName == "UiTests"):
      return ui_tests.UiTests(args, product)
   elif (suiteName == "DeployDamlTests"):
      return ui_e2e_deploy_daml.DeployDamlTests(args, product)
   elif (suiteName == "DamlTests"):
      return pytest_suite.PytestSuite(args, "suites/daml_tests.py", product)
   elif (suiteName == "HlfTests"):
      return hlf_tests.HlfTests(args, product)
   elif (suiteName == "ThinReplicaTests"):
      return pytest_suite.PytestSuite(args, "suites/thin_replica_tests.py", product)
   elif (suiteName == "LoggingTests"):
      return pytest_suite.PytestSuite(args, "suites/logging_tests.py", product)
   elif (suiteName == "MetadataPersistencyTests"):
      return persistency_tests.MetadataPersistencyTests(args, product)
   elif (suiteName == "PrivacyTeeTests"):
      return pytest_suite.PytestSuite(args, "suites/privacy_tee_tests.py", product)
   elif (suiteName == "ApolloBftTests"):
       return pytest_suite.PytestSuite(args, "suites/apollo_bft_tests.py", product)
   else:
      return None


def createResultsDir(suiteName, parent_results_dir=tempfile.gettempdir()):
   '''
   Create
   <parent_results_dir>/ContractCompilerTests/ContractCompilerTests_20200305_140416
   <parent_results_dir>/ContractCompilerTests/ContractCompilerTests.log
   '''
   prefix = suiteName + "_" + strftime("%Y%m%d_%H%M%S", localtime())
   results_dir = os.path.join(suiteName, parent_results_dir, prefix)
   os.makedirs(results_dir)
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
   if failCount > 0:
      msg = msg.format(color=red, symbol="\u2717",
                       tests="{} tests failed".format(failCount))
   else:
      msg = msg.format(color=green, symbol="\u2714",
                       tests="{} tests succeeded".format(passCount))

   if skippedCount > 0:
      msg += " {}{} tests skipped{}".format(yellow, skippedCount, reset)

   log.info(msg)

   return failCount == 0, msg

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

main()