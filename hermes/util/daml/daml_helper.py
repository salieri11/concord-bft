# Copyright 2019-2020 VMware, Inc.  All rights reserved. -- VMware Confidential
# DAML util file to perform daml tests

import sys
import os
import subprocess
from tempfile import NamedTemporaryFile

if 'hermes_util' in sys.modules.keys():
   import hermes_util.daml.upload_dar as darutil
   import hermes_util.helper as helper
   import hermes_util.hermes_logging as hermes_logging
else:
   from . import upload_dar as darutil
   from util import hermes_logging, helper

log = hermes_logging.getMainLogger()

TEST_TOOL_NAME = "ledger-api-test-tool-100.13.54.jar"
TEST_DARS = ["SemanticTests.dar", "Test-dev.dar", "Test-stable.dar"]
TEST_TOOL_CONTAINER = "docker_daml_test_tool_1"
DEFAULT_DAML_TEST = "SemanticTests"
DAML_TESTS_IGNORE_LIST=[
   "TimeIT",
   "LotsOfPartiesIT",
   "TransactionScaleIT",
   "TransactionServiceIT",
   "CommandServiceIT",                      # [FT] Temporarily disabled, works around need for newer test tool
   "ConfigManagementServiceIT",             # [FT] Temporarily disabled, works around time model issue
   "ContractKeysSubmitterIsMaintainerIT",   # [FT] Temporarily disabled, works around need for newer test tool
   "LedgerConfigurationServiceIT",          # [FT] Temporarily disabled, works around need for newer test tool
   "TransactionServiceIT"                   # [FT] Temporarily disabled, works around need for newer test tool
]
error_msg = None

def upload_test_tool_dars(host='localhost', port='6861'):
   '''
   Helper method to upload test tool dar files
   :param host: daml-ledger-api host IP
   :param port: daml-ledger-api service port
   '''
   log.info("Copy test tool DAR files to hermes...")
   tmpDars = []
   for testDar in TEST_DARS:
      with NamedTemporaryFile(delete=False) as tmp:
         getDar = "docker cp {}:/{} {}".format(TEST_TOOL_CONTAINER, testDar, tmp.name)
         try:
            subprocess.check_call(getDar.split())
         except subprocess.CalledProcessError as e:
            msg = "Failed to copy DAR ({}): {}".format(testDar, e)
            msg += "\n{}".format(e.output)
            log.error(msg)
            raise
         log.debug("Save %s to %s", testDar, tmp.name)
         tmpDars.append(tmp.name)

   log.info("Upload DAR files...")
   for testDar in tmpDars:
      dar_uploaded = False
      max_retry_attempts = 3
      for i in range(0, max_retry_attempts):
         log.info("  {} (attempt {}/{})...".format(testDar, i+1, max_retry_attempts))
         try:
            dar_uploaded = darutil.upload_dar(host=host, port=port, darfile=testDar)
         except Exception as e:
            if i != max_retry_attempts:
               log.info("Retrying...")
         else:
            os.remove(testDar)
            break
      assert dar_uploaded, "Failed to upload test DAR " + testDar

def get_list_of_tests(run_all_tests=False):
   '''
   Helper method to list all the tests from test tool.jar
   :param run_all_tests: False to run only default set  of tests(marked with a * from list tests),
    True, to include the complete list of tests
   :return: list of tests
   '''
   getTestToolImage = \
      "docker inspect --format='{{.Config.Image}}' " + TEST_TOOL_CONTAINER
   try:
      output = subprocess.run(getTestToolImage.split(),
         check=True, stdout=subprocess.PIPE).stdout
      testToolImage = output.decode().strip().strip("'")
      log.debug(testToolImage)
   except subprocess.CalledProcessError as e:
      msg = "Failed to retrieve image for {}, error: {}".format(TEST_TOOL_CONTAINER, str(e))
      msg += "\n{}".format(e.output)
      log.error(msg)
      raise

   tests = []
   cmd_list_tests = "docker run -t --rm " + \
                "--network docker_default " + \
                "--link docker_daml_ledger_api1_1:ledger " + \
                testToolImage + " java -jar " + TEST_TOOL_NAME + " --list"
   status, output = helper.execute_ext_command(cmd_list_tests.split())

   if status:
      for item in output:
         if item and item != '\r' and ("Tests marked with * are run by default." not in item):
            if run_all_tests:
               tests.append(item.split("*")[0].strip())
            else:
               if DEFAULT_DAML_TEST in item:
                  tests.append(item.split("*")[0].strip())
   else:
      raise Exception("Failed to fetch list of DAML tests")

   if len(tests) == 0:
      raise Exception("No DAML tests fetched")
   else:
      log.info("List of tests: {}".format(tests))

   return tests

def verify_ledger_api_test_tool(host='ledger', port='6865', run_all_tests=False):
   '''
   Helper method to perform sanity check with uploaded dar files
   :param host: daml-ledger-api host IP
   :param port: daml-ledger-api service port
   '''
   log.info("Performing DAML sanity checks...")
   getTestToolImage = \
      "docker inspect --format='{{.Config.Image}}' " + TEST_TOOL_CONTAINER
   try:
      output = subprocess.run(getTestToolImage.split(),
         check=True, stdout=subprocess.PIPE).stdout
      testToolImage = output.decode().strip().strip("'")
      log.debug(testToolImage)

   except subprocess.CalledProcessError as e:
      msg = "Failed to retrieve image for {}, error: {}".format(TEST_TOOL_CONTAINER, str(e))
      msg += "\n{}".format(e.output)
      log.error(msg)
      raise

   overall_test_status = None
   for test in get_list_of_tests(run_all_tests=run_all_tests):
      log.info(
         "######################################################################")
      if test not in DAML_TESTS_IGNORE_LIST:
         cmd = "docker run -t --rm " + \
                   "--network docker_default " + \
                   "--link docker_daml_ledger_api1_1:ledger " + \
                   testToolImage + \
                   " java -jar " + TEST_TOOL_NAME + \
                      " --include {}".format(test) + \
                      " --timeout-scale-factor 20" + \
                      " --command-submission-ttl-scale-factor 20" + \
                      " --no-wait-for-parties" + \
                      " {}:{}".format(host, port)
         log.info("")
         log.info("#### Run test '{}'... ####".format(test))
         try:
            subprocess.check_call(cmd.split(), timeout=360)
         except subprocess.TimeoutExpired as e:
            msg = "Ledger API test tool timed out: {}".format(e)
            msg += "\n{}".format(e.output)
            log.error(msg)
            overall_test_status = False
            continue
         except subprocess.CalledProcessError as e:
            msg = "Ledger API test tool returned an error: {}".format(e)
            msg += "\n{}".format(e.output)
            log.error(msg)
            overall_test_status = False
            continue

         log.info("Test '{}' passed.".format(test))
      else:
         log.info("#### Ignoring test '{}'".format(test))

   if overall_test_status is False:
      raise Exception("Overall DAML testsuite Status: Fail")
