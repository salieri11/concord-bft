# Copyright 2019-2020 VMware, Inc.  All rights reserved. -- VMware Confidential
# DAML util file to perform daml tests

import logging
import os
import subprocess
from tempfile import NamedTemporaryFile

import util.daml.upload_dar as darutil
import util.helper as helper

log = logging.getLogger(__name__)

# Read by the fxProduct fixture.
productType = helper.TYPE_DAML

TEST_TOOL_NAME = "ledger-api-test-tool-100.13.52.jar"
TEST_DARS = ["SemanticTests.dar", "Test-dev.dar", "Test-stable.dar"]
TEST_TOOL_CONTAINER = "docker_daml_test_tool_1"
DAML_TESTS_IGNORE_LIST=["TimeIT","LotsOfPartiesIT", "TransactionScaleIT", "TransactionServiceIT"]
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
      dar_uploaded = darutil.upload_dar(host=host, port=port, darfile=testDar)
      assert dar_uploaded, "Failed to upload test DAR " + testDar
      os.remove(testDar)

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
               if "*" in item:
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