# Copyright 2019 VMware, Inc.  All rights reserved. -- VMware Confidential

# In order to run DAML tests we need a special docker-compose and concord
# configuration file.
#
# main.py --runConcordConfigurationGeneration
#         --concordConfigurationInput /concord/config/dockerConfigurationInput-daml.yaml
#         --dockerComposeFile=../docker/docker-compose-daml.yml
#         DamlTests

import logging
import os
import pytest
import subprocess
import time
import traceback
from tempfile import NamedTemporaryFile

import util.daml.upload_dar as darutil
import util.helper as helper

from . import test_suite
from fixtures.common_fixtures import fxHermesRunSettings, fxProduct

log = logging.getLogger(__name__)

# Read by the fxProduct fixture.
productType = helper.TYPE_DAML

def test_ledger_api_test_tool(fxProduct):
   """Run ledger_api_test_tool
   """
   TEST_TOOL_NAME = "ledger-api-test-tool-100.13.39.jar"
   TEST_DARS = ["SemanticTests.dar", "Test-dev.dar", "Test-stable.dar"]
   TEST_TOOL_CONTAINER = "docker_daml_test_tool_1"
   error_msg = None

   log.info("Copy DAR files to hermes...")
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
         log.info("Save %s to %s", testDar, tmp.name)
         tmpDars.append(tmp.name)

   log.info("Upload DAR files...")
   for testDar in tmpDars:
      dar_uploaded = darutil.upload_dar(host='localhost', port='6861', darfile=testDar)
      assert dar_uploaded, "Failed to upload test DAR " + testDar
      os.remove(testDar)

   getTestToolImage = \
      "docker inspect --format='{{.Config.Image}}' " + TEST_TOOL_CONTAINER
   try:
      output = subprocess.run(getTestToolImage.split(),
         check=True, stdout=subprocess.PIPE).stdout
      testToolImage = output.decode().strip().strip("'")
   except subprocess.CalledProcessError as e:
      msg = "Failed to retrieve image for {}, error: {}".format(TEST_TOOL_CONTAINER, str(e))
      msg += "\n{}".format(e.output)
      log.error(msg)
      raise

   cmd = "docker run -t --rm " + \
             "--network docker_default " + \
             "--link docker_daml_ledger_api1_1:ledger " + \
             testToolImage + \
             " java -jar " + TEST_TOOL_NAME + \
                " --timeout-scale-factor 3.5" + \
                " --command-submission-ttl-scale-factor 3.5" + \
                " ledger:6865"
   log.info("Run %s...", TEST_TOOL_NAME)
   try:
      subprocess.check_call(cmd.split(), timeout=120)
   except subprocess.TimeoutExpired as e:
      msg = "Ledger API test tool timed out: {}".format(e)
      msg += "\n{}".format(e.output)
      log.error(msg)
      raise
   except subprocess.CalledProcessError as e:
      msg = "Ledger API test tool returned an error: {}".format(e)
      msg += "\n{}".format(e.output)
      log.error(msg)
      raise

   log.info("Semantic test runner passed.")
