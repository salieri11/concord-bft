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

import util.helper as helper
import util.daml.daml_helper as daml_helper

from . import test_suite
from suites.case import describe
from fixtures.common_fixtures import fxHermesRunSettings, fxProduct

import util.hermes_logging
log = util.hermes_logging.getMainLogger()

# Read by the fxProduct fixture.
productType = helper.TYPE_DAML

@describe()
def test_ledger_api_test_tool(fxProduct, fxHermesRunSettings):
   """Run ledger_api_test_tool
   """
   if fxHermesRunSettings["hermesCmdlineArgs"].replicasConfig:
      all_replicas = helper.parseReplicasConfig(
         fxHermesRunSettings["hermesCmdlineArgs"].replicasConfig)
      ledger_api_hosts = all_replicas["daml_participant"]
   else:
      ledger_api_hosts = fxHermesRunSettings[
         "hermesCmdlineArgs"].damlParticipantIP.split(",")

   for ledger_api_host in ledger_api_hosts:
      log.info("ledger_api_host: {}".format(ledger_api_host))
      if ledger_api_host == 'localhost':
         upload_host = ledger_api_host
         upload_port = '6861'
         test_host = 'ledger'
         test_port = '6865'
      else:
         upload_host = test_host = ledger_api_host
         forwarding_src_port = helper.FORWARDED_DAML_LEDGER_API_ENDPOINT_PORT
         upload_port = test_port = str(forwarding_src_port)

      try:
         log.info("Starting DAR upload on {}:{}".format(upload_host, upload_port))
         daml_helper.upload_test_tool_dars(host=upload_host, port=upload_port)
         log.info("Starting DAML verification tests on {}:{}".format(test_host, test_port))
         daml_helper.verify_ledger_api_test_tool(host=test_host, port=test_port, run_all_tests=True)
         log.info("DAR upload and verification successful on {}".format(test_host))
      except Exception as e:
         log.error(e)
         raise
      log.info("DAML tests passed.")
