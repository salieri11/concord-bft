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
   try:
      host = fxHermesRunSettings["hermesCmdlineArgs"].damlParticipantIp
      upload_host = host
      upload_port = '6861'
      test_host = 'ledger'
      test_port = '6865'

      if host != 'localhost':
         credentials = fxHermesRunSettings["hermesUserConfig"]["persephoneTests"]["provisioningService"]["concordNode"]

         # Use port 80 for DAML instead of 443. LedgerApiServer is listening on 6865 over plain text
         # Setting up port forwarding from 443 to 6865 results in the following exception
         # INFO: Transport failed
         # io.netty.handler.codec.http2.Http2Exception: HTTP/2 client preface string missing or corrupt.
         forwarding_src_port = 80
         helper.add_ethrpc_port_forwarding(host, credentials["username"], credentials["password"],
                                           src_port=forwarding_src_port, dest_port=6865)
         upload_port = test_port = str(forwarding_src_port)
         test_host = host

      log.info("Starting DAR upload on {}:{}".format(upload_host, upload_port))
      daml_helper.upload_test_tool_dars(host=upload_host, port=upload_port)
      log.info("Starting DAML verification tests on {}:{}".format(test_host, test_port))
      daml_helper.verify_ledger_api_test_tool(host=test_host, port=test_port, run_all_tests=True)
      log.info("DAR upload and verification successful on {}".format(host))

   except Exception as e:
      log.error(e)
      raise
   log.info("DAML tests passed.")
