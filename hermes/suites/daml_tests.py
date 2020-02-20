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
from fixtures.common_fixtures import fxHermesRunSettings, fxProduct

log = logging.getLogger(__name__)

# Read by the fxProduct fixture.
productType = helper.TYPE_DAML

def test_ledger_api_test_tool(fxProduct):
   """Run ledger_api_test_tool
   """
   try:
      daml_helper.upload_test_tool_dars(host='localhost', port='6861')
      daml_helper.verify_ledger_api_test_tool(host='ledger', port='6865',
                                              tests_to_include="ALL")
   except Exception as e:
      log.error(e)
      raise

   log.info("DAML tests passed.")
