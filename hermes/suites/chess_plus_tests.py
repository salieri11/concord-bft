#################################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#################################################################################
import logging
import os
import pytest
import subprocess
import time
import traceback

import util.helper as helper
import util.chessplus.chessplus_helper as chessplus_helper

from . import test_suite
from suites.case import describe
from fixtures.common_fixtures import fxHermesRunSettings, fxProduct

import util.hermes_logging
log = util.hermes_logging.getMainLogger()

productType = helper.TYPE_CHESSPLUS # Read by the fxProduct fixture


@describe("Chess+ test on Blockchain")
def test_chessplus(fxProduct, fxHermesRunSettings):
   args = fxHermesRunSettings["hermesCmdlineArgs"]
   if args.replicasConfig:
      all_replicas = helper.parseReplicasConfig(
         args.replicasConfig)
      ledger_api_hosts = all_replicas["daml_participant"]
   else:
      ledger_api_hosts = fxHermesRunSettings[
         "hermesCmdlineArgs"].damlParticipantIP.split(",")

   over_all_status = None
   for ledger_api_host in ledger_api_hosts:
      log.info("ledger_api_host: {}".format(ledger_api_host))

      status = chessplus_helper.run_chess_plus(args, ledger_api_host)
      if status:
         log.info("**** Test passed on {}".format(ledger_api_host))
         if over_all_status is None:
            over_all_status = True
      else:
         log.error("**** Test failed on {}".format(ledger_api_host))
         over_all_status = False

   log.info("Overall Run status: {}".format(over_all_status))
   assert over_all_status, "Chess+ Run Failed"

