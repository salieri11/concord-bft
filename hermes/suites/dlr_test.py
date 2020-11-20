#################################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Usage: /var/jenkins/workspace/venv_py37/bin/python -m pytest suites/dlr_test.py
#           --damlParticipantIP <DAML ledger host>
#           --dlrNoOfAgreements <No. of Agreements to run DLR simulation>
#           --dlrNoOfVuser <No. of Users to run DLR simulation>
#           --dlrLoadBatchSize <Batch size of agreements for an iteration>
#           --resultsDir <Results/log directory path>
#################################################################################

from suites.case import describe
from util import helper
from util.dlr import dlr_helper

import util.hermes_logging
log = util.hermes_logging.getMainLogger()

@describe("DLR test on Blockchain")
def test_dlr(fxHermesRunSettings):
   args = fxHermesRunSettings["hermesCmdlineArgs"]
   log.debug("Checking prerequisites for DLR test")
   dlr_helper.install_daml()
   success_node_version = dlr_helper.check_nodejs()
   success_k6 = dlr_helper.check_k6_tool()
   success_repository = dlr_helper.check_broadridge_repo()  # to be replaced in Jenkins Job

   if args.replicasConfig:
      all_replicas = helper.parseReplicasConfig(
         args.replicasConfig)
      ledger_api_hosts = all_replicas["daml_participant"]
   else:
      ledger_api_hosts = fxHermesRunSettings["hermesCmdlineArgs"].damlParticipantIP.split(",")

   test_status = None
   for ledger_api_host in ledger_api_hosts:
      log.info("ledger_api_host: {}".format(ledger_api_host))
      if success_node_version and success_k6 and success_repository:
         dlr_run_status = dlr_helper.run_dlr(args, ledger_api_host)
      else:
         log.error("Prerequisites are not satisfied")
         dlr_run_status = False
      if dlr_run_status:
         log.info("**** Test passed on {}".format(ledger_api_host))
         if test_status is None:
            test_status = True
      else:
         log.error("**** Test failed on {}".format(ledger_api_host))
         test_status = False

   log.info("Overall Run status: {}".format(test_status))
   assert test_status, "DLR Run Failed"
