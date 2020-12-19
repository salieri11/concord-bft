#################################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#################################################################################
# Runs blockbench load generator
#    Perform start and getprogress operations
# Usage:
# /var/jenkins/workspace/venv_py37/bin/python3 -m pytest suites/blockbench_tests.py
#  --noLaunch --blockbenchSpec resources/blockbench_start.json
#   --blockbenchRepoPath /var/jenkins/workspace/block-bench/ --damlParticipantIP 10.72.220.23
#
#   --blockbenchOperation={run,start,getprogress}
#                         Choose the action - Run the test till completion or just start or just get progress


import util.helper as helper
import util.blockbench as blockbench

from suites.case import describe
from fixtures.common_fixtures import fxProduct

import util.hermes_logging
log = util.hermes_logging.getMainLogger()

productType = helper.TYPE_BLOCKBENCH  # Read by the fxProduct fixture


@describe("Blockbench test")
def test_blockbench(fxProduct, fxHermesRunSettings):
    """
    Execute blockbench test
    :param fxProduct: Pytest Fixture
    :param fxHermesRunSettings: Pytest Fixture
    :return: result
    """
    args = fxHermesRunSettings["hermesCmdlineArgs"]
    if args.replicasConfig:
        all_replicas = helper.parseReplicasConfig(
            args.replicasConfig)
        ledger_api_hosts = all_replicas["daml_participant"]
        log.info("Getting client IPs from --replicasConfig")
    elif args.damlParticipantIP:
        ledger_api_hosts = fxHermesRunSettings[
            "hermesCmdlineArgs"].damlParticipantIP.split(",")
        log.info("Getting client IPs from --damlParticipantIP")
    else:
        ledger_api_hosts = []

    over_all_status = None
    status = blockbench.blockbench_main(args, ledger_api_hosts)
    if status:
        log.info("**** Test passed on {}".format(args.blockbench_spec))
        if over_all_status is None:
            over_all_status = True
    else:
        log.error("**** Test failed on {}".format(args.blockbench_spec))
        over_all_status = False
    log.info("Overall Run status: {}".format(over_all_status))
    assert over_all_status, "Blockbench Run Failed"

