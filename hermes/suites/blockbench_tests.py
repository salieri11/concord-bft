#################################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#################################################################################
# Runs blockbench load generator
#    Perform start and getprogress operations
# Usage:
#    /var/jenkins/workspace/venv_py37/bin/python3 -m pytest suites/blockbench_tests.py
#                              --noLaunch --blockbench_participant <ip address>
#  There are several command line options for blockbench
# Blockbench Parameters::
#   --blockbench_participant=BLOCKBENCH_PARTICIPANT
#                         Participant IP
#   --blockbench_name=BLOCKBENCH_NAME
#                         Name of the run
#   --blockbench_desc=BLOCKBENCH_DESC
#                         Blockbench Run Description
#   --blockbench_dapp={IOU,FREETRADE}
#                         Load generator to be used
#   --blockbench_hopCount={1,2}
#                         Hopcount
#   --blockbench_uploadDar
#                         Upload DAR
#   --blockbench_workloadModel={OPEN,CLOSE}
#                         Workload Model
#   --blockbench_workloadSize=BLOCKBENCH_WORKLOAD_SIZE
#                         blockbench count of trades
#   --blockbench_operation={run,start,getprogress}
#                         Choose the action - Run the test till completion or just start or just get progress
#   --blockbench_url=BLOCKBENCH_URL
#                         http://<host>:<port> where blockbench is executing
#   --blockbench_timeout=BLOCKBENCH_TIMEOUT
#                         http://<host>:<port> where blockbench is executing
#   --blockbench_connections=BLOCKBENCH_CONNECTIONS
#                         Number of loadgen connections
#   --blockbench_concurrency=BLOCKBENCH_CONCURRENCY
#                         Blockbench loadgeneration degree of concurrency

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
    over_all_status = None
    status = blockbench.blockbench_main(args)
    if status:
        log.info("**** Test passed on {}".format(args.blockbench_participant))
        if over_all_status is None:
            over_all_status = True
    else:
        log.error("**** Test failed on {}".format(args.blockbench_participant))
        over_all_status = False
    log.info("Overall Run status: {}".format(over_all_status))
    assert over_all_status, "Blockbench Run Failed"

