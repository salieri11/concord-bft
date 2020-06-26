#################################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# This suite covers the tests related to node interruption
# Usage: ./main.py NodeInterruptionTest  --replicasConfig <path to replicasConfig.json>
#################################################################################

import pytest
import util.helper as helper
import util.node_interruption_helper as node_interruption_helper
from suites.case import describe
from fixtures.common_fixtures import fxHermesRunSettings, fxProduct, fxBlockchain, fxNodeInterruption
import util.hermes_logging
log = util.hermes_logging.getMainLogger()
helper.disable_duplicate_logs()

# Read by the fxProduct fixture
productType = helper.TYPE_PREDEPLOYED_BLOCKCHAIN

@describe("Node Interruption - VM Stop/start - 1 node")
def test_1_node_interruption_vm_stop_start(fxHermesRunSettings, fxBlockchain, fxNodeInterruption):
   interruption_details = {
      "interruption_type": node_interruption_helper.NODE_INTERRUPT_VM_STOP_START,
      "params": {
         "place_holder_for_other_interruption_params": "value"
      },
      "no_of_nodes_to_interrupt": 1,
      "skip_master_replica": True
   }

   # skipping master committer from interruption: bug
   committers_available_for_interruption = \
      node_interruption_helper.get_committers_available_for_interruption(
         fxBlockchain, interruption_details)

   status = False
   last_interrupted_node_index = None
   for iteration, ips in enumerate(committers_available_for_interruption):
      nodes_to_interrupt, last_interrupted_node_index = \
         node_interruption_helper.get_list_of_replicas_to_interrupt(
            committers_available_for_interruption, interruption_details,
            last_interrupted_node_index=last_interrupted_node_index)

      log.info("************************************************************")
      log.info(
         "Iteration {} - Nodes to be interrupted: {}".format(iteration + 1,
                                                             nodes_to_interrupt))

      status = node_interruption_helper.crash_and_restore_nodes(fxBlockchain,
                                                                fxHermesRunSettings,
                                                                nodes_to_interrupt,
                                                                interruption_details)
      if not status:
         log.info("")
         log.error("**** Test aborted/failed ****")
         break

   assert status, "Node Interruption Test Failed"
   log.info("**** Test completed successfully ****")


@describe("Node Interruption - VM Stop/start - upto f nodes")
@pytest.mark.skip()
def test_f_node_interruption_vm_stop_start(fxHermesRunSettings, fxBlockchain, fxNodeInterruption):
   interruption_details = {
      "interruption_type": node_interruption_helper.NODE_INTERRUPT_VM_STOP_START,
      "params": {
         "place_holder_for_other_interruption_params": "value"
      },
      "no_of_nodes_to_interrupt": node_interruption_helper.get_f_count(fxBlockchain),
      "skip_master_replica": True
   }

   # skipping master committer from interruption: bug
   committers_available_for_interruption = \
      node_interruption_helper.get_committers_available_for_interruption(
         fxBlockchain, interruption_details)

   status = False
   last_interrupted_node_index = None
   for iteration, ips in enumerate(committers_available_for_interruption):
      nodes_to_interrupt, last_interrupted_node_index = \
         node_interruption_helper.get_list_of_replicas_to_interrupt(
            committers_available_for_interruption, interruption_details,
            last_interrupted_node_index=last_interrupted_node_index)

      log.info("************************************************************")
      log.info(
         "Iteration {} - Nodes to be interrupted: {}".format(iteration + 1,
                                                             nodes_to_interrupt))

      status = node_interruption_helper.crash_and_restore_nodes(fxBlockchain,
                                                                fxHermesRunSettings,
                                                                nodes_to_interrupt,
                                                                interruption_details)
      if not status:
         log.info("")
         log.error("**** Test aborted/failed ****")
         break

   assert status, "Node Interruption Test Failed"
   log.info("**** Test completed successfully ****")
