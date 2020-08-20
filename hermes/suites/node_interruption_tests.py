#################################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# This suite covers the tests related to node interruption
# Usage: ./main.py NodeInterruptionTest  --replicasConfig <path to replicasConfig.json>
#
# Test passes node_interruption_details: {} to node_interruption_helper
#
#    node_interruption_details = {
#       intr_helper.NODE_INTERRUPTION_TYPE: intr_helper.NODE_INTERRUPT_VM_STOP_START,
#       intr_helper.NO_OF_NODES_TO_INTERRUPT: intr_helper.get_f_count(fxBlockchain),
#       intr_helper.SKIP_MASTER_REPLICA: True,
#       intr_helper.CUSTOM_INTERRUPTION_PARAMS: {
#          "place_holder_for_other_interruption_params": "value"
#       }
#    }
#################################################################################

import pytest
import util.helper as helper
import util.node_interruption_helper as intr_helper
from suites.case import describe
from fixtures.common_fixtures import fxHermesRunSettings, fxProduct, fxBlockchain, fxNodeInterruption
import util.hermes_logging

log = util.hermes_logging.getMainLogger()
helper.disable_duplicate_logs()

# Read by the fxProduct fixture
productType = helper.TYPE_NO_VERIFY


@describe("Node Interruption - VM Stop/start - 1 node")
def test_1_node_interruption_vm_stop_start(fxHermesRunSettings, fxBlockchain, fxNodeInterruption):
   node_interruption_details = {
      intr_helper.NODE_INTERRUPTION_TYPE: intr_helper.NODE_INTERRUPT_VM_STOP_START,
      intr_helper.NODE_TYPE_TO_INTERRUPT: helper.TYPE_DAML_COMMITTER,
      intr_helper.NO_OF_NODES_TO_INTERRUPT: 1,
      intr_helper.SKIP_MASTER_REPLICA: True,
      intr_helper.CUSTOM_INTERRUPTION_PARAMS: {
         "place_holder_for_other_interruption_params": "value"
      },
      intr_helper.NODE_INTERRUPTION_TIME: {
         "time_before_node_recovers": 5,
         "time_between_node_interruptions": 10
      }
   }

   # skipping master committer from interruption: bug BC-3264
   nodes_available_for_interruption = \
      intr_helper.get_nodes_available_for_interruption(
         fxBlockchain, node_interruption_details)

   status = False
   last_interrupted_node_index = None
   for iteration, ips in enumerate(nodes_available_for_interruption):
      nodes_to_interrupt, last_interrupted_node_index = \
         intr_helper.get_list_of_nodes_to_interrupt(
            nodes_available_for_interruption, node_interruption_details,
            last_interrupted_node_index=last_interrupted_node_index)

      log.info("************************************************************")
      log.info(
         "Iteration {} - Nodes to be interrupted ({}): {}".format(iteration + 1,
                                                                  node_interruption_details[
                                                                     intr_helper.NODE_TYPE_TO_INTERRUPT],
                                                                  nodes_to_interrupt))

      status = intr_helper.crash_and_restore_nodes(fxBlockchain,
                                                   fxHermesRunSettings,
                                                   nodes_to_interrupt,
                                                   node_interruption_details)
      if not status:
         log.info("")
         log.error("**** Test aborted/failed ****")
         break

   assert status, "Node Interruption Test Failed"
   log.info("**** Test completed successfully ****")


@describe("Node Interruption - VM Stop/start - 1 node")
@pytest.mark.committer_node_interruption
def test_1_node_interruption_vm_stop_start(fxHermesRunSettings, fxBlockchain, fxNodeInterruption):
   node_interruption_details = {
      intr_helper.NODE_INTERRUPTION_TYPE: intr_helper.NODE_INTERRUPT_VM_STOP_START,
      intr_helper.NODE_TYPE_TO_INTERRUPT: helper.TYPE_DAML_COMMITTER,
      intr_helper.NO_OF_NODES_TO_INTERRUPT: 1,
      intr_helper.SKIP_MASTER_REPLICA: True,
      intr_helper.CUSTOM_INTERRUPTION_PARAMS: {
         "place_holder_for_other_interruption_params": "value"
      },
      intr_helper.NODE_INTERRUPTION_TIME: {
         "time_before_node_recovers": 5,
         "time_between_node_interruptions": 10
      }
   }

   # skipping master committer from interruption: bug BC-3264
   nodes_available_for_interruption = \
      intr_helper.get_nodes_available_for_interruption(
         fxBlockchain, node_interruption_details)

   status = False
   last_interrupted_node_index = None
   for iteration, ips in enumerate(nodes_available_for_interruption):
      nodes_to_interrupt, last_interrupted_node_index = \
         intr_helper.get_list_of_nodes_to_interrupt(
            nodes_available_for_interruption, node_interruption_details,
            last_interrupted_node_index=last_interrupted_node_index)

      log.info("************************************************************")
      log.info(
         "Iteration {} - Nodes to be interrupted ({}): {}".format(iteration + 1,
                                                                  node_interruption_details[
                                                                     intr_helper.NODE_TYPE_TO_INTERRUPT],
                                                                  nodes_to_interrupt))

      status = intr_helper.crash_and_restore_nodes(fxBlockchain,
                                                   fxHermesRunSettings,
                                                   nodes_to_interrupt,
                                                   node_interruption_details)
      if not status:
         log.info("")
         log.error("**** Test aborted/failed ****")
         break

   assert status, "Node Interruption Test Failed"
   log.info("**** Test completed successfully ****")


@describe("Node Interruption - VM Stop/start - upto f nodes")
@pytest.mark.committer_node_interruption
def test_f_node_interruption_vm_stop_start(fxHermesRunSettings, fxBlockchain, fxNodeInterruption):
   node_interruption_details = {
      intr_helper.NODE_INTERRUPTION_TYPE: intr_helper.NODE_INTERRUPT_VM_STOP_START,
      intr_helper.NODE_TYPE_TO_INTERRUPT: helper.TYPE_DAML_COMMITTER,
      intr_helper.NO_OF_NODES_TO_INTERRUPT: intr_helper.get_f_count(fxBlockchain),
      intr_helper.SKIP_MASTER_REPLICA: True,
      intr_helper.CUSTOM_INTERRUPTION_PARAMS: {
         "place_holder_for_other_interruption_params": "value"
      },
      intr_helper.NODE_INTERRUPTION_TIME: {
         "time_before_node_recovers": 5,
         "time_between_node_interruptions": 10
      }
   }

   # skipping master committer from interruption: bug BC-3264
   nodes_available_for_interruption = \
      intr_helper.get_nodes_available_for_interruption(
         fxBlockchain, node_interruption_details)

   status = False
   last_interrupted_node_index = None
   for iteration, ips in enumerate(nodes_available_for_interruption):
      nodes_to_interrupt, last_interrupted_node_index = \
         intr_helper.get_list_of_nodes_to_interrupt(
            nodes_available_for_interruption, node_interruption_details,
            last_interrupted_node_index=last_interrupted_node_index)

      log.info("************************************************************")
      log.info(
         "Iteration {} - Nodes to be interrupted ({}): {}".format(iteration + 1,
                                                                  node_interruption_details[
                                                                     intr_helper.NODE_TYPE_TO_INTERRUPT],
                                                                  nodes_to_interrupt))

      status = intr_helper.crash_and_restore_nodes(fxBlockchain,
                                                   fxHermesRunSettings,
                                                   nodes_to_interrupt,
                                                   node_interruption_details)
      if not status:
         log.info("")
         log.error("**** Test aborted/failed ****")
         break

   assert status, "Node Interruption Test Failed"
   log.info("**** Test completed successfully ****")


@describe("1 Participant node crash/recovery - VM Stop/start")
@pytest.mark.participant_node_interruption
def test_1_participant_node_interruption_vm_stop_start(fxHermesRunSettings, fxBlockchain, fxNodeInterruption):
   node_interruption_details = {
      intr_helper.NODE_INTERRUPTION_TYPE: intr_helper.NODE_INTERRUPT_VM_STOP_START,
      intr_helper.NODE_TYPE_TO_INTERRUPT: helper.TYPE_DAML_PARTICIPANT,
      intr_helper.NO_OF_NODES_TO_INTERRUPT: 1,
      intr_helper.SKIP_MASTER_REPLICA: True,
      intr_helper.CUSTOM_INTERRUPTION_PARAMS: {
         "place_holder_for_other_interruption_params": "value"
      }
   }

   nodes_available_for_interruption = \
      intr_helper.get_nodes_available_for_interruption(
         fxBlockchain, node_interruption_details)

   status = False
   last_interrupted_node_index = None
   for iteration, ips in enumerate(nodes_available_for_interruption):
      nodes_to_interrupt, last_interrupted_node_index = \
         intr_helper.get_list_of_nodes_to_interrupt(
            nodes_available_for_interruption, node_interruption_details,
            last_interrupted_node_index=last_interrupted_node_index)

      log.info("************************************************************")
      log.info(
         "Iteration {} - Nodes to be interrupted ({}): {}".format(iteration + 1,
                                                                  node_interruption_details[
                                                                     intr_helper.NODE_TYPE_TO_INTERRUPT],
                                                                  nodes_to_interrupt))

      status = intr_helper.crash_and_restore_nodes(fxBlockchain,
                                                   fxHermesRunSettings,
                                                   nodes_to_interrupt,
                                                   node_interruption_details)
      if not status:
         log.info("")
         log.error("**** Test aborted/failed ****")
         break

   assert status, "Node Interruption Test Failed"
   log.info("**** Test completed successfully ****")
