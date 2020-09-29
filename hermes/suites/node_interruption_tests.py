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
from fixtures.common_fixtures import fxProduct, fxBlockchain, fxNodeInterruption
import util.hermes_logging

log = util.hermes_logging.getMainLogger()
helper.disable_duplicate_logs()
import queue

daml_txn_result_queue = queue.Queue()

# Read by the fxProduct fixture
productType = helper.TYPE_NO_VERIFY

# skipping test from long running tests: bug BC-3861
@pytest.mark.skip
@describe("Node Interruption - VM Stop/start - 1 node")
@pytest.mark.smoke
@pytest.mark.committer_node_interruption
@pytest.mark.committer_node_interruption_smoke
@pytest.mark.committer_node_interruption_longrun
def test_1_node_interruption_vm_stop_start(fxHermesRunSettings, fxBlockchain, fxNodeInterruption):
   node_interruption_details = {
      intr_helper.NODE_INTERRUPTION_TYPE: intr_helper.NODE_INTERRUPT_VM_STOP_START,
      intr_helper.NODE_TYPE_TO_INTERRUPT: helper.TYPE_DAML_COMMITTER,
      intr_helper.NO_OF_NODES_TO_INTERRUPT: 1,
      intr_helper.SKIP_MASTER_REPLICA: True,
      intr_helper.CUSTOM_INTERRUPTION_PARAMS: {
         intr_helper.NODE_OFFLINE_TIME: 30,
         intr_helper.TIME_BETWEEN_INTERRUPTIONS: 180
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

# skipping test from long running tests: bug BC-4721
@pytest.mark.skip
@describe("Node Interruption - crash containers for all containers of committer nodes")
@pytest.mark.committer_container_crash
@pytest.mark.committer_node_interruption_longrun
def test_committer_nodes_all_container_crash(fxHermesRunSettings, fxBlockchain, fxNodeInterruption):
   node_interruption_details = {
      intr_helper.NODE_INTERRUPTION_TYPE: intr_helper.NODE_INTERRUPT_CONTAINER_CRASH,
      intr_helper.NODE_TYPE_TO_INTERRUPT: helper.TYPE_DAML_COMMITTER,
      intr_helper.NO_OF_NODES_TO_INTERRUPT: 1,
      intr_helper.SKIP_MASTER_REPLICA: True,
      intr_helper.CUSTOM_INTERRUPTION_PARAMS: {
         intr_helper.CONTAINERS_TO_CRASH: intr_helper.ALL_CONTAINERS,
         intr_helper.NODE_OFFLINE_TIME: 30,
         intr_helper.TIME_BETWEEN_INTERRUPTIONS: 180
      }
   }

   # skipping master committer from interruption: bug BC-3264
   committers_available_for_interruption = \
      intr_helper.get_nodes_available_for_interruption(
         fxBlockchain, node_interruption_details)

   status = False
   last_interrupted_node_index = None
   for iteration, ips in enumerate(committers_available_for_interruption):
      nodes_to_interrupt, last_interrupted_node_index = \
         intr_helper.get_list_of_nodes_to_interrupt(
            committers_available_for_interruption, node_interruption_details,
            last_interrupted_node_index=last_interrupted_node_index)

      log.info("************************************************************")
      log.info(
         "Iteration {} - Nodes to be interrupted: {}".format(iteration + 1,
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

@describe("Node Interruption - crash containers for user defined containers of committer nodes")
@pytest.mark.committer_container_crash
@pytest.mark.committer_node_interruption_longrun
def test_committer_nodes_few_container_crash(fxHermesRunSettings, fxBlockchain, fxNodeInterruption):
   node_interruption_details = {
      intr_helper.NODE_INTERRUPTION_TYPE: intr_helper.NODE_INTERRUPT_CONTAINER_CRASH,
      intr_helper.NODE_TYPE_TO_INTERRUPT: helper.TYPE_DAML_COMMITTER,
      intr_helper.NO_OF_NODES_TO_INTERRUPT: 1,
      intr_helper.SKIP_MASTER_REPLICA: True,
      intr_helper.CUSTOM_INTERRUPTION_PARAMS: {
         intr_helper.CONTAINERS_TO_CRASH: ["concord", "daml_execution_engine"],
         intr_helper.NODE_OFFLINE_TIME: 30,
         intr_helper.TIME_BETWEEN_INTERRUPTIONS: 180
      }
   }

   # skipping master committer from interruption: bug BC-3264
   committers_available_for_interruption = \
      intr_helper.get_nodes_available_for_interruption(
         fxBlockchain, node_interruption_details)

   status = False
   last_interrupted_node_index = None
   for iteration, ips in enumerate(committers_available_for_interruption):
      nodes_to_interrupt, last_interrupted_node_index = \
         intr_helper.get_list_of_nodes_to_interrupt(
            committers_available_for_interruption, node_interruption_details,
            last_interrupted_node_index=last_interrupted_node_index)

      log.info("************************************************************")
      log.info(
         "Iteration {} - Nodes to be interrupted: {}".format(iteration + 1,
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

# skipping test from long running tests: bug BC-3861
@pytest.mark.skip
@describe("Node Interruption - VM Stop/start - upto f nodes")
@pytest.mark.committer_node_interruption
@pytest.mark.committer_node_interruption_longrun
def test_f_node_interruption_vm_stop_start(fxHermesRunSettings, fxBlockchain, fxNodeInterruption):
   node_interruption_details = {
      intr_helper.NODE_INTERRUPTION_TYPE: intr_helper.NODE_INTERRUPT_VM_STOP_START,
      intr_helper.NODE_TYPE_TO_INTERRUPT: helper.TYPE_DAML_COMMITTER,
      intr_helper.NO_OF_NODES_TO_INTERRUPT: intr_helper.get_f_count(fxBlockchain),
      intr_helper.SKIP_MASTER_REPLICA: True,
      intr_helper.CUSTOM_INTERRUPTION_PARAMS: {
         intr_helper.NODE_OFFLINE_TIME: 30,
         intr_helper.TIME_BETWEEN_INTERRUPTIONS: 180
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
@pytest.mark.smoke
@pytest.mark.participant_node_interruption
@pytest.mark.participant_node_interruption_smoke
@pytest.mark.participant_node_interruption_longrun
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

@describe("Node Interruption - VM Stop/start - upto f nodes with daml tests in background")
@pytest.mark.committer_node_interruption
def test_f_node_interruption_vm_stop_start_in_flight_transaction(fxHermesRunSettings, fxBlockchain, fxNodeInterruption):
   node_interruption_details = {
      intr_helper.NODE_INTERRUPTION_TYPE: intr_helper.NODE_INTERRUPT_VM_STOP_START,
      intr_helper.NODE_TYPE_TO_INTERRUPT: helper.TYPE_DAML_COMMITTER,
      intr_helper.NO_OF_NODES_TO_INTERRUPT: intr_helper.get_f_count(fxBlockchain),
      intr_helper.SKIP_MASTER_REPLICA: True,
      intr_helper.CUSTOM_INTERRUPTION_PARAMS: {
         intr_helper.RUN_TXNS_IN_BACKGROUND: True
      }
   }

   # skipping master committer from interruption: bug BC-3264
   nodes_available_for_interruption = \
      intr_helper.get_nodes_available_for_interruption(
         fxBlockchain, node_interruption_details)

   daml_txn_background_thread = intr_helper.start_daml_txn_background_thread(fxBlockchain,
                                                                             daml_txn_result_queue,
                                                                             node_interruption_details,
                                                                             fxHermesRunSettings)
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
                                                   node_interruption_details,
                                                   daml_txn_result_queue=daml_txn_result_queue)

      if not status:
         log.info("")
         log.error("**** Test aborted/failed ****")
         break

   # join background thread running daml_txns with main execution thread
   daml_txn_background_thread.do_run = False
   daml_txn_background_thread.join()
   assert status, "Node Interruption Test Failed"
   log.info("**** Test completed successfully ****")

@describe("Node Interruption - crash containers for all containers of participant nodes")
@pytest.mark.participant_container_crash
@pytest.mark.participant_node_interruption_longrun
def test_participant_node_interruption_all_container_crash(fxHermesRunSettings, fxBlockchain, fxNodeInterruption):
   node_interruption_details = {
      intr_helper.NODE_INTERRUPTION_TYPE: intr_helper.NODE_INTERRUPT_CONTAINER_CRASH,
      intr_helper.NODE_TYPE_TO_INTERRUPT: helper.TYPE_DAML_PARTICIPANT,
      intr_helper.NO_OF_NODES_TO_INTERRUPT: 1,
      intr_helper.SKIP_MASTER_REPLICA: True,
      intr_helper.CUSTOM_INTERRUPTION_PARAMS: {
         intr_helper.CONTAINERS_TO_CRASH: intr_helper.ALL_CONTAINERS
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

@describe("Node Interruption - crash containers for user defined containers of participant nodes")
@pytest.mark.participant_container_crash
@pytest.mark.participant_node_interruption_longrun
def test_participant_node_interruption_few_container_crash(fxHermesRunSettings, fxBlockchain, fxNodeInterruption):
   node_interruption_details = {
      intr_helper.NODE_INTERRUPTION_TYPE: intr_helper.NODE_INTERRUPT_CONTAINER_CRASH,
      intr_helper.NODE_TYPE_TO_INTERRUPT: helper.TYPE_DAML_PARTICIPANT,
      intr_helper.NO_OF_NODES_TO_INTERRUPT: 1,
      intr_helper.SKIP_MASTER_REPLICA: True,
      intr_helper.CUSTOM_INTERRUPTION_PARAMS: {
         intr_helper.CONTAINERS_TO_CRASH: ["daml_ledger_api", "daml_index_db"]
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

@describe("Node Interruption - simulate read/write failure index db through permission change")
@pytest.mark.index_db_read_write_fail
@pytest.mark.participant_node_interruption_longrun
def test_participant_node_index_db_read_write_fail(fxHermesRunSettings, fxBlockchain, fxNodeInterruption):
   node_interruption_details = {
      intr_helper.NODE_INTERRUPTION_TYPE: intr_helper.NODE_INTERRUPT_INDEX_DB_READ_WRITE_FAIL,
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

@describe("Node Interruption - disconnect f committer nodes from Blockchain network")
@pytest.mark.committer_vm_disconnect
@pytest.mark.committer_node_interruption_longrun
def test_committer_node_interruption_vm_network_disconnect(fxHermesRunSettings, fxBlockchain, fxNodeInterruption):
   node_interruption_details = {
      intr_helper.NODE_INTERRUPTION_TYPE: intr_helper.NODE_INTERRUPT_NETWORK_DISCONNECT,
      intr_helper.NODE_TYPE_TO_INTERRUPT: helper.TYPE_DAML_COMMITTER,
      intr_helper.NO_OF_NODES_TO_INTERRUPT: intr_helper.get_f_count(fxBlockchain),
      intr_helper.SKIP_MASTER_REPLICA: True,
      intr_helper.CUSTOM_INTERRUPTION_PARAMS: {
         intr_helper.NETWORK_DISCONNECT_LEVEL: intr_helper.NETWORK_DISCONNECT_VM_LEVEL,
         intr_helper.NODE_OFFLINE_TIME: 30,
         intr_helper.TIME_BETWEEN_INTERRUPTIONS: 180
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

# skipping test from long running tests
@pytest.mark.skip
@describe("Node Interruption - disconnect committer containers from Blockchain network")
@pytest.mark.committer_container_network_disconnect
@pytest.mark.committer_node_interruption_longrun
def test_committer_node_interruption_container_network_disconnect(fxHermesRunSettings, fxBlockchain, fxNodeInterruption):
   node_interruption_details = {
      intr_helper.NODE_INTERRUPTION_TYPE: intr_helper.NODE_INTERRUPT_NETWORK_DISCONNECT,
      intr_helper.NODE_TYPE_TO_INTERRUPT: helper.TYPE_DAML_COMMITTER,
      intr_helper.NO_OF_NODES_TO_INTERRUPT: intr_helper.get_f_count(fxBlockchain),
      intr_helper.SKIP_MASTER_REPLICA: True,
      intr_helper.CUSTOM_INTERRUPTION_PARAMS: {
         intr_helper.NETWORK_DISCONNECT_LEVEL: intr_helper.NETWORK_DISCONNECT_CONTAINER_LEVEL,
         intr_helper.CONTAINERS_TO_DISCONNECT: ["concord"],
         intr_helper.NODE_OFFLINE_TIME: 30,
         intr_helper.TIME_BETWEEN_INTERRUPTIONS: 180
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

@describe("Node Interruption - disconnect 1 participant nodes from Blockchain network")
@pytest.mark.participant_vm_disconnect
def test_participant_node_interruption_vm_network_disconnect(fxHermesRunSettings, fxBlockchain, fxNodeInterruption):
   node_interruption_details = {
      intr_helper.NODE_INTERRUPTION_TYPE: intr_helper.NODE_INTERRUPT_NETWORK_DISCONNECT,
      intr_helper.NODE_TYPE_TO_INTERRUPT: helper.TYPE_DAML_PARTICIPANT,
      intr_helper.NO_OF_NODES_TO_INTERRUPT: 1,
      intr_helper.SKIP_MASTER_REPLICA: True,
      intr_helper.CUSTOM_INTERRUPTION_PARAMS: {
         intr_helper.NETWORK_DISCONNECT_LEVEL: intr_helper.NETWORK_DISCONNECT_VM_LEVEL
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

@describe("Node Interruption - disconnect participant containers from Blockchain network")
@pytest.mark.participant_container_network_disconnect
def test_participant_node_interruption_container_network_disconnect(fxHermesRunSettings, fxBlockchain, fxNodeInterruption):
   node_interruption_details = {
      intr_helper.NODE_INTERRUPTION_TYPE: intr_helper.NODE_INTERRUPT_NETWORK_DISCONNECT,
      intr_helper.NODE_TYPE_TO_INTERRUPT: helper.TYPE_DAML_PARTICIPANT,
      intr_helper.NO_OF_NODES_TO_INTERRUPT: 1,
      intr_helper.SKIP_MASTER_REPLICA: True,
      intr_helper.CUSTOM_INTERRUPTION_PARAMS: {
         intr_helper.NETWORK_DISCONNECT_LEVEL: intr_helper.NETWORK_DISCONNECT_CONTAINER_LEVEL,
         intr_helper.CONTAINERS_TO_DISCONNECT: ["daml_ledger_api"]
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

