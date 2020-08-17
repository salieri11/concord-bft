#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

# Helper file with common utility methods for node interruption and recovery
import os
from random import randrange
import sys
import tempfile
import time
if 'hermes_util' in sys.modules.keys():
   import hermes_util.daml.daml_helper as daml_helper
   import hermes_util.hermes_logging as hermes_logging_util
   import hermes.util.helper as helper
   import hermes.util.blockchain_ops as blockchain_ops
else:
   import util.daml.daml_helper as daml_helper
   import util.hermes_logging as hermes_logging_util
   import util.helper as helper
   import util.blockchain_ops as blockchain_ops

log = hermes_logging_util.getMainLogger()

# Node interrupt & recovery modes
NODE_INTERRUPT = "Interrupt node"
NODE_RECOVER = "Recover node"
NODE_INTERRUPT_VM_STOP_START = "VM power off/on"

# Preset keys for NODE_INTERRUPTION_DETAILS
NODE_INTERRUPTION_TYPE = "NODE_INTERRUPTION_TYPE"
NO_OF_NODES_TO_INTERRUPT = "NO_OF_NODES_TO_INTERRUPT"
SKIP_MASTER_REPLICA = "SKIP_MASTER_REPLICA"
CUSTOM_INTERRUPTION_PARAMS = "CUSTOM_INTERRUPTION_PARAMS"

def verify_node_interruption_testing_readiness(fxHermesRunSettings):
   '''
   Verify readiness for node interruption testing
   :param fxHermesRunSettings: hermes run settings (fixture)
   '''
   for ip,vm_handle in fxHermesRunSettings.vm_handles.items():
      if vm_handle is None:
         log.error("")
         log.error("**** Failed to fetch VM handles; aborting Run! ****")
         sys.exit(1)


def get_committers_available_for_interruption(fxBlockchain,
                                              node_interruption_details):
   '''
   Return a list of committer nodes allowed for interrupting
   :param fxBlockchain: blockchain fixture
   :param node_interruption_details: node interruption details
   :return: list of committer nodes allowed for interruption
   '''
   # blockchain_ops.reset_blockchain(fxBlockchain)
   if node_interruption_details[SKIP_MASTER_REPLICA]:
      master_replica = blockchain_ops.fetch_master_replica(fxBlockchain)
      committers_available_for_interruption = [ip for ip in
                                               blockchain_ops.committers_of(
                                                  fxBlockchain) if
                                               ip != master_replica]
   else:
      committers_available_for_interruption = blockchain_ops.committers_of(
         fxBlockchain)

   log.info("Committers available for interruption: {}".format(
      committers_available_for_interruption))
   return committers_available_for_interruption


def get_f_count(fxBlockchain):
   '''
   Return f count
   :param fxBlockchain: blockchain fixture
   :return: f count
   '''
   return blockchain_ops.get_f_count(fxBlockchain)


def get_list_of_replicas_to_interrupt(committers_available_for_interruption,
                                      node_interruption_details,
                                      last_interrupted_node_index=None):
   '''
   Return f committer nodes for this iteration of node interruption
   :param committers_available_for_interruption: committer nodes available for interruption
   :param node_interruption_details: node interruption details (dict)
   :param last_interrupted_node_index: 1 committer in f interrupted committer nodes
   :return: f nodes to be interrupted, 1st committer index of f nodes
   '''
   no_of_available_committers_for_interruption = len(committers_available_for_interruption)
   if not last_interrupted_node_index:
      last_interrupted_node_index = randrange(no_of_available_committers_for_interruption)

   start_node_index = (last_interrupted_node_index + 1) % no_of_available_committers_for_interruption
   nodes_to_interrupt = []
   for j in range(node_interruption_details[NO_OF_NODES_TO_INTERRUPT]):
      node_index_to_interrupt = start_node_index+j
      if node_index_to_interrupt >= no_of_available_committers_for_interruption:
         node_index_to_interrupt = node_index_to_interrupt - no_of_available_committers_for_interruption
      log.debug(committers_available_for_interruption[node_index_to_interrupt])
      nodes_to_interrupt.append(
         committers_available_for_interruption[node_index_to_interrupt])

   return nodes_to_interrupt, last_interrupted_node_index + 1


def check_node_health_and_run_sanity_check(fxBlockchain, results_dir,
                                           interrupted_nodes=[]):
   '''
   Check health of non-interrupted nodes and run sanity check
   :param fxBlockchain: blockchain fixture
   :param results_dir: results dir
   :param interrupted_nodes: list of interrupted nodes
   :return: True if blockchain is healthy and ran tests, else False, & crashed node count
   '''
   log.info("")
   log.info("** Verifying health of all nodes...")
   all_crashed_nodes = get_all_crashed_nodes(fxBlockchain, results_dir,
                                              interrupted_nodes)
   crashed_count = len(all_crashed_nodes)

   status = False
   if crashed_count <= blockchain_ops.get_f_count(fxBlockchain):
      blockchain_ops.print_replica_info(fxBlockchain,
                                        interrupted_nodes=all_crashed_nodes)

      log.info("")
      log.info("** Run DAML tests...")
      daml_tests_results_dir = helper.create_results_sub_dir(results_dir,
                                                                   "daml_tests")
      status = helper.run_daml_sanity(
         blockchain_ops.participants_of(fxBlockchain),
         daml_tests_results_dir,
         run_all_tests=False, verbose=False)
      if not status:
         log.info("Collect support logs ({})...".format(daml_tests_results_dir))
         helper.create_concord_support_bundle(
            [ip for ip in blockchain_ops.committers_of(fxBlockchain) if
             ip not in interrupted_nodes], helper.TYPE_DAML_COMMITTER,
            daml_tests_results_dir,
            verbose=False)
         helper.create_concord_support_bundle(
            blockchain_ops.participants_of(fxBlockchain),
            helper.TYPE_DAML_PARTICIPANT, daml_tests_results_dir, verbose=False)

   return status, crashed_count


def get_all_crashed_nodes(fxBlockchain, results_dir, interrupted_nodes):
   '''
   Get list of all crashed nodes
   :param fxBlockchain: blockchain fixture
   :param results_dir: results dir
   :param interrupted_nodes: test interrupted nodes
   :return: list of all crashed nodes
   '''
   all_committers_other_than_interrupted = [ip for ip in
                                            blockchain_ops.committers_of(
                                               fxBlockchain) if
                                            ip not in interrupted_nodes]
   username, password = helper.getNodeCredentials()
   unexpected_interrupted_nodes = []
   for ip in all_committers_other_than_interrupted:
      log.info("  {}...".format(ip))
      if not helper.check_docker_health(ip, username, password,
                                        helper.TYPE_DAML_COMMITTER,
                                        max_timeout=5, verbose=False):
         log.warning("  ** Unexpected crash")
         unexpected_interrupted_nodes.append(ip)

   all_crashed_nodes = unexpected_interrupted_nodes + interrupted_nodes
   total_no_of_nodes_crashed = len(all_crashed_nodes)
   log.info("Summary of Interrupted nodes:")
   if len(unexpected_interrupted_nodes) > 0:
      log.warning("  Unexpectedly crashed nodes: {}".format(
         unexpected_interrupted_nodes))
   if len(interrupted_nodes) > 0:
      log.info("  Interrupted nodes: {}".format(interrupted_nodes))
   log.info("  Total no. of crashed nodes: {}".format(total_no_of_nodes_crashed))

   if len(unexpected_interrupted_nodes) > 0:
      unexpected_crash_results_dir = helper.create_results_sub_dir(results_dir,
                                                                   "unexpected_crash")
      log.info("Collect support logs ({})...".format(unexpected_crash_results_dir))
      helper.create_concord_support_bundle(
         [ip for ip in blockchain_ops.committers_of(fxBlockchain) if
          ip not in interrupted_nodes], helper.TYPE_DAML_COMMITTER,
         unexpected_crash_results_dir, verbose=False)
      helper.create_concord_support_bundle(
         blockchain_ops.participants_of(fxBlockchain),
         helper.TYPE_DAML_PARTICIPANT, unexpected_crash_results_dir,
         verbose=False)

   if total_no_of_nodes_crashed > blockchain_ops.get_f_count(fxBlockchain):
      log.error("**** System is unhealthy")

   return all_crashed_nodes


def workaround_to_rejoin_node(node):
   '''
   Workaround to automatically rejoin a node when it comes up
   :param node: blockchain node
   '''
   rejoin_node_script_name = "rejoin_node.sh"
   src_rejoin_node_script_path = os.path.join('util',
                                              rejoin_node_script_name)
   remote_rejoin_node_script_path = os.path.join(tempfile.gettempdir(),
                                                 rejoin_node_script_name)
   username, password = helper.getNodeCredentials()
   if helper.sftp_client(node, username, password,
                         src_rejoin_node_script_path,
                         remote_rejoin_node_script_path, action="upload"):
      log.debug("  Saved at '{}:{}'".format(node,
                                            remote_rejoin_node_script_path))

      cmd_to_rejoin_node = "sh {}".format(remote_rejoin_node_script_path)

      log.info("  Executing script to rejoin node...")
      ssh_output = helper.ssh_connect(node, username,
                                      password,
                                      cmd_to_rejoin_node)
      log.debug("Output from script '{}': {}".format(
         remote_rejoin_node_script_path,
         ssh_output))

def perform_interrupt_recovery_operation(fxHermesRunSettings, node,
                                         node_interruption_details, mode):
   '''
   Method to perform node interruption and recovery operation
   :param fxHermesRunSettings: hermes run settings (fixture)
   :param node: node to be interrupted/recovered
   :param node_interruption_details: Interruption/recovery details
   :param mode: Interruption/recovery mode
   :return: Success status
   '''
   vm_handle = fxHermesRunSettings["hermesCmdlineArgs"].vm_handles[node]

   node_interruption_type = node_interruption_details[NODE_INTERRUPTION_TYPE]
   custom_interruption_params = node_interruption_details[CUSTOM_INTERRUPTION_PARAMS]

   log.info("({})".format(node_interruption_type))

   if node_interruption_type == NODE_INTERRUPT_VM_STOP_START:
      if mode == NODE_INTERRUPT:
         log.debug("Powering off...")
         vm_handle["entity"].PowerOffVM_Task()
         EXPECTED_POWER_STATE = "poweredOff"

      if mode == NODE_RECOVER:
         log.debug("Powering on...")
         vm_handle["entity"].PowerOnVM_Task()
         EXPECTED_POWER_STATE = "poweredOn"

      start_time = time.time()
      max_timeout = 120  # seconds
      node_interruption_completed = False
      while vm_handle["entity"].runtime.powerState != EXPECTED_POWER_STATE or \
         not node_interruption_completed:
         if mode == NODE_INTERRUPT:
            if vm_handle["entity"].guest.ipAddress is None:
               node_interruption_completed = True
         if mode == NODE_RECOVER:
            if vm_handle["entity"].guest.ipAddress is not None:
               node_interruption_completed = True

         if time.time() <= (start_time + max_timeout):
            log.debug("Wait 5 seconds and check power status...")
            time.sleep(5)
         else:
            log.error("Failed to '{}' node".format(EXPECTED_POWER_STATE))
            return False

      log.info("Wait for a min... (** THIS SHOULD BE REMOVED AFTER A STABLE RUN **)")
      time.sleep(60)
      log.info("{}".format(vm_handle["entity"].runtime.powerState))

   return True

def crash_and_restore_nodes(fxBlockchain, fxHermesRunSettings,
                            nodes_to_interrupt, node_interruption_details):
   '''
   Util to trigger crash & recovery operations
   :param fxBlockchain: blockchain
   :param fxHermesRunSettings: hermes run settings (fixture)
   :param nodes_to_interrupt: node to interrupt
   :param node_interruption_details: interruptions/recovery details
   :return: success status
   '''
   results_dir_name = ''.join(
      e for e in node_interruption_details[NODE_INTERRUPTION_TYPE] if e.isalnum())
   results_dir = helper.create_results_sub_dir(
      fxHermesRunSettings["hermesTestLogDir"],
      results_dir_name)
   f_count = blockchain_ops.get_f_count(fxBlockchain)
   interrupted_nodes = []
   result = False
   for node in nodes_to_interrupt:
      if not interrupted_nodes:
         result, crashed_count = check_node_health_and_run_sanity_check(
            fxBlockchain, results_dir, interrupted_nodes)

      log.info("")
      if crashed_count < f_count:
         log.info("** Interrupting node: {}...".format(node))
         if perform_interrupt_recovery_operation(fxHermesRunSettings, node,
                                                 node_interruption_details,
                                                 mode=NODE_INTERRUPT):
            interrupted_nodes.append(node)
         result, crashed_count = check_node_health_and_run_sanity_check(
            fxBlockchain, results_dir, interrupted_nodes)
      else:
         log.error("")
         log.error("** There are already >= {} crashed committers".format(f_count))
         log.error("** Not proceeding with node interruption")
         return False

   # restore nodes
   for node in nodes_to_interrupt:
      log.info("")
      log.info("** Restoring node: {}...".format(node))
      perform_interrupt_recovery_operation(fxHermesRunSettings, node,
                                           node_interruption_details,
                                           mode=NODE_RECOVER)
      interrupted_nodes.remove(node)
   result, crashed_count = check_node_health_and_run_sanity_check(fxBlockchain,
                                                                  results_dir,
                                                                  interrupted_nodes)

   return result


