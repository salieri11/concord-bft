#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

# Helper file with common utility methods for node interruption and recovery
import os
import threading
from random import randrange
import sys
import tempfile
import time
import datetime
import json
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
interrupted_nodes = []
# Node interrupt & recovery modes
NODE_INTERRUPT = "Interrupt node"
NODE_RECOVER = "Recover node"
NODE_INTERRUPT_VM_STOP_START = "VM power off/on"
ALL_CONTAINERS = "ALL_CONTAINERS"
NODE_INTERRUPT_CONTAINER_CRASH = "Container Crash"
NODE_INTERRUPT_INDEX_DB_READ_WRITE_FAIL = "Index DB read/write failure"
NODE_INTERRUPT_NETWORK_DISCONNECT = "Network disconnect"
NETWORK_DISCONNECT_VM_LEVEL = "VM network disconnect"
NETWORK_DISCONNECT_CONTAINER_LEVEL = "Container network disconnect"
CONTAINER_BLOCKCHAIN_NETWORK = "blockchain-fabric"
EXCEPTION_LIST_OF_INTR_TYPES_TO_RUN_DAML_TEST = [NODE_INTERRUPT_INDEX_DB_READ_WRITE_FAIL]

# Preset keys for NODE_INTERRUPTION_DETAILS
NODE_TYPE_TO_INTERRUPT = "NODE_TYPE_TO_INTERRUPT"
NODE_INTERRUPTION_TYPE = "NODE_INTERRUPTION_TYPE"
NO_OF_NODES_TO_INTERRUPT = "NO_OF_NODES_TO_INTERRUPT"
SKIP_MASTER_REPLICA = "SKIP_MASTER_REPLICA"
CUSTOM_INTERRUPTION_PARAMS = "CUSTOM_INTERRUPTION_PARAMS"
NODE_OFFLINE_TIME = "NODE_OFFLINE_TIME"
TIME_BETWEEN_INTERRUPTIONS = "TIME_BETWEEN_INTERRUPTIONS"
CONTAINERS_TO_CRASH = "CONTAINERS_TO_CRASH"
INDEX_DB_CONTAINER_NAME = "daml_index_db"
CONTAINERS_TO_DISCONNECT = "CONTAINERS_TO_DISCONNECT"
NETWORK_DISCONNECT_LEVEL = "NETWORK_DISCONNECT_LEVEL"
RUN_TXNS_IN_BACKGROUND = "RUN_TXNS_IN_BACKGROUND"


def verify_node_interruption_testing_readiness(fxHermesRunSettings):
   '''
   Verify readiness for node interruption testing
   :param fxHermesRunSettings: hermes run settings (fixture)
   '''
   for ip, vm_handle in fxHermesRunSettings.vm_handles.items():
      if vm_handle is None:
         log.error("")
         log.error("**** Failed to fetch VM handles; aborting Run! ****")
         sys.exit(1)

def get_nodes_available_for_interruption(fxBlockchain,
                                         node_interruption_details):
   '''
   Return a list of committer nodes allowed for interrupting
   :param fxBlockchain: blockchain fixture
   :param node_interruption_details: node interruption details
   :return: list of committer nodes allowed for interruption
   '''
   # blockchain_ops.reset_blockchain(fxBlockchain)
   nodes_available_for_interruption = []
   if node_interruption_details[NODE_TYPE_TO_INTERRUPT] == helper.TYPE_DAML_COMMITTER:
      if node_interruption_details[SKIP_MASTER_REPLICA]:
         master_replica = blockchain_ops.fetch_master_replica(fxBlockchain)
         nodes_available_for_interruption = [ip for ip in
                                             blockchain_ops.committers_of(
                                                fxBlockchain) if
                                             ip != master_replica]
      else:
         nodes_available_for_interruption = blockchain_ops.committers_of(
            fxBlockchain)
   elif node_interruption_details[NODE_TYPE_TO_INTERRUPT] == helper.TYPE_DAML_PARTICIPANT:
      nodes_available_for_interruption = blockchain_ops.participants_of(
         fxBlockchain)

   log.info("Nodes available for interruption: {}".format(
      nodes_available_for_interruption))
   return nodes_available_for_interruption

def get_f_count(fxBlockchain):
   '''
   Return f count
   :param fxBlockchain: blockchain fixture
   :return: f count
   '''
   return blockchain_ops.get_f_count(fxBlockchain)

def get_list_of_nodes_to_interrupt(nodes_available_for_interruption,
                                   node_interruption_details,
                                   last_interrupted_node_index=None):
   '''
   Return list of nodes for this iteration of node interruption
   :param nodes_available_for_interruption: nodes available for interruption
   :param node_interruption_details: node interruption details (dict)
   :param last_interrupted_node_index: last interrupted node index in the list of nodes
   :return: f nodes to be interrupted
   '''
   no_of_available_committers_for_interruption = len(nodes_available_for_interruption)
   if not last_interrupted_node_index:
      last_interrupted_node_index = randrange(no_of_available_committers_for_interruption)

   start_node_index = (last_interrupted_node_index + 1) % no_of_available_committers_for_interruption
   nodes_to_interrupt = []
   for j in range(node_interruption_details[NO_OF_NODES_TO_INTERRUPT]):
      node_index_to_interrupt = start_node_index + j
      if node_index_to_interrupt >= no_of_available_committers_for_interruption:
         node_index_to_interrupt = node_index_to_interrupt - no_of_available_committers_for_interruption
      log.debug(nodes_available_for_interruption[node_index_to_interrupt])
      nodes_to_interrupt.append(
         nodes_available_for_interruption[node_index_to_interrupt])

   return nodes_to_interrupt, last_interrupted_node_index + 1

def check_node_health_and_run_sanity_check(fxBlockchain, results_dir,
                                           node_interruption_details,
                                           duration_to_run_transaction=0,
                                           mode=None):
   '''
   Check health of non-interrupted nodes and run sanity check
   :param fxBlockchain: blockchain fixture
   :param results_dir: results dir
   :param node_interruption_details: node interruption details (dict)
   :param duration_to_run_transaction: duration to run transactions (in minutes)
   :param mode: Interruption/recovery mode
   :return: True if blockchain is healthy and ran tests, else False, & crashed node count
   '''
   global interrupted_nodes
   run_txn_in_background = node_interruption_details[CUSTOM_INTERRUPTION_PARAMS].get(RUN_TXNS_IN_BACKGROUND)
   log.info("")
   log.info("** Verifying health of all nodes...")
   crashed_committers, crashed_participants = get_all_crashed_nodes(
      fxBlockchain, results_dir, node_interruption_details[NODE_TYPE_TO_INTERRUPT], interrupted_nodes)
   crashed_committer_count = len(crashed_committers)

   status = False
   if crashed_committer_count <= blockchain_ops.get_f_count(fxBlockchain):
      blockchain_ops.print_replica_info(fxBlockchain,
                                        interrupted_nodes=crashed_committers)

      uninterrupted_participants = [ip for ip in
                                    blockchain_ops.participants_of(fxBlockchain)
                                    if ip not in crashed_participants]
      list_of_participant_nodes_to_run_txns = uninterrupted_participants
      if mode == NODE_INTERRUPT and node_interruption_details[NODE_INTERRUPTION_TYPE] \
            in EXCEPTION_LIST_OF_INTR_TYPES_TO_RUN_DAML_TEST:
         list_of_participant_nodes_to_run_txns = crashed_participants
      log.info("")

      if run_txn_in_background:
         status = True
      else:
         if list_of_participant_nodes_to_run_txns:
            start_time = datetime.datetime.now()
            log.info("** Run DAML tests...")
            daml_tests_results_dir = helper.create_results_sub_dir(results_dir,
                                                                   "daml_tests")
            while True:
               status = run_daml_sanity(fxBlockchain, daml_tests_results_dir,
                                        list_of_participant_nodes_to_run_txns=list_of_participant_nodes_to_run_txns,
                                        mode=mode,
                                        node_interruption_type=node_interruption_details[NODE_INTERRUPTION_TYPE])
               if not status:
                  break
               if datetime.datetime.now() >= start_time + datetime.timedelta(minutes=duration_to_run_transaction):
                  break
               else:
                  elapsed_time = round(((datetime.datetime.now() - start_time).seconds / 60), 1)
                  log.info(
                     "Repeating Daml transactions ({} / {} mins)...".format(elapsed_time, duration_to_run_transaction))
         else:
            log.info("** Skipping DAML test as all participant nodes are interrupted")
            status = True

   return status, crashed_committer_count

def get_all_crashed_nodes(fxBlockchain, results_dir, interrupted_node_type,
                          interrupted_nodes):
   '''
   Get list of all crashed nodes
   :param fxBlockchain: blockchain fixture
   :param results_dir: results dir
   :param interrupted_nodes: test interrupted nodes
   :return: list of all crashed nodes
   '''
   username, password = helper.getNodeCredentials()
   all_committers_other_than_interrupted = [ip for ip in
                                            blockchain_ops.committers_of(
                                               fxBlockchain) if
                                            ip not in interrupted_nodes]
   log.info("** committers **")
   unexpected_interrupted_committers = []
   for ip in all_committers_other_than_interrupted:
      log.info("  {}...".format(ip))
      if not helper.check_docker_health(ip, username, password,
                                        helper.TYPE_DAML_COMMITTER,
                                        max_timeout=5, verbose=False):
         log.warning("  ** Unexpected crash")
         unexpected_interrupted_committers.append(ip)

   uninterrupted_participants = [ip for ip in
                                 blockchain_ops.participants_of(fxBlockchain) if
                                 ip not in interrupted_nodes]

   log.info("** participants **")
   if len(uninterrupted_participants) == 0:
      log.info("  None")
   unexpected_crashed_participants = []
   for ip in uninterrupted_participants:
      log.info("  {}...".format(ip))
      if not helper.check_docker_health(ip, username, password,
                                        helper.TYPE_DAML_PARTICIPANT,
                                        max_timeout=5, verbose=False):
         log.warning("  ** Unexpected crash")
         unexpected_crashed_participants.append(ip)

   if interrupted_node_type == helper.TYPE_DAML_COMMITTER:
      crashed_committers = unexpected_interrupted_committers + interrupted_nodes
      crashed_participants = unexpected_crashed_participants
   else:
      crashed_committers = unexpected_interrupted_committers
      crashed_participants = unexpected_crashed_participants + interrupted_nodes

   total_no_of_committers_crashed = len(crashed_committers)
   log.info("")
   log.info("Summary of Interrupted nodes:")
   if len(unexpected_interrupted_committers) > 0:
      log.warning("  Unexpectedly crashed committers: {}".format(
         unexpected_interrupted_committers))
   if len(unexpected_crashed_participants) > 0:
      log.warning("  Unexpectedly crashed participants: {}".format(
         unexpected_crashed_participants))

   if len(interrupted_nodes) > 0:
      log.info("  Interrupted '{}' nodes: {}".format(interrupted_node_type,
                                                     interrupted_nodes))
   log.info("  Total no. of crashed committer nodes: {}".format(
      total_no_of_committers_crashed))
   log.info("  Total no. of crashed participant nodes: {}".format(
      len(crashed_participants)))

   if len(unexpected_interrupted_committers) > 0:
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

   if total_no_of_committers_crashed > blockchain_ops.get_f_count(fxBlockchain):
      log.error("**** System is unhealthy")

   return crashed_committers, crashed_participants

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

   elif node_interruption_type == NODE_INTERRUPT_CONTAINER_CRASH:
      if mode == NODE_INTERRUPT:
         if custom_interruption_params[CONTAINERS_TO_CRASH] == ALL_CONTAINERS:
            containers = fxHermesRunSettings["hermesUserConfig"]["persephoneTests"] \
               ["modelService"]["defaults"]["deployment_components"][
               node_interruption_details[NODE_TYPE_TO_INTERRUPT]].values()
         else:
            containers = custom_interruption_params[CONTAINERS_TO_CRASH]

         username, password = helper.getNodeCredentials()
         log.info("containers to be crashed: {}".format(containers))
         for container in containers:
            log.info("Performing container crash for VM: {} and container: {}".format(node, container))
            command_to_get_pid_and_status = "docker inspect --format='{}' {}; docker inspect --format='{}'\
                         {}".format('{{.State.Pid}}', container, '{{.State.Status}}', container)
            command_to_get_status = "docker inspect --format '{}' {}".format('{{.State.Status}}', container)
            pid_and_status_before_crash = helper.ssh_connect(node, username, password, command_to_get_pid_and_status)
            log.debug("process id and status before crash: {}".format(pid_and_status_before_crash))

            pid_before_crash, status_before_crash = pid_and_status_before_crash.split("\r\n", 1)
            command_to_crash_container = "kill -9 {}".format(pid_before_crash)
            crash_output = helper.ssh_connect(node, username, password, command_to_crash_container)
            log.debug("Container crash output for VM {} and container {}: {}".format(node, container, crash_output))

            status_of_container = None
            count = 0
            while status_of_container != "running\r\n" and count <= 5:
               count = count + 1
               time.sleep(2)
               status_of_container = helper.ssh_connect(node, username, password, command_to_get_status)

            pid_and_status_after_crash = helper.ssh_connect(node, username, password, command_to_get_pid_and_status)
            pid_after_crash, status_after_crash = pid_and_status_after_crash.split("\r\n", 1)

            if pid_before_crash != pid_after_crash and status_of_container == "running\r\n":
               log.info("Container: {} crashed and started automatically for VM: {}".format(container, node))
            elif pid_before_crash == pid_after_crash:
               log.error("Container: {} could not be crashed for VM: {}".format(container, node))
               sys.exit(1)
            elif status_of_container != "running\r\n":
               log.error("container: {} did not start automatically for VM: {}".format(container, node))
               sys.exit(1)

   elif node_interruption_type == NODE_INTERRUPT_INDEX_DB_READ_WRITE_FAIL:
      username, password = helper.getNodeCredentials()
      command_to_get_index_db_locations = "docker inspect  --format '{}' {}".format('{{json .Mounts}}',
                                                                                    INDEX_DB_CONTAINER_NAME)
      index_db_locations = helper.ssh_connect(node, username, password, command_to_get_index_db_locations)
      index_db_locations = json.loads(index_db_locations)
      for location in index_db_locations:
         if "postgresql" in location["Destination"]:
            index_db_location = location["Destination"].strip('\'')

      if mode == NODE_INTERRUPT:
         log.info("setting no access (000) on {}".format(index_db_location))
         command_for_index_db_permission_status = \
            "docker exec -it daml_index_db bash -c \"chmod 000 {} ; ls -ld {} | cut -c2-10\"" \
               .format(index_db_location, index_db_location)
         index_db_status = helper.ssh_connect(node, username, password, command_for_index_db_permission_status)
         if index_db_status == "---------\r\n":
            log.debug("removed access (000) on container: {}".format(INDEX_DB_CONTAINER_NAME))
         else:
            log.error("failed to remove access (000) on container: {}".format(INDEX_DB_CONTAINER_NAME))
            sys.exit(1)

      if mode == NODE_RECOVER:
         log.info("reverting access (700) on {}".format(index_db_location))
         command_for_index_db_permission_status = \
            "docker exec -it daml_index_db bash -c \"chmod 700 {} ; ls -ld {} | cut -c2-10\"" \
               .format(index_db_location, index_db_location)
         index_db_status = helper.ssh_connect(node, username, password, command_for_index_db_permission_status)
         if index_db_status == "rwx------\r\n":
            log.debug("restored access (700) on container: {}".format(INDEX_DB_CONTAINER_NAME))
         else:
            log.error("failed to restore access (700) on container: {}".format(INDEX_DB_CONTAINER_NAME))
            sys.exit(1)

   elif node_interruption_type == NODE_INTERRUPT_NETWORK_DISCONNECT:
      username, password = helper.getNodeCredentials()
      if custom_interruption_params[NETWORK_DISCONNECT_LEVEL] == NETWORK_DISCONNECT_CONTAINER_LEVEL:
         containers = custom_interruption_params[CONTAINERS_TO_DISCONNECT]
         for container in containers:
            if mode == NODE_INTERRUPT:
               log.info("Container disconnect: {}".format(container))
               command_to_disconnect_container = "docker network disconnect {} {}".format(CONTAINER_BLOCKCHAIN_NETWORK,
                                                                                          container)
               disconnect_output = helper.ssh_connect(node, username, password, command_to_disconnect_container)
               log.debug("Output of network disconnect for container: {} and VM: {}".format(container, node),
                         disconnect_output)
               command_to_get_container_network = "docker inspect --format '{}' {}".format(
                  '{{.NetworkSettings.Networks}}',
                  container)
               container_network = helper.ssh_connect(node, username, password, command_to_get_container_network)
               if not CONTAINER_BLOCKCHAIN_NETWORK in container_network:
                  log.debug("{} Container disconnected successfully from blockchain on VM {}".format(container, node))
               else:
                  log.error("Unable to disconnect {} container from blockchain on VM: {}".format(container, node))
                  sys.exit(1)
            elif mode == NODE_RECOVER:
               log.info("Container reconnect: {}".format(container))
               command_to_reconnect_container = "docker network connect {} {}".format(CONTAINER_BLOCKCHAIN_NETWORK,
                                                                                      container)
               reconnect_output = helper.ssh_connect(node, username, password, command_to_reconnect_container)
               log.debug("Output of VM reconnect command for VM: {}".format(node), reconnect_output)
               command_to_get_container_network = "docker inspect --format '{}' {}".format(
                  '{{.NetworkSettings.Networks}}',
                  container)
               container_network = helper.ssh_connect(node, username, password, command_to_get_container_network)
               if CONTAINER_BLOCKCHAIN_NETWORK in container_network:
                  log.debug("Container: {} reconnected successfully to blockchain".format(container))
               else:
                  log.error("Unable to reconnect container: {} to blockchain".format(container))
                  sys.exit(1)

      elif custom_interruption_params[NETWORK_DISCONNECT_LEVEL] == NETWORK_DISCONNECT_VM_LEVEL:
         command_to_get_network_id_if_running = "ifconfig | grep br- | cut -f 1 -d ' '"
         if mode == NODE_INTERRUPT:
            log.info("Performing network disconnect for VM: {}".format(node))
            command_to_disconnect_vm = "network_id=$(ifconfig -a | grep br- | cut -f 1 -d ' '); ifconfig $network_id down"
            disconnect_output = helper.ssh_connect(node, username, password, command_to_disconnect_vm)
            log.debug("Output of VM disconnect command for VM: {}".format(node), disconnect_output)
            network_id = helper.ssh_connect(node, username, password, command_to_get_network_id_if_running)
            if network_id == "":
               log.info("VM disconnect successful")
            else:
               log.error("Network disconnect failed for VM: {}".format(node))
               sys.exit(1)
         elif mode == NODE_RECOVER:
            log.info("VM reconnect")
            command_to_reconnect_vm = "network_id=$(ifconfig -a | grep br- | cut -f 1 -d ' '); ifconfig $network_id up"
            reconnect_output = helper.ssh_connect(node, username, password, command_to_reconnect_vm)
            log.debug("Output of VM reconnect command for VM: {}".format(node), reconnect_output)
            network_id = helper.ssh_connect(node, username, password, command_to_get_network_id_if_running)
            if network_id == "":
               log.error("Network reconnect failed for VM: {}".format(node))
               sys.exit(1)
            else:
               log.debug("Network reconnect successful for VM: {}".format(node))

   log.info("Wait for a min... (** THIS SHOULD BE REMOVED AFTER A STABLE RUN **)")
   time.sleep(60)
   log.info("{}".format(vm_handle["entity"].runtime.powerState))

   return True

def crash_and_restore_nodes(fxBlockchain, fxHermesRunSettings,
                            nodes_to_interrupt, node_interruption_details,
                            daml_txn_result_queue=None):
   '''
   Util to trigger crash & recovery operations
   :param fxBlockchain: blockchain
   :param fxHermesRunSettings: hermes run settings (fixture)
   :param nodes_to_interrupt: node to interrupt
   :param node_interruption_details: interruptions/recovery details
   :param daml_txn_result_queue: flag to notify main thread about background daml test thread status
   :return: success status
   '''
   custom_interruption_params = node_interruption_details[CUSTOM_INTERRUPTION_PARAMS]
   node_offline_time = custom_interruption_params.get(NODE_OFFLINE_TIME)
   time_between_interruptions = custom_interruption_params.get(TIME_BETWEEN_INTERRUPTIONS)
   # If time gaps are not specified, assign default value
   if node_offline_time is None:
      node_offline_time = 0
   if time_between_interruptions is None:
      time_between_interruptions = 0
   # node interruption time must be greater than node recovery time, else delta becomes 0
   time_remaining_before_next_interruption = time_between_interruptions - node_offline_time \
      if time_between_interruptions > node_offline_time else 0
   results_dir_name = ''.join(
      e for e in node_interruption_details[NODE_INTERRUPTION_TYPE] if e.isalnum())
   results_dir = helper.create_results_sub_dir(
      fxHermesRunSettings["hermesTestLogDir"],
      results_dir_name)
   f_count = blockchain_ops.get_f_count(fxBlockchain)
   global interrupted_nodes
   interrupted_nodes = []
   result = False
   for node in nodes_to_interrupt:
      if not interrupted_nodes:
         result, crashed_committer_count = check_node_health_and_run_sanity_check(
            fxBlockchain, results_dir,
            node_interruption_details)

      log.info("")
      if crashed_committer_count < f_count:
         log.info("** Interrupting node: {}...".format(node))
         if perform_interrupt_recovery_operation(fxHermesRunSettings, node,
                                                 node_interruption_details,
                                                 mode=NODE_INTERRUPT):
            if not node_interruption_details[NODE_INTERRUPTION_TYPE] == NODE_INTERRUPT_CONTAINER_CRASH:
               interrupted_nodes.append(node)
         result, crashed_committer_count = check_node_health_and_run_sanity_check(
            fxBlockchain, results_dir,
            node_interruption_details,
            mode=NODE_INTERRUPT)
      else:
         log.error("")
         log.error("** There are already >= {} crashed committers".format(f_count))
         log.error("** Not proceeding with node interruption")
         return False

   # restore nodes
   if node_interruption_details[NODE_INTERRUPTION_TYPE] == NODE_INTERRUPT_CONTAINER_CRASH:
      log.info("Skipping nodes restore as this should be done automatically for container crash test")
   else:
      # Run DAML test for the period of node_offline_time
      result, crashed_committer_count = check_node_health_and_run_sanity_check(
         fxBlockchain, results_dir,
         node_interruption_details,
         duration_to_run_transaction=node_offline_time)

      for node in nodes_to_interrupt:
         log.info("")
         log.info("** Restoring node: {}...".format(node))
         perform_interrupt_recovery_operation(fxHermesRunSettings, node,
                                              node_interruption_details,
                                              mode=NODE_RECOVER)
      interrupted_nodes.remove(node)
   # Run Daml test for the period of time_remaining_before_next_interruption
   result, crashed_committer_count = check_node_health_and_run_sanity_check(
      fxBlockchain, results_dir,
      node_interruption_details,
      duration_to_run_transaction=time_remaining_before_next_interruption)

   status = result
   if daml_txn_result_queue and not daml_txn_result_queue.empty():
      txn_run_status = daml_txn_result_queue.queue[-1]
      if not txn_run_status:
         status = txn_run_status
   return status

def run_daml_sanity(fxBlockchain, daml_tests_results_dir,
                    daml_txn_result_queue=None,
                    list_of_participant_nodes_to_run_txns=[],
                    mode=None, node_interruption_type=None):
   '''
   Util to run daml txns and collect support logs
   :param fxBlockchain: blockchain
   :param daml_tests_results_dir: daml tests result directory
   :param daml_txn_result_queue: flag to notify main thread about background daml test thread status
   :param list_of_participant_nodes_to_run_txns: list of participant nodes to run daml txns
   :param mode: mode of node interruption
   :param node_interruption_type: Type of node interruption
   :return: success status
   '''
   global interrupted_nodes
   if not list_of_participant_nodes_to_run_txns:
      list_of_participant_nodes_to_run_txns = blockchain_ops.participants_of(fxBlockchain)

   status = helper.run_daml_sanity(
      list_of_participant_nodes_to_run_txns,
      daml_tests_results_dir,
      run_all_tests=False, verbose=False)
   if daml_txn_result_queue:
      daml_txn_result_queue.put(status)
   if mode == NODE_INTERRUPT and node_interruption_type \
         in EXCEPTION_LIST_OF_INTR_TYPES_TO_RUN_DAML_TEST:
      if not status:
         log.info("DAML transactions failed as expected (db failure simulated)")
         status = True
      else:
         log.error("DAML transactions succeeded, whereas db has read/write error")
         status = False
   if not status:
      log.info(
         "Collect support logs ({})...".format(daml_tests_results_dir))
      helper.create_concord_support_bundle(
         [ip for ip in blockchain_ops.committers_of(fxBlockchain) if
          ip not in interrupted_nodes], helper.TYPE_DAML_COMMITTER,
         daml_tests_results_dir,
         verbose=False)
      helper.create_concord_support_bundle(
         blockchain_ops.participants_of(fxBlockchain),
         helper.TYPE_DAML_PARTICIPANT, daml_tests_results_dir,
         verbose=False)
   return status

def run_daml_sanity_in_background_loop(fxBlockchain, daml_txn_result_queue, daml_tests_results_dir):
   '''
   Util to run daml txns in continuously in loop concurrently with node interruption & recovery operation
   :param fxBlockchain: blockchain
   :param daml_txn_result_queue: flag to notify main thread about background daml test thread status
   :param daml_tests_results_dir: daml tests result directory
   '''
   t = threading.currentThread()
   # While True run daml txns in concurrency with node interruption.
   while getattr(t, "do_run", True):
      log.info("")
      log.info("** Running DAML tests concurrently with node interruption operation in background")
      status = run_daml_sanity(fxBlockchain, daml_tests_results_dir,
                               daml_txn_result_queue=daml_txn_result_queue)
      if not status:
         log.info("")
         log.info("** DAML test FAILED in background")
         break
      log.info("")
      log.info("** DAML test PASSED in background")

def start_daml_txn_background_thread(fxBlockchain, daml_txn_result_queue,
                                     node_interruption_details, fxHermesRunSettings):
   '''
      Util to trigger daml transactions in background
      :param fxBlockchain: blockchain
      :param daml_txn_result_queue: flag to notify main thread about background daml test thread status
      :param node_interruption_details: interruptions/recovery detail
      :param fxHermesRunSettings: hermes run settings (fixture)
      :return: reference of background thread to main thread
      '''
   results_dir_name = ''.join(
      e for e in node_interruption_details[NODE_INTERRUPTION_TYPE] if e.isalnum())
   results_dir = helper.create_results_sub_dir(
      fxHermesRunSettings["hermesTestLogDir"],
      results_dir_name)
   daml_tests_results_dir = helper.create_results_sub_dir(results_dir, "daml_tests")
   daml_txn_background_thread = threading.Thread(target=run_daml_sanity_in_background_loop,
                                                 args=(fxBlockchain, daml_txn_result_queue, daml_tests_results_dir))

   daml_txn_background_thread.start()
   log.info("")
   log.info("***sleeping for 5 seconds after running daml tests in  background")
   time.sleep(5)
   return daml_txn_background_thread
