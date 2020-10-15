#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#
#   Blockchain Operations
#
#   manipulations that can be done with fxBlockchain
#
#########################################################################

import json
import traceback
import random
import threading
import time
import sys
import yaml

import rest
from util import auth, helper, hermes_logging, infra

log = hermes_logging.getMainLogger()

LAST_STABLE_SEQ_NUMBER_CMD = "docker exec -t telegraf /bin/bash -c 'curl concord:9891/metrics' | grep 'lastStableSeqNum{'"


# ===================================================================================
#   State Information Operation
#   (e.g. primary info & which committers are the submission endpoints)
# ===================================================================================
def refresh_current_state_info(fxBlockchain, verbose=False):
  '''
    Get the latest state info detailing:
    1. What the primary is (rid, idx, IP)
    2. Submission endpoints used for participants
    and output to console
  '''
  map_committers_info(fxBlockchain, verbose=verbose)
  map_participants_submission_endpoints(fxBlockchain, verbose=verbose)
  echo_current_state_info(fxBlockchain)
  return True


def wait_for_state_transfer_complete(fxBlockchain, interrupted_replicas=[], timeout=180):
  '''
  Determine whether all running replicas have finished state transfer.
  This is done by checking "lastStableSeqNum" on each replica.  If they
  are all the same, then they are all in sync.
  '''
  all_replicas = committers_of(fxBlockchain)
  target_replicas = [ip for ip in all_replicas if ip not in interrupted_replicas]
  username, password = helper.getNodeCredentials()
  seq_num = None
  complete = False
  start_time = time.time()

  while time.time() - start_time < timeout:
    responses = helper.ssh_parallel(target_replicas, LAST_STABLE_SEQ_NUMBER_CMD, verbose=True)

    for r in responses:
      output = r["output"]

      try:
        seq_num_to_check = float(output.split(" ")[-1].strip())
      except ValueError:
        complete = False
        seq_num = None
        break

      log.info("Replica '{}' lastStableSeqNum is '{}'".format(r["ip"], seq_num_to_check))

      if not seq_num:
        # First one is in sync with itself.
        complete = True
        seq_num = seq_num_to_check
      else:
        if not seq_num_to_check == seq_num:
          complete = False
          seq_num = None
          break

    if complete:
      break
    else:
      log.info("State transfer not complete.  Waiting.")
      time.sleep(1)

def get_primary_rid(fxBlockchain, interrupted_nodes=[], verbose=True):
  '''
    Get primary rid (id used internally for concord nodes)
    This is different from persephone-returned list of committers
    Nodes can differ opinions on what the primary is, in that case
    output warning to which rid is thought to be primary by which nodes
  '''
  all_committers = committers_of(fxBlockchain)
  target_committers = [ip for ip in all_committers if ip not in interrupted_nodes]
  current_primary_match = 'concord_concordbft_currentPrimary{source="concordbft",component="replica"} '
  current_active_view_match = 'concord_concordbft_currentActiveView{source="concordbft",component="replica"} '
  cmd = ';'.join([
    "docker exec -t telegraf /bin/bash -c 'curl concord:9891/metrics' > tmp; grep -a -m 1 -h -r '{}'".format(current_primary_match),
    "docker exec -t telegraf /bin/bash -c 'curl concord:9891/metrics' > tmp; grep -a -m 1 -h -r '{}'".format(current_active_view_match),
  ])
  results = helper.ssh_parallel(target_committers, cmd, verbose=verbose)
  primary_indexes = {}; last_added_index = None
  for result in results:
    lines = result["output"].split('\n')
    current_primary = None
    current_active_view = None
    for line in lines:
      if line.startswith(current_primary_match):
        current_primary = int(float(line.split(current_primary_match)[1]))
      elif line.startswith(current_active_view_match):
        current_active_view = int(float(line.split(current_active_view_match)[1]))
    if current_primary is not None:
      log.debug("         {} thinks primary is rid {}".format(result["ip"], current_primary))
      if current_primary not in primary_indexes: primary_indexes[current_primary] = []
      primary_indexes[current_primary].append(result["ip"])
      last_added_index = current_primary
    if current_active_view and (current_primary != current_active_view % len(all_committers)):
      # Active view strictly iterates according to next rid
      # If active view % total replica count is not matching with current primary,
      # it's a sign that view change is happening.
      log.warning("         {} is undergoing view change; currentPrimary ({}) => activeView ({})"
                        .format(result["ip"], current_primary, current_active_view))

  if len(primary_indexes.keys()) != 1: # disagreement on primary
    highest_agreed_idx = None; highest_agreed_count = 0
    for idx in primary_indexes.keys():
      replicas = primary_indexes[idx]
      log.warning("         Primary rid is {} according to {}".format(idx, ", ".join(replicas)))
      if len(replicas) > highest_agreed_count:
        highest_agreed_count = len(replicas)
        highest_agreed_idx = idx
    if highest_agreed_count > 0:
      if highest_agreed_count >= get_f_count(fxBlockchain):
        # no problem if (n-f) agrees on primary. e.g. 5/7 => quorum achieved
        log.warning("         Going with {} (most votes, {})"
                      .format(highest_agreed_idx, highest_agreed_count))
      else:
        # Very unstable state if less than (n-f) agree on primary; possible no quorum situation
        # You might have to wait a while for concord to sort this out.
        log.error("         !!! Less than (n-f) agreement. Going with {} (most votes, {})"
                      .format(highest_agreed_idx, highest_agreed_count))
    return highest_agreed_idx
  else:
    return last_added_index


def map_committers_info(fxBlockchain, interrupted_nodes=[], verbose=True):
  '''
    This will get primary rid, ip and map out committer idx and rid relation.
  '''
  if verbose: log.info("")
  all_committers = committers_of(fxBlockchain)
  target_committers = [ip for ip in all_committers if ip not in interrupted_nodes]
  # Below will get principal_id from deployment config
  replicaIdGetCommand = "cat /config/concord/config-local/deployment.config"
  committersMapping = {
    "primary_ip": None,
    "primary_index": None,
    "primary_rid": None,
    "committers": [None] * len(all_committers),
    "committer_index_by_rid": [None] * len(all_committers)
  }
  committersOutput = [""] * len(all_committers)
  if verbose: log.info("Mapping out the rid and index relationship in committers...")
  primary_rid = get_primary_rid(fxBlockchain, interrupted_nodes=interrupted_nodes, verbose=verbose)
  success = False
  try:
    results = helper.ssh_parallel(target_committers, replicaIdGetCommand)
    success = True
  except Exception as e:
    msg = "Error fetching deployment config from one of committer nodes is : '{}'".format(str(e))
    log.error(msg)
  
  if not success:
        raise(e)

  committerRespondedCount = 0; errored = []
  for result in results:
    if result["output"]:
      try:
        config = yaml.safe_load(result["output"])
        node = [x for x in config["node"] if "current_node" in x and x["current_node"]][0]
        replicaId = node["replica"][0]["principal_id"]
        committerIndex = target_committers.index(result["ip"])
        committersMapping["committers"][committerIndex] = {
          "ip": result["ip"], "index": committerIndex, "rid": replicaId
        }
        committerRespondedCount += 1
        committersMapping["committer_index_by_rid"][replicaId] = committerIndex
        committersOutput[committerIndex] = "    replica_id={}, ip={} (Committer {})".format(
          replicaId, result["ip"], committerIndex
        )
        if primary_rid == replicaId:
          committersMapping["primary_ip"] = result["ip"]
          committersMapping["primary_index"] = committerIndex
          committersMapping["primary_rid"] = primary_rid
          if verbose: log.info("Primary replica_id is {}, which is committers list index {} with ip={}".format(
            primary_rid, committerIndex, result["ip"]
          ))
      except yaml.YAMLError as exc:  errored.append(result["ip"])
    else: errored.append(result["ip"])
  if verbose:
    mappingComplete = (committerRespondedCount == len(target_committers))
    if mappingComplete: print("Committer index, rid and IP mapping:\n" + "\n".join(committersOutput))
    else: log.warning("Mapping is incomplete; problem IPs: {}".format(errored))
  if "primary_index" in fxBlockchain.replicas and fxBlockchain.replicas["primary_index"] is not None:
    primary_idx_before = fxBlockchain.replicas["primary_index"]
    primary_idx_now = committersMapping["primary_index"]
    if primary_idx_before != primary_idx_now:
      log.warning("       View Change detected: primary from rid={} to rid={}" \
                        .format(primary_idx_before, primary_idx_now))
  fxBlockchain.replicas["primary_index"] = committersMapping["primary_index"]
  fxBlockchain.replicas["primary_rid"] = committersMapping["primary_rid"]
  fxBlockchain.replicas["primary_ip"] = committersMapping["primary_ip"]
  fxBlockchain.replicas["committer_index_by_rid"] = committersMapping["committer_index_by_rid"]
  return committersMapping

# Below must change once participants are able to connect to multiple committers
def map_participants_submission_endpoints(fxBlockchain, verbose=True):
  '''
    This will map out exactly, which participants are connected to which committer
    (e.g. which committers are used for participant N as tx submission endpoint)
  '''
  participants = participants_of(fxBlockchain)
  participantsMapping = {
    "allSubmissionEndpoints": [],
    "participants": [None] * len(participants)
  }
  submissionEndpointGetCommand = "cat /config/daml-ledger-api/environment-vars"
  if verbose: log.info("Mapping out participants for their target committer endpoints for submission...")
  results = helper.ssh_parallel(participants, submissionEndpointGetCommand)
  all_submission_endpoints = []
  for result in results:
    participantIndex = participants.index(result["ip"])
    participantIP = result["ip"]
    lines = result["output"].split("\n")
    for line in lines:
      if "REPLICAS" in line:
        # Find index 0 of `export REPLICAS=` line in /config/daml-ledger-api/environment-vars
        # This should currently be the main committer (master replica) which participants use
        # for submitting transactions (if this committer is disabled, no tx will go through).
        first_replica_with_port = line.split('=')[1]
        submissionEndpoint = first_replica_with_port.split(':')[0] # remove port from endpoint
        submission_endpoints = [submissionEndpoint] # TODO: Only one committer used for now
        participantsMapping["participants"][participantIndex] = {
          "submission_endpoints": submission_endpoints
        }
        all_submission_endpoints += submission_endpoints
        for endpoint in submission_endpoints:
          if endpoint not in participantsMapping["allSubmissionEndpoints"]:
            participantsMapping["allSubmissionEndpoints"].append(endpoint)
        if verbose: log.info("Participant {} ({}) is using {} as its main submission endpoint".format(
          participantIndex, participantIP, submissionEndpoint
        ))
  all_submission_endpoints = list(set(all_submission_endpoints)) # unique
  fxBlockchain.replicas["submission_endpoints"] = all_submission_endpoints
  return participantsMapping





# ===================================================================================
#   Fixture Reset Operations
# ===================================================================================
def reset_blockchain(fxBlockchain, concordConfig=None, useOriginalConfig=False,
                        keepData=False, resetOnlyTheseIPs=None, verbose=True):
  '''
    ! DO NOT RUN AGAINST PRODUCTION BLOCKCHAIN

    Reset blockchain and remove all its persisted data
    (Only covers DAML network for now.)

    `keepData` flag will skip persistency deletion (Currently, agent will fail if persistency is kept)

    `resetOnlyTheseIPs`:  list of target to be selectively reset; for wiping out only selected nodes.
                          Supplying it with [] or any falsey value will skip filtering and reset all nodes.

    e.g.: helper.reset_blockchain(fxBlockchain, concordConfig={
      "concord-bft_max_num_of_reserved_pages": 8192,
      "view_change_timeout": 20000,
    })
  '''
  replicas = fxBlockchain.replicas
  wipeOutPre = [
    "docker stop $(docker ps -a -q)",
    "docker rm jaeger-agent",
    "docker rm $(docker ps -a | grep -v \"agent\" | cut -d ' ' -f1)",
    "cp -n /config/concord/config-local/concord.config /config/concord/config-local/concord.original.config",
    "cp /config/concord/config-local/concord.original.config /config/concord/config-local/concord.config",
    "chmod 777 /config/concord/config-local/concord.config",
  ]
  wipeOutDeleteData = [] if keepData else [
    "rm -rf /config/concord/rocksdbdata", # Committer persistency
    "rm -rf /config/daml-index-db/db", # participant Ledger API persistency
    "rm -rf /var/log/deployment_support_logs/*" # avoid VM running out of storage due to massive support bundles
  ]
  wipeOutConcordConfigSet = []
  if concordConfig:
    if useOriginalConfig:
      wipeOutConcordConfigSet = [
        "cp /config/concord/config-local/concord.original.config /config/concord/config-local/concord.config",
        "chmod 777 /config/concord/config-local/concord.config",
      ]
    else:
      for configParam in concordConfig:
        configParamValue = concordConfig[configParam]
        wipeOutConcordConfigSet.append(
          "sed -i '/{}/c\\{}: {}' /config/concord/config-local/concord.config".format(configParam, configParam, configParamValue))
  wipeOutPost = [
    "df -h", "cat /config/concord/config-local/concord.config | grep concord-bft",
    "cp \"{}\" \"{}\"".format(helper.HEALTHD_LOG_PATH, helper.HEALTHD_LOG_PATH.replace(".log", "2.log")), # copy healthd.log => healthd2.log
  ]
  nodeWipeOutCommand = '; '.join(wipeOutPre + wipeOutDeleteData + wipeOutConcordConfigSet + wipeOutPost)
  nodeRetartCommand = "docker start agent" # Agent handles everything as long as persistency is deleted
  nodeResetCrashStatusCommand = "rm -rf \"{}\"; cd /root/health-daemon; nohup bash healthd.sh > /dev/null".format(helper.HEALTHD_CRASH_FILE)
  committerIPs = committers_of(fxBlockchain)
  participantIPs = participants_of(fxBlockchain)
  if resetOnlyTheseIPs:
    committerIPs = list(filter(lambda ip: ip in resetOnlyTheseIPs, committerIPs))
    participantIPs = list(filter(lambda ip: ip in resetOnlyTheseIPs, participantIPs))
  allNodesIPs = committerIPs + participantIPs
  if verbose: log.info("Resetting blockchain...")
  if participantIPs:
    if verbose: log.info("Wiping out participant nodes: {}".format(', '.join(participantIPs)))
    results = helper.ssh_parallel(participantIPs, nodeWipeOutCommand, verbose=verbose)
    if verbose:
      for result in results: log.debug("\n\n[Wiping participant containers] SSH outputs [{}]:\n{}".format(result["ip"], result["output"]))
  if committerIPs:
    if verbose: log.info("Wiping out committer nodes: {}".format(', '.join(committerIPs)))
    results = helper.ssh_parallel(committerIPs, nodeWipeOutCommand, verbose=verbose)
    if verbose:
      for result in results: log.debug("\n\n[Wiping committer containers] SSH outputs [{}]:\n{}".format(result["ip"], result["output"]))
  if committerIPs:
    if verbose: log.info("Restarting committer nodes (through agent): {}".format(', '.join(committerIPs)))
    results = helper.ssh_parallel(committerIPs, nodeRetartCommand, verbose=verbose)
    if verbose:
      for result in results: log.debug("\n\n[Restart agent on committers] SSH outputs [{}]:\n{}".format(result["ip"], result["output"]))
  if participantIPs:
    if verbose: log.info("Restarting participant nodes (through agent): {}".format(', '.join(participantIPs)))
    results = helper.ssh_parallel(participantIPs, nodeRetartCommand, verbose=verbose)
    if verbose:
      for result in results: log.debug("\n\n[Restart agent on participant] SSH outputs [{}]:\n{}".format(result["ip"], result["output"]))
  initializationGracePeriod = 30
  if verbose:  log.info("Giving another {}s for all nodes to initialize...".format(initializationGracePeriod))
  time.sleep(initializationGracePeriod)
  if allNodesIPs:
    if verbose: log.info("Resetting crash status of the nodes...")
    results = helper.ssh_parallel(allNodesIPs, nodeResetCrashStatusCommand, verbose=verbose)
    if verbose:
      for result in results: log.debug("\n\n[Reset crashed status] SSH outputs [{}]:\n{}".format(result["ip"], result["output"]))
  state_info = {
    "primary_ip": None, "primary_index": None, "primary_rid": None,
    "committer_index_by_rid": None, "submission_endpoints": None
  }
  if committerIPs: map_committers_info(fxBlockchain, verbose=verbose)
  if participantIPs:
    # need to avoid submission endpoints until there are more connections from participants to other committers
    map_participants_submission_endpoints(fxBlockchain, verbose)
  if verbose: log.info("Blockchain network reset completed.")
  return state_info





# ===================================================================================
#   Convenience functions
# ===================================================================================
def get_f_count(fxBlockchain):
  return int((len(fxBlockchain.replicas[helper.TYPE_DAML_COMMITTER]) - 1) / 3)

def committers_of(fxBlockchain):
  return fxBlockchain.replicas[helper.TYPE_DAML_COMMITTER]

def participants_of(fxBlockchain):
  return fxBlockchain.replicas[helper.TYPE_DAML_PARTICIPANT]

def get_all_node(fxBlockchain):
  return committers_of(fxBlockchain) + participants_of(fxBlockchain)

def echo_current_state_info(fxBlockchain):
  log.info("       Current primary rid={}, is {} (committer idx {}), submissions on {}".format(
    fxBlockchain.replicas["primary_rid"],
    fxBlockchain.replicas["primary_ip"],
    fxBlockchain.replicas["primary_index"],
    ", ".join(fxBlockchain.replicas["submission_endpoints"]),
  ))

def fetch_master_replica(fxBlockchain):
  '''
  Get master replica IP
  :param fxBlockchain: blockchain fixture
  :return: master replica IP
  '''
  username, password = helper.getNodeCredentials()
  cmd = "cat /config/daml-ledger-api/environment-vars"
  master_replica = None
  for ip in participants_of(fxBlockchain):
    ssh_output = helper.ssh_connect(ip, username, password, cmd)
    if ssh_output:
      for line in ssh_output.split():
        if "REPLICAS" in line:
          first_replica_with_port = line.split('=')[1]
          master_replica = first_replica_with_port.split(':')[0]
          break

  log.info("Master replica: {}".format(master_replica))
  return master_replica


def print_replica_info(fxBlockchain, interrupted_nodes=[]):
  username, password = helper.getNodeCredentials()
  cmd = 'grep -2 -w private_key /config/concord/config-local/concord.config | ' \
        'grep principal_id ; docker exec -t telegraf /bin/bash -c "curl concord:9891/metrics" | ' \
        'grep concord_concordbft_current | grep source='

  filtered_ips = [ip for ip in committers_of(fxBlockchain) if ip not in interrupted_nodes]
  log.info("")
  for ip in filtered_ips:
    ssh_output = helper.ssh_connect(ip, username, password, cmd)
    primary_should_be = principal_id = currentPrimary = '?'
    if ssh_output:
      for line in ssh_output.split('\r'):
        try:
          if "principal_id" in line:
            principal_id = line.split(':')[1]
          if "concord_concordbft_currentActiveView" in line:
            concord_concordbft_currentActiveView = line.split(' ')[1]
            if concord_concordbft_currentActiveView:
              primary_should_be = int(concord_concordbft_currentActiveView.split('.')[0]) % len(committers_of(fxBlockchain))
          if "concord_concordbft_currentPrimary" in line:
            concord_concordbft_currentPrimary = line.split(' ')[1]
            if concord_concordbft_currentPrimary:
              currentPrimary = int(concord_concordbft_currentPrimary.split('.')[0])
        except Exception as e:
          pass

      if currentPrimary == primary_should_be:
        log.info(
          "[current primary replica_id: {}, remains at {}] (replica_id {} == {})".format(
            currentPrimary, primary_should_be, principal_id, ip))
      else:
        log.info(
          "[current primary replica_id: {}, changing to {}] (replica_id {} == {})".format(
            currentPrimary, primary_should_be, principal_id, ip))

      if primary_should_be == '?' or principal_id == '?' or currentPrimary == '?':
        log.warning("Couldn't fetch replica_id/primary replica")
        log.info(ssh_output)


def deregister_blockchain(org_name, bc_id, service):
  '''
  Utility method to deregister a blockchain.
  bc_id of "all" means to deregister all of them for this org.
  '''
  token_descriptor = {
    "org": org_name,
    "user": "vmbc_test_con_admin",
    "role": auth.ROLE_CON_ADMIN
  }

  req = rest.request.Request("deregister_blockchain",
                             "Deregister blockchain",
                             service,
                             None,
                             tokenDescriptor=token_descriptor,
                             service=service)

  if bc_id == "all":
    resp = req.getBlockchains()

    for bc in resp:
      resp = req.deregisterBlockchain(bc["id"])
      log.info("response: {}".format(resp))
  else:
    log.info("Deregistering blockchain {} from {}".format(bc_id, service))
    resp = req.deregisterBlockchain(bc_id)
    log.info("Response: {}".format(resp))

  resp = req.getBlockchains()
  log.info("Blockchains remaining: {}".format(resp))


def get_blockchain_nodes(org_name, bc_id, service):
  '''
  Utility method to get blockchain node information in a format which is compatible
  with the --replicasConfig parameter of Hermes.
  '''
  token_descriptor = {
    "org": org_name,
    "user": "vmbc_test_con_admin",
    "role": auth.ROLE_CON_ADMIN
  }

  req = rest.request.Request("get_blockchain_nodes",
                             "get_blockchain_nodes",
                             service,
                             None,
                             tokenDescriptor=token_descriptor,
                             service=service)

  log.info("Getting node information for {} from {}".format(bc_id, service))
  participants = req.get_participant_details(bc_id)

  replicas = req.getReplicas(bc_id)

  output = {
    "daml_committer": [],
    "daml_participant": []
  }

  for p in participants:
    output["daml_participant"].append(p)

  for r in replicas:
    output["daml_committer"].append(r)

  output = json.dumps(output, indent=4)
  log.info(output)
  return output


def wait_for_docker_startup(host, user, password, container_names):
  '''
  Given a host, creds, and list of services, wait for them to be up.
  Raises an exception on failure.
  '''
  log.info("Waiting for docker startup for {}".format(host))
  needs_wait = False
  reachable = False
  wait_time = 60

  while not reachable and wait_time > 0:
    statuses = helper.ssh_connect(host,
                                  user, password,
                                  "docker ps -a --format '{{ .Names }}\t\t{{ .Status }}'")
    if statuses:
      reachable = True
    else:
      time.sleep(1)
      wait_time -= 1

  if not reachable:
    raise Exception("VM {} was not reachable".format(host))

  statuses = statuses.split("\n")
  log.info("  Statuses:")
  for status in statuses:
    log.info("    {}".format(status))

    if "Exited" in status or "Restarting" in status or "Paused" in status:
      needs_wait = True

  if needs_wait:
    log.info("Waiting for containers to come up")
    time.sleep(10)
    wait_time = 60
    all_up = False

    while not all_up and wait_time > 0:
      all_up = True
      statuses = helper.ssh_connect(host,
                                    user, password,
                                    "docker ps -a --format '{{ .Names }} {{ .Status }}'")
      statuses = statuses.split("\n")
      log.info("  Statuses:")

      for name in container_names:
        name_found = False

        for status in statuses:
          if status.split(" ")[0] == name:
            name_found = True
            break

        if not name_found:
          log.info("Docker container {} not found yet".format(name))
          all_up = False
          time.sleep(1)
          wait_time -= 1
          continue

      for status in statuses:
        log.info("    {}".format(status))
        if "Restarting" in status or "Exited" in status:
          all_up = False
          time.sleep(1)
          wait_time -= 1
          continue

    if wait_time <= 0:
      raise Exception("Docker startup failed")


def pause_services(host, user, password, services):
  '''
  services: A list of the docker containers to pause.
  '''
  for svc in services:
    log.info("Pausing {}".format(svc))
    cmd = "docker pause {}".format(svc)
    helper.ssh_connect(host, user, password, cmd)


def unpause_services(host, user, password, services):
  '''
  services: A list of the docker containers to unpause.
  '''
  for svc in services:
    log.info("Unpausing {}".format(svc))
    cmd = "docker unpause {}".format(svc)
    helper.ssh_connect(host, user, password, cmd)


def stop_services(host, user, password, services):
  '''
  services: A list of the docker containers to stop.
  '''
  for svc in services:
    log.info("Stopping {} on {}".format(svc, host))
    cmd = "docker stop {}".format(svc)
    helper.durable_ssh_connect(host, user, password, cmd)


def start_services(host, user, password, services):
  '''
  services: A list of the docker containers to start.
  '''
  for svc in services:
    log.info("Starting {} on {}".format(svc, host))
    cmd = "docker start  {}".format(svc)
    helper.durable_ssh_connect(host, user, password, cmd)


def shutdown(host, timeout=10):
  '''
  Gets a VM handle before shutting down the VM, and returns the handle.
  The handle is needed to power it back on.
  '''
  vm = infra.findVMByInternalIP(host)
  vm["entity"].PowerOffVM_Task()
  time.sleep(timeout)
  return vm


def powerup(vm, timeout=20):
  '''
  The vm parameter is a vim handle returned by, for example, shutdown().
  '''
  vm["entity"].PowerOnVM_Task()
  time.sleep(timeout)


def reboot(host, timeout=20):
  log.info("Node {} rebooting".format(host))
  vm = shutdown(host, timeout)
  powerup(vm, timeout)

def move_vms(blockchain_id, dest_dir="HermesTesting", sddcs=None):
  '''
  Move VMs to a destination folder (garbage collecttion after a testrun)
  :param blockchain_id: Blockchain ID
  :param dest_dir: destination folder
  :param sddcs: Optionals list of SDDCs
  '''
  sddcs = sddcs if sddcs is not None else infra.getListFromZoneConfig()
  infra.prepareConnections(sddcs)
  for sddc_name in sddcs:
    sddc_conn = infra.getConnection(sddc_name)
    sddc_conn.vmMoveToFolderByName(dest_dir, [blockchain_id])


def get_all_crashed_nodes(fxBlockchain, results_dir, interrupted_node_type=None,
                          interrupted_nodes=[]):
  '''
  Get list of all crashed nodes
  :param fxBlockchain: blockchain fixture
  :param results_dir: results dir
  :param interrupted_nodes: test interrupted nodes
  :return: list of all crashed nodes, and crash log directory
  '''
  log.info("")
  log.info("** Verifying health of all nodes...")
  username, password = helper.getNodeCredentials()
  all_committers_other_than_interrupted = [ip for ip in
                                           committers_of(fxBlockchain) if
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

  uninterrupted_participants = [ip for ip in participants_of(fxBlockchain) if
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

  unexpected_crash_results_dir = ""
  if len(unexpected_interrupted_committers) > 0:
    unexpected_crash_results_dir = helper.create_results_sub_dir(results_dir,
                                                                 "unexpected_crash")
    log.info(
      "Collect support logs ({})...".format(unexpected_crash_results_dir))
    helper.create_concord_support_bundle(
      [ip for ip in committers_of(fxBlockchain) if
       ip not in interrupted_nodes], helper.TYPE_DAML_COMMITTER,
      unexpected_crash_results_dir, verbose=False)
    helper.create_concord_support_bundle(
      participants_of(fxBlockchain),
      helper.TYPE_DAML_PARTICIPANT, unexpected_crash_results_dir,
      verbose=False)

  if total_no_of_committers_crashed > get_f_count(fxBlockchain):
    log.error("**** System is unhealthy")

  return crashed_committers, crashed_participants, unexpected_crash_results_dir

def get_docker_timestamp(host, username, password, service):
    '''
    Retrieves the most recent timestamp from `docker logs` for the given service.
    '''
    output = helper.durable_ssh_connect(host, username, password,
                                        "docker logs --tail 1 {}".format(service))
    return output.split("|")[0]
