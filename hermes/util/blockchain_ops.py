#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#
#   Blockchain Operations
#
#   manipulations that can be done with fxBlockchain
#
#########################################################################

import traceback
import random
import threading
import time
import sys
import yaml

import rest
from util import auth, helper, hermes_logging, infra

log = hermes_logging.getMainLogger()




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
    "curl 127.0.0.1:9891/metrics > tmp; grep -a -m 1 -h -r '{}'".format(current_primary_match),
    "curl 127.0.0.1:9891/metrics > tmp; grep -a -m 1 -h -r '{}'".format(current_active_view_match),
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
    if current_primary != current_active_view % len(all_committers):
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
    This will get primary rid and map out commiter idx and rid relation.
  '''
  if verbose: log.info("")
  all_committers = committers_of(fxBlockchain)
  target_committers = [ip for ip in all_committers if ip not in interrupted_nodes]
  # Below will get principal_id, private_key, public_key from concord config (3 lines)
  replicaIdGetCommand = "cat /config/concord/config-local/concord.config"
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
  results = helper.ssh_parallel(target_committers, replicaIdGetCommand)
  committerRespondedCount = 0; errored = []
  for result in results:
    if result["output"]:
      try:
        concordConfig = yaml.safe_load(result["output"])
        fileterCondition = lambda nodeConfig: "private_key" in nodeConfig["replica"][0]
        nodeWithPrivateKey = list(filter(fileterCondition, concordConfig["node"]))[0]
        replicaId = nodeWithPrivateKey["replica"][0]["principal_id"]
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

  log.info("Deregistering blockchain {} from {}".format(bc_id, service))
  resp = req.deregisterBlockchain(bc_id)
  log.info("Response: {}".format(resp))
  resp = req.getBlockchains()
  log.info("Blockchains remaining: {}".format(resp))
