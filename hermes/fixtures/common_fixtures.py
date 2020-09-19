#################################################################################
# Copyright 2018 - 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#################################################################################
import collections
import json
import logging
import os
import pprint
import pytest
import socket
import struct
import time
import types
from urllib.parse import urlparse
from util.product import Product
from rest.request import Request
from rpc.rpc_call import RPC
from util import auth, csp, helper, infra, hermes_logging, numbers_strings, blockchain_ops, node_interruption_helper
from util.daml import daml_helper
from util.blockchain import eth as eth_helper
from suites.case import describe
import util.generate_zones_migration as migration

log = hermes_logging.getMainLogger()
ConnectionFixture = collections.namedtuple("ConnectionFixture", "request, rpc")
BlockchainFixture = collections.namedtuple("BlockchainFixture", "blockchainId, consortiumId, replicas, clientNodes")
ProductFixture = collections.namedtuple("ProductFixture", "product")

# These orgs are artifically inserted into Helen and do not respond to all API calls
# the way standard orgs do.
BUILTIN_ORGS = ["0460bc7f-41a4-4570-acdb-adbade2acb86", "4c722759-fc17-408d-bc41-e6775fc1e111"]

def isBuiltInOrg(orgId):
   return orgId in BUILTIN_ORGS


# TODO: refactor this method to make it generic
def setUpPortForwarding(url_or_host, creds, blockchainType, logDir, src_port=443, dest_port=8545, timeout=600,):
   '''
   Given a url/host  and credentials, set up port forwarding.
   The VMs should be ready in two minutes; default timeout is 2.5 min just
   in case.
   '''
   if "http" in url_or_host:
      urlObject = urlparse(url_or_host)
      host = urlObject.hostname
   else:
      host = url_or_host
   log.info("Setting up port forwarding on deployed nodes to comply with VMware IT policies.")

   timeTaken = 0
   interval = 10
   portForwardingSuccess = False

   while not portForwardingSuccess and timeTaken < timeout:
      portForwardingSuccess = helper.add_ethrpc_port_forwarding(host, creds[
         "username"], creds["password"], src_port=src_port, dest_port=dest_port)

      if not portForwardingSuccess:
         log.info("Port forwarding setup failed.  The VM is probably still coming up. " \
                  "Trying again in {} seconds. ({} seconds taken so far.)".format(interval, timeTaken))
         time.sleep(interval)
         timeTaken += interval

   if not portForwardingSuccess:
       helper.create_concord_support_bundle([host], blockchainType, logDir)
       raise Exception("Failed to set up port forwarding on deployed nodes. Aborting.")


def getBlockchainFullDetails(blockchainId, conAdminRequest):
  '''
    Fetches detailed data of the entire topology of the blockchain from Helen
    1) blockchain info
    2) all nodes list (`nodes_list` = committers + participants + ...)
    3) nodes credentials (injected into each member of `node_list` as "password")
  '''
  blockchainDetails = conAdminRequest.getBlockchainDetails(blockchainId)
  committersDetails = conAdminRequest.getReplicas(blockchainId)
  clientsDetails = conAdminRequest.get_participant_details(blockchainId)
  allNodesDetails = committersDetails + clientsDetails
  for committerDetails in committersDetails:
    committerDetails["type_name"] = helper.TYPE_DAML_COMMITTER
  for clientDetails in clientsDetails:
    clientDetails["type_name"] = helper.TYPE_DAML_PARTICIPANT
  for nodeDetails in allNodesDetails: # get strong passwords for each node
    try:
      nodeCredentials = conAdminRequest.getNodeCredentials(
                          blockchainId, nodeDetails["id"], nodeDetails["type_name"])
      nodeDetails["password"] = nodeCredentials["password"]
    except Exception as e:
      helper.hermesNonCriticalTrace(e)
      pass
  blockchainFullDetails = {
    "id": blockchainDetails["id"],
    "consortium_id": blockchainDetails["consortium_id"],
    "blockchain_type": blockchainDetails["blockchain_type"], # e.g. DAML | ETHEREUM | HLF
    "blockchain_state": blockchainDetails["blockchain_state"], # e.g. ACTIVE
    "version": blockchainDetails["version"], # e.g. Blockchain Version: 0.0.0.1635, DAML SDK Version: 1.0.0
    "created_by": blockchainDetails["created_by"],
    "created": blockchainDetails["created"], # time UNIX timestamp in seconds
    "deployed_from": "Helen",
    "nodes_list": allNodesDetails # all nodes (committers + participants + ...)
  }
  return blockchainFullDetails


def getExistingBlockchainDetails(logDir, hermesData):
   '''
   Return the blockchain passed in, if any.
   '''
   blockchainId = hermesData["hermesUserConfig"]["product"][auth.CUSTOM_BLOCKCHAIN]
   tokenDescriptor = auth.getTokenDescriptor(auth.ROLE_CON_ADMIN,
                                                  True,
                                                  auth.default_con_admin)
   conAdminRequest = Request(logDir,
                             "fxBlockchain",
                             hermesData["hermesCmdlineArgs"].reverseProxyApiBaseUrl,
                             hermesData["hermesUserConfig"],
                             tokenDescriptor=tokenDescriptor)
   blockchainDetails = conAdminRequest.getBlockchainDetails(blockchainId)

   if "consortium_id" in blockchainDetails and \
      blockchainDetails["consortium_id"]:
       return blockchainId, blockchainDetails["consortium_id"]
   else:
       raise Exception("Unable to get details of blockchain ID {} with " \
                       "user {} using request {}".format(blockchainId, \
                                                         tokenDescriptor, \
                                                         conAdminRequest))


def getTokenDescriptor(hermesData):
   '''
   If we're deploying and Hermes was given an org to use, use a con admin for that org.
   If we're deploying and no org was specified, use the default deployment org (hemes_org0).
   If we're not deploying (docker container blockchain), use the internal admin user and org.
   '''
   if hermesData["hermesCmdlineArgs"].blockchainLocation in \
      [helper.LOCATION_SDDC, helper.LOCATION_ONPREM]:

      if hermesData["hermesCmdlineArgs"].deploymentOrg:
         return {
            "org": hermesData["hermesCmdlineArgs"].deploymentOrg,
            "user": "vmbc_test_con_admin",
            "role": auth.ROLE_CON_ADMIN
         }
      else:
         return auth.getTokenDescriptor(auth.ROLE_CON_ADMIN,
                                        True,
                                        auth.default_con_admin)
   else:
      return auth.getTokenDescriptor(auth.ROLE_CON_ADMIN,
                                     True,
                                     auth.internal_admin)


def validZoneDetails(zone):
   '''
   Given the results of a Helen API call for details of a zone, like
   /api/blockchains/zones/05790a88-7396-42bf-be16-a4e80e1ffee7, determine if
   the returned value is valid.
   A local Helen instance can create a default zone which can misbehave. Regular
   zones are ok.
   '''
   return "type" in zone


def getZoneIds(conAdminRequest, newZones, blockchainLocation):
   '''
   conAdminRequest: Reqest object for a consortium admin for the org & service we are working with.
   newZones: Zones which we think we need to add.
   Before adding newZones, check to see if a matching zone is already there.
   Add only zones that need to be added.
   Return the zone IDs to which the blockchain will be deployed.
   '''
   existingZoneIds = []
   existingZones = []
   newZonesToAdd = []
   returnZoneIds = []

   resp = conAdminRequest.getZones()

   for zone in resp:
      if not isBuiltInOrg(zone["id"]):
         existingZoneIds.append(zone["id"])

   for zoneId in existingZoneIds:
      resp = conAdminRequest.getZone(zoneId)
      existingZones.append(resp)

   for newZone in newZones:
      found = False

      for existingZone in existingZones:
         if validZoneDetails(existingZone) and newZoneEqualsExistingZone(existingZone, newZone):
            found = True
            returnZoneIds.append(existingZone["id"])
            break

      if not found:
         newZonesToAdd.append(newZone)

   newZoneIds = conAdminRequest.addUserConfigZones(newZonesToAdd, blockchainLocation)
   returnZoneIds.extend(newZoneIds)
   return returnZoneIds


def newZoneEqualsExistingZone(existingZone, newZone):
   '''
   Structures and names differ between what info we have to create a zone, and what we
   get back when asking for zones, so we have to do a careful field-to-field comparison.
   '''
   # Check on-prem specific items in a separate block because they have a "vsphere" section.
   if existingZone["type"] == "ON_PREM":
      if (not newZone["info"]["type"] == "VSPHERE") or \
         (not existingZone["resource_pool"] == newZone["vsphere"]["resourcePool"]) or \
         (not existingZone["storage"] == newZone["vsphere"]["datastore"]) or \
         (not existingZone["folder"] == newZone["vsphere"]["folder"]) or \
         (not existingZone["network"]["name"] == newZone["vsphere"]["network"]["name"]) or \
         (not existingZone["network"]["gateway"]) == newZone["vsphere"]["network"]["gateway"] or \
         (not int(existingZone["network"]["subnet"]) == newZone["vsphere"]["network"]["subnet"]):
         return False
   elif existingZone["type"] == "VMC_AWS":
      # Test automation does not add these here yet.
      return False
   else:
      # No idea what we found, but it isn't something we add.
      return False

   # Check items common to on-prem and cloud/sddc.
   if existingZone["name"] == newZone["info"]["labels"]["name"] and \
      existingZone["vcenter"]["url"] == newZone["api"]["address"]:
      return True
   else:
      return False


def deployToSddc(logDir, hermesData, blockchainLocation):
   tokenDescriptor = getTokenDescriptor(hermesData)
   log.info("deployToSddc using tokenDescriptor {}".format(tokenDescriptor))
   log.info("Deployment service: {}".format(hermesData["hermesCmdlineArgs"].deploymentService))
   log.info("tokenDescriptor: {}".format(tokenDescriptor))

   conAdminRequest = Request(logDir,
                             "fxBlockchain",
                             hermesData["hermesCmdlineArgs"].deploymentService,
                             hermesData["hermesUserConfig"],
                             tokenDescriptor=tokenDescriptor,
                             service=hermesData["hermesCmdlineArgs"].deploymentService)

   # Use an existing blockchain if present?
   # blockchains = conAdminRequest.getBlockchains()
   suffix = numbers_strings.random_string_generator()
   conName = "con_{}".format(suffix)
   conResponse = conAdminRequest.createConsortium(conName)
   conId = conResponse["consortium_id"]
   zoneIds = []

   if blockchainLocation == helper.LOCATION_SDDC:
      # There is no Helen API for adding SDDC zones, so use the ones loaded
      # via the backdoor sql approach.
      for zone in conAdminRequest.getZones():
         log.info(zone)
         zoneIds.append(zone["id"])
   else:
      # Use the Helen API to add on prem zones.
      onpremZones = hermesData["hermesZoneConfig"]["zones"][helper.LOCATION_ONPREM]
      zoneIds = getZoneIds(conAdminRequest, onpremZones, blockchainLocation)

   log.info("Zone IDs: {}".format(zoneIds))

   if not zoneIds:
       raise Exception("No zones available to proceed with deployment.")
   numNodes = int(hermesData["hermesCmdlineArgs"].numReplicas)

   client_zone_ids = []
   # client nodes array
   client_nodes = []
   num_participants = 0
   blockchain_type = hermesData["hermesCmdlineArgs"].blockchainType

   if blockchain_type.lower() == helper.TYPE_DAML:
       num_participants = int(hermesData["hermesCmdlineArgs"].numParticipants)
       client_zone_ids = helper.distributeItemsRoundRobin(num_participants, zoneIds)
       # How many groups do we have to create?
       num_groups = int(hermesData["hermesCmdlineArgs"].numGroups)
       # We know how many groups, how many clients/participants.
       client_nodes = helper.getClientNodes(num_groups, client_zone_ids)
       log.debug("client_nodes after getClientNodes {}".format(client_nodes))

   # numNodes is the number of committers and does not include participants.
   siteIds = helper.distributeItemsRoundRobin(numNodes, zoneIds)
   response = conAdminRequest.createBlockchain(conId, siteIds, client_nodes, blockchain_type.upper())
   log.debug("client nodes in common fixtures {}".format(client_nodes))

   if "task_id" not in response:
       raise Exception("task_id not found in response to create blockchain.")
   taskId = response["task_id"]
   success, response = helper.waitForTask(conAdminRequest, taskId, timeout=60*15)
   blockchainId = response["resource_id"]

   replica_dict = None

   if success:
      blockchainDetails = conAdminRequest.getBlockchainDetails(blockchainId)
      replica_details = conAdminRequest.getReplicas(blockchainId)
      credentials = hermesData["hermesUserConfig"]["persephoneTests"]["provisioningService"]["concordNode"]

      # VM Annotations (optional)
      blockchainFullDetails = getBlockchainFullDetails(blockchainId, conAdminRequest)
      log.info("Details of the deployed blockchain, in case you need to delete its resources " \
               "manually: {}\n".format(json.dumps(blockchainFullDetails, indent=4)))
      log.info("Annotating VMs with deployment context...")
      fatal_errors = infra.giveDeploymentContext(blockchainFullDetails)
      if fatal_errors: # e.g. IP conflicts
            infra.save_fatal_errors_to_summary(fatal_errors)

      ethereum_replicas = []
      daml_committer_replicas = []
      daml_participant_replicas = []
      id_dict = {"blockchain_id": blockchainId}

      # Ethereum
      if blockchain_type.lower() == helper.TYPE_ETHEREUM:
         ethereum_replicas = [{**replica_entry, **id_dict} for replica_entry in replica_details]
         success = verify_ethereum_deployment(replica_details, credentials, blockchain_type, logDir)

      # DAML
      elif blockchain_type.lower() == helper.TYPE_DAML:
         daml_committer_replicas = [{**replica_entry, **id_dict} for replica_entry in replica_details]
         success = verify_daml_committers_deployment(replica_details, credentials)
         if success:
            # verify_daml_committers_deployment() waits for docker containers to come up, which can take longer
            # than the CSP auth token is good for.
            conAdminRequest = Request(logDir,
                                      "fxBlockchain",
                                      hermesData["hermesCmdlineArgs"].deploymentService,
                                      hermesData["hermesUserConfig"],
                                      tokenDescriptor=tokenDescriptor,
                                      service=hermesData["hermesCmdlineArgs"].deploymentService)
            success, daml_participant_replicas = validate_daml_participants(conAdminRequest, blockchainId, credentials,
                                                                            num_participants)
            daml_participant_replicas = [{**replica_entry, **id_dict} for replica_entry in daml_participant_replicas]
      else:
         raise NotImplementedError("Deployment not supported for blockchain type: {}".format(blockchain_type))

      replica_dict = save_replicas_to_json(blockchain_type, ethereum_replicas, daml_committer_replicas,
                                           daml_participant_replicas, logDir)

   if success:
      log.info("Blockchain deployed successfully")
   else:
      create_support_bundle_from_replicas_info(blockchain_type, logDir)
      raise Exception("Failed to deploy a new blockchain.")

   return blockchainId, conId, replica_dict, client_nodes


def verify_ethereum_deployment(replica_details, credentials, blockchain_type, logDir):
    """
    Verifies containers on Ethereum nodes are running fine
    :param replica_details: Details of the replicas to verify for
    :param credentials: Credentials of the VM
    :param blockchain_type: Blockchain type
    :param logDir: Directory of logs
    :return: Status of verification
    """
    success = False
    try:
        for replica_entry in replica_details:
            setUpPortForwarding(replica_entry["rpc_url"], credentials, blockchain_type, logDir)
            host = replica_entry["private_ip"]
            helper.waitForDockerContainers(host, credentials["username"], credentials["password"], helper.TYPE_ETHEREUM)
            log.info("Ethereum node {} running successfully".format(host))
        log.info("All Ethereum nodes running successfully")
        success = True
    except Exception as e:
        log.error(e)
        log.error("Failed to deploy Ethereum nodes")

    return success


def verify_daml_committers_deployment(replica_details, credentials):
    """
    Verifies containers on DAML Committer nodes are running fine
    :param replica_details: Details of the replicas to verify for
    :param credentials: Credentials of the VM
    :return: Status of verification
    """
    success = False
    try:
        for replica_entry in replica_details:
            host = replica_entry["private_ip"]  # Assumption, peered network.
            helper.waitForDockerContainers(host, credentials["username"], credentials["password"],
                                           helper.TYPE_DAML_COMMITTER)
            log.info("Committer node {} running successfully".format(host))
        log.info("All committer nodes running successfully")
        success = True
    except Exception as e:
        log.error(e)
        log.error("Failed to deploy DAML committers")

    return success


def validate_daml_participants(con_admin_request, blockchain_id, credentials, num_participants=1):
    """
    Deploys participants in the given DAML blockchain
    :param con_admin_request: REST requests helper object
    :param blockchain_id: The blockchain ID for the blockchain to deploy participants in
    :param site_ids: Zone IDs to deploy in
    :param credentials: Credential information for accessing VMs after deployment
    :param num_participants: Number of participants to deploy (defaults to 1)
    :return: status, List of participant IPs
    """
    participant_replicas = []
    username = credentials["username"]
    password = credentials["password"]
    participant_details = con_admin_request.get_participant_details(blockchain_id)
    participant_replicas = [participant_entry for participant_entry in participant_details]

    log.info(participant_replicas)
    success = False
    try:
        # Upload DAR and verification block
        for participant_entry in participant_details:
            public_ip = participant_entry["private_ip"] # Assumption, peered network.
            helper.waitForDockerContainers(public_ip, username, password, helper.TYPE_DAML_PARTICIPANT)
            log.info("Participant node {} running successfully".format(public_ip))

            # Use port 80 for DAML instead of 443. LedgerApiServer is listening on 6865 over plain text
            # Setting up port forwarding from 443 to 6865 results in the following exception
            # INFO: Transport failed
            # io.netty.handler.codec.http2.Http2Exception: HTTP/2 client preface string missing or corrupt.
            src_port = helper.FORWARDED_DAML_LEDGER_API_ENDPOINT_PORT
            helper.add_ethrpc_port_forwarding(public_ip, username, password, src_port=src_port, dest_port=6865)
            log.info("Starting DAR upload on participant {}:{}".format(public_ip, src_port))
            daml_helper.upload_test_tool_dars(host=public_ip, port=str(src_port))
            log.info("Starting DAR upload verification test on participant {}".format(public_ip))
            daml_helper.verify_ledger_api_test_tool(host=public_ip, port=str(src_port))
            log.info("DAR upload and verification successful on participant {}".format(public_ip))
            success = True

    except Exception as e:
        log.error(e)
        log.error("Failed to validate DAML participants")
        success = False

    return success, participant_replicas


def save_replicas_to_json(blockchain_type, ethereum_replicas, daml_committer_replicas, daml_participant_replicas,
                          log_dir):
    """
    Saves committer and participant IPs in a json file
    :param blockchain_type: Type of blockchain. Will determine the replica dictionary structure
    :param ethereum_replicas: List of ethereum IPs
    :param daml_committer_replicas: List of DAML committer IPs
    :param daml_participant_replicas: List of DAML participant IPs
    :param log_dir: Log directory to save replica information in
    :return: None
    """

    replica_dict = {}
    if blockchain_type.lower() == helper.TYPE_ETHEREUM:
        replica_dict[helper.TYPE_ETHEREUM] = ethereum_replicas
    else:
        replica_dict[helper.TYPE_DAML_COMMITTER] = daml_committer_replicas
        replica_dict[helper.TYPE_DAML_PARTICIPANT] = daml_participant_replicas

    replica_log_dir_path = os.path.join(log_dir, helper.REPLICAS_JSON_FILE)
    try:
        os.remove(helper.REPLICAS_JSON_PATH)
        os.remove(replica_log_dir_path)
    except OSError:
        pass

    with open(helper.REPLICAS_JSON_PATH, 'w') as fp:
        json.dump(replica_dict, fp, indent=2)

    with open(replica_log_dir_path, 'w') as fp:
        json.dump(replica_dict, fp, indent=2)

    log.info("Saved replicas information in {} and {}".format(helper.REPLICAS_JSON_PATH, replica_log_dir_path))
    log.info("Saved replicas:")
    log.info(pprint.pformat(replica_dict, indent=4))
    return replica_dict


def create_support_bundle_from_replicas_info(blockchain_type, log_dir):
    """
    Given the blockchain type, creates support bundles in the log directory specified
    :param blockchain_type: Ethereum/DAML
    :param log_dir: Log directory to save the deployment bundle into
    :return: None
    """
    try:
        with open(helper.REPLICAS_JSON_PATH, 'r') as replicas_file:
            replica_dict = json.load(replicas_file)
            if blockchain_type.lower() == helper.TYPE_ETHEREUM:
                eth_ips = [replica["private_ip"] for replica in replica_dict[helper.TYPE_ETHEREUM]]
                if eth_ips:
                    log.debug("Collecting support bundle from following ethereum nodes: {}".format(eth_ips))
                    helper.create_concord_support_bundle(eth_ips, helper.TYPE_ETHEREUM, log_dir)
            elif blockchain_type.lower() == helper.TYPE_DAML:
                committers = [replica["private_ip"] for replica in replica_dict[helper.TYPE_DAML_COMMITTER]]
                if committers:
                    log.debug("Collecting support bundle from following daml committer nodes: {}".format(committers))
                    helper.create_concord_support_bundle(committers, helper.TYPE_DAML_COMMITTER, log_dir)
                participants = [replica["private_ip"] for replica in replica_dict[helper.TYPE_DAML_PARTICIPANT]]
                if participants:
                    log.debug("Collecting support bundle from following daml participant nodes: {}".format(participants))
                    helper.create_concord_support_bundle(participants, helper.TYPE_DAML_PARTICIPANT, log_dir)
            else:
                raise NotImplementedError("Support bundle creation not implemented for blockchain type: {}"
                                          .format(blockchain_type))

    except Exception as e:
        log.error("Exception while creating support bundle: {}".format(e))


@pytest.fixture(scope="module")
@describe("fixture; product")
def fxProduct(request, fxHermesRunSettings):
   '''
   An fxProduct provides a launched instance of the product
   to the tests being run.
   '''

   if not fxHermesRunSettings["hermesCmdlineArgs"].noLaunch:
      logDir = os.path.join(fxHermesRunSettings["hermesTestLogDir"], "fxBlockchain")
      if fxHermesRunSettings["hermesCmdlineArgs"].replicasConfig:
         all_replicas = helper.parseReplicasConfig(
            fxHermesRunSettings["hermesCmdlineArgs"].replicasConfig)

      endpoint_hosts = ["localhost"]
      try:
         productType = getattr(request.module, "productType",
                               helper.TYPE_ETHEREUM)

         deploymentService = fxHermesRunSettings["hermesCmdlineArgs"].deploymentService.lower()
         deploymentServiceIsRemote = True not in (host in deploymentService for host in ["localhost", "127.0.0.1"])

         if productType == helper.TYPE_DAML:
            credentials = \
               fxHermesRunSettings["hermesUserConfig"]["persephoneTests"][
                  "provisioningService"]["concordNode"]

            if fxHermesRunSettings["hermesCmdlineArgs"].replicasConfig:
               endpoint_hosts = all_replicas["daml_participant"]
            elif fxHermesRunSettings["hermesCmdlineArgs"].damlParticipantIP:
               endpoint_hosts = fxHermesRunSettings[
                  "hermesCmdlineArgs"].damlParticipantIP.split(",")

            endpoint_port = 6861
            for daml_participant_ip in endpoint_hosts:
               if daml_participant_ip != 'localhost':
                  endpoint_port = helper.FORWARDED_DAML_LEDGER_API_ENDPOINT_PORT
                  setUpPortForwarding(daml_participant_ip,
                                      credentials,
                                      productType,
                                      logDir,
                                      src_port=endpoint_port,
                                      dest_port=6865)

            waitForStartupFunction = helper.verify_daml_test_ready
            waitForStartupParams = {
               "endpoint_hosts": endpoint_hosts,
               "endpoint_port": endpoint_port}
            checkProductStatusParams = {
               "endpoint_hosts": endpoint_hosts,
               "endpoint_port": endpoint_port, "max_tries": 1}

         elif productType == helper.TYPE_TEE:
            waitForStartupFunction = helper.verify_connectivity
            waitForStartupParams = {"ip": "localhost", "port": 50051}
            checkProductStatusParams = {"ip": "localhost", "port": 50051, "max_tries": 1}

         elif productType == helper.TYPE_CHESSPLUS:
            waitForStartupFunction = helper.verify_chessplus_test_ready
            waitForStartupParams = {}
            checkProductStatusParams = {"max_tries": 1}

         elif productType == helper.TYPE_NO_VERIFY or deploymentServiceIsRemote:
            waitForStartupFunction = helper.no_blockchain_readiness_verification_required
            waitForStartupParams = {}
            checkProductStatusParams = {"max_tries": 1}

         else:
            waitForStartupFunction = None
            waitForStartupParams = {}
            checkProductStatusParams = {"retries": 1}

         # Run migration generation script before starting the product
         log.info("Generating Helen DB migration")
         migrationFile = fxHermesRunSettings["hermesCmdlineArgs"].migrationFile
         if migration.build_migrations(fxHermesRunSettings["hermesZoneConfig"],
                                       fxHermesRunSettings["hermesCmdlineArgs"].blockchainLocation, migrationFile):
            log.info("Helen DB migration generated successfully in {}".format(migrationFile))

         product = Product(fxHermesRunSettings["hermesCmdlineArgs"],
                           fxHermesRunSettings["hermesUserConfig"],
                           waitForStartupFunction=waitForStartupFunction,
                           waitForStartupParams=waitForStartupParams,
                           checkProductStatusParams=checkProductStatusParams)
         product.launchProduct()
         # Instance of product, in case someone wants to access it
         return ProductFixture(product=product)

      except Exception as e:
         log.error("The product did not start.")
         raise
   # Eventually:
   # yield (which allows the tests to run),
   # then include steps to stop the product.


@pytest.fixture(scope="module")
@describe("fixture; blockchain")
def fxBlockchain(request, fxHermesRunSettings, fxProduct):
   '''
   This module level fixture returns a BlockchainFixture namedtuple.
   If --blockchainLocation was set to sddc or onprem on the command line, Helen will be invoked
   to create a consortium, then deploy a blockchain.
   Otherwise, the default consortium and blockchain pre-added to Helen for R&D, will be returned.
   The accepted parameter, "request", is an internal PyTest name and must be that.  It contains
   information about the PyTest invocation.

   WARNING: The blockchain "replicas" field is an array of IPs if passing in replicasConfig,
            but an array of objects if a blockchain is deployed.
   '''
   blockchainId = None
   conId = None
   replicas = None
   clientNodes = None
   hermesData = fxHermesRunSettings
   logDir = os.path.join(hermesData["hermesTestLogDir"], "fxBlockchain")

   if not auth.tokens[auth.CUSTOM_ORG]:
       auth.readUsersFromConfig(fxHermesRunSettings["hermesUserConfig"])

   devAdminRequest = Request(logDir,
                             "fxBlockchain",
                             hermesData["hermesCmdlineArgs"].reverseProxyApiBaseUrl,
                             hermesData["hermesUserConfig"],
                             auth.internal_admin)

   if auth.CUSTOM_BLOCKCHAIN in hermesData["hermesUserConfig"]["product"] and \
      hermesData["hermesUserConfig"]["product"][auth.CUSTOM_BLOCKCHAIN]:
      blockchainId, conId = getExistingBlockchainDetails(logDir, hermesData)
   elif hermesData["hermesCmdlineArgs"].blockchainLocation in \
        [helper.LOCATION_SDDC, helper.LOCATION_ONPREM]:
      log.warning("Some test suites do not work with remote deployments yet.")
      blockchainId, conId, replicas, clientNodes = deployToSddc(logDir, hermesData,
                                                   hermesData["hermesCmdlineArgs"].blockchainLocation)
   elif not hermesData["hermesCmdlineArgs"].replicasConfig and len(devAdminRequest.getBlockchains()) > 0:
      # Hermes was not told to deloy a new blockchain, and there is one.  That means
      # we are using the default built in test blockchain.
      # TODO: Create a hermes consortium and add the Hermes org to it, here,
      #       so that all test cases are run as a non-admin.
      blockchain = devAdminRequest.getBlockchains()[0]
      blockchainId = blockchain["id"]
      conId = blockchain["consortium_id"]
   elif hermesData["hermesCmdlineArgs"].replicasConfig:
      # Hermes was told to use a passed in blockchain
      replicas = helper.parseReplicasConfig(hermesData["hermesCmdlineArgs"].replicasConfig)
      blockchainId = None
      conId = None
   else:
      # The product was started with no blockchains.
      blockchainId = None
      conId = None

   return BlockchainFixture(blockchainId=blockchainId, consortiumId=conId, replicas=replicas, clientNodes=clientNodes)


@pytest.fixture(scope="module")
@describe("fixture; Helen with initial org registered")
def fxInitializeOrgs(request, fxHermesRunSettings):
   '''
   Inserts some orgs used for testing into the local Helen database.  This is needed, for example,
   when adding an org to a consortium.  Helen needs to know about the org first, so we
   need to do something to generate a record about it.
   '''
   hermesData = fxHermesRunSettings
   request = Request(hermesData["hermesTestLogDir"],
                     "initializeOrgs",
                     hermesData["hermesCmdlineArgs"].reverseProxyApiBaseUrl,
                     hermesData["hermesUserConfig"])
   tokenDescriptors = [
       {
           "org": "hermes_org1",
           "user": "vmbc_test_con_admin",
           "role": auth.ROLE_CON_ADMIN
       },
       {
           "org": "hermes_org0",
           "user": "vmbc_test_con_admin",
           "role": auth.ROLE_CON_ADMIN
       }
   ]

   for tokenDescriptor in tokenDescriptors:
       req = request.newWithToken(tokenDescriptor)
       req.getBlockchains()


@pytest.fixture(scope="module")
@describe("fixture; blockchain and run settings")
def fxNodeInterruption(request, fxBlockchain, fxHermesRunSettings):
   committers = blockchain_ops.committers_of(fxBlockchain)
   participants = blockchain_ops.participants_of(fxBlockchain)

   all_nodes = []
   for blockchain_type, ips in fxBlockchain.replicas.items():
      all_nodes = all_nodes + ips

   log.info("Committers: {}".format(committers))
   log.info("Participants: {}".format(participants))
   vm_handles = infra.fetch_vm_handles(all_nodes)
   log.debug("vm handles: {}".format(vm_handles))
   fxHermesRunSettings["hermesCmdlineArgs"].vm_handles = vm_handles

   node_interruption_helper.verify_node_interruption_testing_readiness(
      fxHermesRunSettings["hermesCmdlineArgs"])


@pytest.fixture
@describe("fixture; authenticated API connection to Helen")
def fxConnection(request, fxBlockchain, fxHermesRunSettings):
   '''
   This returns a basic fixture containing a Hermes Request object,
   and RPC object.
   The accepted parameter, "request", is an internal PyTest name and must be that.
   '''
   hermesData = fxHermesRunSettings
   longName = os.environ.get('PYTEST_CURRENT_TEST')
   shortName = longName[longName.rindex(":")+1:longName.rindex(" ")]

   if not auth.tokens[auth.CUSTOM_ORG]:
       auth.readUsersFromConfig(fxHermesRunSettings["hermesUserConfig"])

   tokenDescriptor = getTokenDescriptor(hermesData)

   # We don't do this earlier because reverseProxy... is used to monitor for
   # service startup.
   if hermesData["hermesCmdlineArgs"].deploymentService.lower() == auth.SERVICE_STAGING:
      hermesData["hermesCmdlineArgs"].reverseProxyApiBaseUrl = auth.SERVICE_STAGING

   request = Request(hermesData["hermesTestLogDir"],
                     shortName,
                     hermesData["hermesCmdlineArgs"].reverseProxyApiBaseUrl,
                     hermesData["hermesUserConfig"],
                     tokenDescriptor=tokenDescriptor,
                     service=hermesData["hermesCmdlineArgs"].deploymentService)

   if fxBlockchain.blockchainId and \
      hermesData["hermesCmdlineArgs"].blockchainType == helper.TYPE_ETHEREUM:

      ethrpcUrl = eth_helper.getEthrpcApiUrl(request, fxBlockchain.blockchainId)
      rpc = RPC(request.logDir,
                request.testName,
                ethrpcUrl,
                hermesData["hermesUserConfig"],
                tokenDescriptor=tokenDescriptor)
   else:
       rpc = None

   log.debug("request {}".format(request))
   return ConnectionFixture(request=request, rpc=rpc)
