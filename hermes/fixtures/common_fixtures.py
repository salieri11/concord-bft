#################################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#################################################################################
import collections
import json
import logging
import os
import pytest
import time
import types
from urllib.parse import urlparse
from util.product import Product
from rest.request import Request
from rpc.rpc_call import RPC
from util import auth, helper, infra, hermes_logging, numbers_strings
from util.daml import daml_helper
from util.blockchain import eth as eth_helper
import util.generate_zones_migration as migration

log = hermes_logging.getMainLogger()
ConnectionFixture = collections.namedtuple("ConnectionFixture", "request, rpc")
BlockchainFixture = collections.namedtuple("BlockchainFixture", "blockchainId, consortiumId, replicas")

def retrieveCustomCmdlineData(pytestRequest):
    '''
    Given a PyTest fixture's request object, returns a dictionary of various
    pieces of Hermes info that has been passed to PyTest via custom PyTest
    command line parameters.
    cmdlineArgs: The argparse object containing arguments passed to Hermes.
    userConfig: The dictionary containing the contents of user_config.json.
    logDir: The log directory path, as a string.
    '''
    cmdlineArgsDict = json.loads(pytestRequest.config.getoption("--hermesCmdlineArgs"))
    cmdlineArgsObject = types.SimpleNamespace(**cmdlineArgsDict)
    userConfig = json.loads(pytestRequest.config.getoption("--hermesUserConfig"))
    logDir = pytestRequest.config.getoption("--hermesTestLogDir")
    supportBundleFile = pytestRequest.config.getoption("--supportBundleFile")

    return {
        "hermesCmdlineArgs": cmdlineArgsObject,
        "hermesUserConfig": userConfig,
        "hermesTestLogDir": logDir,
        "supportBundleFile": supportBundleFile
    }


# TODO: refactor this method to make it generic
def setUpPortForwarding(url, creds, blockchainType, logDir, timeout=600,):
   '''
   Given a url and credentials, set up port forwarding.
   The VMs should be ready in two minutes; default timeout is 2.5 min just
   in case.
   '''
   urlObject = urlparse(url)
   host = urlObject.hostname
   log.info("Setting up port forwarding on deployed nodes to comply with VMware IT policies.")

   timeTaken = 0
   interval = 10
   portForwardingSuccess = False

   while not portForwardingSuccess and timeTaken < timeout:
      portForwardingSuccess = helper.add_ethrpc_port_forwarding(host, creds["username"], creds["password"])

      if not portForwardingSuccess:
         log.info("Port forwarding setup failed.  The VM is probably still coming up. " \
                  "Trying again in {} seconds. ({} seconds taken so far.)".format(interval, timeTaken))
         time.sleep(interval)
         timeTaken += interval

   if not portForwardingSuccess:
       helper.create_concord_support_bundle([host], blockchainType, logDir)
       raise Exception("Failed to set up port forwarding on deployed nodes. Aborting.")


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


def deployToSddc(logDir, hermesData):
   tokenDescriptor = auth.getTokenDescriptor(auth.ROLE_CON_ADMIN,
                                                  True,
                                                  auth.default_con_admin)
   conAdminRequest = Request(logDir,
                             "fxBlockchain",
                             hermesData["hermesCmdlineArgs"].reverseProxyApiBaseUrl,
                             hermesData["hermesUserConfig"],
                             tokenDescriptor=tokenDescriptor)
   # Use an existing blockchain if present?
   # blockchains = conAdminRequest.getBlockchains()
   suffix = numbers_strings.random_string_generator()
   conName = "con_{}".format(suffix)
   conResponse = conAdminRequest.createConsortium(conName)
   conId = conResponse["consortium_id"]
   zoneIds = []

   for zone in conAdminRequest.getZones():
       log.info(zone)
       zoneIds.append(zone["id"])

   log.info(zoneIds)
   numNodes = int(hermesData["hermesCmdlineArgs"].numReplicas)
   f = (numNodes - 1) / 3
   siteIds = helper.distributeItemsRoundRobin(numNodes, zoneIds)
   blockchain_type = hermesData["hermesCmdlineArgs"].blockchainType
   response = conAdminRequest.createBlockchain(conId,
                                               siteIds,
                                               f,
                                               0,
                                               blockchain_type.upper()
                                               )
   taskId = response["task_id"]
   timeout=60*15
   success, response = helper.waitForTask(conAdminRequest, taskId, timeout=timeout)
   blockchainId = response["resource_id"]
   replica_dict = None

   if success:
      blockchainDetails = conAdminRequest.getBlockchainDetails(blockchainId)
      log.info("Details of the deployed blockchain, in case you need to delete its resources " \
               "manually: {}".format(json.dumps(blockchainDetails, indent=4)))
      credentials = hermesData["hermesUserConfig"]["persephoneTests"]["provisioningService"]["concordNode"]

      log.info("Annotating VMs with deployment context...")
      blockchainDetails["nodes_type"] = infra.PRETTY_TYPE_COMMITTER
      infra.giveDeploymentContext(blockchainDetails)
      replica_details = blockchainDetails["replica_list"]

      ethereum_replicas = []
      daml_committer_replicas = []
      daml_participant_replicas = []

      # Ethereum
      if blockchain_type.lower() == helper.TYPE_ETHEREUM:
         ethereum_replicas = [replica_entry for replica_entry in replica_details]
         success = verify_ethereum_deployment(replica_details, credentials, blockchain_type, logDir)

      # DAML
      elif blockchain_type.lower() == helper.TYPE_DAML:
         daml_committer_replicas = [replica_entry for replica_entry in replica_details]
         success = verify_daml_committers_deployment(replica_details, credentials)

         if success:
            num_participants = int(hermesData["hermesCmdlineArgs"].numParticipants)
            success, daml_participant_replicas = deploy_daml_participants(conAdminRequest, blockchainId, siteIds, credentials,
                                                                          num_participants)

      replica_dict = save_replicas_to_json(blockchain_type, ethereum_replicas, daml_committer_replicas,
                                           daml_participant_replicas, logDir)

   if success:
      log.info("Blockchain deployed successfully")
   else:
      raise Exception("Failed to deploy a new blockchain.")

   return blockchainId, conId, replica_dict


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
            setUpPortForwarding(replica_entry["url"], credentials, blockchain_type, logDir)
            host = replica_entry["ip"]
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
            host = replica_entry["ip"]
            helper.waitForDockerContainers(host, credentials["username"], credentials["password"],
                                           helper.TYPE_DAML_COMMITTER)
            log.info("Committer node {} running successfully".format(host))
        log.info("All committer nodes running successfully")
        success = True
    except Exception as e:
        log.error(e)
        log.error("Failed to deploy DAML committers")

    return success


def deploy_daml_participants(con_admin_request, blockchain_id, site_ids, credentials, num_participants=1):
    """
    Deploys participants in the given DAML blockchain
    :param con_admin_request: REST requests helper object
    :param blockchain_id: The blockchain ID for the blockchain to deploy participants in
    :param site_ids: Zone IDs to deploy in
    :param credentials: Credential information for accessing VMs after deployment
    :param num_participants: Number of participants to deploy (defaults to 1)
    :return: status, List of participant IPs
    """

    log.info("Deploying {} participants for blockchain {}".format(num_participants, blockchain_id))
    zone_ids = helper.distributeItemsRoundRobin(num_participants, site_ids)
    response = con_admin_request.create_participant(blockchain_id, zone_ids)
    task_id = response["task_id"]
    success, response = helper.waitForTask(con_admin_request, task_id)

    participant_replicas = []
    if success:
        username = credentials["username"]
        password = credentials["password"]
        participant_details = con_admin_request.get_participant_details(blockchain_id)
        participant_replicas = [participant_entry for participant_entry in participant_details]
        try:
            # Upload DAR and verification block
            for participant_entry in participant_details:
                public_ip = participant_entry["public_ip"]
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

        except Exception as e:
            log.error(e)
            log.error("Failed to deploy DAML participants")
            success = False

        # Deployment context
        give_deployment_context_participants(blockchain_id, participant_details)

    return success, participant_replicas


def give_deployment_context_participants(blockchain_id, participant_details):
    """
    Give deployment context of participants
    :param blockchain_id: Blockchain ID of the blockchain
    :param participant_details: Participant details
    :return: None
    """

    replica_list = []
    try:
        for participant_entry in participant_details:
            public_ip = participant_entry["public_ip"]
            private_ip = participant_entry["private_ip"]
            vm_handle = infra.findVMByInternalIP(private_ip)
            if vm_handle:
                replica_list.append({
                    "ip": public_ip,
                    "replica_id": vm_handle["replicaId"] if vm_handle is not None else "None"
                })

        log.info("Annotating VMs with deployment context...")
        infra.giveDeploymentContext({
            "id": blockchain_id,
            "consortium_id": "None",
            "blockchain_type": helper.TYPE_DAML,
            "nodes_type": infra.PRETTY_TYPE_PARTICIPANT,
            "replica_list": replica_list
        })

    except Exception as e:
        log.error(e)


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

    replica_file_path = "/tmp/replicas.json"
    replica_log_dir_path = os.path.join(log_dir, 'replicas.json')
    try:
        os.remove(replica_file_path)
        os.remove(replica_log_dir_path)
    except OSError:
        pass

    with open(replica_file_path, 'w') as fp:
        json.dump(replica_dict, fp, indent=2)

    with open(replica_log_dir_path, 'w') as fp:
        json.dump(replica_dict, fp, indent=2)

    log.info("Saved replicas information in {} and {}".format(replica_file_path, replica_log_dir_path))
    return replica_dict


@pytest.fixture(scope="module")
def fxHermesRunSettings(request):
    '''
    Returns a dictionary of information about the Hermes run.
    '''
    return retrieveCustomCmdlineData(request)


@pytest.fixture(scope="module")
def fxProduct(request, fxHermesRunSettings):
   '''
   An fxProduct provides a launched instance of the product
   to the tests being run.
   '''
   if not fxHermesRunSettings["hermesCmdlineArgs"].noLaunch:
      try:
         waitForStartupFunction = None
         waitForStartupParams = {}
         checkProductStatusParams = {"retries": 1}
         productType = getattr(request.module, "productType", helper.TYPE_ETHEREUM)

         if productType == helper.TYPE_DAML:
             waitForStartupFunction = helper.verify_connectivity
             waitForStartupParams = {"ip": "localhost", "port": 6861}
             checkProductStatusParams = {"ip": "localhost", "port": 6861, "max_tries": 1}

         if productType == helper.TYPE_TEE:
             waitForStartupFunction = helper.verify_connectivity
             waitForStartupParams = {"ip": "localhost", "port": 50051}
             checkProductStatusParams = {"ip": "localhost", "port": 50051, "max_tries": 1}

         # Run migration generation script before starting the product
         log.info("Generating Helen DB migration")
         migrationFile = fxHermesRunSettings["hermesCmdlineArgs"].migrationFile
         if migration.build_migrations(fxHermesRunSettings["hermesUserConfig"],
                                    fxHermesRunSettings["hermesCmdlineArgs"].blockchainLocation, migrationFile):
            log.info("Helen DB migration generated successfully in {}".format(migrationFile))

         product = Product(fxHermesRunSettings["hermesCmdlineArgs"],
                           fxHermesRunSettings["hermesUserConfig"],
                           waitForStartupFunction=waitForStartupFunction,
                           waitForStartupParams=waitForStartupParams,
                           checkProductStatusParams=checkProductStatusParams)
         product.launchProduct()

      except Exception as e:
         log.error("The product did not start.")
         raise
   # Eventually:
   # yield (which allows the tests to run),
   # then include steps to stop the product.


@pytest.fixture(scope="module")
def fxBlockchain(request, fxHermesRunSettings, fxProduct):
   '''
   This module level fixture returns a BlockchainFixture namedtuple.
   If --blockchainLocation was set to sddc or onprem on the command line, Helen will be invoked
   to create a consortium, then deploy a blockchain.
   Otherwise, the default consortium and blockchain pre-added to Helen for R&D, will be returned.
   The accepted parameter, "request", is an internal PyTest name and must be that.  It contains
   information about the PyTest invocation.
   '''
   blockchainId = None
   conId = None
   replicas = None
   hermesData = retrieveCustomCmdlineData(request)
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
   elif hermesData["hermesCmdlineArgs"].blockchainLocation == "onprem":
      raise Exception("On prem deployments not supported yet.")
   elif hermesData["hermesCmdlineArgs"].blockchainLocation == "sddc":
      log.warning("Some test suites do not work with SDDC deployments yet.")
      blockchainId, conId, replicas = deployToSddc(logDir, hermesData)
   elif len(devAdminRequest.getBlockchains()) > 0:
      # Hermes was not told to deloy a new blockchain, and there is one.  That means
      # we are using the default built in test blockchain.
      # TODO: Create a hermes consortium and add the Hermes org to it, here,
      #       so that all test cases are run as a non-admin.
      blockchain = devAdminRequest.getBlockchains()[0]
      blockchainId = blockchain["id"]
      conId = blockchain["consortium_id"]
   else:
      # The product was started with no blockchains.
      blockchainId = None
      conId = None

   return BlockchainFixture(blockchainId=blockchainId, consortiumId=conId, replicas=replicas)


@pytest.fixture(scope="module")
def fxInitializeOrgs(request):
   '''
   Inserts some orgs used for testing into the Helen database.  This is needed, for example,
   when adding an org to a consortium.  Helen needs to know about the org first, so we
   need to do something to generate a record about it.
   '''
   hermesData = retrieveCustomCmdlineData(request)
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


@pytest.fixture
def fxConnection(request, fxBlockchain, fxHermesRunSettings):
   '''
   This returns a basic fixture containing a Hermes Request object,
   and RPC object.
   The accepted parameter, "request", is an internal PyTest name and must be that.
   '''
   hermesData = retrieveCustomCmdlineData(request)
   longName = os.environ.get('PYTEST_CURRENT_TEST')
   shortName = longName[longName.rindex(":")+1:longName.rindex(" ")]

   if not auth.tokens[auth.CUSTOM_ORG]:
       auth.readUsersFromConfig(fxHermesRunSettings["hermesUserConfig"])

   # TODO: Always add the hermes org to the built in consortium.
   # (Today, that is only done in certain Helen API tests.)
   tokenDescriptor = None

   if hermesData["hermesCmdlineArgs"].blockchainLocation == "onprem":
      raise Exception("On prem deployments not supported yet.")
   elif hermesData["hermesCmdlineArgs"].blockchainLocation == "sddc":
       tokenDescriptor = auth.getTokenDescriptor(auth.ROLE_CON_ADMIN,
                                                      True,
                                                      auth.default_con_admin)
   else:
       tokenDescriptor = auth.getTokenDescriptor(auth.ROLE_CON_ADMIN,
                                                      True,
                                                      auth.internal_admin)

   request = Request(hermesData["hermesTestLogDir"],
                     shortName,
                     hermesData["hermesCmdlineArgs"].reverseProxyApiBaseUrl,
                     hermesData["hermesUserConfig"],
                     tokenDescriptor=tokenDescriptor)

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

   return ConnectionFixture(request=request, rpc=rpc)
