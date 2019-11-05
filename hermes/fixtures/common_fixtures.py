#################################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#################################################################################
import collections
import json
import logging
import os
import pytest
import time
from urllib.parse import urlparse

from rest.request import Request
from rpc.rpc_call import RPC
import types
import util

log = logging.getLogger(__name__)
ConnectionFixture = collections.namedtuple("ConnectionFixture", "request, rpc")
BlockchainFixture = collections.namedtuple("BlockchainFixture", "blockchainId, consortiumId")

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

    return {
        "hermesCmdlineArgs": cmdlineArgsObject,
        "hermesUserConfig": userConfig,
        "hermesTestLogDir": logDir
    }


def setUpPortForwarding(url, creds, containerList, logDir, timeout=300,):
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
      portForwardingSuccess = util.helper.add_ethrpc_port_forwarding(host, creds["username"], creds["password"])

      if not portForwardingSuccess:
         log.info("Port forwarding setup failed.  The VM is probably still coming up. " \
                  "Trying again in {} seconds. ({} seconds taken so far.)".format(interval, timeTaken))
         time.sleep(interval)
         timeTaken += interval

   if not portForwardingSuccess:
       util.helper.create_concord_support_bundle([host], creds["username"], creds["password"],
                                                 containerList, logDir)
       raise Exception("Failed to set up port forwarding on deployed nodes. Aborting.")


def getExistingBlockchainDetails(logDir, hermesData):
   '''
   Return the blockchain passed in, if any.
   '''
   blockchainId = hermesData["hermesUserConfig"]["product"][util.auth.CUSTOM_BLOCKCHAIN]
   tokenDescriptor = util.auth.getTokenDescriptor(util.auth.ROLE_CON_ADMIN,
                                                  True,
                                                  util.auth.default_con_admin)
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
   tokenDescriptor = util.auth.getTokenDescriptor(util.auth.ROLE_CON_ADMIN,
                                                  True,
                                                  util.auth.default_con_admin)
   conAdminRequest = Request(logDir,
                             "fxBlockchain",
                             hermesData["hermesCmdlineArgs"].reverseProxyApiBaseUrl,
                             hermesData["hermesUserConfig"],
                             tokenDescriptor=tokenDescriptor)
   # Use an existing blockchain if present?
   # blockchains = conAdminRequest.getBlockchains()
   suffix = util.numbers_strings.random_string_generator()
   conName = "con_{}".format(suffix)
   conResponse = conAdminRequest.createConsortium(conName)
   conId = conResponse["consortium_id"]
   zoneIds = []

   for zone in conAdminRequest.getZones():
       log.info(zone)
       zoneIds.append(zone["id"])

   log.info(zoneIds)
   siteIds = util.helper.distributeItemsRoundRobin(4, zoneIds)
   response = conAdminRequest.createBlockchain(conId,
                                               siteIds,
                                               blockchainType=hermesData["hermesCmdlineArgs"].blockchainType.upper())
   taskId = response["task_id"]
   timeout=60*15
   success, response = util.helper.waitForTask(conAdminRequest, taskId, timeout=timeout)

   if success:
      blockchainId = response["resource_id"]
      blockchainDetails = conAdminRequest.getBlockchainDetails(blockchainId)
      log.info("Details of the deployed blockchain, in case you need to delete its resources " \
               "manually: {}".format(json.dumps(blockchainDetails, indent=4)))

      if hermesData["hermesCmdlineArgs"].blockchainType.lower() == util.helper.TYPE_ETHEREUM:
         for replicaDetails in blockchainDetails["node_list"]:
             credentials = hermesData["hermesUserConfig"]["persephoneTests"]["provisioningService"]["concordNode"]
             blockchainType = hermesData["hermesCmdlineArgs"].blockchainType
             containerList = list(hermesData["hermesUserConfig"]["persephoneTests"]["modelService"]["defaults"]
                                  ["deployment_components"][blockchainType].values())
             setUpPortForwarding(replicaDetails["url"], credentials, containerList, logDir)

   else:
      raise Exception("Failed to deploy a new blockchain.")

   return blockchainId, conId


@pytest.fixture(scope="module")
def fxHermesRunSettings(request):
    '''
    Returne a dictionary of information about the Hermes run.
    '''
    return retrieveCustomCmdlineData(request)


@pytest.fixture(scope="module")
def fxBlockchain(request, fxHermesRunSettings):
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
   hermesData = retrieveCustomCmdlineData(request)
   logDir = os.path.join(hermesData["hermesTestLogDir"], "fxBlockchain")

   if not util.auth.tokens[util.auth.CUSTOM_ORG]:
       util.auth.readUsersFromConfig(fxHermesRunSettings["hermesUserConfig"])

   devAdminRequest = Request(logDir,
                             "fxBlockchain",
                             hermesData["hermesCmdlineArgs"].reverseProxyApiBaseUrl,
                             hermesData["hermesUserConfig"],
                             util.auth.internal_admin)

   if util.auth.CUSTOM_BLOCKCHAIN in hermesData["hermesUserConfig"]["product"] and \
      hermesData["hermesUserConfig"]["product"][util.auth.CUSTOM_BLOCKCHAIN]:
      blockchainId, conId = getExistingBlockchainDetails(logDir, hermesData)
   elif hermesData["hermesCmdlineArgs"].blockchainLocation == "onprem":
      raise Exception("On prem deployments not supported yet.")
   elif hermesData["hermesCmdlineArgs"].blockchainLocation == "sddc":
      log.warn("Some test suites do not work with SDDC deployments yet.")
      blockchainId, conId = deployToSddc(logDir, hermesData)
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

   return BlockchainFixture(blockchainId=blockchainId, consortiumId=conId)


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
           "role": util.auth.ROLE_CON_ADMIN
       },
       {
           "org": "hermes_org0",
           "user": "vmbc_test_con_admin",
           "role": util.auth.ROLE_CON_ADMIN
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

   if not util.auth.tokens[util.auth.CUSTOM_ORG]:
       util.auth.readUsersFromConfig(fxHermesRunSettings["hermesUserConfig"])

   # TODO: Always add the hermes org to the built in consortium.
   # (Today, that is only done in certain Helen API tests.)
   tokenDescriptor = None

   if hermesData["hermesCmdlineArgs"].blockchainLocation == "onprem":
      raise Exception("On prem deployments not supported yet.")
   elif hermesData["hermesCmdlineArgs"].blockchainLocation == "sddc":
       tokenDescriptor = util.auth.getTokenDescriptor(util.auth.ROLE_CON_ADMIN,
                                                      True,
                                                      util.auth.default_con_admin)
   else:
       tokenDescriptor = util.auth.getTokenDescriptor(util.auth.ROLE_CON_ADMIN,
                                                      True,
                                                      util.auth.internal_admin)

   request = Request(hermesData["hermesTestLogDir"],
                     shortName,
                     hermesData["hermesCmdlineArgs"].reverseProxyApiBaseUrl,
                     hermesData["hermesUserConfig"],
                     tokenDescriptor=tokenDescriptor)

   if fxBlockchain.blockchainId and \
      hermesData["hermesCmdlineArgs"].blockchainType == util.helper.TYPE_ETHEREUM:

      ethrpcUrl = util.blockchain.eth.getEthrpcApiUrl(request, fxBlockchain.blockchainId)
      rpc = RPC(request.logDir,
                request.testName,
                ethrpcUrl,
                hermesData["hermesUserConfig"],
                tokenDescriptor=tokenDescriptor)
   else:
       rpc = None

   return ConnectionFixture(request=request, rpc=rpc)
