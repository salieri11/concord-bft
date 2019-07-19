#################################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#################################################################################
import collections
import json
import logging
import os
import pytest

from rest.request import Request
from rpc.rpc_call import RPC
import util.blockchain.eth
import util.helper
import util.product

log = logging.getLogger(__name__)
ConnectionFixture = collections.namedtuple("ConnectionFixture", "request, rpc")
BlockchainFixture = collections.namedtuple("BlockchainFixture", "blockchainId, consortiumId")

def retrieveCustomCmdlineData(pytestRequest):
    '''
    Given a PyTest fixture's request object, returns a dictionary of various
    pieces of Hermes info that has been passed to PyTest via custom PyTest
    command line parameters.
    '''
    return {
        "hermesCmdlineArgs": json.loads(pytestRequest.config.getoption("--hermesCmdlineArgs")),
        "hermesUserConfig": json.loads(pytestRequest.config.getoption("--hermesUserConfig")),
        "hermesTestLogDir": pytestRequest.config.getoption("--hermesTestLogDir"),
    }


@pytest.fixture(scope="module")
def fxHermesRunSettings(request):
    '''
    Returne a dictionary of information about the Hermes run.
    '''
    return retrieveCustomCmdlineData(request)


@pytest.fixture(scope="module")
def fxBlockchain(request):
   '''
   This module level fixture returns a BlockchainFixture namedtuple.
   If --deployNewBlockchain was set on the command line, Helen will be invoked to create a
   consortium, then deploy a blockchain onto an SDDC via Persephone.
   Otherwise, the default consortium pre-added to Helen, and the blockchain running in concord
   nodes on the local system via docker-compose, will be returned.
   The accepted parameter, "request", is an internal PyTest name and must be that.
   '''
   hermesData = retrieveCustomCmdlineData(request)
   logDir = os.path.join(hermesData["hermesTestLogDir"], "fxBlockchain")
   blockchainRequest = Request(logDir,
                               "fxBlockchain",
                               hermesData["hermesCmdlineArgs"]["reverseProxyApiBaseUrl"],
                               hermesData["hermesUserConfig"])
   if hermesData["hermesCmdlineArgs"]["deployNewBlockchain"]:
      # CSP integration: Won't need to involve an org.
      suffix = util.numbers_strings.random_string_generator()
      orgName = "org_{}".format(suffix)
      conName = "con_{}".format(suffix)
      prod = Product(hermesData["hermesCmdlineArgs"],
                     hermesData["userConfig"],
                     hermesData["hermesCmdlineArgs"]["suite"])
      blockchainId, conId = prod.deployBlockchain(blockchainRequest, conName, orgName)
   elif len(blockchainRequest.getBlockchains()) > 0:
      # We're using the default built in test blockchain.
      blockchain = blockchainRequest.getBlockchains()[0]
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
                     hermesData["hermesCmdlineArgs"]["reverseProxyApiBaseUrl"],
                     hermesData["hermesUserConfig"])
   tokenDescriptors = [
       {
           "org": "hermes_org1",
           "user": "vmbc_test_con_admin",
           "role": "consortium_admin"
       },
       {
           "org": "hermes_org0",
           "user": "vmbc_test_con_admin",
           "role": "consortium_admin"
       }
   ]

   for tokenDescriptor in tokenDescriptors:
       req = request.newWithToken(tokenDescriptor)
       req.getBlockchains()


@pytest.fixture
def fxConnection(request, fxBlockchain):
   '''
   This returns a basic fixture containing a Hermes Request object,
   and RPC object.
   The accepted parameter, "request", is an internal PyTest name and must be that.
   '''
   hermesData = retrieveCustomCmdlineData(request)
   longName = os.environ.get('PYTEST_CURRENT_TEST')
   shortName = longName[longName.rindex(":")+1:longName.rindex(" ")]

   request = Request(hermesData["hermesTestLogDir"],
                     shortName,
                     hermesData["hermesCmdlineArgs"]["reverseProxyApiBaseUrl"],
                     hermesData["hermesUserConfig"])

   if fxBlockchain.blockchainId:
      ethrpcNode = util.blockchain.eth.getAnEthrpcNode(request, fxBlockchain.blockchainId)
      ethrpcUrl = util.blockchain.eth.getUrlFromEthrpcNode(ethrpcNode)
      rpc = RPC(request.logDir,
                request.testName,
                ethrpcUrl,
                hermesData["hermesUserConfig"])
   else:
       rpc = None

   return ConnectionFixture(request=request, rpc=rpc)
