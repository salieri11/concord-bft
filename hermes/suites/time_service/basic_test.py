#########################################################################
# Copyright 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# "Basic" Time Service tests (i.e. the happy-path, non-evil ones)
#########################################################################
import collections
import difflib
import inspect
import json
import logging
import os
import pickle
import pytest
import queue
import re
import sys
import time
from urllib.parse import urlparse
from uuid import UUID

from suites import test_suite
from rest.request import Request
from rpc.rpc_call import RPC
import util.numbers_strings

log = logging.getLogger(__name__)

ConnectionFixture = collections.namedtuple("ConnectionFixture", "request, rpc")
BlockchainFixture = collections.namedtuple("BlockchainFixture", "blockchainId, consortiumId")

# Ideally this would be a fixture.  However, fixtures and
# parametrize decorations don't play well together.  (There
# are threads about it.) So just brute force run this code
# and shove the info into global variables.  :(
with open("pickled_time_service_basic_tests", "rb") as f:
   suiteObject = pickle.load(f)


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
   logDir = os.path.join(suiteObject._testLogDir, "fxBlockchain")
   request = Request(logDir,
                     "fxBlockchain",
                     suiteObject.reverseProxyApiBaseUrl,
                     suiteObject._userConfig)
   if suiteObject._args.deployNewBlockchain:
      # CSP integration: Won't need to involve an org.
      suffix = suiteObject.random_string_generator()
      orgName = "org_{}".format(suffix)
      conName = "con_{}".format(suffix)
      blockchainId, conId = suiteObject.product.deployBlockchain(request, conName, orgName)
   else:
      blockchain = request.getBlockchains()[0]
      blockchainId = blockchain["id"]
      conId = blockchain["consortium_id"]

   return BlockchainFixture(blockchainId=blockchainId, consortiumId=conId)


@pytest.fixture
def fxConnection(request, fxBlockchain):
   '''
   This returns a basic fixture containing a Hermes Request object,
   and RPC object.
   The accepted parameter, "request", is an internal PyTest name and must be that.
   '''
   longName = os.environ.get('PYTEST_CURRENT_TEST')
   shortName = longName[longName.rindex(":")+1:longName.rindex(" ")]
   testLogDir = os.path.join(suiteObject._testLogDir, shortName)
   request = Request(testLogDir,
                     shortName,
                     suiteObject.reverseProxyApiBaseUrl,
                     suiteObject._userConfig)

   ethrpcNode = getAnEthrpcNode(request, fxBlockchain.blockchainId)
   ethrpcUrl = suiteObject.product.getUrlFromEthrpcNode(ethrpcNode)
   rpc = RPC(request.logDir,
             request.testName,
             ethrpcUrl,
             suiteObject._userConfig)

   return ConnectionFixture(request=request, rpc=rpc)


def getAnEthrpcNode(request, blockchainId):
   '''
   Return the first ethrpc node for the given blockchain.
   '''
   members = suiteObject.product.getEthrpcNodes(request, blockchainId)

   log.info("getAnEthrpcNode found members: {}".format(members))

   if members:
      return members[0]
   else:
      raise Exception("getAnEthrpcNode could not get an ethrpc node.")

def skip_if_disabled(output):
   '''
   If we received an error saying the time service is disabled, skip the rest of the test.
   '''
   if re.findall("error_response", output):
      if re.findall("Time service is disabled", output):
         pytest.skip("Time service is disabled.")

@pytest.mark.smoke
def test_cli_get(fxConnection):
   '''
   Attempt to use conc_time to get the current blockchain
   time. Passes if there is no error_response, and there is a
   time_response that contains a summary field filled with digits.
   '''
   concordContainer = suiteObject.product.get_concord_container_name(1)
   output = suiteObject.product.exec_in_concord_container(concordContainer, "./conc_time -g")

   skip_if_disabled(output)

   assert re.findall("time_response", output), "No time response found: {}".format(output)
   assert re.findall("summary: ([:digit:]*)", output), "Summary with time not found: {}".format(output)
