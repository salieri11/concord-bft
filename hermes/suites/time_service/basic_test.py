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
   # text format uses snake_case, json format uses camelCase
   if re.findall("error_response", output) or re.findall("errorResponse", output):
      if re.findall("Time service is disabled", output):
         pytest.skip("Time service is disabled.")


@pytest.mark.smoke
def test_cli_get():
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


def extract_samples_from_response(output):
   '''
   Dig through conc_time output to find a TimeResponse message, and
   extract samples from it. The conc_time script should have been
   exected with `-l` and `-o json` flags.
   '''
   responseText = re.findall("Received response: (.*)", output)[0]
   responseJson = json.loads(responseText)
   sampleList = responseJson["timeResponse"]["sample"]

   sampleMap = {}
   for sample in sampleList:
      sampleMap[sample["source"]] = sample["time"]

   return sampleMap


def test_low_load_updates():
   '''
   Try to observe time updates happening with no input being
   generated. It is assumed that all sources are configured to publish
   regularly, without transactions being sent to the system
   (i.e. `time_pusher_period_ms` is greater than zero for all
   sources). Note: if anything else is submitting transactions to the
   system, what is really being tested is that updates to time service
   happen at all, and not necessarily that the low-load system is
   functioning.
   '''
   concordContainer = suiteObject.product.get_concord_container_name(1)
   output = suiteObject.product.exec_in_concord_container(concordContainer,
                                                          "./conc_time -l -o json")

   skip_if_disabled(output)

   startTimes = extract_samples_from_response(output)
   assert startTimes.keys(), "No sources found"

   # What we expect time_pusher_period_ms to be, but in seconds.
   expectedUpdatePeriodSec = 1

   # How many times we're going to query before giving up. This
   # shouldn't need to be larger than 2, if we sleep for
   # expectedUpdatePeriodSec between attempts, but using 3 for now for
   # extra stability.
   maxAttempts = 3

   # How often we want to remind the log that we're waiting, in
   # attempts. This is really only applicable if maxAttempts >
   # logPeriod, but including it now even with a smaller maxAttempts,
   # to fight spew before debugging beings.
   logPeriod = 5

   for attempt in range(1,maxAttempts+1):
      output = suiteObject.product.exec_in_concord_container(concordContainer,
                                                             "./conc_time -l -o json")
      newTimes = extract_samples_from_response(output)
      for k,v in newTimes.items():
         if startTimes[k] == newTimes[k]:
            if (attempt % logPeriod) == 1:
               log.info("Waiting for time samples to update... (attempt {})".format(attempt))
            time.sleep(expectedUpdatePeriodSec)
            break
      else:
         # if the loop exited without break-ing, we don't need to read again
         break

   assert len(newTimes.keys()) == len(startTimes.keys()), "All sources should be present in the update"
   for k,v in newTimes.items():
      assert startTimes[k] != newTimes[k], "All sources should have updated"


def extract_time_summary_response(output):
   '''
   Dig through conc_time output to find a TimeResponse message, and
   extract the summary from it. The conc_time script should have been
   exected with `-g` and `-o json` flags.
   '''
   responseText = re.findall("Received response: (.*)", output)[0]
   responseJson = json.loads(responseText)
   return int(responseJson["timeResponse"]["summary"])


def test_time_service_in_ethereum_block(fxConnection):
   '''
   Test that the value stored as the timestamp in an ethereum block
   is [likely] sourced from the time service.
   '''
   concordContainer = suiteObject.product.get_concord_container_name(1)
   output = suiteObject.product.exec_in_concord_container(concordContainer,
                                                          "./conc_time -g -o json")

   skip_if_disabled(output)

   # Since low-load time updates may move the time service forward at
   # any moment, we check to make sure that a block's timestamp is
   # between the time service's state before and its state after the
   # block was created.
   preTxTime = extract_time_summary_response(output)
   caller = "0x1111111111111111111111111111111111111111" # fake address
   data = suiteObject._addCodePrefix("f3")
   receipt = fxConnection.rpc.getTransactionReceipt(
      fxConnection.rpc.sendTransaction(caller, data))
   output = suiteObject.product.exec_in_concord_container(concordContainer,
                                                          "./conc_time -g -o json")
   postTxTime = extract_time_summary_response(output)

   block = fxConnection.rpc.getBlockByHash(receipt["blockHash"])
   blockTime = int(block["timestamp"], 16)

   # "//1000" = the time service reports in milliseconds, but ethereum
   # block timestamps are in seconds
   assert preTxTime // 1000 <= blockTime, "Block timestamp should be no earlier than pre-tx check"
   assert blockTime <= postTxTime // 1000, "Block timestamp should be no later than post-tx check"


def test_time_service_in_ethereum_code(fxConnection):
   '''
   Test that the time returned from executing a TIMESTAMP ethereum
   opcode is [likely] sourced from the time service.
   '''
   concordContainer = suiteObject.product.get_concord_container_name(1)
   output = suiteObject.product.exec_in_concord_container(concordContainer,
                                                          "./conc_time -g -o json")

   skip_if_disabled(output)

   # This contract just returns the ethereum timestamp when called.
   bytecode = "4260005260206000f3"
   #           ^ ^   ^ ^   ^   ^-- RETURN
   #           | |   | |   +-- PUSH1 0x00 (where the timestamp is stored)
   #           | |   | +-- PUSH1 0x20 (length of the timestamp)
   #           | |   +-- MSTORE (save the timestamp in memory)
   #           | +-- PUSH1 0x00 (where to store the timestamp)
   #           +-- TIMESTAMP
   caller = "0x1111111111111111111111111111111111111111" # fake address
   receipt = fxConnection.rpc.getTransactionReceipt(
      fxConnection.rpc.sendTransaction(caller, suiteObject._addCodePrefix(bytecode)))
   address = receipt["contractAddress"]

   # Since low-load time updates may move the time service forward at
   # any moment, we check to make sure that an execution's timestamp is
   # between the time service's state before and its state after the
   # call was executed.
   preTxTime = extract_time_summary_response(output)
   contractTime = int(fxConnection.rpc.callContract(address), 16)
   output = suiteObject.product.exec_in_concord_container(concordContainer,
                                                          "./conc_time -g -o json")
   postTxTime = extract_time_summary_response(output)

   # "//1000" = the time service reports in milliseconds, but ethereum
   # timestamps are in seconds
   assert preTxTime // 1000 <= contractTime, "Opcode time should be no earlier than pre-call check"
   assert contractTime <= postTxTime // 1000, "Opcode time should be no later than post-call check"
