#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import collections
import copy
import difflib
import inspect
import json
import logging
import os
import pickle
import pytest
import queue
import random
import sys
import threading
import time
import types
from urllib.parse import urlparse
from uuid import UUID

from suites import test_suite
from rest.request import Request
from rpc.rpc_call import RPC

from fixtures.common_fixtures import fxBlockchain, fxConnection, fxHermesRunSettings, fxInitializeOrgs, fxProduct
import rest.test_methods
import util.auth
import util.blockchain.eth
import util.helper
import util.numbers_strings
import util.product

# For hermes/lib/persephone, used for saving streamed events and deleting them.
sys.path.append('lib')

log = logging.getLogger(__name__)

# A user.  This will only work until CSP is implemented and
# concord starts requiring signed transactions.
fromUser = "0x1111111111111111111111111111111111111111"

# The compiler version passed to Helen when uploading contracts.
compilerVersion = "v0.5.2+commit.1df8f40c"

# Automation's contract and version IDs are generated with random_string_generator(),
# which creates strings of six uppercase characters and digits.
nonexistantContractId = "aaaaaaaaaaaaaaaaaaaa"
nonexistantVersionId = "bbbbbbbbbbbbbbbbbbbb"
defaultTokenDescriptor = util.auth.getTokenDescriptor(util.auth.ROLE_CON_ADMIN,
                                                      True,
                                                      util.auth.internal_admin)

# These zones are hard coded and added by the test framework until we set up zones
# in the fixture infra.
precreatedZones = ["6adaf48a-9075-4e35-9a71-4ef1fb4ac90f",
                   "623e6f81-5954-4b32-9cdb-eb5d8dd913db"]

def removePrecreatedZones(zones):
   ret = []
   numRemovedZones = 0

   for zone in zones:
      if zone in precreatedZones:
         numRemovedZones += 1
      else:
         ret.append(zone)

   return ret, numRemovedZones


def addBlocksAndSearchForThem(request, blockchainId, rpc, numBlocks, pageSize):
   '''
   Adds numBlocks blocks and searches for them one pageSize
   of blocks at a time.  This calls a method which handles
   the asserts.
   '''
   origBlockNumber = util.blockchain.eth.getLatestBlockNumber(request, blockchainId)
   txResponses = util.blockchain.eth.addBlocks(request, rpc, blockchainId, numBlocks)
   newBlockNumber = util.blockchain.eth.getLatestBlockNumber(request, blockchainId)
   # If time service is running, there may be additional blocks that
   # were not added by addBlocks
   assert newBlockNumber - origBlockNumber >= numBlocks, \
      "Expected new block to have number {}.".format(origBlockNumber + numBlocks)
   verifyBlocksWithPaging(request, blockchainId, txResponses, pageSize)


def verifyBlocksWithPaging(restRequest, blockchainId, txResponses, pageSize=None):
   '''
   Verifies that the transactions in txResponses are all in blockchain blockchainId
   and the fields are correct.  Searches with paging, instead of fetching the block
   directly.
   '''
   assert txResponses, "No transaction responses passed to verifyBlocks()"

   for txResponse in txResponses:
      blockHash = txResponse["blockHash"]
      blockNumber = int(txResponse["blockNumber"], 16)
      blockUrl = "/api/concord/blocks/" + str(blockNumber)
      block = findBlockWithPaging(restRequest, blockchainId, blockHash,
                                  pageSize)
      assert block, "Block with hash {} not found".format(blockHash)
      assert block["number"] == blockNumber, \
         "Block number should be {}".format(blockNumber)
      assert block["url"] == blockUrl, \
         "Block url should be {}".format(blockUrl)

      # The fields returned when specifying one block are different and
      # are tested via the concord/blocks/{index} tests. Just sanity
      # test here.
      blockFromUrl = restRequest.getBlockByUrl(block["url"])
      assert blockFromUrl["hash"] == blockHash, \
         "Block retrived via URL has an incorrect hash"
      assert blockFromUrl["number"] == blockNumber, \
         "Block retrieved via URL has an incorrect number"
      assert isinstance(blockFromUrl["transactions"], list), \
         "'transactions' field is not a list."
      present, missing = util.helper.requireFields(blockFromUrl, ["number", "hash",
                                                                  "parentHash", "nonce",
                                                                  "size", "transactions",
                                                                  "timestamp"])
      assert present, "No '{}' field in block response.".format(missing)


def findBlockWithPaging(restRequest, blockchainId, blockHash, pageSize):
   '''
   Given a request, looks for the block with the given blockHash and returns it.
   Instead of asking for a specific block, requests pages of blocks from Helen.
   - pageSize: The number of blocks in a page.
   Returns None if not found.
   '''
   foundBlock = None
   nextUrl = None
   blockList = restRequest.getBlockList(blockchainId, count=pageSize)
   counter = blockList["blocks"][0]["number"]
   lowestBlockNumber = None

   while counter >= 0 and not foundBlock:
      # The last page might not be a full page.  But all other pages should be
      # the expected length.
      if not blockList["blocks"][0]["number"] == 0:
         assert len(blockList["blocks"]) == pageSize, \
            "Returned page of blocks was not the correct size."

         highestInList = blockList["blocks"][0]["number"]
         lowestInList = blockList["blocks"][pageSize-1]["number"]
         assert highestInList - lowestInList == pageSize - 1, \
            "Difference in the block numbers returned do not match the page size."

      for block in blockList["blocks"]:
         counter -= 1

         if block["hash"] == blockHash:
            foundBlock = block
            break

         if lowestBlockNumber == None:
            lowestBlockNumber = block["number"]
         else:
            lowestBlockNumber = min(lowestBlockNumber, block["number"])

      if blockList["next"] and not foundBlock:
         nextUrl = blockList["next"]
         assert nextUrl == "/api/concord/blocks?latest={}".format(lowestBlockNumber-1), \
            "The nextUrl contained the incorrect block number."
         blockList = restRequest.getBlockList(blockchainId, nextUrl=nextUrl, count=pageSize)

   return foundBlock


def checkInvalidIndex(restRequest, blockchainId, index, messageContents):
   '''
   Check that we get an exception containing messageContents when passing
   the given invalid index to concord/blocks/{index}.
   '''
   exceptionThrown = False
   exception = None
   block = None

   try:
      block = restRequest.getBlockByNumber(blockchainId, index)
   except Exception as ex:
      exceptionThrown = True
      exception = ex

   assert exceptionThrown, \
      "Expected an error when requesting a block with invalid index '{}', instead received block {}.".format(index, block)
   assert messageContents in str(exception), \
      "Incorrect error message for invalid index '{}'.".format(index)


def checkTimestamp(expectedTime, actualTime):
   '''
   Checks that actualTime is near the expectedTime.  The buffer is to
   account for clocks being out of sync on different systems.  We are
   not testing the accuracy of the time; we are testing that the time
   is generally appropriate.
   '''
   buff = 600
   earliestTime = expectedTime - buff
   earliestTimeString = util.numbers_strings.epochToLegible(earliestTime)
   latestTime = expectedTime + buff
   latestTimeString = util.numbers_strings.epochToLegible(latestTime)
   actualTimeString = util.numbers_strings.epochToLegible(actualTime)

   assert actualTime >= earliestTime and actualTime <= latestTime, \
      "Block's timestamp, '{}', was expected to be between '{}' and '{}'. (Converted: " \
      "Block's timestamp, '{}', was expected to be between '{}' and '{}'.)".format(actualTime, \
                                                                earliestTime, latestTime,
                                                                actualTimeString, earliestTimeString,
                                                                latestTimeString)


def verifyContractCreationTx(request, blockchainId, contractCreationTx):
   '''
   Check the fields of a transaction used to create a contract.
   We verify some fields by getting the block the transaction claims to be part of,
   and comparing values with that.
   '''
   # The bytecode changes with Solidity versions.  Just verify the standard first few instructions
   # to verify that the Helen API is working.
   assert contractCreationTx["input"].startswith("0x60806040"), \
      "Input does not appear to be ethereum bytecode."

   block = request.getBlockByNumber(blockchainId, contractCreationTx["block_number"])
   assert block, "Unable to get a block with the transactions block_number."
   assert contractCreationTx["block_hash"] and contractCreationTx["block_hash"] == block["hash"], \
      "The block_hash was not correct."
   assert contractCreationTx["from"] == "0x1111111111111111111111111111111111111111", \
      "The from field was not correct."
   assert contractCreationTx["contract_address"].startswith("0x") and \
      len(contractCreationTx["contract_address"]) == 42, \
      "The value in the contract_address field is not valid"
   # Test with a value?  Maybe use a contract which accepts a value.
   assert contractCreationTx["value"] == "0x0", \
      "The value field is not correct."
   assert isinstance(contractCreationTx["nonce"], int), \
      "Nonce is not an int"
   assert contractCreationTx["hash"] == block["transactions"][0]["hash"], \
      "The hash is not correct."
   assert contractCreationTx["status"] == 1, \
      "The status is not correct."


def verifyContractInvocationTx(request, blockchainId, contractCreationTx,
                               contractInvocationTx):
   '''
   Check the fields of a transaction used to execute a contract.
   We verify some fields by getting the block the transaction claims to be part of.
   We also verify some fields by comparing to fields in transaction which
   created the contract.
   '''
   assert contractInvocationTx["input"] == util.blockchain.eth.helloFunction, \
      "Input field was not correct"

   block = request.getBlockByNumber(blockchainId, contractInvocationTx["block_number"])
   assert contractInvocationTx["block_hash"] == block["hash"], \
      "The block_hash field was not correct"
   assert contractInvocationTx["from"] == "0x1111111111111111111111111111111111111111", \
      "The from field was not correct."
   assert contractInvocationTx["to"] == contractCreationTx["contract_address"], \
      "The to field was not equal to the contract's address."
   # Test with a value?  Maybe use a contract which accepts a value.
   assert contractCreationTx["value"] == "0x0", \
      "The value field is not correct."
   assert contractInvocationTx["nonce"] > contractCreationTx["nonce"], \
      "Nonce is not greater than the contract creation transaction's nonce"
   assert contractInvocationTx["hash"] == block["transactions"][0]["hash"], \
      "The hash is not correct."
   assert contractInvocationTx["status"] == 1, \
      "The status is not correct."


def verifyBlockchainFields(request, blockchain):
   '''
   Given request and blockchain objects, verify the fields of the blockchain object.
   When this test is being run, we don't know how many concord nodes there will
   really be.  So make sure we get something a) sensible that b) is consistent with
   the another API.
   '''
   expectedRpcUrls = []
   log.info("Calling getMemberList for blockchain {}".format(blockchain["id"]))
   members = request.getMemberList(blockchain["id"])
   for member in members:
      expectedRpcUrls.append(member["rpc_url"])

   # There will always be a genesis block.
   block = request.getBlockByNumber(blockchain["id"], 0)
   assert block["hash"], "Unable to retrive block 0 using the returned blockchain ID."
   UUID(blockchain["consortium_id"])

   # For the case of zero nodes, verify that corner case in its own test case.
   assert blockchain["node_list"], "No nodes were returned in the blockchain."

   for node in blockchain["node_list"]:
      UUID(node["node_id"])

      # IP address and possibly port.
      ipFields = node["ip"].split(":")
      assert ipFields[0], "Host field invalid."
      if len(ipFields) > 1:
         int(ipFields[1])

      urlParts = urlparse(node["url"])
      int(urlParts.port)
      assert urlParts.scheme == "https", "The url field is not using https."
      assert urlParts.hostname, "The url field hostname is empty."

      # VB-1006
      # When Helen deploys via Persephone, /api/blockchains/{bid}/concord/members
      # returns structures which have an empty rpc_url field.
      # assert node["url"] in expectedRpcUrls, \
      #    "The url field contained a value not matching one returned by the /concord/members API."
      # expectedRpcUrls.remove(node["url"])

      # TODO: Verify the cert by actually using it.
      # Bug: Persephone doesn't return certs.  See VB-1001.

      assert "zone_id" in node.keys(), "The zone_id field does not exist."

   # VB-1006
   # assert len(expectedRpcUrls) == 0, \
   #    "More nodes were returned by the /concord/members API than were present in the /blockchains node list."


def validateContractFields(testObj, blockchainId, contractId, contractVersion):
   '''
   testObj: The object to check.
   Other fields: Used as expected values.
   '''
   assert testObj["contract_id"] == contractId, "The contract_id field is not correct."
   assert testObj["version"] == contractVersion, "The contract_version field is not correct."

   # VB-848: The blockchain ID should be in this url, but it is not.
   # assert testObj["url"] == "/api/blockchains/" + blockchainId + "/concord/contracts/" + \
   #    contractId + "/versions/" + contractVersion
   assert len(testObj.keys()) == 3


def verifyContractVersionFields(blockchainId, request, rpc, actualDetails, expectedDetails, expectedVersion,
                                testFunction, testFunctionExpectedResult):
   '''
   Verify the details of a specific contract version.
   expectedVersion varies with every test case, so is determined at run time and passed in.
   actualDetails is a dictionary of all of the other contract values such as the
   abi, devdoc, compiler, etc...
   expectedDetails is loaded from a file and compared to actualDetails.
   testFunction and testFunctionExpectedResult are the hex encoded function to verify the
   address and the expected result.
   '''
   contractCallResult = rpc.callContract(actualDetails["address"], data=testFunction)
   assert testFunctionExpectedResult in contractCallResult, "The test function {} returned an expected result without {}".format(testFunction, testFunctionExpectedResult)

   assert actualDetails["version"] == expectedVersion, \
      "Version was {}, expected {}".format(actualDetails["version"], contractVersion)

   # Do a text diff on the rest of the fields.  Remove the items which always differ
   # first.
   del actualDetails["address"]
   del expectedDetails["address"]
   del actualDetails["version"]
   del expectedDetails["version"]

   result = json.dumps(actualDetails, sort_keys=True, indent=2).split("\n")
   expectedResult = json.dumps(expectedDetails, sort_keys=True, indent=2).split("\n")
   diffs = ""

   for line in difflib.unified_diff(result, expectedResult, lineterm=""):
      diffs += line + "\n"

   assert not diffs, "Differences found in details: {}".format(diffs)


def validateBadRequest(result, expectedPath,
                       errorCode="BadRequestException",
                       testErrorMessage = True,
                       errorMessage="Bad request (e.g. missing request body)."):
   '''
   Validates the returned result of a Bad Request error.
   The error code, error message, and status are the same.
   Accepts the result to evaluate and the expected value for "path".
   Check that a valid op_id has been returned
   '''
   log.info("Looking for bad request error in {}".format(result))

   assert "error_code" in result, "Expected a field called 'error_code'"
   assert result["error_code"] == errorCode, "Expected different error code."

   assert "path" in result, "Expected a field called 'path'"
   assert result["path"] == expectedPath, "Expected different path."

   if(testErrorMessage):
       assert "error_message" in result, "Expected a field called 'error_message'"
       assert result["error_message"] == errorMessage, "Expected different error message."
   else:
       log.info("Error message testing is disabled.")

   assert "status" in result, "Expected a field called 'status'"
   assert result["status"] == 400, "Expected HTTP status 400."

   assert "op_id" in result, "Expected a field called 'op_id'"
   # opid is a uuid string
   assert len(result["op_id"]) == 36


def createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM):
   '''
   Returns a zone object, filling in all fields with generic info.
   Does not actually send the zone to Helen.

   '''
   uniqueId = util.numbers_strings.random_string_generator()
   gpsPoint = util.numbers_strings.randomGpsPoint()
   return {
      "type": zoneType,
      "name": "Test Zone {}".format(uniqueId),
      "latitude": str(gpsPoint[0]),
      "longitude": str(gpsPoint[1]),
      "vcenter": {
         "url": "https://{}.com".format(uniqueId),
         "username": "admin@{}.com".format(uniqueId),
         "password": "vcenter_pa$$w0rd4{}".format(uniqueId)
      },
      "network": collections.OrderedDict({
         "name": "Blockchain Network for {}".format(uniqueId),
         "ip_pool": [
            "{}-{}".format(util.numbers_strings.randomIP4Address(),util.numbers_strings.randomIP4Address()),
            "{}-{}".format(util.numbers_strings.randomIP4Address(),util.numbers_strings.randomIP4Address()),
            "{}-{}".format(util.numbers_strings.randomIP4Address(),util.numbers_strings.randomIP4Address()),
            "{}".format(util.numbers_strings.randomIP4Address())
         ],
         "gateway": "{}".format(util.numbers_strings.randomIP4Address()),
         "subnet": str(random.randrange(8, 32)),
         "name_servers": [
            "{}".format(util.numbers_strings.randomIP4Address()),
            "{}".format(util.numbers_strings.randomIP4Address())
         ]
      }),
      "outbound_proxy": {
         "http_host": "http://{}".format(util.numbers_strings.randomIP4Address()),
         "http_port": 8080,
         "https_host": "https://{}".format(util.numbers_strings.randomIP4Address()),
         "https_port": 8081
      },
      "resource_pool": "Resource Pool for {}".format(uniqueId),
      "storage": "Datastore for {}".format(uniqueId),
      "folder": "Blockchain Folder for {}".format(uniqueId),
      "container_repo": {
         "url": "https://{}.com".format(uniqueId),
         "username": "user@{}.com".format(uniqueId),
         "password": "container_repo_pa$$w0rd4{}".format(uniqueId)
      },
      "log_managements": [createLogManagementObject(uniqueId)],
      "wavefront": {
         "url": "https://wavefront.com",
         "token": "<test token>"
      }
   }


def createLogManagementObject(uniqueId, destination=util.helper.LOG_DESTINATION_LOG_INTELLIGENCE):
   return {
            "destination": destination,
            "address": "https://example.com/",
            "port": 8080,
            "username": "admin@{}.com".format(uniqueId),
            "password": "logging_pa$$w0rd4{}".format(uniqueId),
            "log_insight_agent_id": 100
}


def createDefaultConsortiumAdminRequest(request):
   '''
   Given something like the fixture request, use it as a model
   to create a new request object with the permissions of a
   consortium admin and hermes_org0.
   '''
   descriptor = {
      "org": "hermes_org0",
      "user": "vmbc_test_con_admin",
      "role": "consortium_admin"
    }
   tokenDescriptor = util.auth.getTokenDescriptor(util.auth.ROLE_CON_ADMIN,
                                                  True,
                                                  descriptor)
   return request.newWithToken(tokenDescriptor)


def validateZoneResponse(origZoneInfo, zoneResponse, orgId):
   '''
   Compares the original zone info to the response.
   The response has two additional fields, org_id and id. Those
   are checked separately, and then everything else is compared
   to the original info.
   '''
   zoneResponse = dict(zoneResponse)

   assert zoneResponse["org_id"] == orgId, "Org_id was {}, expected {}". \
      format(zoneResponse["org_id"], orgId)
   UUID(zoneResponse["id"])

   expected = copy.deepcopy(origZoneInfo)
   newZoneResponse = copy.deepcopy(zoneResponse)
   del(newZoneResponse["org_id"])
   del(newZoneResponse["id"])

   # If the log management section was missing in the creation
   # request, it is supposed to be present but empty in the response.
   if "log_managements" not in expected:
      expected["log_managements"] = []

   # If the outbound_proxy section was missing in the creation
   # request, it is supposed to be present but empty in the response.
   if "outbound_proxy" not in expected:
      expected["outbound_proxy"] = None
   else:
      for port in ["http_port", "https_port"]:
         if port not in expected["outbound_proxy"] or \
            expected["outbound_proxy"][port] == None:
            expected["outbound_proxy"][port] = 0

      # Handle a test case where we provide an int for a host. Helen puts it
      # into a string.
      for host in ["http_host", "https_host"]:
         if isinstance(expected["outbound_proxy"][host], int):
            expected["outbound_proxy"][host] = str(expected["outbound_proxy"][host])

   assert expected == newZoneResponse, "Expected {}, response: {}". \
      format(json.dumps(origZoneInfo, sort_keys=True, indent=4),
             json.dumps(newZoneResponse, sort_keys=True, indent=4))


@pytest.mark.smoke
@pytest.mark.foo
def test_blockchains_fields(fxConnection):
   blockchains = fxConnection.request.getBlockchains()
   idValid = False
   consortiumIdValid = False

   for b in blockchains:
      blockchainId = UUID(b["id"])
      consortiumId = UUID(b["consortium_id"])


@pytest.mark.smoke
def test_members_fields(fxConnection):
   blockchains = fxConnection.request.getBlockchains()
   result = fxConnection.request.getMemberList(blockchains[0]["id"])

   assert type(result) is list, "Response was not a list"
   assert len(result) >= 1, "No members returned"

   for m in result:
      (present, missing) = util.helper.requireFields(m, ["hostname", "status", "address",
                                                         "millis_since_last_message",
                                                         "millis_since_last_message_threshold",
                                                         "rpc_url" ])
      assert present, "No '{}' field in member entry.".format(missing)
      assert isinstance(m["hostname"], str), "'hostname' field in member entry is not a string"
      assert isinstance(m["status"], str), "'status' field in member entry is not a string"
      assert isinstance(m["address"], str), "'address' field in member entry is not a string"
      assert isinstance(m["millis_since_last_message"], int), \
         "'millis_since_last_message' field in member entry is not a string"
      assert isinstance(m["millis_since_last_message_threshold"], int), \
         "'millis_since_last_message_threshold field in member entry is not a string"
      assert isinstance(m["rpc_url"], str), "'rpc_url' field in member entry is not a string"
      assert m["rpc_url"] != "", "'rpc_url' field in member entry is empty string"
      assert not "rpc_cert" in m, "'rpc_cert' field should not be included if certs=true is not passed"


@pytest.mark.smoke
def test_members_rpc_url(fxConnection, fxBlockchain, fxHermesRunSettings):
   '''
   Test that the returned value for "rpc_url" is an ethrpc node.
   We'll do that by invoking the API. At the moment, Helen still
   supports the API (it is planned to be removed), so also verify
   that we aren't getting Helen's address back by ensuring a
   Helen-only API call fails.
   '''
   result = fxConnection.request.getMemberList(fxBlockchain.blockchainId)
   ethrpcUrl = None

   for member in result:
      ethrpcUrl = member["rpc_url"]

      # Ensure we have a node that responds to our API.
      # Will throw an exception if not.
      fxConnection.rpc.mining()

      # Ensure that the rpc_url isn't Helen.  This will give a 404
      # and throw an exception.
      userConfig = fxHermesRunSettings["hermesUserConfig"]
      invalidRequest = Request(fxConnection.request.logDir,
                               fxConnection.request.testName,
                               ethrpcUrl + "blockchains/local",
                               userConfig)
      try:
         result = invalidRequest.getBlockList(fxBlockchain.blockchainId)
         assert False, "An exception should have been thrown when asking an ethrpc node for blocks."
      except Exception as e:
         # There are of course various reasons a 404 could be returned.  But let's at least
         # be sure we got back 404 for the given path, indicating this call is not available.
         assert "Not Found" in str(e), "Expected a 404 error about calling 'blocks'."


@pytest.mark.smoke
def test_members_hostname(fxConnection):
   '''
   Verify the "hostname" fields are "replica1", "replica2", ...
   '''
   blockchains = fxConnection.request.getBlockchains()
   result = fxConnection.request.getMemberList(blockchains[0]["id"])
   nodeCount = len(result)
   hostNames = []

   for nodeData in result:
      hostNames.append(nodeData["hostname"])

   for i in range(0, nodeCount):
      findMe = "replica" + str(i)
      assert findMe in hostNames, "Could not find host {} in the response.".format(findMe)
      hostNames.remove(findMe)

   assert len(hostNames) == 0, "Hosts not returned in the response: {}".format(hostNames)


@pytest.mark.smoke
def test_members_millis_since_last_message(fxConnection, fxBlockchain, fxHermesRunSettings):
   '''
   Pause a node, get sleep time millis, and make sure it is at least as long as we slept.
   Unpause it, and make sure it decreased.
   The numbers are not exact, but we're not testing concord.  We're just
   testing that Helen is receiving/communicating new values, not always
   showing a default, etc...
   '''
   if fxHermesRunSettings["hermesCmdlineArgs"].blockchainLocation != util.helper.LOCATION_LOCAL:
      pytest.skip("Skipping because this test requires pausing a Concord node, and " \
                  "this Concord deployment is on SDDC or on-prem infra.")

   allMembers = fxConnection.request.getMemberList(fxBlockchain.blockchainId)
   nodeData = allMembers[0] # Any will do.
   hostName = nodeData["hostname"]
   concordIndex = int(hostName[len("replica"):]) + 1 # replica0 == concord1
   testTime = 0
   sleepTime = 5
   expectedMinimum = sleepTime * 1000

   # The functionality we need in Product is a bit tied into it, so make
   # a patchy object so we can use what we need.
   HermesArgs = collections.namedtuple("HermesArgs", "resultsDir")
   hermesArgs = HermesArgs(resultsDir = fxHermesRunSettings["hermesCmdlineArgs"].resultsDir)
   product = util.product.Product(hermesArgs,
                                  fxHermesRunSettings["hermesUserConfig"])

   try:
      product.resumeMembers(allMembers)
      log.info("Pausing concord{}".format(concordIndex))
      paused = product.pause_concord_replica(str(concordIndex))
      assert paused, "Unable to pause the container.  Hostname: {}, concord #: {}". \
         format(hostName, concordIndex)
      time.sleep(sleepTime)

      result = fxConnection.request.getMemberList(fxBlockchain.blockchainId)
      for nodeData in result:
         if nodeData["hostname"] == hostName:
            testTime = int(nodeData["millis_since_last_message"])
            break

      assert testTime > expectedMinimum, "Expected millis_since_last_message of " \
         "at least {}, got {}.".format(expectedMinimum, testTime)

      log.info("Resuming concord{}".format(concordIndex))
      resumed = product.resume_concord_replica(str(concordIndex))
      assert resumed, "Unable to resume the container.  Hostname: {}, concord #: {}". \
         format(hostName, concordIndex)

      result = fxConnection.request.getMemberList(fxBlockchain.blockchainId)
      assert len(result) > 0, "No members returned"

      for nodeData in result:
         if nodeData["hostname"] == hostName:
            testTimeResumed = int(nodeData["millis_since_last_message"])
            assert testTimeResumed < testTime, "Expected millis_since_last_message " \
               "to be less than {}, received {}.".format(testTime, testTimeResumed)
   finally:
      product.resumeMembers(allMembers)


@pytest.mark.smoke
def test_blockList_noNextField_allBlocks(fxConnection, fxBlockchain):
   '''
   Cause no "next" paging by requesting all blocks.
   '''
   util.blockchain.eth.ensureEnoughBlocksForPaging(fxConnection,
                                               fxBlockchain.blockchainId)
   latestBlock = util.blockchain.eth.getLatestBlockNumber(fxConnection.request,
                                                      fxBlockchain.blockchainId)
   # Time service may add blocks in between these requests, so +10
   # ensures that we account for a few rounds of that.
   result = fxConnection.request.getBlockList(fxBlockchain.blockchainId, count=latestBlock+10)
   assert "next" not in result, \
      "There should not be a 'next' field when requesting all blocks."


@pytest.mark.smoke
def test_blockList_noNextField_firstBlock(fxConnection, fxBlockchain):
   '''
   Cause no "next" paging by requesting the genesis block.
   '''
   result = fxConnection.request.getBlockList(fxBlockchain.blockchainId, latest=0)
   assert "next" not in result, \
      "There should not be a 'next' field when latest is 0."


@pytest.mark.smoke
def test_newBlocks_onePage(fxConnection, fxBlockchain):
   '''
   Add a bunch of blocks and get them all back in a page which is
   larger than the default.
   '''
   addBlocksAndSearchForThem(fxConnection.request,
                             fxBlockchain.blockchainId,
                             fxConnection.rpc, 11, 11)


@pytest.mark.smoke
def test_newBlocks_spanPages(fxConnection, fxBlockchain):
   '''
   Add multiple blocks and get them all back via checking many small pages.
   '''
   addBlocksAndSearchForThem(fxConnection.request,
                             fxBlockchain.blockchainId,
                             fxConnection.rpc, 5, 2)


@pytest.mark.smoke
def test_pageSize_zero(fxConnection, fxBlockchain):
   util.blockchain.eth.ensureEnoughBlocksForPaging(fxConnection, fxBlockchain.blockchainId)
   result = fxConnection.request.getBlockList(fxBlockchain.blockchainId, count=0)
   assert len(result["blocks"]) == 0, "Expected zero blocks returned."


@pytest.mark.smoke
def test_pageSize_negative(fxConnection, fxBlockchain):
   util.blockchain.eth.ensureEnoughBlocksForPaging(fxConnection, fxBlockchain.blockchainId)
   result = fxConnection.request.getBlockList(fxBlockchain.blockchainId, count=-1)
   assert len(result["blocks"]) == util.blockchain.eth.defaultBlocksInAPage, \
      "Expected {} blocks returned.".format(util.blockchain.eth.defaultBlocksInAPage)


@pytest.mark.smoke
def test_pageSize_exceedsBlockCount(fxConnection, fxBlockchain):
   util.blockchain.eth.ensureEnoughBlocksForPaging(fxConnection, fxBlockchain.blockchainId)
   blockCount = util.blockchain.eth.getLatestBlockNumber(fxConnection.request, fxBlockchain.blockchainId) + 1
   result = fxConnection.request.getBlockList(fxBlockchain.blockchainId, count=blockCount+1)
   assert len(result["blocks"]) == blockCount, "Expected {} blocks returned.".format(blockCount)


@pytest.mark.smoke
def test_paging_latest_negative(fxConnection, fxBlockchain):
   util.blockchain.eth.ensureEnoughBlocksForPaging(fxConnection, fxBlockchain.blockchainId)
   # Reminder that time service might append blocks between these
   # operations, so we can't assert that they all return the same
   # number; only that they're in order
   highestBlockNumberBefore = util.blockchain.eth.getLatestBlockNumber(fxConnection.request, fxBlockchain.blockchainId)
   result = fxConnection.request.getBlockList(fxBlockchain.blockchainId, latest=-1)
   highestBlockNumberAfter = util.blockchain.eth.getLatestBlockNumber(fxConnection.request, fxBlockchain.blockchainId)
   assert (result["blocks"][0]["number"] >= highestBlockNumberBefore and
           result["blocks"][0]["number"] <= highestBlockNumberAfter), \
           "Expected the latest block to be {}-{}".format(
              highestBlockNumberBefore, highestBlockNumberAfter)


@pytest.mark.smoke
def test_paging_latest_exceedsBlockCount(fxConnection, fxBlockchain):
   util.blockchain.eth.ensureEnoughBlocksForPaging(fxConnection, fxBlockchain.blockchainId)
   # Reminder that time service might append blocks between these
   # operations, so we can't assert that they all return the same
   # number; only that they're in order
   highestBlockNumberBefore = util.blockchain.eth.getLatestBlockNumber(fxConnection.request, fxBlockchain.blockchainId)
   result = fxConnection.request.getBlockList(fxBlockchain.blockchainId, latest=highestBlockNumberBefore+1)
   highestBlockNumberAfter = util.blockchain.eth.getLatestBlockNumber(fxConnection.request, fxBlockchain.blockchainId)
   assert (result["blocks"][0]["number"] >= highestBlockNumberBefore and
           result["blocks"][0]["number"] <= highestBlockNumberAfter), \
           "Expected the latest block to be {}-{}".format(
              highestBlockNumberBefore, highestBlockNumberAfter)


@pytest.mark.smoke
def test_blockIndex_negative(fxConnection, fxBlockchain):
   checkInvalidIndex(fxConnection.request, fxBlockchain.blockchainId, -1, "Invalid block number or hash")


@pytest.mark.smoke
def test_blockIndex_outOfRange(fxConnection, fxBlockchain):
   latestBlockNumber = util.blockchain.eth.getLatestBlockNumber(fxConnection.request, fxBlockchain.blockchainId)
   # Time service may add blocks between these requests, so +10
   # accounts for a few rounds of that.
   checkInvalidIndex(fxConnection.request, fxBlockchain.blockchainId, latestBlockNumber+10, "block not found")


# @pytest.mark.smoke
# %5c (backslash) causes HTTP/1.1 401 Unauthorized.  Why?  Is that a bug?
# Filed as VB-800.
# def test_blockIndex_backslash(fxConnection):
#
#    checkInvalidIndex(fxConnection.request, fxBlockchain.blockchainId, "%5c", "Invalid block number or hash")


@pytest.mark.smoke
def test_blockIndex_atSymbol(fxConnection, fxBlockchain):
   checkInvalidIndex(fxConnection.request, fxBlockchain.blockchainId, "%40", "Invalid block number or hash")


@pytest.mark.smoke
def test_blockIndex_word(fxConnection, fxBlockchain):
   checkInvalidIndex(fxConnection.request, fxBlockchain.blockchainId, "elbow", "Invalid block number or hash")


@pytest.mark.smoke
def test_blockIndex_zero(fxConnection, fxBlockchain):
   '''
   This test case will only work when running locally, such as a dev/
   CI/CD environment.  It's being run because block 0 is a special case:
   - Block 0 is the only block (today anyway) in the VMware Blockchain
     system which can contain multiple transactions, and we want to test
     that.
   - Block 0's timestamp is not created the same way as the others.
   - Block 0 is created from genesis.json.  e.g. Some accounts are
     preloaded with ether.
   '''
   genObject = util.blockchain.eth.loadGenesisJson()
   block = fxConnection.request.getBlockByNumber(fxBlockchain.blockchainId, 0)
   foundAccounts = []
   expectedNonce = 0

   assert block["number"] == 0, "Block 0's number was not 0"
   assert block["size"] == 1, "Block 0's size was '{}', expected 1".format(block["size"])
   assert int(block["parentHash"], 16) == 0, \
      "Block 0's parent hash was '{}', expected 0".format(block["parentHash"])
   assert int(block["nonce"], 16) == 0, \
      "Block 0's nonce was '{}', expected 0".format(block["nonce"])
   # VB-801: Block 0's timestamp is 0.
   # assert block["timestamp"] == ...

   assert len(block["transactions"]) == len(genObject["alloc"]), \
      "The number of transactions in block 0 does not match the number in genesis.json."

   for tx in block["transactions"]:
      expectedUrl = "/api/concord/transactions/" + tx["hash"]
      assert tx["url"] == expectedUrl, \
         "Transaction url in block 0 was {}, expected {}".format(tx["url"], expectedUrl)

      # Look up the transaction in block 0 and save its recipient.  Later, we will
      # verify that the accounts in genesis.json which have pre-allocated accounts
      # match the recipients in the transactions in block 0.
      fullTx = fxConnection.request.getTransaction(fxBlockchain.blockchainId, tx["hash"])
      foundAccounts.append(util.numbers_strings.trimHexIndicator(fullTx["to"]))

      assert fullTx["block_number"] == 0, \
         "Transaction in block 0 is listed as being in {}".format(fullTx["block_number"])
      assert int(fullTx["from"], 16) == 0, \
         "Expected initial alloc of ether to be from account 0"
      assert fullTx["hash"] == tx["hash"], \
         "Hash field in transaction {} was {}".format(tx["hash"], fullTx["hash"])
      assert fullTx["status"] == 1, "Expected a status of 1"

      # Genesis.json specifies the opening balance in hex and dec.
      expectedBalance = genObject["alloc"][util.numbers_strings.trimHexIndicator(fullTx["to"])]["balance"]
      if expectedBalance.startswith("0x"):
         expectedBalance = int(expectedBalance, 16)
      else:
         expectedBalance = int(expectedBalance)
      assert expectedBalance == int(fullTx["value"], 16), \
         "Expected balance to be '{}', was '{}'".format(expectedBalance, fullTx["value"])

      assert fullTx["nonce"] == expectedNonce, \
         "Expected block 0 transaction to have nonce '{}'".format(expectedNonce)
      expectedNonce += 1

   expectedAccounts = list(genObject["alloc"].keys())
   assert sorted(expectedAccounts) == sorted(foundAccounts), \
      "In block 0, expected accounts {} not equal to found accounts {}".format(expectedAccounts, foundAccounts)


@pytest.mark.smoke
def test_blockIndex_basic(fxConnection, fxBlockchain):
   '''
   Add a few blocks, fetch them by block index, and check the fields.
   Getting a block by index returns:
   {
     "number": 0,
     "hash": "string",
     "parentHash": "string",
     "nonce": "string",
     "size": 0,
     "timestamp": 0,
     "transactions": [
       "string"
     ]
   }
   '''
   numBlocks = 3
   txResponses = util.blockchain.eth.addBlocks(fxConnection.request, fxConnection.rpc, fxBlockchain.blockchainId, numBlocks)
   parentHash = None

   for txResponse in txResponses:
      block = fxConnection.request.getBlockByNumber(fxBlockchain.blockchainId, int(txResponse["blockNumber"], 16))
      assert block["number"] == int(txResponse["blockNumber"], 16), "Number is not correct."
      assert block["hash"] == txResponse["blockHash"], "Hash is not correct."

      if parentHash:
         # Time service may insert blocks between these transactions,
         # so we're going to verify that a previous transaction's
         # block is somewhere in this block's lineage, instead of
         # being this block's direct parent
         searchBlock = block
         while searchBlock["parentHash"] != parentHash:
            assert searchBlock["number"] > 0, "Block with parent hash not found"
            if searchBlock["number"] > 0:
               searchBlock = fxConnection.request.getBlockByNumber(fxBlockchain.blockchainId, searchBlock["number"]-1)

      # This block's hash is the parentHash of the next one.
      parentHash = block["hash"]

      # The block nonce is not used, but it is required for compliance.
      assert int(block["nonce"], 16) == 0

      # Block size is always 1 right now.  Investigate.  Not a Helen issue though.
      # Internet searches say Ethereum block size has to do with gas, not the
      # size of something in bytes.
      assert block["size"] == 1

      checkTimestamp(txResponse["apiCallTime"], block["timestamp"])

      # We have one transaction per block, except block 0, which is handled
      # in a different use case.
      assert len(block["transactions"]) == 1, \
         "Expected one transaction per block (except block zero)"
      assert txResponse["transactionHash"] == block["transactions"][0]["hash"], \
         "The block's transaction hash does not match the transaction hash " \
         "given when the block was added."


@pytest.mark.smoke
def test_transactionHash_basic(fxConnection, fxBlockchain):
   '''
   Add a contract, invoke it, and check that the two transactions added can be
   retrieved as well as contain appropriate values.
   '''
   txReceipts = util.blockchain.eth.addBlocks(fxConnection.request, fxConnection.rpc, fxBlockchain.blockchainId, 1, True)
   contractCreationTxHash = txReceipts[0]["transactionHash"]
   contractInvocationTxHash = txReceipts[1]["transactionHash"]
   contractCreationTx = fxConnection.request.getTransaction(fxBlockchain.blockchainId, contractCreationTxHash)
   contractInvocationTx = fxConnection.request.getTransaction(fxBlockchain.blockchainId, contractInvocationTxHash)

   # VB-814: The transaction_index field is missing.

   verifyContractCreationTx(fxConnection.request, fxBlockchain.blockchainId, contractCreationTx)
   verifyContractInvocationTx(fxConnection.request, fxBlockchain.blockchainId, contractCreationTx,
                              contractInvocationTx)


@pytest.mark.smoke
def test_transactionHash_invalid_zero(fxConnection, fxBlockchain):
   '''
   Submit an invalid value for the transaction.
   '''
   invalidTx = fxConnection.request.getTransaction(fxBlockchain.blockchainId, "0")
   assert len(invalidTx) == 0, "Invalid transaction ID should return an empty set."


@pytest.mark.smoke
def test_transactionHash_invalid_negOne(fxConnection, fxBlockchain):
   '''
   Submit an invalid value for the transaction.
   '''
   invalidTx = fxConnection.request.getTransaction(fxBlockchain.blockchainId, "-1")
   assert len(invalidTx) == 0, "Invalid transaction ID should return an empty set."


@pytest.mark.smoke
def test_transactionHash_invalid_tooLong(fxConnection, fxBlockchain):
   '''
   Submit an invalid value for the transaction.
   '''
   invalidTx = fxConnection.request.getTransaction(fxBlockchain.blockchainId, "0xc5555c44eabcc1fcf93ca1b69bcc2a56a4960bc1380fcbb2121eca5ba6aa6f41a")
   assert len(invalidTx) == 0, "Invalid transaction ID should return an empty set."


@pytest.mark.smoke
def test_blockchains_one(fxConnection):
   '''
   Test with one blockchain deployed, which is the default.
   '''
   blockchains = fxConnection.request.getBlockchains()
   assert len(blockchains) == 1, "Expected one blockchain to be returned"
   blockchain = blockchains[0]
   verifyBlockchainFields(fxConnection.request, blockchain)


@pytest.mark.smoke
def test_getABlockchain_valid(fxConnection):
   '''
   Test GET /blockchains/{bid}, which gets details for a given blockchain.
   '''
   blockchainId = fxConnection.request.getBlockchains()[0]["id"]
   details = fxConnection.request.getBlockchainDetails(blockchainId)
   verifyBlockchainFields(fxConnection.request, details)


@pytest.mark.smoke
def test_getABlockhain_invalid_uuid(fxConnection):
   '''
   Test GET /blockchains/{bid} with a UUID that is already used.
   This test's UUID was generated by Helen once, so the test *should*
   pass for a billion or more years.  Willing to accept the risk.
   '''
   blockchainId = "8fecf880-26e3-4d71-9778-ad1592324684"
   response = fxConnection.request.getBlockchainDetails(blockchainId)
   assert response["error_code"] == "NotFoundException", "Expected NotFoundException"
   assert response["status"] == 404, "Expected 404 response"
   expectedPath = "/api/blockchains/{}".format(blockchainId)
   assert response["path"] == expectedPath, "Expected path {}".format(expectedPath)



@pytest.mark.smoke
def test_getABlockhain_invalid_uuid_format(fxConnection):
   '''
   Test GET /blockchains/{bid} with an invalid uuid format.
   '''
   blockchainId = "3"
   response = fxConnection.request.getBlockchainDetails(blockchainId)
   validateBadRequest(response, "/api/blockchains/{}".format(blockchainId),
                      errorCode="MethodArgumentTypeMismatchException",
                      errorMessage="Failed to convert value of type " \
                      "'java.lang.String' to required type 'java.util.UUID'; " \
                      "nested exception is java.lang.IllegalArgumentException: " \
                      "Invalid UUID string: 3")


@pytest.mark.skip(reason="Need Hermes ability to stop/start the product.")
def test_blockchains_none(fxConnection, fxHermesRunSettings):
   '''
   How to start the product with no blockchains?
   Filed VB-841: Not able to start Helen with no blockchains.
   '''
   # restartTheProductWithNoBlockchains()
   product = util.product.Product(hermesArgs,
                                  fxHermesRunSettings["hermesUserConfig"])
   product.stopProduct()
   util.helper.setHelenProperty("vmbc.default.blockchain", "false")

   # Something goes wrong launching the product.  This is not a super high priority
   # test case because it will only be possible in the product the first time it is
   # ever launched.  Come back to it.
   product.launchProduct()
   blockchains = fxConnection.request.getBlockchains()
   assert len(blockchains) == 0, "Expected zero blockchains to be returned"

   # Clean up
   product.stopProduct()
   util.helper.setHelenProperty("vmbc.default.blockchain", "true")
   product.launchProduct()


@pytest.mark.skip(reason="Waiting for blockchain deletion capability")
def test_blockchains_multiple(fxConnection):
   '''
   Ensure > 1 blockchains, and be sure they are really different.
   '''
   # while len(fxConnection.request.getBlockchains()) < 2:
   #    addAnotherBlockchain()
   #
   # beSureTheBlockchainsAreDifferentAndUsingDifferentConcordNodes()
   #
   # for blockchain in fxConnection.request.getBlockchains():
   #    verifyBlockchainFields(fxConnection.request, blockchain)
   pass


@pytest.mark.smoke
def test_getContracts(fxConnection, fxBlockchain):
   '''
   Verify:
   Post several contracts and be sure we can retrieve them.
   '''
   beforeContractList = fxConnection.request.getContracts(fxBlockchain.blockchainId)
   numNew = 3
   newContractResults = []

   for _ in range(numNew):
      newContractResults.append(util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                            "resources/contracts/HelloWorld.sol",
                                                            "HelloWorld",
                                                            fromAddr=fromUser,
                                                            compilerVersion=compilerVersion))
   afterContractList = fxConnection.request.getContracts(fxBlockchain.blockchainId)
   assert len(beforeContractList) + numNew == len(afterContractList), \
      "Unexpected new number of contracts."

   # We could use the contract id/version api to check, but since it's easy to avoid
   # using the API we're testing in this case, let's not.
   for newContract in newContractResults:
      found = False

      for contract in afterContractList:
         if contract["contract_id"] == newContract["contract_id"] and \
            contract["owner"] == fromUser and \
            newContract["url"].startswith(contract["url"]):
            found = True
            break

      assert found, "Newly added contract not found"


@pytest.mark.smoke
def test_postContract_simple(fxConnection, fxBlockchain):
   '''
   Post a basic contract, check the result values, and run it.
   '''
   contractId = util.numbers_strings.random_string_generator()
   contractVersion = util.numbers_strings.random_string_generator()
   contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                "resources/contracts/HelloWorld.sol",
                                                "HelloWorld",
                                                fromAddr=fromUser,
                                                compilerVersion=compilerVersion,
                                                contractId=contractId,
                                                contractVersion=contractVersion)
   validateContractFields(contractResult, fxBlockchain, contractId, contractVersion)
   contract = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)

   result = fxConnection.rpc.callContract(contract["address"], data=util.blockchain.eth.helloFunction)
   assert util.blockchain.eth.helloHex in result, "Simple uploaded contract not executed correctly."


@pytest.mark.smoke
def test_postContract_constructor(fxConnection, fxBlockchain):
   '''
   Post a contract with a constructor and run it.
   The constructor data must be even length hex string, no 0x prefix.
   (It gets appended to the bytecode.)
   '''

   contractId = util.numbers_strings.random_string_generator()
   contractVersion = util.numbers_strings.random_string_generator()
   constructorParam = util.numbers_strings.decToInt256HexNo0x(10)
   contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                "resources/contracts/CounterWithConstructorParam.sol",
                                                "Counter",
                                                fromAddr=fromUser,
                                                compilerVersion=compilerVersion,
                                                contractId=contractId,
                                                contractVersion=contractVersion,
                                                ctorParams=constructorParam)
   validateContractFields(contractResult, fxBlockchain, contractId, contractVersion)
   contract = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)

   callContractResult = fxConnection.rpc.callContract(contract["address"], data="0xa87d942c")
   assert int(callContractResult, 16) == 10, "Constructor value was not used."


@pytest.mark.smoke
def test_postContract_optimized(fxConnection, fxBlockchain):
   '''
   Post a contract that is optimized.  Prove that Helen used the optimize
   flag by comparing to bytecode which is not optimized.
   '''
   contractId = util.numbers_strings.random_string_generator()
   contractVersion = util.numbers_strings.random_string_generator()
   contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                "resources/contracts/CounterWithConstructorParam.sol",
                                                "Counter",
                                                fromAddr=fromUser,
                                                compilerVersion=compilerVersion,
                                                contractId=contractId,
                                                contractVersion=contractVersion,
                                                optimize = False)
   validateContractFields(contractResult, fxBlockchain, contractId, contractVersion)
   contract = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)
   unoptimizedBytecode = contract["bytecode"]

   contractId = util.numbers_strings.random_string_generator()
   contractVersion = util.numbers_strings.random_string_generator()
   contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                "resources/contracts/CounterWithConstructorParam.sol",
                                                "Counter",
                                                fromAddr=fromUser,
                                                compilerVersion=compilerVersion,
                                                contractId=contractId,
                                                contractVersion=contractVersion,
                                                optimize=True,
                                                runs="1")
   validateContractFields(contractResult, fxBlockchain, contractId, contractVersion)
   contract = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)
   optimizedBytecode1Run = contract["bytecode"]
   assert optimizedBytecode1Run != unoptimizedBytecode, "Bytecode was not optimized"


@pytest.mark.smoke
def test_postContract_optimizeRuns(fxConnection, fxBlockchain):
   '''
   Optimize the contract for different run frequencies. Prove
   that Helen used the run parameter by comparing bytecode.
   '''
   contractId = util.numbers_strings.random_string_generator()
   contractVersion = util.numbers_strings.random_string_generator()
   contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                "resources/contracts/HelloWorld.sol",
                                                "HelloWorld",
                                                fromAddr=fromUser,
                                                compilerVersion=compilerVersion,
                                                contractId=contractId,
                                                contractVersion=contractVersion,
                                                optimize=True,
                                                runs="1")
   contract = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)
   optimizedBytecode1Run = contract["bytecode"]

   contractId = util.numbers_strings.random_string_generator()
   contractVersion = util.numbers_strings.random_string_generator()
   contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                "resources/contracts/HelloWorld.sol",
                                                "HelloWorld",
                                                fromAddr=fromUser,
                                                compilerVersion=compilerVersion,
                                                contractId=contractId,
                                                contractVersion=contractVersion,
                                                optimize=True,
                                                runs="200")
   contract = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)
   optimizedBytecode200Runs = contract["bytecode"]

   assert optimizedBytecode200Runs != optimizedBytecode1Run, \
      "Change in runs did not produce different optimized bytecode."


@pytest.mark.smoke
def test_postContract_multiple_first(fxConnection, fxBlockchain):
   '''
   Submit a file with multiple contracts, specifying the first as the contract.
   '''
   contractId = util.numbers_strings.random_string_generator()
   contractVersion = util.numbers_strings.random_string_generator()
   contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                "resources/contracts/HelloWorldMultiple.sol",
                                                "HelloWorld",
                                                fromAddr=fromUser,
                                                compilerVersion=compilerVersion,
                                                contractId=contractId,
                                                contractVersion=contractVersion)
   contract = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)
   assert util.blockchain.eth.helloHex in contract["bytecode"], "HelloWorld! is not present in bytecode."
   assert util.blockchain.eth.howdyHex not in contract["bytecode"], "HowdyWorld! should not be in the bytecode."


@pytest.mark.smoke
def test_postContract_multiple_second(fxConnection, fxBlockchain):
   '''
   Submit a file with multiple contracts, specifying the second as the contract.
   '''
   contractId = util.numbers_strings.random_string_generator()
   contractVersion = util.numbers_strings.random_string_generator()
   contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                "resources/contracts/HelloWorldMultiple.sol",
                                                "HowdyWorld",
                                                fromAddr=fromUser,
                                                compilerVersion=compilerVersion,
                                                contractId=contractId,
                                                contractVersion=contractVersion)
   contract = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)
   assert util.blockchain.eth.howdyHex in contract["bytecode"], "HowdyWorld! is not present in the bytecode."
   assert util.blockchain.eth.helloHex not in contract["bytecode"], "HelloWorld! should not be in the bytecode."


@pytest.mark.smoke
def test_postContract_noContractId(fxConnection, fxBlockchain):
   '''
   Try to submit a contract without an ID.
   '''
   contractVersion = util.numbers_strings.random_string_generator()
   contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                "resources/contracts/HelloWorld.sol",
                                                "HelloWorld",
                                                contractId=None,
                                                contractVersion=contractVersion,
                                                fromAddr=fromUser,
                                                compilerVersion=compilerVersion,
                                                ctorParams="",
                                                optimize=False,
                                                generateDefaults=False)
   validateBadRequest(contractResult,
                      "/api/blockchains/{}/concord/contracts".format(fxBlockchain.blockchainId))


@pytest.mark.smoke
def test_postContract_noContractVersion(fxConnection, fxBlockchain):
   '''
   Try to submit a contract without a version.
   '''
   contractId = util.numbers_strings.random_string_generator()
   contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                "resources/contracts/HelloWorld.sol",
                                                "HelloWorld",
                                                contractId=contractId,
                                                contractVersion=None,
                                                fromAddr=fromUser,
                                                compilerVersion=compilerVersion,
                                                ctorParams="",
                                                optimize=False,
                                                generateDefaults=False)
   validateBadRequest(contractResult,
                      "/api/blockchains/{}/concord/contracts".format(fxBlockchain.blockchainId))


@pytest.mark.smoke
def test_postContract_noContractFrom(fxConnection, fxBlockchain):
   '''
   Try to submit a contract without a "from".
   '''
   contractId = util.numbers_strings.random_string_generator()
   contractVersion = util.numbers_strings.random_string_generator()
   contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                "resources/contracts/HelloWorld.sol",
                                                "HelloWorld",
                                                contractId=contractId,
                                                contractVersion=contractVersion,
                                                fromAddr=None,
                                                compilerVersion=compilerVersion,
                                                ctorParams="",
                                                optimize=False,
                                                generateDefaults=False)
   contract = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)


@pytest.mark.smoke
def test_postContract_noContractSource(fxConnection, fxBlockchain):
   '''
   Try to submit a contract without source code.
   '''
   contractId = util.numbers_strings.random_string_generator()
   contractVersion = util.numbers_strings.random_string_generator()
   contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                None,
                                                "HelloWorld",
                                                contractId=contractId,
                                                contractVersion=contractVersion,
                                                fromAddr=fromUser,
                                                compilerVersion=compilerVersion,
                                                ctorParams="",
                                                optimize=False,
                                                generateDefaults=False)
   validateBadRequest(contractResult,
                      "/api/blockchains/{}/concord/contracts".format(fxBlockchain.blockchainId))


@pytest.mark.smoke
def test_postContract_noContractName(fxConnection, fxBlockchain):
   '''
   Try to submit a contract without a name.
   '''
   contractId = util.numbers_strings.random_string_generator()
   contractVersion = util.numbers_strings.random_string_generator()
   contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                "resources/contracts/HelloWorld.sol",
                                                None,
                                                contractId=contractId,
                                                contractVersion=contractVersion,
                                                fromAddr=fromUser,
                                                compilerVersion=compilerVersion,
                                                ctorParams="",
                                                optimize=False,
                                                generateDefaults=False)
   contract = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)
   validateBadRequest(contractResult,
                      "/api/blockchains/{}/concord/contracts".format(fxBlockchain.blockchainId))


@pytest.mark.smoke
def test_postContract_noContractConstructorOK(fxConnection, fxBlockchain):
   '''
   Try to submit a contract without constructor parameters when the
   constructor is not needed.
   '''
   contractId = util.numbers_strings.random_string_generator()
   contractVersion = util.numbers_strings.random_string_generator()
   contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                "resources/contracts/HelloWorld.sol",
                                                "HelloWorld",
                                                contractId=contractId,
                                                contractVersion=contractVersion,
                                                fromAddr=fromUser,
                                                compilerVersion=compilerVersion,
                                                ctorParams=None,
                                                optimize=False,
                                                generateDefaults=False)
   contract = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)
   result = fxConnection.rpc.callContract(contract["address"], data=util.blockchain.eth.helloFunction)
   assert util.blockchain.eth.helloHex in result, "Simple uploaded contract not executed correctly."


@pytest.mark.skip(reson="What should happen?  Helen accepts it with no error.")
def test_postContract_noContractConstructorFail(fxConnection, fxBlockchain):
   '''
   Try to submit a contract without constructor parameters when the
   constructor requires one.
   '''
   contractId = util.numbers_strings.random_string_generator()
   contractVersion = util.numbers_strings.random_string_generator()
   contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                        "resources/contracts/CounterWithConstructorParam.sol",
                                                        "Counter",
                                                        contractId=contractId,
                                                        contractVersion=contractVersion,
                                                        fromAddr=fromUser,
                                                        compilerVersion=compilerVersion,
                                                        ctorParams=None,
                                                        optimize=False,
                                                        generateDefaults=False)
   # contract = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)
   # result = fxConnection.rpc.callContract(contract["address"], data=util.blockchain.eth.helloFunction)
   # validateBadRequest(contractResult,
   #                    "/api/blockchains/{}/concord/contracts".format(fxBlockchain.blockchainId))
   #   assert util.blockchain.eth.helloHex in result, "Simple uploaded contract not executed correctly."


@pytest.mark.smoke
def test_postContract_noContractCompilerVersion(fxConnection, fxBlockchain):
   '''
   Try to submit a contract without a compiler version.
   '''
   contractId = util.numbers_strings.random_string_generator()
   contractVersion = util.numbers_strings.random_string_generator()
   contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                "resources/contracts/HelloWorld.sol",
                                                "HelloWorld",
                                                contractId=contractId,
                                                contractVersion=contractVersion,
                                                fromAddr=fromUser,
                                                compilerVersion=None,
                                                ctorParams=None,
                                                optimize=False,
                                                generateDefaults=False)
   contract = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)
   validateBadRequest(contractResult,
                      "/api/blockchains/{}/concord/contracts".format(fxBlockchain.blockchainId))


@pytest.mark.smoke
def test_postContract_duplicateContractAndVersion(fxConnection, fxBlockchain):
   '''
   Try to submit a contract with an id/version matching one that exists.
   '''
   contractId = util.numbers_strings.random_string_generator()
   contractVersion = util.numbers_strings.random_string_generator()
   contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                "resources/contracts/HelloWorld.sol",
                                                "HelloWorld",
                                                contractId=contractId,
                                                contractVersion=contractVersion)
   contract = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)
   assert util.blockchain.eth.helloHex in contract["bytecode"], "HelloWorld! should be in the bytecode."

   contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                "resources/contracts/HelloWorld.sol",
                                                "HelloWorld",
                                                contractId=contractId,
                                                contractVersion=contractVersion)
   assert contractResult["error_code"] == "ConflictException", \
      "Expected a 'ConflictException' error code, got '{}'".format(contractResult["error_code"])
   expectedMessage = "ContractVersion with id {} and version {} already exists".format(contractId, contractVersion)
   assert contractResult["error_message"] == expectedMessage, \
      "Expected error message '{}', got '{}'".format(expectedMessage, contractResult["error_message"])


@pytest.mark.smoke
def test_postContract_duplicateContractNewVersion(fxConnection, fxBlockchain):
   '''
   Try to submit a contract with the same ID and a new version.
   '''
   contractId = util.numbers_strings.random_string_generator()
   contractVersion = util.numbers_strings.random_string_generator()
   contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                "resources/contracts/HelloWorldMultiple.sol",
                                                "HelloWorld",
                                                contractId=contractId,
                                                contractVersion=contractVersion)

   contractVersion = util.numbers_strings.random_string_generator(mustNotMatch=contractVersion)
   contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                "resources/contracts/HelloWorldMultiple.sol",
                                                "HowdyWorld",
                                                contractId=contractId,
                                                contractVersion=contractVersion)
   contract = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)
   assert util.blockchain.eth.howdyHex in contract["bytecode"], "HowdyWorld! should be in the bytecode."
   assert util.blockchain.eth.helloHex not in contract["bytecode"], "HelloWorld! should not be in the bytecode."


@pytest.mark.smoke
def test_postContract_newContractDuplicateVersion(fxConnection, fxBlockchain):
   '''
   Submit a contract with a new ID and the same version.
   '''
   contractId = util.numbers_strings.random_string_generator()
   contractVersion = util.numbers_strings.random_string_generator()
   contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                "resources/contracts/HelloWorldMultiple.sol",
                                                "HelloWorld",
                                                contractId=contractId,
                                                contractVersion=contractVersion)

   contractId = util.numbers_strings.random_string_generator(mustNotMatch=contractId)
   contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                "resources/contracts/HelloWorldMultiple.sol",
                                                "HowdyWorld",
                                                contractId=contractId,
                                                contractVersion=contractVersion)
   contract = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)
   assert util.blockchain.eth.howdyHex in contract["bytecode"], "HowdyWorld! should be in the bytecode."
   assert util.blockchain.eth.helloHex not in contract["bytecode"], "HelloWorld! should not be in the bytecode."


@pytest.mark.smoke
def test_getContractById_idInvalid(fxConnection, fxBlockchain):
   '''
   Try to get a contract by ID when the ID is invalid.
   '''
   result = fxConnection.request.getAllContractVersions(fxBlockchain.blockchainId, nonexistantContractId)
   expectedPath = "/api/blockchains/{}/concord/contracts/{}".format(fxBlockchain.blockchainId, nonexistantContractId)

   assert result["error_code"] == "NotFoundException", \
      "Error code was {}, expected {}".format(result["error_code"], code)

   assert result["error_message"] == "Contract not found: {}".format(nonexistantContractId), \
      "Error message was {}, expected {}".format(result["error_message"], message)

   assert result["status"] == 404, \
      "Status was {}, expected {}".format(result["status"], status)

   assert result["path"] == expectedPath, \
      "Path was {}, expected {}".format(result["path"], expectedPath)


@pytest.mark.smoke
def test_getContractById_oneVersion(fxConnection, fxBlockchain):
   '''
   Upload one version of a contract, get it with /contracts/{id}, and
   verify that it is correct.
   '''
   contractId = util.numbers_strings.random_string_generator()
   contractVersion = util.numbers_strings.random_string_generator()
   util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                               "resources/contracts/HelloWorldMultipleWithDoc.sol",
                               "HelloWorld",
                               contractId=contractId,
                               contractVersion=contractVersion)
   helloExpectedDetails = util.json_helper.readJsonFile("resources/contracts/HelloWorldMultiple_helloExpectedData.json")
   result = fxConnection.request.getAllContractVersions(fxBlockchain.blockchainId, contractId)

   # These are properties of the contract, not versions.
   assert result["contract_id"] == contractId, \
      "Contract ID was {}, expected {}".format(result["contract_id"], contractId)

   assert result["owner"] == fromUser, \
      "Contract user was {}, expected {}".format(result["owner"], fromUser)

   assert len(result["versions"]) == 1, \
      "Contract should have had one version, actually had {}".format(len(result["versions"]))

   # The data returned from this call doesn't include the bytecode or sourcecode fields.
   assert not hasattr(result["versions"][0], "bytecode"), \
      "Did not expect to find the bytecode field."
   assert not hasattr(result["versions"][0], "sourcecode"), \
      "Did not expect to find the sourcecode field."
   del helloExpectedDetails["bytecode"]
   del helloExpectedDetails["sourcecode"]

   verifyContractVersionFields(fxBlockchain.blockchainId,
                               fxConnection.request,
                               fxConnection.rpc,
                               result["versions"][0],
                               helloExpectedDetails,
                               contractVersion,
                               util.blockchain.eth.helloFunction,
                               util.blockchain.eth.helloHex)


@pytest.mark.smoke
def test_getContractById_multipleVersions(fxConnection, fxBlockchain):
   '''
   Upload multiple versions of a contract, get all of them in one call with /contracts/{id},
   and verify that they are correct.
   '''
   contractId = util.numbers_strings.random_string_generator()
   helloVersion = util.numbers_strings.random_string_generator()

   # Send Hello.
   util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                               "resources/contracts/HelloWorldMultipleWithDoc.sol",
                               "HelloWorld",
                               contractId=contractId,
                               contractVersion=helloVersion,
                               optimize=False)
   helloExpectedDetails = util.json_helper.readJsonFile\
                          ("resources/contracts/HelloWorldMultiple_helloExpectedData.json")

   # Send Howdy as a new version of the same contract.
   howdyVersion = util.numbers_strings.random_string_generator(mustNotMatch=helloVersion)
   util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                               "resources/contracts/HelloWorldMultipleWithDoc.sol",
                               "HowdyWorld",
                               contractId=contractId,
                               contractVersion=howdyVersion,
                               optimize=True,
                               runs=100)
   howdyExpectedDetails = util.json_helper.readJsonFile\
                          ("resources/contracts/HelloWorldMultiple_howdyExpectedData.json")
   result = fxConnection.request.getAllContractVersions(fxBlockchain.blockchainId, contractId)

   # These are properties of the contract.
   assert result["contract_id"] == contractId, \
      "Contract ID was {}, expected {}".format(result["contract_id"], contractId)

   assert result["owner"] == fromUser, \
      "Contract user was {}, expected {}".format(result["owner"], fromUser)

   assert len(result["versions"]) == 2, \
      "Contract should have had two versions, actually had {}".format(len(result["versions"]))

   # The data returned from this call doesn't include the bytecode or sourcecode fields.
   assert not hasattr(result["versions"][0], "bytecode"), \
      "Did not expect to find the bytecode field."
   assert not hasattr(result["versions"][0], "sourcecode"), \
      "Did not expect to find the sourcecode field."
   del helloExpectedDetails["bytecode"]
   del helloExpectedDetails["sourcecode"]
   verifyContractVersionFields(fxBlockchain.blockchainId,
                               fxConnection.request,
                               fxConnection.rpc,
                               result["versions"][0],
                               helloExpectedDetails,
                               helloVersion,
                               util.blockchain.eth.helloFunction,
                               util.blockchain.eth.helloHex)

   assert not hasattr(result["versions"][1], "bytecode"), \
      "Did not expect to find the bytecode field."
   assert not hasattr(result["versions"][1], "sourcecode"), \
      "Did not expect to find the sourcecode field."
   del howdyExpectedDetails["bytecode"]
   del howdyExpectedDetails["sourcecode"]
   verifyContractVersionFields(fxBlockchain.blockchainId,
                               fxConnection.request,
                               fxConnection.rpc,
                               result["versions"][1],
                               howdyExpectedDetails,
                               howdyVersion,
                               util.blockchain.eth.howdyFunction,
                               util.blockchain.eth.howdyHex)

@pytest.mark.smoke
def test_getContractVersionById_oneVersion(fxConnection, fxBlockchain):
   '''
   Upload one version of a contract, fetch it with /contract/{id}/version/{id}, and
   verify that the fields are correct.
   '''
   contractId = util.numbers_strings.random_string_generator()
   contractVersion = util.numbers_strings.random_string_generator()
   util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                               "resources/contracts/HelloWorldMultipleWithDoc.sol",
                               "HelloWorld",
                               contractId=contractId,
                               contractVersion=contractVersion,
                               optimize=False)
   result = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)

   # These are in the contract part of the structure when fetching all versions of a contract.
   # When fetching version info, they are included with the version.
   assert result["contract_id"] == contractId, \
      "Contract ID was {}, expected {}".format(result["contract_id"], contractId)
   del result["contract_id"]

   assert result["owner"] == fromUser, \
      "Contract user was {}, expected {}".format(result["owner"], fromUser)
   del result["owner"]

   helloExpectedDetails = util.json_helper.readJsonFile("resources/contracts/HelloWorldMultiple_helloExpectedData.json")

   verifyContractVersionFields(fxBlockchain.blockchainId,
                               fxConnection.request,
                               fxConnection.rpc,
                               result,
                               helloExpectedDetails,
                               contractVersion,
                               util.blockchain.eth.helloFunction,
                               util.blockchain.eth.helloHex)


@pytest.mark.smoke
def test_getContractVersionById_firstVersion(fxConnection, fxBlockchain):
   '''
   Upload multiple versions of a contract, fetch the first with /contract/{id}/version/{id}, and
   verify that the fields are correct.
   '''
   contractId = util.numbers_strings.random_string_generator()
   helloVersion = util.numbers_strings.random_string_generator()
   howdyVersion = util.numbers_strings.random_string_generator(mustNotMatch=helloVersion)
   util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                               "resources/contracts/HelloWorldMultipleWithDoc.sol",
                               "HelloWorld",
                               contractId=contractId,
                               contractVersion=helloVersion,
                               optimize=False)
   util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                               "resources/contracts/HelloWorldMultipleWithDoc.sol",
                               "HelloWorld",
                               contractId=contractId,
                               contractVersion=howdyVersion,
                               optimize=False)
   result = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, helloVersion)

   # These are in the contract part of the structure when fetching all versions of a contract.
   # When fetching version info, they are included with the version.
   assert result["contract_id"] == contractId, \
      "Contract ID was {}, expected {}".format(result["contract_id"], contractId)
   del result["contract_id"]

   assert result["owner"] == fromUser, \
      "Contract user was {}, expected {}".format(result["owner"], fromUser)
   del result["owner"]

   helloExpectedDetails = util.json_helper.readJsonFile("resources/contracts/HelloWorldMultiple_helloExpectedData.json")

   verifyContractVersionFields(fxBlockchain.blockchainId,
                               fxConnection.request,
                               fxConnection.rpc,
                               result,
                               helloExpectedDetails,
                               helloVersion,
                               util.blockchain.eth.helloFunction,
                               util.blockchain.eth.helloHex)


@pytest.mark.smoke
def test_getContractVersionById_lastVersion(fxConnection, fxBlockchain):
   '''
   Upload multiple versions of a contract, fetch the last with /contract/{id}/version/{id}, and
   verify that the fields are correct.
   '''
   contractId = util.numbers_strings.random_string_generator()
   helloVersion = util.numbers_strings.random_string_generator()
   howdyVersion = util.numbers_strings.random_string_generator(mustNotMatch=helloVersion)
   util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                               "resources/contracts/HelloWorldMultipleWithDoc.sol",
                               "HelloWorld",
                               contractId=contractId,
                               contractVersion=helloVersion,
                               optimize=False)
   util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                               "resources/contracts/HelloWorldMultipleWithDoc.sol",
                               "HowdyWorld",
                               contractId=contractId,
                               contractVersion=howdyVersion,
                               optimize=True,
                               runs=100)
   result = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, howdyVersion)

   # These are in the contract part of the structure when fetching all versions of a contract.
   # When fetching version info, they are included with the version.
   assert result["contract_id"] == contractId, \
      "Contract ID was {}, expected {}".format(result["contract_id"], contractId)
   del result["contract_id"]

   assert result["owner"] == fromUser, \
      "Contract user was {}, expected {}".format(result["owner"], fromUser)
   del result["owner"]

   howdyExpectedDetails = util.json_helper.readJsonFile("resources/contracts/HelloWorldMultiple_howdyExpectedData.json")

   verifyContractVersionFields(fxBlockchain.blockchainId,
                               fxConnection.request,
                               fxConnection.rpc,
                               result,
                               howdyExpectedDetails,
                               howdyVersion,
                               util.blockchain.eth.howdyFunction,
                               util.blockchain.eth.howdyHex)


@pytest.mark.smoke
def test_getContractVersionById_invalidVersion(fxConnection, fxBlockchain):
   '''
   Pass an invalid version to /contracts/{id}/versions/{id}.
   '''
   contractId = util.numbers_strings.random_string_generator()
   contractVersion = util.numbers_strings.random_string_generator()
   util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                               "resources/contracts/HelloWorldMultipleWithDoc.sol",
                               "HelloWorld",
                               contractId=contractId,
                               contractVersion=contractVersion,
                               optimize=False)
   result = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, nonexistantVersionId)

   assert result["error_code"] == "NotFoundException", \
      "Expected a 'NotFoundException' error code, got '{}'".format(result["error_code"])
   expectedMessage = "Contract version not found  {}:{}".format(contractId, nonexistantVersionId)
   assert result["error_message"] == expectedMessage, \
      "Expected error message '{}', got '{}'".format(expectedMessage, result["error_message"])
   assert result["status"] == 404, \
      "Expected status 404, received {}".format(result["status"])
   expectedPath = "/api/blockchains/{}/concord/contracts/{}/versions/{}". \
                  format(fxBlockchain.blockchainId, contractId, nonexistantVersionId)
   assert result["path"] == expectedPath, \
      "Expected path {}, received {}".format(expectedPath, result["path"])


@pytest.mark.smoke
def test_getContractVersionById_invalidContract(fxConnection, fxBlockchain):
   '''
   Pass an invalid contract to /contracts/{id}/versions/{id}.
   '''
   contractId = util.numbers_strings.random_string_generator()
   contractVersion = util.numbers_strings.random_string_generator()
   util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                               "resources/contracts/HelloWorldMultipleWithDoc.sol",
                               "HelloWorld",
                               contractId=contractId,
                               contractVersion=contractVersion,
                               optimize=False)
   result = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, nonexistantContractId, contractVersion)
   assert result["error_code"] == "NotFoundException", \
      "Expected a 'NotFoundException' error code, got '{}'".format(result["error_code"])
   expectedMessage = "Contract version not found  {}:{}".format(nonexistantContractId, contractVersion)
   assert result["error_message"] == expectedMessage, \
      "Expected error message '{}', got '{}'".format(expectedMessage, result["error_message"])
   assert result["status"] == 404, \
      "Expected status 404, received {}".format(result["status"])
   expectedPath = "/api/blockchains/{}/concord/contracts/{}/versions/{}". \
                  format(fxBlockchain.blockchainId, nonexistantContractId, contractVersion)
   assert result["path"] == expectedPath, \
      "Expected path {}, received {}".format(expectedPath, result["path"])


@pytest.mark.smoke
@pytest.mark.skip(reason="Unlike blocks, tx count cannot exceed ten, so this is an invalid test. Probably.")
def test_transactionList_noNextField_allTransactions(fxConnection, fxBlockchain):
   '''
   Cause there to be no "next" field by requesting all transactions.
   '''
   # txResponses = util.blockchain.eth.addBlocks(fxConnection.request, fxConnection.rpc, fxBlockchain.blockchainId, 1)
   # txList = fxConnection.request.getTransactionList(fxBlockchain.blockchainId, count=12)
   pass


@pytest.mark.smoke
def test_transactionList_genesisBlockTransactions(fxConnection, fxBlockchain):
   '''
   Verify that the transactions from the genesis block appear in the
   transaction list.
   '''
   genesisBlock = fxConnection.request.getBlockByNumber(fxBlockchain.blockchainId, 0)
   genTxs = []

   for genTx in genesisBlock["transactions"]:
      genTxs.append(genTx["hash"])

   q = queue.Queue(len(genTxs))
   nextUrl = None
   txList = fxConnection.request.getTransactionList(fxBlockchain.blockchainId)

   while True:
      for tx in txList["transactions"]:
         if q.full():
            q.get()
         q.put(tx["hash"])

      if "next" in txList.keys():
         txList = fxConnection.request.getNextTransactionList(txList["next"])
      else:
         break

   assert q.full, "Expected to get at least {} transactions.".format(len(genTxs))

   while not q.empty():
      tx = q.get()
      assert tx in genTxs, "Expected {} in {}.".format(tx, genTxs)


@pytest.mark.smoke
def test_transactionList_newItems_onePage(fxConnection, fxBlockchain):
   '''
   Add some transactions, get a page of transactions, and ensure
   they are there.
   '''
   expectedTxHashes = []
   receivedTxHashes = []

   for receipt in util.blockchain.eth.addBlocks(fxConnection.request, fxConnection.rpc, fxBlockchain.blockchainId, 3):
      expectedTxHashes.append(receipt["transactionHash"])

   for tx in fxConnection.request.getTransactionList(fxBlockchain.blockchainId)["transactions"]:
      receivedTxHashes.append(tx["hash"])

   for expectedTxHash in expectedTxHashes:
      assert expectedTxHash in receivedTxHashes, \
         "Expected {} in {}".format(expectedTxHash, receivedTxHashes)


@pytest.mark.smoke
def test_transactionList_spanPages(fxConnection, fxBlockchain):
   '''
   Add transactions and get them back via checking small pages,
   using the "next" field.
   '''
   trCount = 10
   expectedTxHashes = []

   for receipt in util.blockchain.eth.addBlocks(fxConnection.request, fxConnection.rpc, fxBlockchain.blockchainId, trCount):
      expectedTxHashes.append(receipt["transactionHash"])

   expectedTxHashes.reverse()

   receivedTrList1 = fxConnection.request.getTransactionList(fxBlockchain.blockchainId, count=int((trCount / 2)))
   nextUrl = receivedTrList1['next']
   receivedTrList1 = list(map(lambda x : x['hash'], receivedTrList1['transactions']))

   assert (expectedTxHashes[:5] == receivedTrList1), \
      "Transaction list query did not return correct transactions"

   receivedTrList2 = fxConnection.request.getNextTransactionList(nextUrl)
   receivedTrList2 = list(map(lambda x : x['hash'], receivedTrList2['transactions']))

   assert expectedTxHashes[5:] == receivedTrList2[:5], \
      "Transaction list query did not return correct transactions"


def _test_transactionList_pageSize_invalid(fxConnection, fxBlockchain, testCount):
   '''
   Request <count> transactions.  We get the default (10) when invalid.
   Used to test invalid testCount values.
   '''
   expectedTxHashes = []
   receivedTxHashes = []

   util.blockchain.eth.addBlocks(fxConnection.request, fxConnection.rpc, fxBlockchain.blockchainId, util.blockchain.eth.defaultTxInAPage-3)

   for receipt in util.blockchain.eth.addBlocks(fxConnection.request, fxConnection.rpc, fxBlockchain.blockchainId, 3):
      expectedTxHashes.append(receipt["transactionHash"])

   for tx in fxConnection.request.getTransactionList(fxBlockchain.blockchainId, count=testCount)["transactions"]:
      receivedTxHashes.append(tx["hash"])

   assert len(receivedTxHashes) == util.blockchain.eth.defaultTxInAPage, \
      "Expected {} transactions returned".format(util.blockchain.eth.defaultTxInAPage)

   for expectedTx in expectedTxHashes:
      assert expectedTx in receivedTxHashes, \
         "Expected {} in {}".format(expectedTx, receivedTxHashes)


@pytest.mark.smoke
def test_transactionList_count(fxConnection, fxBlockchain):
   txReceipt = util.blockchain.eth.addBlocks(fxConnection.request, fxConnection.rpc, fxBlockchain.blockchainId, 1)[0]
   txList = fxConnection.request.getTransactionList(fxBlockchain.blockchainId, count=1)
   txList = txList['transactions']

   assert (len(txList) == 1 and txList[0]['hash'] == txReceipt['transactionHash']), \
       "Trasaction list response did not follow count parameter."


@pytest.mark.smoke
def test_transactionList_pageSize_zero(fxConnection, fxBlockchain):
   _test_transactionList_pageSize_invalid(fxConnection, fxBlockchain, 0)


@pytest.mark.smoke
def test_transactionList_pageSize_negative(fxConnection, fxBlockchain):
   _test_transactionList_pageSize_invalid(fxConnection, fxBlockchain, -1)


@pytest.mark.smoke
@pytest.mark.skip(reason="Count does not exceed ten.")
def test_transactionList_pageSize_exceedsTxCount(fxConnection):
   '''
   Request more transactions than exist.  We can estimate the
   number of transactions by looking at the block count because
   we only store one transaction per block.  But maybe that will
   change; multiple transactions per block was suggested in a
   conversation regarding ways to improve performance.
   '''
   pass

@pytest.mark.smoke
def test_transactionList_paging_latest_invalid(fxConnection, fxBlockchain):
   '''
   Pass in an invalid hash for latest.
   (Valid values for latest are tested via the "next" url.)
   '''
   try:
      fxConnection.request.getTransactionList(fxBlockchain.blockchainId, latest="aa")
      assert False, "Expected to get an error fetching with latest being an invalid hash."
   except Exception as ex:
      errorMessage = "latest transaction not found"
      assert errorMessage in str(ex), \
         "Expected error message {}".format(errorMessage)


@pytest.mark.smoke
def test_transactionList_max_size(fxConnection, fxBlockchain):
   util.blockchain.eth.addBlocks(fxConnection.request, fxConnection.rpc, fxBlockchain.blockchainId, util.blockchain.eth.defaultTxInAPage+1)
   txList = fxConnection.request.getTransactionList(fxBlockchain.blockchainId, count=1000)
   txList = txList['transactions']
   assert len(txList) == util.blockchain.eth.defaultTxInAPage, \
      "Expected maximum page size to be {}".format(util.blockchain.eth.defaultTxInAPage)


@pytest.mark.smoke
def test_transactionList_fields(fxConnection, fxBlockchain):
   txReceipt = util.blockchain.eth.addBlocks(fxConnection.request, fxConnection.rpc, fxBlockchain.blockchainId, 1)[0]
   txList = fxConnection.request.getTransactionList(fxBlockchain.blockchainId)
   found = False

   for tx in txList["transactions"]:
      if txReceipt["transactionHash"] == tx["hash"]:
         found = True
         assert util.numbers_strings.trimHexIndicator(util.blockchain.eth.helloFunction) in tx["input"], \
            "Expected the hello function '{}' to be in the input '{}'.".format(util.blockchain.eth.helloFunction,tx["input"])
         assert tx["block_hash"] == txReceipt["blockHash"], "Block hash did not match."
         assert int(tx["block_number"]) == int(txReceipt["blockNumber"], 16), "Block number did not match."
         assert tx["from"] == txReceipt["from"], "From user did not match."
         assert tx["contract_address"] == txReceipt["contractAddress"], "Contract address did not match."
         assert tx["value"] == "0x0", "Value was not correct."
         assert tx["nonce"], "Nonce was not set."
         assert tx["url"] == "/api/concord/transactions/{}".format(txReceipt["transactionHash"]), "Url was not correct."

   assert found, "Transaction not found."


@pytest.mark.smoke
@pytest.mark.consortiums
def test_consortiums_add_basic(fxConnection):
   '''
   Basic consortium creation test.
   Issues:
   - What is the consoritum type?
   - Swagger shows consortiumName, body actually has to be consortium_name.
   - Swagger shows it takes an org, but it just links to the org that the
     user is part of.  So it seems org should not be a parameter.
   '''
   suffix = util.numbers_strings.random_string_generator()
   conName = "con_" + suffix
   req = fxConnection.request.newWithToken(defaultTokenDescriptor)
   con = req.createConsortium(conName)
   UUID(con["consortium_id"])
   assert con["consortium_name"] == conName
   assert con["organization_id"] == util.auth.orgs["blockchain_dev_service_org"]


@pytest.mark.smoke
@pytest.mark.consortiums
def test_consortiums_add_same_name(fxConnection):
   '''
   Multiple consortiums can have the same name, but IDs must be different.
   '''
   suffix = util.numbers_strings.random_string_generator()
   conName = "con_" + suffix
   req = fxConnection.request.newWithToken(defaultTokenDescriptor)
   con1 = req.createConsortium(conName)
   con2 = req.createConsortium(conName)
   UUID(con1["consortium_id"])
   UUID(con2["consortium_id"])
   assert con1["consortium_id"] != con2["consortium_id"]
   assert con1["consortium_name"] == conName
   assert con2["consortium_name"] == conName


@pytest.mark.smoke
@pytest.mark.consortiums
def test_consortiums_empty_name(fxConnection):
   '''
   Create a consortium with an empty string as a name.
   '''
   req = fxConnection.request.newWithToken(defaultTokenDescriptor)
   con = req.createConsortium("")

   # TODO
   # The field testErrorMessage was introduced because something in the error
   # message was breaking the comparison
   validateBadRequest(con, "/api/consortiums/", \
        errorCode = "MethodArgumentNotValidException", testErrorMessage = False)


@pytest.mark.smoke
@pytest.mark.consortiums
def test_consortiums_no_name(fxConnection):
   '''
   Create a consortium with no name field.
   '''
   req = fxConnection.request.newWithToken(defaultTokenDescriptor)
   con = req.createConsortium(None)
   validateBadRequest(con, "/api/consortiums/", errorCode = "MethodArgumentNotValidException", testErrorMessage = False)


@pytest.mark.smoke
@pytest.mark.consortiums
def test_consortiums_get_all(fxConnection):
   '''
   Test the API to retrieve all consoritiums.
   '''
   savedCons = []
   req = fxConnection.request.newWithToken(defaultTokenDescriptor)

   for i  in range(3):
      suffix = util.numbers_strings.random_string_generator()
      con = req.createConsortium("con_{}".format(suffix))
      savedCons.append(con)

   fetchedCons = req.getConsortiums()

   for saved in savedCons:
      found = False

      for fetched in fetchedCons:
         if saved["consortium_id"] == fetched["consortium_id"] and \
            saved["consortium_name"] == fetched["consortium_name"]:
            found = True
            break

      assert found, "Created consortium was not retrieved."


@pytest.mark.smoke
@pytest.mark.consortiums
def test_consortiums_get_specific(fxConnection):
   '''
   Test the API to retrieve a specific consortium.
   '''
   req = fxConnection.request.newWithToken(defaultTokenDescriptor)
   suffix = util.numbers_strings.random_string_generator()
   savedCon = req.createConsortium("con_{}".format(suffix))
   for i  in range(3):
      suffix = util.numbers_strings.random_string_generator()
      req.createConsortium("con_{}".format(suffix))

   fetchedCon = fxConnection.request.getConsortium(savedCon["consortium_id"])
   assert savedCon == fetchedCon, "Failed to retrieve the saved consortium."


@pytest.mark.smoke
@pytest.mark.consortiums
def test_consortiums_get_nonexistant(fxConnection):
   '''
   Try to retrieve a consortium which does not exist.
   Assumes 865d9e5c-aa7d-4a69-a7b5-1744be8d56f9 won't be generated again.
   Note VB-951, that the column name is included in the error message.  I'm
   not going to exclude this test case for that.
   '''
   oldId = "865d9e5c-aa7d-4a69-a7b5-1744be8d56f9"
   req = fxConnection.request.newWithToken(defaultTokenDescriptor)
   response = req.getConsortium(oldId)
   assert response["error_code"] == "NotFoundException", "Expected NotFoundException"
   assert response["status"] == 404, "Expected 404 response"
   expectedPath = "/api/consortiums/{}".format(oldId)
   assert response["path"] == expectedPath, "Expected path {}".format(expectedPath)


@pytest.mark.smoke
@pytest.mark.consortiums
def test_consortiums_get_bad_format(fxConnection):
   '''
   Try to retrieve a consortium using an incorrect ID format.
   '''
   req = fxConnection.request.newWithToken(defaultTokenDescriptor)
   result = req.getConsortium("a")
   assert result["error_code"] == "MethodArgumentTypeMismatchException", \
     "Expected different error code"
   msg = "Failed to convert value of type 'java.lang.String' to required type " \
         "'java.util.UUID'"
   assert msg in result["error_message"], "Expected '{}' in the error_message".format(msg)
   assert result["status"] == 400, "Expected status 400"
   assert result["path"] == "/api/consortiums/a", "Expected different path"


@pytest.mark.smoke
@pytest.mark.consortiums
def test_patch_consortium_name(fxConnection):
   suffix = util.numbers_strings.random_string_generator()
   conName = "con_" + suffix
   defaultDescriptor = {
      "org": "blockchain_service_dev",
      "user": "vmbc_test_con_admin",
      "role": "consortium_admin"
   }
   tokenDescriptor = util.auth.getTokenDescriptor(util.auth.ROLE_CON_ADMIN,
                                                  True,
                                                  defaultDescriptor)

   req = fxConnection.request.newWithToken(tokenDescriptor)
   conResponse = req.createConsortium(conName)
   consortiumId = conResponse["consortium_id"]
   renameResponse = req.patchConsortium(consortiumId,
                                        newName="Fred")
   renamedCon = req.getConsortium(consortiumId)
   assert renameResponse["consortium_name"] == "Fred", \
      "Expected the name to change to Fred"


@pytest.mark.smoke
@pytest.mark.consortiums
def test_patch_consortium_add_org(fxConnection, fxInitializeOrgs):
   suffix = util.numbers_strings.random_string_generator()
   conName = "con_" + suffix
   defaultDescriptor = {
      "org": "hermes_org0",
      "user": "vmbc_test_con_admin",
      "role": "consortium_admin"
   }
   tokenDescriptor = util.auth.getTokenDescriptor(util.auth.ROLE_CON_ADMIN,
                                                  True,
                                                  defaultDescriptor)
   originalOrg = tokenDescriptor["org"]
   req = fxConnection.request.newWithToken(tokenDescriptor)
   createResponse = req.createConsortium(conName)
   patchResponse = req.patchConsortium(createResponse["consortium_id"],
                                       orgsToAdd=[util.auth.orgs["hermes_org1"]])
   assert patchResponse["organization_id"] == createResponse["organization_id"], \
      "Adding an org should not have changed the consortium's main organization id."
   assert len(patchResponse["members"]) == 2, "Expected two organizations."

   for m in patchResponse["members"]:
      assert m["org_id"] in [util.auth.getOrgId(originalOrg),
                             util.auth.getOrgId("hermes_org1")]
      if m["org_id"] == util.auth.getOrgId(originalOrg):
         assert m["organization_name"] == originalOrg
      else:
         assert m["organization_name"] == "hermes_org1"


@pytest.mark.smoke
@pytest.mark.consortiums
def test_get_consortium_orgs(fxConnection, fxInitializeOrgs):
   '''
   Get the orgs for a consortium.
   It is impossible to have a consortium with no orgs, so we have
   only a positive test case
   '''
   suffix = util.numbers_strings.random_string_generator()
   conName = "con_" + suffix
   originalOrg = "hermes_org0"
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   createResponse = req.createConsortium(conName)
   req.patchConsortium(createResponse["consortium_id"],
                       orgsToAdd=[util.auth.orgs["hermes_org1"]])
   getOrgsResponse = req.getOrgs(createResponse["consortium_id"])
   assert len(getOrgsResponse) == 2, "Expected 2 orgs"

   for org in getOrgsResponse:
      assert org["org_id"] in [util.auth.getOrgId(originalOrg),
                               util.auth.getOrgId("hermes_org1")]

      if org["org_id"] == util.auth.getOrgId(originalOrg):
         assert org["organization_name"] == originalOrg
      else:
         assert org["organization_name"] == "hermes_org1"


@pytest.mark.smoke
def test_getCerts(fxConnection, fxBlockchain):
   '''
   Test that if we pass "?certs=true" to the Members endpoint, we get at
   least one non-empty rpc_cert in the response.
   '''
   blockchains = fxConnection.request.getBlockchains()
   result = fxConnection.request.getMemberList(blockchains[0]["id"],certs=True)

   assert type(result) is list, "Response was not a list"
   assert len(result) > 1, "No members returned"

   foundACert = False
   for m in result:
      # rpc_cert must be present
      assert isinstance(m["rpc_cert"], str), "'rpc_cert' field in member entry is not a string"
      if m["rpc_cert"] != "":
         foundACert = True

   assert foundACert, "No non-empty rpc_cert found in response"


@pytest.mark.smoke
def test_blockHash(fxConnection, fxBlockchain):
   txReceipt = util.blockchain.eth.mock_transaction(fxConnection.rpc)
   blockNumber = txReceipt['blockNumber']
   blockHash = txReceipt['blockHash']
   # query same block with hash and number and compare results
   block1 = fxConnection.request.getBlockByUrl("/api/concord/blocks/{}".format(int(blockNumber, 16)))
   block2 = fxConnection.request.getBlockByUrl("/api/concord/blocks/{}".format(blockHash))
   assert block1 == block2, \
      "Block returned with block hash API doesn't match with block returned by block Number"


@pytest.mark.smoke
def test_invalidBlockHash(fxConnection, fxBlockchain):
   try:
      block = fx.request.getBlockByUrl("/api/concord/blocks/0xbadbeef")
      assert False, "invalid block hash exception should be thrown"
   except Exception as e:
      pass


@pytest.mark.smoke
def test_largeReply(fxConnection, fxBlockchain):
   ### 1. Create three contracts, each 16kb in size
   ### 2. Request latest transaction list
   ### 3. Reply will be 48k+ in size
   ###
   ### This will require the highest bit in the size prefix of the
   ### concord->Helen response to be set, which makes the length look
   ### negative to Java's signed short type. If we get a response, then
   ### HEL-34 remains fixed.

   # Contract bytecode that is 16kb
   largeContract = "0x"+("aa" * 16384)

   sentTrList = []
   tr_count = 3
   for i in range(tr_count):
      tr = util.blockchain.eth.mock_transaction(fxConnection.rpc, data=largeContract)
      sentTrList.append(tr)
   sentTrList = list(map(lambda x : x['transactionHash'], sentTrList))
   sentTrList.reverse()

   receivedTrList = fxConnection.request.getTransactionList(fxBlockchain.blockchainId, count=tr_count)
   receivedTrHashes = list(map(lambda x : x['hash'], receivedTrList['transactions']))
   receivedDataSum = sum(len(x['input']) for x in receivedTrList['transactions'])

   assert sentTrList == receivedTrHashes, \
      "transaction list query did not return correct transactions"

   expectedDataSum = len(largeContract)*tr_count
   assert receivedDataSum == expectedDataSum, \
      "received only %d bytes, but expected %d" % (receviedDataSum, expectedDataSum)


@pytest.mark.zones
def test_create_zone(fxConnection, fxBlockchain):
   '''
   Basic test to create a zone.
   '''
   zoneInfo = createZoneObject()
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   orgId = util.auth.getOrgId("hermes_org0")
   response = req.createZone(zoneInfo)
   validateZoneResponse(zoneInfo, response, orgId)


@pytest.mark.zones
@pytest.mark.skip(reason="VB-2082")
def test_create_zone_invalid_field(fxConnection, fxBlockchain):
   '''
   Oops, I misspelled the name of a key.  A bad request error should catch it.
   '''
   zoneInfo = createZoneObject()
   zoneInfo["resurse_poule"] = "The Resource Pool"
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   response = req.createZone(zoneInfo)
   validateBadRequest(response, "/api/zones")

@pytest.mark.zones
def test_create_zone_blank_field(fxConnection, fxBlockchain):
   '''
   Oops, a key is blank. A bad request error should catch it.
   '''
   zoneInfo = createZoneObject()
   zoneInfo["name"] = "    "

   errorMessage = "Validation failed for argument [1] in org.springframework.http.ResponseEntity<com.vmware." + \
        "blockchain.services.blockchains.zones.ZoneController$ZoneResponse> com.vmware.blockchain.services.blockchains" + \
        ".zones.ZoneController.postZone(com.vmware.blockchain.services.blockchains.zones.Zone$Action,com.vmware." + \
        "blockchain.services.blockchains.zones.ZoneController$ZoneRequest) throws java.lang.Exception: " + \
        "[Field error in object 'zoneRequest' on field 'name': rejected value [    ]; codes [NotBlank.zoneRequest." + \
        "name,NotBlank.name,NotBlank.java.lang.String,NotBlank]; arguments [org.springframework.context.support." + \
        "DefaultMessageSourceResolvable: codes [zoneRequest.name,name]; arguments []; default message [name]]; " + \
        "default message [Name cannot be blank]] "

   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   response = req.createZone(zoneInfo)
   validateBadRequest(response, "/api/blockchains/zones", errorCode = "MethodArgumentNotValidException", \
   errorMessage = errorMessage)


@pytest.mark.zones
def test_create_aws_zone(fxConnection, fxBlockchain):
   '''
   The zone type must be "ON_PREM" or "VMC_AWS".
   '''
   zoneInfo = createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   orgId = util.auth.getOrgId("hermes_org0")
   response = req.createZone(zoneInfo)
   validateZoneResponse(zoneInfo, response, orgId)


@pytest.mark.zones
@pytest.mark.skip(reason="VB-2154")
def test_invalid_zone_type(fxConnection, fxBlockchain):
   '''
   The zone type must be "ON_PREM" or "VMC_AWS".  We cannot deploy
   into a mango.
   '''
   zoneInfo = createZoneObject(zoneType="mango")
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   response = req.createZone(zoneInfo)
   validateBadRequest(response, "/api/zones")


@pytest.mark.zones
@pytest.mark.skip(reason="VB-2089")
def test_create_zone_missing_required_field(fxConnection, fxBlockchain):
   '''
   Per helen/src/main/resources/api-doc/api.yaml, some fields are required.
   e.g. See OnPremZonePost.
   '''
   # Fields which are required for the zone apis.
   requiredZoneFields = {
      "always": {
         "type": {},
         "name": {}
      },
      util.helper.ZONE_TYPE_ON_PREM: {
         "vcenter": {
            "url": {},
            "username": {},
            "password": {}
         },
         "network": {
            "name": {},
            "ip_pool": {},
            "gateway": {},
            "subnet": {},
            "name_servers": {}
         },
         "resource_pool": {},
         "storage": {},
         "folder": {}
      },
      util.helper.ZONE_TYPE_SDDC: {}
   }

   req = createDefaultConsortiumAdminRequest(fxConnection.request)

   for zoneType in [util.helper.ZONE_TYPE_ON_PREM, util.helper.ZONE_TYPE_SDDC]:
      zoneObject = createZoneObject(zoneType)
      checkRequiredZoneFields(req, zoneObject, zoneObject, requiredZoneFields["always"])
      checkRequiredZoneFields(req, zoneObject, zoneObject, requiredZoneFields[zoneType])


def checkRequiredZoneFields(req, fullZoneObject, partialZoneObject, requiredFields):
   '''
   For each field name in requiredFields, create a zone object without it and verify
   that we get a Bad Request error when trying to use it.
   '''
   for keyToRemove in requiredFields.keys():
      removedVal = partialZoneObject[keyToRemove]
      del(partialZoneObject[keyToRemove])
      response = req.createZone(fullZoneObject)
      log.debug("Posting a new zone with the '{}' field missing".format(keyToRemove))
      validateBadRequest(response, "/api/zones")

      # Put it back.
      partialZoneObject[keyToRemove] = removedVal

      # If there are keys to remove within this key, test those also.
      if requiredFields[keyToRemove]:
         checkRequiredZoneFields(req, fullZoneObject, partialZoneObject[keyToRemove],
                                 requiredFields[keyToRemove])


@pytest.mark.zones
def test_get_single_zone(fxConnection, fxBlockchain):
   '''
   Create a zone, then retrieve it.
   '''
   zoneInfo = createZoneObject()
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   createZoneResponse = req.createZone(zoneInfo)
   zoneId = createZoneResponse["id"]
   getZoneResponse = req.getZone(zoneId)
   log.info("getZoneResponse: {}".format(getZoneResponse))
   assert createZoneResponse == getZoneResponse, "Responses should be equal."


@pytest.mark.zones
@pytest.mark.skip(reason="VB-2090")
def test_get_missing_zone(fxConnection, fxBlockchain):
   '''
   Try to get a zone which does not exist.
   7d9cea53-33d3-4fe7-8cb3-7d367d2eb30b was a unique zone id seen during testing.
   '''
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   getZoneResponse = req.getZone("7d9cea53-33d3-4fe7-8cb3-7d367d2eb30b")
   log.info("getZoneResponse: {}".format(getZoneResponse))


@pytest.mark.zones
def test_get_invalid_zone(fxConnection, fxBlockchain):
   '''
   Try to get a zone with an invalid ID format.
   '''
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   response = req.getZone("3")
   validateBadRequest(response, "/api/blockchains/zones/3",
                      errorCode="MethodArgumentTypeMismatchException",
                      errorMessage="Failed to convert value of type " \
                      "'java.lang.String' to required type 'java.util.UUID'; " \
                      "nested exception is java.lang.IllegalArgumentException: " \
                      "Invalid UUID string: 3")


@pytest.mark.zones
def test_get_all_zones(fxConnection, fxBlockchain):
   '''
   Create and retrieve multiple zones.
   '''
   addedZones = []
   req = createDefaultConsortiumAdminRequest(fxConnection.request)

   for _ in range(3):
      zoneInfo = createZoneObject()
      zoneInfo = req.createZone(zoneInfo)
      # The /api/blockchains/zones api call returns a subset of fields for each zone.
      addedZones.append({
         "type": zoneInfo["type"],
         "name": zoneInfo["name"],
         "latitude": zoneInfo["latitude"],
         "longitude": zoneInfo["longitude"],
         "id": zoneInfo["id"]
      })

   retrievedZones = req.getZones()

   for addedZone in addedZones:
      log.debug("Looking for zone {}".format(addedZone["id"]))
      assert addedZone in retrievedZones, "Zone {} not found in {}".format(addedZone, retrievedZones)


@pytest.mark.zones
@pytest.mark.skip(reason="VB-2150")
def test_get_all_no_zones(fxConnection, fxBlockchain):
   '''
   Get all zones when there are none.  Uses the delete api if there are existing zones.
   That does *not* mean this function tests the delete api, since it's possible this
   test gets called before any zones have been added.
   '''
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   retrievedZones = req.getZones()

   if retrievedZones:
      for zoneId in map(lambda x: x["id"], retrievedZones):
         log.debug("Deleting zone {}".format(zoneId))
         req.deleteZone(zoneId)

      retrievedZones = req.getZones()

   assert not retrievedZones, "Expected an empty list."


@pytest.mark.zones
def test_delete_some_zones(fxConnection, fxBlockchain):
   '''
   Add some zones, then delete them.
   '''
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   zonesToAdd = list(map(lambda _: createZoneObject(), range(3)))
   zoneIds = list(map(lambda zone: req.createZone(zone)["id"], zonesToAdd))
   deleteResponses = list(map(lambda zoneId: req.deleteZone(zoneId), zoneIds))
   expectedResponses = list(map(lambda zoneId: {"id": zoneId}, zoneIds))
   assert expectedResponses == deleteResponses, "Zone IDs submitted for deletion, '{}' " \
      "do not match the deletion responses, '{}'.".format(expectedResponses, deleteResponses)

   allZones = req.getZones()
   allZoneIds = list(map(lambda zone: zone["id"], allZones))
   log.info("zoneIds added and deleted: {}".format(zoneIds))
   log.info("zoneIds retrieved after deletion: {}".format(allZoneIds))

@pytest.mark.zones
def test_delete_all_zones(fxConnection, fxBlockchain):
   '''
   Add some zones, then delete all zones. Be sure Helen is still responsive
   after doing so.
   '''
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   zonesToAdd = list(map(lambda _: createZoneObject(), range(3)))
   [req.createZone(zone) for zone in zonesToAdd]
   allZoneIds = list(map(lambda zone: zone["id"], req.getZones()))

   # Kinda defeats the purpose of a test to delete all zones.  But this will become
   # valid when we stop hard coding some zones.
   allZoneIds, numPrecreatedZonesRemoved = removePrecreatedZones(allZoneIds)

   expectedResponses = list(map(lambda zoneId: {"id": zoneId}, allZoneIds))
   deleteResponses = list(map(lambda zoneId: req.deleteZone(zoneId), allZoneIds))
   assert expectedResponses == deleteResponses, "Zone IDs submitted for deletion, '{}' " \
      "do not match the deletion responses, '{}'.".format(expectedResponses, deleteResponses)
   assert len(req.getZones()) == numPrecreatedZonesRemoved, "Expected to have zero zones after deleting them all."

   # Now just check Helen.
   zoneObject = createZoneObject()
   newZoneId = req.createZone(zoneObject)["id"]
   validateZoneResponse(zoneObject,
                        req.getZone(newZoneId),
                        util.auth.getOrgId("hermes_org0"))


@pytest.mark.zones
@pytest.mark.skip(reason="VB-2090")
def test_delete_nonexistant_node(fxConnection, fxBlockchain):
   '''
   Try to delete a zone which does not exist.
   7d9cea53-33d3-4fe7-8cb3-7d367d2eb30b was a unique zone id seen during testing.
   '''
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   deleteZoneResponse = req.deleteZone("7d9cea53-33d3-4fe7-8cb3-7d367d2eb30b")
   log.info("getZoneResponse: {}".format(deleteZoneResponse))


@pytest.mark.zones
def test_delete_invalid_uuid_node(fxConnection, fxBlockchain):
   '''
   Try to delete a zone with an invalid ID format.
   '''
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   response = req.deleteZone("3")
   validateBadRequest(response, "/api/blockchains/zones/3",
                      errorCode="MethodArgumentTypeMismatchException",
                      errorMessage="Failed to convert value of type " \
                      "'java.lang.String' to required type 'java.util.UUID'; " \
                      "nested exception is java.lang.IllegalArgumentException: " \
                      "Invalid UUID string: 3")

@pytest.mark.zones
def test_no_log_management(fxConnection, fxBlockchain):
   '''
   The log management section is optional.  Remove it and test.
   '''
   zoneInfo = createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   orgId = util.auth.getOrgId("hermes_org0")
   del(zoneInfo["log_managements"])
   response = req.createZone(zoneInfo)
   validateZoneResponse(zoneInfo, response, orgId)


@pytest.mark.zones
def test_log_management_multiple(fxConnection, fxBlockchain):
   '''
   Should be able to specify multiple log management structures.
   '''
   zoneInfo = createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   orgId = util.auth.getOrgId("hermes_org0")
   newLogManagementObject = createLogManagementObject(util.numbers_strings.random_string_generator(),
                                                      destination=util.helper.LOG_DESTINATION_LOG_INSIGHT)
   zoneInfo["log_managements"].append(newLogManagementObject)
   response = req.createZone(zoneInfo)
   validateZoneResponse(zoneInfo, response, orgId)


@pytest.mark.zones
@pytest.mark.skip(reason="VB-2157")
def test_log_management_no_destination(fxConnection, fxBlockchain):
   '''
   Destination is a required field.
   '''
   zoneInfo = createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   orgId = util.auth.getOrgId("hermes_org0")
   del(zoneInfo["log_managements"][0]["destination"])
   response = req.createZone(zoneInfo)
   log.info("**** test_log_management_no_destination response: {}".format(response))
   # validateBadRequest(...)


@pytest.mark.zones
@pytest.mark.skip(reason="VB-2154")
def test_log_management_wrong_destination_type(fxConnection, fxBlockchain):
   '''
   Must be one of [LOG_INTELLIGENCE, LOG_INSIGHT].
   '''
   zoneInfo = createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   orgId = util.auth.getOrgId("hermes_org0")
   zoneInfo["log_managements"][0]["destination"] = 3
   response = req.createZone(zoneInfo)
   # validateBadRequest(...)


@pytest.mark.zones
@pytest.mark.skip(reason="VB-2157")
def test_log_management_no_address(fxConnection, fxBlockchain):
   '''
   Required field.
   '''
   zoneInfo = createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   orgId = util.auth.getOrgId("hermes_org0")
   del(zoneInfo["log_managements"][0]["address"])
   response = req.createZone(zoneInfo)
   log.info("**** test_log_management_no_address response: {}".format(response))
   # validateBadRequest(...)


@pytest.mark.zones
def test_log_management_wrong_address_type(fxConnection, fxBlockchain):
   '''
   Must be a string.
   '''
   zoneInfo = createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   orgId = util.auth.getOrgId("hermes_org0")
   zoneInfo["log_managements"][0]["address"] = 3
   response = req.createZone(zoneInfo)
   # The product auto-converts it.  That's fine.
   zoneInfo["log_managements"][0]["address"] = "3"
   validateZoneResponse(zoneInfo, response, orgId)


@pytest.mark.zones
@pytest.mark.skip(reason="VB-2157")
def test_log_management_no_username(fxConnection, fxBlockchain):
   '''
   Required field.
   '''
   zoneInfo = createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   orgId = util.auth.getOrgId("hermes_org0")
   del(zoneInfo["log_managements"][0]["username"])
   response = req.createZone(zoneInfo)
   log.info("**** test_log_management_no_usernames response: {}".format(response))
   # validateBadRequest(...)


@pytest.mark.zones
def test_log_management_wrong_username_type(fxConnection, fxBlockchain):
   '''
   Must be a string
   '''
   zoneInfo = createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   orgId = util.auth.getOrgId("hermes_org0")
   zoneInfo["log_managements"][0]["username"] = 3
   response = req.createZone(zoneInfo)
   # The product auto-converts it.  That's fine.
   zoneInfo["log_managements"][0]["username"] = "3"
   validateZoneResponse(zoneInfo, response, orgId)


@pytest.mark.zones
@pytest.mark.skip(reason="VB-2157")
def test_log_management_no_password(fxConnection, fxBlockchain):
   '''
   Required field.
   '''
   zoneInfo = createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   orgId = util.auth.getOrgId("hermes_org0")
   del(zoneInfo["log_managements"][0]["password"])
   response = req.createZone(zoneInfo)
   log.info("**** test_log_management_no_password response: {}".format(response))
   # validateBadRequest(...)


@pytest.mark.zones
def test_log_management_wrong_password_type(fxConnection, fxBlockchain):
   '''
   Must be a string
   '''
   zoneInfo = createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   orgId = util.auth.getOrgId("hermes_org0")
   zoneInfo["log_managements"][0]["password"] = 3
   response = req.createZone(zoneInfo)
   # The product auto-converts it.  That's fine.
   zoneInfo["log_managements"][0]["password"] = "3"
   validateZoneResponse(zoneInfo, response, orgId)


@pytest.mark.zones
@pytest.mark.skip(reason="VB-2099 (not supported yet)")
def test_patch_zone_no_change(fxConnection, fxBlockchain):
   '''
   Create a zone, patch it with identical information, and ensure no fields changed.
   '''
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   zoneInfo = createZoneObject()
   createResponse = req.createZone(zoneInfo)
   patchResponse = req.patchZone(createResponse["id"], zoneInfo)
   getResponse = req.getZone(createResponse["id"])
   assert createResponse == patchResponse, "Expected the zone patch response, '{}' " \
      "to equal the zone creation response, '{}'".format(patchResponse, createResponse)
   assert createResponse == getResponse, "Expected the zone get response, '{}' " \
      "to equal the zone creation response, '{}'".format(getResponse, createResponse)


@pytest.mark.zones
@pytest.mark.skip(reason="VB-2099 (not supported yet)")
def test_patch_zone_change_fields(fxConnection, fxBlockchain):
   '''
   Create a zone, patch all fields with new values.
   '''
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   zoneInfo = createZoneObject()
   createResponse = req.createZone(zoneInfo)
   zoneId = createResponse["id"]

   newZoneInfo = createZoneObject()
   patchResponse = req.patchZone(zoneId, newZoneInfo)
   assert patchResponse != createResponse, "Expected the response for creating the " \
                           "zone, '{}', to differ from the response for patching " \
                           "the zone, '{}'".format(createResponse, patchResponse)

   getResponse = req.getZone(zoneId)
   assert getResponse == patchResponse, "Expected the zone's new get response, '{}' " \
      "to equal the patch response, '{}'".format(getResponse, patchResponse)


@pytest.mark.zones
@pytest.mark.skip(reason="VB-2099 (not supported yet)")
def test_patch_zone_clear_required_fields(fxConnection, fxBlockchain):
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   zoneInfo = createZoneObject()
   zoneId = req.createZone(zoneInfo)["id"]

   newZoneInfo = createZoneObject()
   patchResponse = req.patchZone(zoneId)

   getResponse = req.getZone(zoneId)
   assert getResponse == patchResponse, "Expected the zone's new get response, '{}' " \
      "to equal the patch response, '{}'".format(getResponse, patchResponse)


@pytest.mark.zones
@pytest.mark.skip(reason="VB-2099 (not supported yet)")
def test_patch_zone_missing_zone(fxConnection, fxBlockchain):
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   req.patchZone("7d9cea53-33d3-4fe7-8cb3-7d367d2eb30b")


@pytest.mark.zones
@pytest.mark.skip(reason="VB-2099 (not supported yet)")
def test_patch_zone_invalid_uuid(fxConnection, fxBlockchain):
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   response = req.patchZone("3", createZoneObject())
   validateBadRequest(response, "/api/blockchains/zones/3",
                      errorCode="MethodArgumentTypeMismatchException",
                      errorMessage="Failed to convert value of type " \
                      "'java.lang.String' to required type 'java.util.UUID'; " \
                      "nested exception is java.lang.IllegalArgumentException: " \
                      "Invalid UUID string: 3")


@pytest.mark.zones
def test_missing_outbound_proxy(fxConnection, fxBlockchain):
   '''
   The entire outbound_proxy field is missing.
   '''
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   orgId = util.auth.getOrgId("hermes_org0")
   zoneInfo = createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
   del(zoneInfo["outbound_proxy"])
   response = req.createZone(zoneInfo)
   validateZoneResponse(zoneInfo, response, orgId)


@pytest.mark.zones
def test_empty_outbound_proxy_fields(fxConnection, fxBlockchain):
   '''
   All fields of the outbound proxy present, with empty values.
   '''
   zoneInfo = createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   orgId = util.auth.getOrgId("hermes_org0")
   zoneInfo["outbound_proxy"] = {
      "http_host": None,
      "http_port": None,
      "https_host": None,
      "https_port": None
   }
   response = req.createZone(zoneInfo)
   validateZoneResponse(zoneInfo, response, orgId)


@pytest.mark.zones
def test_empty_outbound_http_proxy(fxConnection, fxBlockchain):
   '''
   User defines an https proxy, and nothing for the http proxy.
   '''
   zoneInfo = createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   orgId = util.auth.getOrgId("hermes_org0")
   zoneInfo["outbound_proxy"]["http_host"] = None
   zoneInfo["outbound_proxy"]["http_port"] = None
   response = req.createZone(zoneInfo)
   validateZoneResponse(zoneInfo, response, orgId)


@pytest.mark.zones
def test_empty_outbound_https_proxy(fxConnection, fxBlockchain):
   '''
   User defines an http proxy, and nothing for the https proxy.
   '''
   zoneInfo = createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   orgId = util.auth.getOrgId("hermes_org0")
   zoneInfo["outbound_proxy"]["https_host"] = None
   zoneInfo["outbound_proxy"]["https_port"] = None
   response = req.createZone(zoneInfo)
   validateZoneResponse(zoneInfo, response, orgId)


@pytest.mark.zones
def test_invalid_outbound_host(fxConnection, fxBlockchain):
   '''
   User defines an invalid value for one of the outbound proxy hosts.
   We just take whatever they provide and make it a string.
   '''
   zoneInfo = createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   orgId = util.auth.getOrgId("hermes_org0")
   zoneInfo["outbound_proxy"]["https_host"] = 3
   response = req.createZone(zoneInfo)
   validateZoneResponse(zoneInfo, response, orgId)


@pytest.mark.zones
@pytest.mark.skip(reason="VB-2176")
def test_invalid_outbound_port(fxConnection, fxBlockchain):
   '''
   User defines an invalid value for one of the outbound proxy ports.
   This should be a 400 Bad Request.
   '''
   zoneInfo = createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
   req = createDefaultConsortiumAdminRequest(fxConnection.request)
   orgId = util.auth.getOrgId("hermes_org0")
   zoneInfo["outbound_proxy"]["https_port"] = "a"
   response = req.createZone(zoneInfo)
   validateBadRequest(response, "/api/zones")
