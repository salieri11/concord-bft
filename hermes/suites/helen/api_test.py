import inspect
import json
import logging
import os
import pickle
import pytest
import sys
import time
from uuid import UUID

from suites import test_suite
from rest.request import Request
from rpc.rpc_call import RPC
from util.numbers_strings import decToEvenHex, trimHexIndicator, epochToLegible

log = logging.getLogger(__name__)

# The number if blocks in a page when invoking the
# concord/blocks call.
defaultBlocksInAPage = 10

# The HelenAPITests object.
suiteObject = None

# Holds the results of the _getTests() method.
apiTests = []

# So the pytest fixture will display sensible names.
apiTestNames = []

# Ideally this would be a fixture.  However, fixtures and
# parametrize decorations don't play well together.  (There
# are threads about it.) So just brute force run this code
# and shove the info into global variables.  :(
with open("pickled_helen_api_tests", "rb") as f:
   suiteObject = pickle.load(f)
   apiTests = suiteObject._getTests()
   for test in apiTests:
      apiTestNames.append(test[0])


@pytest.fixture
def restRequest(request):
   '''
   This returns a Request object.  The accepted parameter, "request",
   is an internal PyTest name and must be that.
   The name of the function is what we get in a test case.
   '''
   # Format: suites/helen/api_test.py::test_blockchains_fields (setup)
   longName = os.environ.get('PYTEST_CURRENT_TEST')
   shortName = longName[longName.rindex(":")+1:longName.rindex(" ")]
   testLogDir = os.path.join(suiteObject._testLogDir, shortName)
   actualRequest = Request(testLogDir,
                           shortName,
                           suiteObject.reverseProxyApiBaseUrl,
                           suiteObject._userConfig)
   return actualRequest


def getAnEthrpcNode(request, blockchainId):
   '''
   Return the first ethrpc node for the given blockchain.
   '''
   members = request.getMemberList(blockchainId)

   if members:
      return members[0]
   else:
      raise Exception("getAnEthrpcNode could not get an ethrpc node.")


def ensureEnoughBlocksForPaging(request, blockchainId):
   '''
   Make sure the passed in blockchain has enough blocks to page multiple
   times with the default page size.
   '''
   latestBlockNumber = getLatestBlockNumber(request, blockchainId)
   numBlocks = latestBlockNumber + 1
   minBlocks = defaultBlocksInAPage * 3
   blocksToAdd = minBlocks - numBlocks

   if blocksToAdd > 0:
      log.info("ensureEnoughBocksForPaging is adding {} blocks.".format(blocksToAdd))
      ethrpcNode = getAnEthrpcNode(request, blockchainId)

      for i in range(blocksToAdd):
         suiteObject._mock_transaction(request, data=decToEvenHex(i),
                                       ethrpcNode=ethrpcNode["rpc_url"])

      latestBlockNumber = getLatestBlockNumber(request, blockchainId)
      assert latestBlockNumber + 1 >= minBlocks, "Failed to add enough blocks for paging."


def getLatestBlockNumber(request, blockchainId):
   '''
   Returns the newest block's number for the passed in blockchain.
   '''
   blockList = request.getBlockList(blockchainId)
   return blockList["blocks"][0]["number"]


def addBlocks(request, blockchainId, numBlocks):
   '''
   Adds numBlocks to the given blockchain.
   Returns a list of transaction responses.
   '''
   ethrpcNode = getAnEthrpcNode(request, blockchainId)
   txResponses = []

   for i in range(numBlocks):
      txResponse = (suiteObject._mock_transaction(request,
                                                  data=decToEvenHex(i),
                                                  ethrpcNode=ethrpcNode["rpc_url"]))
      # Add the time so we can check the timestamp field of a block.
      # It won't match exactly, obviously, but we can check that it's within a
      # reasonable range.
      txResponse["apiCallTime"] = int(time.time())

      txResponses.append(txResponse)
   return txResponses


def addBlocksAndSearchForThem(request, blockchainId, numBlocks, pageSize):
   '''
   Adds numBlocks blocks and searches for them one pageSize
   of blocks at a time.  This calls a method which handles
   the asserts.
   '''
   origBlockNumber = getLatestBlockNumber(request, blockchainId)
   txResponses = addBlocks(request, blockchainId, numBlocks)
   newBlockNumber = getLatestBlockNumber(request, blockchainId)
   assert newBlockNumber - origBlockNumber == numBlocks, \
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
      present, missing = suiteObject.requireFields(blockFromUrl, ["number", "hash",
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


def loadGenesisJson():
   '''
   Loads the genesis.json file that is used when launching via Docker.
   Won't work when launching in a real environment.
   '''
   hermes = os.getcwd()
   genFile = os.path.join(hermes, "..", "docker", "config-public", "genesis.json")
   genObject = None

   with open(genFile, "r") as f:
      genObject = json.loads(f.read())

   return genObject


def checkTimestamp(expectedTime, actualTime):
   '''
   Checks that actualTime is near the expectedTime.  The buffer is to
   account for clocks being out of sync on different systems.  We are
   not testing the accuracy of the time; we are testing that the time
   is generally appropriate.
   '''
   buff = 600
   earliestTime = expectedTime - buff
   earliestTimeString = epochToLegible(earliestTime)
   latestTime = expectedTime + buff
   latestTimeString = epochToLegible(latestTime)
   actualTimeString = epochToLegible(actualTime)

   assert actualTime >= earliestTime and actualTime <= latestTime, \
      "Block's timestamp, '{}', was expected to be between '{}' and '{}'. (Converted: " \
      "Block's timestamp, '{}', was expected to be between '{}' and '{}'.)".format(actualTime, \
                                                                earliestTime, latestTime,
                                                                actualTimeString, earliestTimeString,
                                                                latestTimeString)


# Runs all of the tests from helen_api_tests.py.
@pytest.mark.smoke
@pytest.mark.parametrize("testName", apiTestNames)
def test_existing(testName):
   testLogDir = os.path.join(suiteObject._testLogDir, testName)
   request = Request(testLogDir,
                     testName,
                     suiteObject.reverseProxyApiBaseUrl,
                     suiteObject._userConfig)
   testFn = None
   for apiTest in apiTests:
      if testName in apiTest:
         testFn = apiTest[1]
   assert testFn, "Test named {} not found.".format(testName)
   suiteObject.setEthrpcNode()
   result, info = testFn(request)
   assert result, info


@pytest.mark.smoke
def test_blockchains_fields(restRequest):
   blockchains = restRequest.getBlockchains()
   idValid = False
   consortiumIdValid = False

   for b in blockchains:
      blockchainId = UUID(b["id"])
      consortiumId = UUID(b["consortium_id"])
      assert blockchainId, "'id' field is not a valid UUID."
      assert consortiumId, "'consortium_id' field is not a valid UUID."


@pytest.mark.smoke
def test_members_fields(restRequest):
   blockchains = restRequest.getBlockchains()
   result = restRequest.getMemberList(blockchains[0]["id"])

   assert type(result) is list, "Response was not a list"
   assert len(result) >= 1, "No members returned"

   for m in result:
      (present, missing) = suiteObject.requireFields(m, ["hostname", "status", "address",
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
def test_members_rpc_url(restRequest):
   '''
   Test that the returned value for "rpc_url" is an ethrpc node.
   We'll do that by invoking the API. At the moment, Helen still
   supports the API (it is planned to be removed), so also verify
   that we aren't getting Helen's address back by ensuring a
   Helen-only API call fails.
   '''
   blockchainId = suiteObject.getABlockchainId(restRequest)
   result = restRequest.getMemberList(blockchainId)
   ethrpcUrl = None

   for member in result:
      ethrpcUrl = member["rpc_url"]

      # Ensure we have a node that responds to our API.
      # Will throw an exception if not.
      rpc = RPC(restRequest.logDir,
                restRequest.testName,
                ethrpcUrl,
                suiteObject._userConfig)
      rpc.mining()

      # Ensure that the rpc_url isn't Helen.  This will give a 404
      # and throw an exception.
      invalidRequest = Request(restRequest.logDir,
                               restRequest.testName,
                               ethrpcUrl + "blockchains/local",
                               suiteObject._userConfig)
      try:
         result = invalidRequest.getBlockList(blockchainId)
         assert False, "An exception should have been thrown when asking an ethrpc node for blocks."
      except Exception as e:
         # There are of course various reasons a 404 could be returned.  But let's at least
         # be sure we got back 404 for the given path, indicating this call is not available.
         assert "Not Found" in str(e), "Expected a 404 error about calling 'blocks'."


@pytest.mark.smoke
def test_members_hostname(restRequest):
   '''
   Verify the "hostname" fields are "replica1", "replica2", ...
   '''
   blockchains = restRequest.getBlockchains()
   result = restRequest.getMemberList(blockchains[0]["id"])
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
def test_members_millis_since_last_message(restRequest):
   '''
   Pause a node, get sleep time millis, and make sure it is at least as long as we slept.
   Unpause it, and make sure it decreased.
   The numbers are not exact, but we're not testing concord.  We're just
   testing that Helen is receiving/communicating new values, not always
   showing a default, etc...
   '''
   blockchainId = suiteObject.getABlockchainId(restRequest)
   allMembers = restRequest.getMemberList(blockchainId)
   nodeData = allMembers[0] # Any will do.
   hostName = nodeData["hostname"]
   concordIndex = int(hostName[len("replica"):]) + 1 # replica0 == concord1
   testTime = 0
   sleepTime = 5
   expectedMinimum = sleepTime * 1000

   try:
      suiteObject._resumeMembers(allMembers)

      log.info("Pausing concord{}".format(concordIndex))
      paused = suiteObject.product.pause_concord_replica(str(concordIndex))
      assert paused, "Unable to pause the container.  Hostname: {}, concord #: {}". \
         format(hostName, concordIndex)
      time.sleep(sleepTime)

      result = restRequest.getMemberList(blockchainId)
      for nodeData in result:
         if nodeData["hostname"] == hostName:
            testTime = int(nodeData["millis_since_last_message"])
            break

      assert testTime > expectedMinimum, "Expected millis_since_last_message of " \
         "at least {}, got {}.".format(expectedMinimum, testTime)

      log.info("Resuming concord{}".format(concordIndex))
      resumed = suiteObject.product.resume_concord_replica(str(concordIndex))
      assert resumed, "Unable to resume the container.  Hostname: {}, concord #: {}". \
         format(hostName, concordIndex)

      result = restRequest.getMemberList(blockchainId)
      assert len(result) > 0, "No members returned"

      for nodeData in result:
         if nodeData["hostname"] == hostName:
            testTimeResumed = int(nodeData["millis_since_last_message"])
            assert testTimeResumed < testTime, "Expected millis_since_last_message " \
               "to be less than {}, received {}.".format(reportedSilentTime, finalTime)
   finally:
      suiteObject._resumeMembers(allMembers)


@pytest.mark.smoke
def test_blockList_noNextField_allBlocks(restRequest):
   '''
   Cause no "next" paging by requesting all blocks.
   '''
   blockchainId = suiteObject.getABlockchainId(restRequest)
   ensureEnoughBlocksForPaging(restRequest, blockchainId)
   latestBlock = getLatestBlockNumber(restRequest, blockchainId)
   result = restRequest.getBlockList(blockchainId, count=latestBlock+1)
   assert "next" not in result, \
      "There should not be a 'next' field when requesting all blocks."


@pytest.mark.smoke
def test_blockList_noNextField_firstBlock(restRequest):
   '''
   Cause no "next" paging by requesting the genesis block.
   '''
   blockchainId = suiteObject.getABlockchainId(restRequest)
   result = restRequest.getBlockList(blockchainId, latest=0)
   assert "next" not in result, \
      "There should not be a 'next' field when latest is 0."


@pytest.mark.smoke
def test_newBlocks_onePage(restRequest):
   '''
   Add a bunch of blocks and get them all back in a page which is
   larger than the default.
   '''
   blockchainId = suiteObject.getABlockchainId(restRequest)
   addBlocksAndSearchForThem(restRequest, blockchainId, 11, 11)


@pytest.mark.smoke
def test_newBlocks_spanPages(restRequest):
   '''
   Add multiple blocks and get them all back via checking many small pages.
   '''
   blockchainId = suiteObject.getABlockchainId(restRequest)
   addBlocksAndSearchForThem(restRequest, blockchainId, 5, 2)


@pytest.mark.smoke
def test_pageSize_zero(restRequest):
   blockchainId = suiteObject.getABlockchainId(restRequest)
   ensureEnoughBlocksForPaging(restRequest, blockchainId)
   result = restRequest.getBlockList(blockchainId, count=0)
   assert len(result["blocks"]) == 0, "Expected zero blocks returned."


@pytest.mark.smoke
def test_pageSize_negative(restRequest):
   blockchainId = suiteObject.getABlockchainId(restRequest)
   ensureEnoughBlocksForPaging(restRequest, blockchainId)
   result = restRequest.getBlockList(blockchainId, count=-1)
   assert len(result["blocks"]) == defaultBlocksInAPage, "Expected {} blocks returned.".format(defaultBlocksInAPage)


@pytest.mark.smoke
def test_pageSize_exceedsBlockCount(restRequest):
   blockchainId = suiteObject.getABlockchainId(restRequest)
   ensureEnoughBlocksForPaging(restRequest, blockchainId)
   blockCount = getLatestBlockNumber(restRequest, blockchainId) + 1
   result = restRequest.getBlockList(blockchainId, count=blockCount+1)
   assert len(result["blocks"]) == blockCount, "Expected {} blocks returned.".format(blockCount)


@pytest.mark.smoke
def test_paging_latest_negative(restRequest):
   blockchainId = suiteObject.getABlockchainId(restRequest)
   ensureEnoughBlocksForPaging(restRequest, blockchainId)
   highestBlockNumber = getLatestBlockNumber(restRequest, blockchainId)
   result = restRequest.getBlockList(blockchainId, latest=-1)
   assert result["blocks"][0]["number"] == highestBlockNumber, \
      "Expected the latest block to be {}".format(highestBlockNumber)


@pytest.mark.smoke
def test_paging_latest_exceedsBlockCount(restRequest):
   blockchainId = suiteObject.getABlockchainId(restRequest)
   ensureEnoughBlocksForPaging(restRequest, blockchainId)
   highestBlockNumber = getLatestBlockNumber(restRequest, blockchainId)
   result = restRequest.getBlockList(blockchainId, latest=highestBlockNumber+1)
   assert result["blocks"][0]["number"] == highestBlockNumber, \
      "Expected the latest block to be {}".format(highestBlockNumber)


@pytest.mark.smoke
def test_blockIndex_negative(restRequest):
   blockchainId = suiteObject.getABlockchainId(restRequest)
   checkInvalidIndex(restRequest, blockchainId, -1, "Invalid block number or hash")


@pytest.mark.smoke
def test_blockIndex_outOfRange(restRequest):
   blockchainId = suiteObject.getABlockchainId(restRequest)
   latestBlockNumber = getLatestBlockNumber(restRequest, blockchainId)
   checkInvalidIndex(restRequest, blockchainId, latestBlockNumber+1, "block not found")


# @pytest.mark.smoke
# %5c (backslash) causes HTTP/1.1 401 Unauthorized.  Why?  Is that a bug?
# Filed as VB-800.
# def test_blockIndex_backslash(restRequest):
#    blockchainId = suiteObject.getABlockchainId(restRequest)
#    checkInvalidIndex(restRequest, blockchainId, "%5c", "Invalid block number or hash")


@pytest.mark.smoke
def test_blockIndex_atSymbol(restRequest):
   blockchainId = suiteObject.getABlockchainId(restRequest)
   checkInvalidIndex(restRequest, blockchainId, "%40", "Invalid block number or hash")


@pytest.mark.smoke
def test_blockIndex_word(restRequest):
   blockchainId = suiteObject.getABlockchainId(restRequest)
   checkInvalidIndex(restRequest, blockchainId, "elbow", "Invalid block number or hash")


@pytest.mark.smoke
def test_blockIndex_zero(restRequest):
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
   genObject = loadGenesisJson()
   blockchainId = suiteObject.getABlockchainId(restRequest)
   block = restRequest.getBlockByNumber(blockchainId, 0)
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
      fullTx = restRequest.getTransaction(tx["hash"])
      foundAccounts.append(trimHexIndicator(fullTx["to"]))

      assert fullTx["block_number"] == 0, \
         "Transaction in block 0 is listed as being in {}".format(fullTx["block_number"])
      assert int(fullTx["from"], 16) == 0, \
         "Expected initial alloc of ether to be from account 0"
      assert fullTx["hash"] == tx["hash"], \
         "Hash field in transaction {} was {}".format(tx["hash"], fullTx["hash"])
      assert fullTx["status"] == 1, "Expected a status of 1"

      # Genesis.json specifies the opening balance in hex and dec.
      expectedBalance = genObject["alloc"][trimHexIndicator(fullTx["to"])]["balance"]
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
def test_blockIndex_basic(restRequest):
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
   blockchainId = suiteObject.getABlockchainId(restRequest)
   txResponses = addBlocks(restRequest, blockchainId, numBlocks)
   parentHash = None

   for txResponse in txResponses:
      block = restRequest.getBlockByNumber(blockchainId, int(txResponse["blockNumber"], 16))
      assert block["number"] == int(txResponse["blockNumber"], 16), "Number is not correct."
      assert block["hash"] == txResponse["blockHash"], "Hash is not correct."

      if parentHash:
         assert block["parentHash"] == parentHash, "Parent hash is not correct."

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
