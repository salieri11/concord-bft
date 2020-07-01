#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

from fixtures.common_fixtures import fxBlockchain, fxConnection, fxHermesRunSettings, fxInitializeOrgs, fxProduct
import util.blockchain.eth
import util.helper
import util.numbers_strings

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

