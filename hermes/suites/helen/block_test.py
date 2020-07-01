#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import pytest
from suites.case import describe, passed, failed

from fixtures.common_fixtures import fxBlockchain, fxConnection, fxHermesRunSettings, fxInitializeOrgs, fxProduct
import util.blockchain.eth
import util.helen.block
import util.numbers_strings

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# HELEN BLOCK TESTS
# TODO VALIDATION
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

@describe()
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


@describe()
@pytest.mark.smoke
def test_blockList_noNextField_firstBlock(fxConnection, fxBlockchain):
    '''
    Cause no "next" paging by requesting the genesis block.
    '''
    result = fxConnection.request.getBlockList(fxBlockchain.blockchainId, latest=0)
    assert "next" not in result, \
        "There should not be a 'next' field when latest is 0."


@describe()
@pytest.mark.smoke
def test_newBlocks_onePage(fxConnection, fxBlockchain):
    '''
    Add a bunch of blocks and get them all back in a page which is
    larger than the default.
    '''
    util.helen.block.addBlocksAndSearchForThem(fxConnection.request,
                                               fxBlockchain.blockchainId,
                                               fxConnection.rpc, 11, 11)


@describe()
@pytest.mark.smoke
def test_newBlocks_spanPages(fxConnection, fxBlockchain):
    '''
    Add multiple blocks and get them all back via checking many small pages.
    '''
    util.helen.block.addBlocksAndSearchForThem(fxConnection.request,
                                               fxBlockchain.blockchainId,
                                               fxConnection.rpc, 5, 2)


@describe()
@pytest.mark.smoke
def test_pageSize_zero(fxConnection, fxBlockchain):
    util.blockchain.eth.ensureEnoughBlocksForPaging(fxConnection, fxBlockchain.blockchainId)
    result = fxConnection.request.getBlockList(fxBlockchain.blockchainId, count=0)
    assert len(result["blocks"]) == 0, "Expected zero blocks returned."


@describe()
@pytest.mark.smoke
def test_pageSize_negative(fxConnection, fxBlockchain):
    util.blockchain.eth.ensureEnoughBlocksForPaging(fxConnection, fxBlockchain.blockchainId)
    result = fxConnection.request.getBlockList(fxBlockchain.blockchainId, count=-1)
    assert len(result["blocks"]) == util.blockchain.eth.defaultBlocksInAPage, \
        "Expected {} blocks returned.".format(util.blockchain.eth.defaultBlocksInAPage)


@describe()
@pytest.mark.smoke
def test_pageSize_exceedsBlockCount(fxConnection, fxBlockchain):
    util.blockchain.eth.ensureEnoughBlocksForPaging(fxConnection, fxBlockchain.blockchainId)
    blockCount = util.blockchain.eth.getLatestBlockNumber(fxConnection.request, fxBlockchain.blockchainId) + 1
    result = fxConnection.request.getBlockList(fxBlockchain.blockchainId, count=blockCount+1)
    assert len(result["blocks"]) == blockCount, "Expected {} blocks returned.".format(blockCount)


@describe()
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


@describe()
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


@describe()
@pytest.mark.smoke
def test_blockIndex_negative(fxConnection, fxBlockchain):
    util.helen.block.checkInvalidIndex(fxConnection.request, fxBlockchain.blockchainId, -1, "Invalid block number or hash")


@describe()
@pytest.mark.smoke
def test_blockIndex_outOfRange(fxConnection, fxBlockchain):
    latestBlockNumber = util.blockchain.eth.getLatestBlockNumber(fxConnection.request, fxBlockchain.blockchainId)
    # Time service may add blocks between these requests, so +10
    # accounts for a few rounds of that.
    util.helen.block.checkInvalidIndex(fxConnection.request, fxBlockchain.blockchainId, latestBlockNumber+10, "block not found")


# @pytest.mark.smoke
# %5c (backslash) causes HTTP/1.1 401 Unauthorized.  Why?  Is that a bug?
# Filed as VB-800.
# def test_blockIndex_backslash(fxConnection):
#
#    util.helen.block.checkInvalidIndex(fxConnection.request, fxBlockchain.blockchainId, "%5c", "Invalid block number or hash")


@describe()
@pytest.mark.smoke
def test_blockIndex_atSymbol(fxConnection, fxBlockchain):
    util.helen.block.checkInvalidIndex(fxConnection.request, fxBlockchain.blockchainId, "%40", "Invalid block number or hash")


@describe()
@pytest.mark.smoke
def test_blockIndex_word(fxConnection, fxBlockchain):
    util.helen.block.checkInvalidIndex(fxConnection.request, fxBlockchain.blockchainId, "elbow", "Invalid block number or hash")


@describe()
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


@describe()
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

        util.helen.block.checkTimestamp(txResponse["apiCallTime"], block["timestamp"])

        # We have one transaction per block, except block 0, which is handled
        # in a different use case.
        assert len(block["transactions"]) == 1, \
            "Expected one transaction per block (except block zero)"
        assert txResponse["transactionHash"] == block["transactions"][0]["hash"], \
            "The block's transaction hash does not match the transaction hash " \
            "given when the block was added."