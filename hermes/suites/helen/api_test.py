#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import json
import pytest
import queue
import sys

from suites.case import describe, passed, failed

from fixtures.common_fixtures import fxBlockchain, fxConnection, fxHermesRunSettings, fxInitializeOrgs, fxProduct
import util.auth
import util.blockchain.eth
import util.helen.validators
import util.helper
import util.numbers_strings

# For hermes/lib/persephone, used for saving streamed events and deleting them.
sys.path.append('lib')

import util.hermes_logging
log = util.hermes_logging.getMainLogger()

defaultTokenDescriptor = util.auth.getTokenDescriptor(util.auth.ROLE_CON_ADMIN,
                                                      True,
                                                      util.auth.internal_admin)


@describe()
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

    util.helen.validators.verifyContractCreationTx(fxConnection.request, fxBlockchain.blockchainId, contractCreationTx)
    util.helen.validators.verifyContractInvocationTx(fxConnection.request, fxBlockchain.blockchainId, contractCreationTx,
                               contractInvocationTx)


@describe()
@pytest.mark.smoke
def test_transactionHash_invalid_zero(fxConnection, fxBlockchain):
    '''
    Submit an invalid value for the transaction.
    '''
    invalidTx = fxConnection.request.getTransaction(fxBlockchain.blockchainId, "0")
    assert len(invalidTx) == 0, "Invalid transaction ID should return an empty set."


@describe()
@pytest.mark.smoke
def test_transactionHash_invalid_negOne(fxConnection, fxBlockchain):
    '''
    Submit an invalid value for the transaction.
    '''
    invalidTx = fxConnection.request.getTransaction(fxBlockchain.blockchainId, "-1")
    assert len(invalidTx) == 0, "Invalid transaction ID should return an empty set."


@describe()
@pytest.mark.smoke
def test_transactionHash_invalid_tooLong(fxConnection, fxBlockchain):
    '''
    Submit an invalid value for the transaction.
    '''
    invalidTx = fxConnection.request.getTransaction(fxBlockchain.blockchainId, "0xc5555c44eabcc1fcf93ca1b69bcc2a56a4960bc1380fcbb2121eca5ba6aa6f41a")
    assert len(invalidTx) == 0, "Invalid transaction ID should return an empty set."


@describe()
@pytest.mark.smoke
@pytest.mark.skip(reason="Unlike blocks, tx count cannot exceed ten, so this is an invalid test. Probably.")
def test_transactionList_noNextField_allTransactions(fxConnection, fxBlockchain):
    '''
    Cause there to be no "next" field by requesting all transactions.
    '''
    # txResponses = util.blockchain.eth.addBlocks(fxConnection.request, fxConnection.rpc, fxBlockchain.blockchainId, 1)
    # txList = fxConnection.request.getTransactionList(fxBlockchain.blockchainId, count=12)
    pass


@describe()
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


@describe()
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


@describe()
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


@describe()
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


@describe()
@pytest.mark.smoke
def test_transactionList_count(fxConnection, fxBlockchain):
    txReceipt = util.blockchain.eth.addBlocks(fxConnection.request, fxConnection.rpc, fxBlockchain.blockchainId, 1)[0]
    txList = fxConnection.request.getTransactionList(fxBlockchain.blockchainId, count=1)
    txList = txList['transactions']

    assert (len(txList) == 1 and txList[0]['hash'] == txReceipt['transactionHash']), \
        "Trasaction list response did not follow count parameter."


@describe()
@pytest.mark.smoke
def test_transactionList_pageSize_zero(fxConnection, fxBlockchain):
    _test_transactionList_pageSize_invalid(fxConnection, fxBlockchain, 0)


@describe()
@pytest.mark.smoke
def test_transactionList_pageSize_negative(fxConnection, fxBlockchain):
    _test_transactionList_pageSize_invalid(fxConnection, fxBlockchain, -1)


@describe()
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

@describe()
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


@describe()
@pytest.mark.smoke
def test_transactionList_max_size(fxConnection, fxBlockchain):
    util.blockchain.eth.addBlocks(fxConnection.request, fxConnection.rpc, fxBlockchain.blockchainId, util.blockchain.eth.defaultTxInAPage+1)
    txList = fxConnection.request.getTransactionList(fxBlockchain.blockchainId, count=1000)
    txList = txList['transactions']
    assert len(txList) == util.blockchain.eth.defaultTxInAPage, \
        "Expected maximum page size to be {}".format(util.blockchain.eth.defaultTxInAPage)


@describe()
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

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# HELEN CERTS TESTS
# TODO
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

@describe()
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

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# HELEN BLOCKHASH TESTS
# TODO
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

@describe()
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


@describe()
@pytest.mark.smoke
def test_invalidBlockHash(fxConnection, fxBlockchain):
    try:
        block = fx.request.getBlockByUrl("/api/concord/blocks/0xbadbeef")
        assert False, "invalid block hash exception should be thrown"
    except Exception as e:
        pass


@describe()
@pytest.mark.smoke
@pytest.mark.skip(reason="BC-2131")
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

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# DAML DEPLOYMENT TESTS
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

@describe()
@pytest.mark.deployment_only
def test_daml_deployment(fxConnection, fxBlockchain, fxHermesRunSettings):

    # APIs tested
    # /blockchains/

    blockchain_type = fxHermesRunSettings["hermesCmdlineArgs"].blockchainType.lower()
    blockchain_location = fxHermesRunSettings["hermesCmdlineArgs"].blockchainLocation.lower()

    if blockchain_type == util.helper.TYPE_DAML and \
            blockchain_location in [util.helper.LOCATION_SDDC, util.helper.LOCATION_ONPREM]:
        replicas_response = fxConnection.request.getReplicas(fxBlockchain.blockchainId)
        participants_response = fxConnection.request.get_participant_details(fxBlockchain.blockchainId)
        with open(util.helper.REPLICAS_JSON_PATH, "r") as replicas_fp:
            replicas_file_contents = json.load(replicas_fp)

            # Basic assertions
            assert util.helper.TYPE_DAML_COMMITTER in replicas_file_contents, "{} entry not present in {}" \
                .format(util.helper.TYPE_DAML_COMMITTER, util.helper.REPLICAS_JSON_PATH)
            assert util.helper.TYPE_DAML_PARTICIPANT in replicas_file_contents, "{} entry not present in {}" \
                .format(util.helper.TYPE_DAML_PARTICIPANT, util.helper.REPLICAS_JSON_PATH)

            # Committer verifications
            committer_ips = [c["private_ip"] for c in replicas_file_contents[util.helper.TYPE_DAML_COMMITTER]]
            for committer in replicas_response:
                if committer["private_ip"] not in committer_ips:
                    raise Exception("Committer {} not found in {}".format(committer["private_ip"],
                                                                          util.helper.REPLICAS_JSON_PATH))
            log.info("Committers verifications successful")

            # Participant verifications
            participant_ips = [p["private_ip"] for p in replicas_file_contents[util.helper.TYPE_DAML_PARTICIPANT]]
            for participant in participants_response:
                if participant["private_ip"] not in participant_ips:
                    raise Exception("Participant {} not found in {}".format(participant["private_ip"],
                                                                            util.helper.REPLICAS_JSON_PATH))
            log.info("Participants verifications successful")

            log.info("DAML deployment on SDDC is successful")
    else:
        log.warning("blockchainType must be {} and blockchainLocation must be onprem or sddc - Test skipped"
                    .format(blockchain_type, blockchain_location))

@describe()
@pytest.mark.deployment_only
def test_blockchain_metadata(fxConnection, fxBlockchain, fxHermesRunSettings):

    blockchain_type = fxHermesRunSettings["hermesCmdlineArgs"].blockchainType.lower()
    blockchain_location = fxHermesRunSettings["hermesCmdlineArgs"].blockchainLocation.lower()

    if blockchain_type == util.helper.TYPE_DAML and \
            blockchain_location in [util.helper.LOCATION_SDDC, util.helper.LOCATION_ONPREM]:
        blockchain_id = fxBlockchain.blockchainId
        req = fxConnection.request.newWithToken(defaultTokenDescriptor)

        blockchain = req.getBlockchainDetails(blockchain_id)

        log.info(blockchain["version"])

        assert "Blockchain Version" in blockchain["version"]

@describe()
@pytest.mark.deployment_only
def test_getClientsDamlBlockchain(fxConnection, fxBlockchain, fxHermesRunSettings):
    '''
    Test for no participants/clients
    '''
    blockchain_id = fxBlockchain.blockchainId
    clients = fxConnection.request.get_participant_details(blockchain_id)

    util.helen.validators.validateClientListResponse(clients)
    log.debug("client nodes in api_test {}".format(fxBlockchain.client_nodes))
    # Check Client group name.
    if fxBlockchain.client_nodes and client["group_name"]:
        assert client["group_name"] == fxBlockchain.client_nodes["group_name"], "Client group name does not match."
