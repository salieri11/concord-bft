#########################################################################
# Copyright 2018 - 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Test the parts of the Ethereum JSON RPC API beyond what the
# EthCoreVmTests cover. This checks things like web3_sha3,
# eth_clientVersion, eth_mining, ...
#########################################################################
import collections
import os
import random
import re
import pytest
from fixtures.common_fixtures import fxHermesRunSettings, fxProduct, fxBlockchain, fxConnection
from util.blockchain import eth as eth_helper
from suites.case import describe
import util.helper
from util.auth import getAccessToken
from web3 import Web3, HTTPProvider
import util.hermes_logging

log = util.hermes_logging.getMainLogger()

LocalSetupFixture = collections.namedtuple(
    "LocalSetupFixture", "args,request,rpc,ethereumMode,productMode,productUserConfig,ethrpcApiUrl,blockchainId")

CONTRACTS_DIR = "resources/contracts"


@pytest.fixture(scope="function")
@describe("fixture; local setup for given test suite")
def fxLocalSetup(fxBlockchain, fxHermesRunSettings, fxProduct, fxConnection):
    '''
    Local fixture which takes existing common fixtures as input and returns the collection
    of various variables available for all the test functions.
    Improves code reusability.
    '''
    args = fxHermesRunSettings["hermesCmdlineArgs"]
    userConfig = fxHermesRunSettings["hermesUserConfig"]

    if args.ethrpcApiUrl:
        ethrpcApiUrl = args.ethrpcApiUrl
    else:
        ethrpcApiUrl = eth_helper.getEthrpcApiUrl(
            fxConnection.request, fxBlockchain.blockchainId)

    return LocalSetupFixture(args=args, request=fxConnection.request, rpc=fxConnection.rpc,
                             ethereumMode=args.ethereumMode, productMode=not args.ethereumMode,
                             productUserConfig=userConfig["product"],
                             ethrpcApiUrl=ethrpcApiUrl, blockchainId=fxBlockchain.blockchainId)


def getReverseProxyHttpProvider():
    '''
    Get reverse proxy http provider
    '''
    return HTTPProvider(
        "https://localhost/blockchains/local/api/concord/eth/",
        request_kwargs={
            'headers': {'Authorization': 'Bearer {0}'.format(getAccessToken())},
            'verify': False
        }
    )


def isDATA(value):
    '''
    Internal function called from requireDATAFields(). It will be moved to a common utility in near future.
    '''
    # Hex-encoded string
    if not isinstance(value, str):
        return False
    # Leading 0x
    if not value.startswith("0x"):
        return False
    # Even number of chars because it represents bytes
    if (len(value) % 2) != 0:
        return False
    return True


def requireDATAFields(ob, fieldList):
    '''
    Function to check if required data fields are available. It will be moved to a common utility in near future.
    '''
    for f in fieldList:
        if not isDATA(ob[f]):
            return (False, f)
    return (True, None)


def isQUANTITY(value):
    '''
    Internal function called from requireQUANTITYFields(). It will be moved to a common utility in near future.
    '''
    # Hex-encoded string
    if not isinstance(value, str):
        return False
    # Leading 0x
    if not value.startswith("0x"):
        return False
    # Valid number (will flag "0x")
    try:
        int(value, 16)
    except ValueError as e:
        return False
    # No leading 0s
    if len(value) > 3 and value[2] == "0":
        return False
    return True


def requireQUANTITYFields(ob, fieldList):
    '''
    Internal function called from requireQUANTITYFields(). It will be moved to a common utility in near future.
    '''
    for f in fieldList:
        if not isQUANTITY(ob[f]):
            return (False, f)
    return (True, None)


def getWeb3Instance(localSetup):
    '''
    Connect the web3 framework to an ethrpc node
    '''
    return Web3(HTTPProvider(
        localSetup.ethrpcApiUrl,
        request_kwargs={
            'headers': {'Authorization': 'Bearer {0}'.format(getAccessToken())},
            'verify': False,
            'timeout': 60
        }
    ))


def loadContract(name):
    '''
    Return contract object to deploy and run queries on.
    Note: We assume that there is only one contract per file.
    Technically, the solidity file isn't required but should be there anyways
    for documentation.
    '''
    sol_path = "{}.sol".format(os.path.join(CONTRACTS_DIR, name))
    abi_path = "{}.abi".format(os.path.join(CONTRACTS_DIR, name))
    hex_path = "{}.hex".format(os.path.join(CONTRACTS_DIR, name))

    assert os.path.isfile(sol_path), "Not a file : {}".format(sol_path)
    assert os.path.isfile(abi_path), "Not a file : {}".format(abi_path)
    assert os.path.isfile(hex_path), "Not a file : {}".format(hex_path)

    with open(abi_path, "r") as f:
        abi = f.read()

    with open(hex_path, "r") as f:
        hex_str = f.read()

    return (abi.strip(), hex_str.strip())


def _createBlockFilterAndSendTransactions(rpc, txCount):
    '''
    Setup a new block filter, and send txCount transactions that it
    should catch. txCount is assumed to be at least 1.
    '''
    # These are just dummy addresses, that could be passed as
    # parameters if needed in a future version
    addrFrom = "0x262c0d7ab5ffd4ede2199f6ea793f819e1abb019"
    addrTo = "0x5bb088f57365907b1840e45984cae028a82af934"
    transferAmount = "1"

    # Ethereum apps all create filters after submitting the
    # transaction that they expect the filter to catch. Mimic that
    # here, to make sure the filter actually returns the block with
    # this transaction.
    #
    # WARNING: this will be flaky with the time service in play. If
    # a time update is added between this transaction and the
    # creation of the filter, this transaction will not show up.
    transactions = [rpc.sendTransaction(addrFrom,
                                        data=None,
                                        to=addrTo,
                                        value=transferAmount)]

    # create the block filter now, so it sees our transactions
    filter = rpc.newBlockFilter()

    # submit a bunch of tranasactions to create a bunch of blocks
    for x in range(1, txCount):
        # data has to be set as None for transferring-fund kind of transaction
        transactions.append(rpc.sendTransaction(addrFrom,
                                                data=None,
                                                to=addrTo,
                                                value=transferAmount))

    return (filter, transactions)


def _readBlockFilterToEnd(rpc, filter):
    '''
    Read all of the blocks a filter currently matches. Returns the
    block hashes found before the poll for changes returns an
    empty list.
    '''
    lastBlock = rpc.getBlockNumber()
    lastBlockHash = rpc.getBlockByNumber(number=lastBlock)["hash"]

    doubleEmpty = False
    blocksCaught = []
    # now read until the filter says there's nothing more
    while True:
        result = rpc.getFilterChanges(filter)
        if len(result) == 0:
            # web3 doesn't expect an immediate update to a filter, so
            # during the first poll we return "no new blocks" - don't
            # stop yet, try one more read
            if doubleEmpty:
                # no more blocks to read
                break
            doubleEmpty = True

        # list is reversed because we want to search in order of
        # transactions later, and filter response has latest block
        # first
        blocksCaught.extend(reversed(result))

        # Time Service may continue adding blocks, so we need to stop
        # checking the filter if we've seen the block that was at the
        # end before we started reading.
        if lastBlockHash in result:
            break

    return blocksCaught


def _allTransactionBlocksCaught(request, blockchainId, transactions, blocks):
    '''
    Check that the blocks named by the transaction receipts are in the
    blocks list. This is used to verify that a list of blocks
    returned by a filter includes transactions we expect.
    '''

    # transactions and blocks should be in the same order, so we can
    # just iterate and skip blocks that aren't relatvant, instead of
    # having to start the search over again every time
    blocksCopy = blocks[:]
    for t in transactions[1:]:
        tx = request.getTransaction(blockchainId, t)

        found = False
        while blocksCopy:
            if tx["block_hash"] == blocksCopy.pop(0):
                found = True
                break

        if not found:
            return False

    # Because of timing updates, and the crazy way that most
    # ethereum clients create a filter *after* sending the
    # transaction that they want to catch in the filter, we may miss
    # the first transaction. This can't be helped, but let's warn
    # about it to find out how often it really happens.
    tx0 = request.getTransaction(blockchainId, transactions[0])
    if not tx0["block_hash"] in blocks:
        log.warn("First transaction missing from filter")

    return True


@describe("test block filter update")
def test_block_filter(fxLocalSetup):
    '''
    Check that a block filter sees updates
    '''
    testCount = 25

    (filter, transactions) = _createBlockFilterAndSendTransactions(
        fxLocalSetup.rpc, testCount)
    blocksCaught = _readBlockFilterToEnd(fxLocalSetup.rpc, filter)

    assert _allTransactionBlocksCaught(fxLocalSetup.request, fxLocalSetup.blockchainId, transactions,
                                       blocksCaught), "Expected %d blocks, but read %d from filter" % (
        testCount, len(blocksCaught))


@describe("test block filter independence")
def test_block_filter_independence(fxLocalSetup):
    '''
    Check that two block filters see updates independently.
    '''
    testCount1 = 5
    testCount2 = 5

    (filter1, transactions1) = _createBlockFilterAndSendTransactions(
        fxLocalSetup.rpc, testCount1)
    (filter2, transactions2) = _createBlockFilterAndSendTransactions(
        fxLocalSetup.rpc, testCount2)

    blocksCaught1 = _readBlockFilterToEnd(fxLocalSetup.rpc, filter1)

    assert (_allTransactionBlocksCaught(fxLocalSetup.request, fxLocalSetup.blockchainId, transactions1,
                                        blocksCaught1) and
            _allTransactionBlocksCaught(fxLocalSetup.request, fxLocalSetup.blockchainId, transactions2,
                                        blocksCaught1)), "Expected %d blocks, but read %d from filter1" % (
        testCount1 + testCount2, len(blocksCaught1))

    blocksCaught2 = _readBlockFilterToEnd(fxLocalSetup.rpc, filter2)

    assert (_allTransactionBlocksCaught(fxLocalSetup.request, fxLocalSetup.blockchainId, transactions2,
                                        blocksCaught2) and
            not _allTransactionBlocksCaught(fxLocalSetup.request, fxLocalSetup.blockchainId, transactions1,
                                            blocksCaught2)), "Expected %d blocks, but read %d from filter2" % (
        testCount2, len(blocksCaught2))


@describe("test block filter uninstall")
def test_block_filter_uninstall(fxLocalSetup):
    '''
    Check that a filter can't be found after uninstalling it
    '''
    filter = fxLocalSetup.rpc.newBlockFilter()
    result = fxLocalSetup.rpc.getFilterChanges(filter)
    success = fxLocalSetup.rpc.uninstallFilter(filter)
    try:
        fxLocalSetup.rpc.getFilterChanges(filter)
        assert False, "Deleted filter should not be found"
    except:
        assert True


@describe("test correct reporting of gas price")
def test_eth_estimateGas(fxLocalSetup):
    '''
    Check that gas price is reported correctly
    '''
    result = fxLocalSetup.rpc.estimateGas()

    errorMessage = "Expected ethereumMode to have 0x... gas price, but found '{}'".format(
        result)
    assert not fxLocalSetup.ethereumMode or (len(result) > 2), errorMessage

    errorMessage = "Expected product to have zero gas price, but found '{}'".format(
        result)
    assert not fxLocalSetup.productMode or (result == "0x0"), errorMessage


@describe("test contract fallback")
def test_fallback(fxLocalSetup):
    '''
    Check that a contract's fallback function is called if the data passed in a transaction
    does not match any of the other functions in the contract. For the contract in use,
    please refer resources/contracts/Counter.sol for the detail.
    '''

    contract_interface = {
        "abi": "[{\"constant\":false,\"inputs\":[{\"name\":\"x\",\"type\":\"int256\"}],\"name\":\"decrementCounter\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[{\"name\":\"x\",\"type\":\"int256\"}],\"name\":\"incrementCounter\",\"outputs\":[],\"payable\":true,\"stateMutability\":\"payable\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[],\"name\":\"getCount\",\"outputs\":[{\"name\":\"\",\"type\":\"int256\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"inputs\":[],\"payable\":true,\"stateMutability\":\"payable\",\"type\":\"constructor\"},{\"payable\":true,\"stateMutability\":\"payable\",\"type\":\"fallback\"}]",
        "bin": "60806040526000805560e7806100166000396000f30060806040526004361060525763ffffffff7c0100000000000000000000000000000000000000000000000000000000600035041663645962108114605a5780639867b4aa146071578063a87d942c14607a575b6103e8600055005b348015606557600080fd5b50606f600435609e565b005b606f60043560aa565b348015608557600080fd5b50608c60b5565b60408051918252519081900360200190f35b60008054919091039055565b600080549091019055565b600054905600a165627a7a72305820388f79153f456193bb5fb284fa52a73de823a1add68bbf8bf11023fc527ad60d0029"
    }

    wallet = {
        "address": "0000a12b3f3d6c9b0d3f126a83ec2dd3dad15f39",
        "id": "30e15474-1056-4316-b3d9-d2942a1397d6",
        "version": 3,
        "crypto": {
            "cipher": "aes-128-ctr",
            "ciphertext": "47a0f60dab255972bf5bf7f6c57ad119e6e0018df05a997b277b54335736ac21",
            "cipherparams": {"iv": "da55653a91e84b10043860bc3e995c47"},
            "kdf": "scrypt",
            "kdfparams": {"dklen": 32, "n": 262144, "p": 1, "r": 8,
                          "salt": "5aeee57524423766d08c643fca8d23da655614465895d352c57b506130a05ac9"},
            "mac": "4afa549ab91d0a3328df6b28ab62f723f30c699fa40848b1d75a15579b44aebc"
        }
    }
    # Password to decrypt the wallet
    password = "Test123456"
    # will trigger fallback function in this test, the fallback function will set the counter to 1000,
    # please refer resouces/contracts/Counter.sol for detail
    expectedCount = 1000

    user = fxLocalSetup.productUserConfig.get('db_users')[0]
    web3 = getWeb3Instance(fxLocalSetup)

    Counter = web3.eth.contract(
        abi=contract_interface['abi'], bytecode=contract_interface['bin'])
    private_key = web3.eth.account.decrypt(wallet, password)
    account = web3.eth.account.privateKeyToAccount(private_key)
    contract_tx = Counter.constructor().buildTransaction({
        'from': account.address,
        'nonce': web3.eth.getTransactionCount(account.address),
        'gas': 2000000,
        'gasPrice': web3.eth.gasPrice,
        'value': 10
    })
    signed = web3.eth.account.signTransaction(contract_tx, private_key)
    txResult = web3.eth.sendRawTransaction(signed.rawTransaction)
    assert txResult, "Transaction was not accepted"

    if not fxLocalSetup.productMode:
        log.warn("No verification done in ethereum mode")
    else:
        hexstring = txResult.hex()
        tx = web3.eth.getTransactionReceipt(hexstring)
        assert tx, "No transaction receipt found"

        assert "contractAddress" in tx, "Contract deployment failed."

        # Trigger the fallback function, the passed 'data' does not match any
        # function in the contract
        counter = web3.eth.contract(
            address=tx.contractAddress, abi=contract_interface["abi"])
        transaction = {
            'from': account.address,
            'to': tx.contractAddress,
            'value': 0,
            'gas': 2000000,
            'gasPrice': web3.eth.gasPrice,
            'nonce': web3.eth.getTransactionCount(account.address),
            'data': '0xff'}
        signed = web3.eth.account.signTransaction(transaction, private_key)
        txResult = web3.eth.sendRawTransaction(signed.rawTransaction)
        assert txResult, "Transaction was not accepted"

        count = counter.functions.getCount().call()
        assert count == expectedCount, "Did not trigger fallback function"


@describe("test ethereum balance")
def test_eth_getBalance(fxLocalSetup):
    addrFrom = "0x262c0d7ab5ffd4ede2199f6ea793f819e1abb019"
    addrTo = "0x5bb088f57365907b1840e45984cae028a82af934"
    transferAmount = 1

    previousBlockNumber = fxLocalSetup.rpc.getBlockNumber()
    # data has to be set as None for transferring-fund kind of transaction
    txResult = fxLocalSetup.rpc.sendTransaction(addrFrom,
                                                data=None,
                                                to=addrTo,
                                                value=hex(transferAmount))

    currentBlockNumber = fxLocalSetup.rpc.getBlockNumber()
    addrFromBalance = int(fxLocalSetup.rpc.getBalance(
        addrFrom, previousBlockNumber), 16)
    addrToBalance = int(fxLocalSetup.rpc.getBalance(
        addrTo, previousBlockNumber), 16)
    expectedAddrFromBalance = addrFromBalance - transferAmount
    expectedAddrToBalance = addrToBalance + transferAmount

    assert txResult, "Transaction was not accepted"

    if not fxLocalSetup.productMode:
        log.warn("No verification done in ethereum mode")
    else:
        tx = fxLocalSetup.request.getTransaction(
            fxLocalSetup.blockchainId, txResult)
        assert tx, "No transaction receipt found"

        # This is the important one: it tells whether signature address
        # recovery works.
        assert tx["from"] == addrFrom, "Found from does not match expected from"

        # The rest of these are just checking parsing.
        assert tx["to"] == addrTo, "Found to does not match expectd to"
        assert tx["value"] == hex(
            transferAmount), "Found value does not match expected value"
        assert expectedAddrFromBalance == int(
            fxLocalSetup.rpc.getBalance(addrFrom, currentBlockNumber),
            16), "sender balance does not match expected value"
        assert expectedAddrToBalance == int(
            fxLocalSetup.rpc.getBalance(addrTo, currentBlockNumber),
            16), "receiver balance does not match expected value"


@describe("test that blocks can be fetched by number")
def test_eth_getBlockByNumber(fxConnection):
    '''
    Check that blocks can be fetched by number.
    '''
    currentBlockNumber = fxConnection.rpc.getBlockNumber()
    latestBlock = fxConnection.rpc.getBlockByNumber("latest")
    dataFields = ["hash", "parentHash"]
    quantityFields = ["number", "timestamp", "gasLimit", "gasUsed"]
    expectedFields = dataFields + quantityFields
    (present, missing) = util.helper.requireFields(latestBlock, expectedFields)
    assert present, "No '{}' field in block response.".format(missing)

    (success, field) = requireDATAFields(latestBlock, dataFields)
    assert success, 'DATA expected for field "{}"'.format(field)

    (success, field) = requireQUANTITYFields(latestBlock, quantityFields)
    assert success, 'QUANTITY expected for field "{}"'.format(field)

    assert int(latestBlock["number"], 16) >= int(
        currentBlockNumber, 16), "Latest block is before current block number"

    currentBlock = fxConnection.rpc.getBlockByNumber(currentBlockNumber)

    assert currentBlock["number"] == currentBlockNumber, "Current block does not have current block number"

    # this gasLimit value is exactly as specified by --gas_limit param
    # of concord CLI
    assert currentBlock["gasLimit"] == "0x989680", "Gas limit isn't 0x989680"

    # Reminder that if time service is running, new blocks might be
    # added at any time, so predict something semi-far future to
    # keep this test stable
    futureBlockNumber = 1000 + int(currentBlockNumber, 16)

    try:
        futureBlock = fxConnection.rpc.getBlockByNumber(futureBlockNumber)
        assert False, "Expected an error for future block {},but received block {}".format(
            futureBlockNumber, futureBlock["number"])
    except:
        # requesting an uncommitted block should return an error
        pass


@describe("test ethereum code")
def test_eth_getCode(fxLocalSetup):
    contractTransaction = "0xf901628085051f4d5c0083419ce08080b9010f608060405234801561001057600080fd5b506104d260005560ea806100256000396000f30060806040526004361060525763ffffffff7c01000000000000000000000000000000000000000000000000000000006000350416634f2be91f811460575780636deebae314606b5780638ada066e14607d575b600080fd5b348015606257600080fd5b50606960a1565b005b348015607657600080fd5b50606960ac565b348015608857600080fd5b50608f60b8565b60408051918252519081900360200190f35b600080546001019055565b60008054600019019055565b600054905600a165627a7a72305820b827241483c0f1a78e00de3ba4a4cb1e67a03bf6fb9f5ecc0491712f7e0aeb8000291ca080c9884eefca39aece8d308136f3bc2b95e44bd812afc33a9d741fcacee2f874a0125e2c8cd8af9e32cdbabf525a2e69ba7ebdd16411314a9bfa1e6bf414db6122"
    expectedCode = "0x60806040526004361060525763ffffffff7c01000000000000000000000000000000000000000000000000000000006000350416634f2be91f811460575780636deebae314606b5780638ada066e14607d575b600080fd5b348015606257600080fd5b50606960a1565b005b348015607657600080fd5b50606960ac565b348015608857600080fd5b50608f60b8565b60408051918252519081900360200190f35b600080546001019055565b60008054600019019055565b600054905600a165627a7a72305820b827241483c0f1a78e00de3ba4a4cb1e67a03bf6fb9f5ecc0491712f7e0aeb800029"
    address = "0x7c29bd452fae057dfd4e44b746846e4293db5060"
    startBlockNumber = fxLocalSetup.rpc.getBlockNumber()
    fxLocalSetup.rpc.sendRawTransaction(contractTransaction)
    endingBlockNumber = fxLocalSetup.rpc.getBlockNumber()

    try:
        txResult = fxLocalSetup.rpc.getCode(address, startBlockNumber)
        assert False, "getCode at the block before the contract deployed should fail"
    except:
        pass

    txResult = fxLocalSetup.rpc.getCode(address, endingBlockNumber)
    txResultLatest = fxLocalSetup.rpc.getCode(address, "latest")

    assert txResult == expectedCode, "code does not match expected"
    assert txResultLatest == expectedCode, "code does not match expected"


@describe("test ethereum logs")
def test_eth_getLogs(fxLocalSetup):
    w3 = getWeb3Instance(fxLocalSetup)
    abi, bin = loadContract("SimpleEvent")
    contract = w3.eth.contract(abi=abi, bytecode=bin)
    tx_args = {"from": "0x09b86aa450c61A6ed96824021beFfD32680B8B64"}

    # Deploy contract
    contract_tx = contract.constructor().transact(tx_args)
    contract_txr = w3.eth.waitForTransactionReceipt(contract_tx)

    contract = w3.eth.contract(
        address=contract_txr.contractAddress, abi=abi)

    # Invoke function that generates an event which should be logged
    func = contract.get_function_by_name("foo")
    func_tx = func(w3.toInt(hexstr="0xdeadbeef")).transact(tx_args)
    func_txr = w3.eth.waitForTransactionReceipt(func_tx)

    # eth_getLogs()
    logsLatest = fxLocalSetup.rpc.getLogs()

    # eth_getLogs(blockHash)
    logs = fxLocalSetup.rpc.getLogs({"blockHash": func_txr.blockHash.hex()})

    # If the time service is publishing, the latest block might not
    # be the block that our transaction is in. If that's the case,
    # the logs will be empty, so skip this check.
    assert logsLatest == [] or logsLatest == logs, "getLogs() != getLogs(blockHash)"

    logFound = False
    for l in logs:
        # The first element is the event signature
        # The second is the first argument of the event
        if int(l["topics"][1], 16) == 0xdeadbeef:
            logFound = True
            assert True

    assert logFound == True, "Couldn't find log in block #" + \
                             str(func_txr.blockNumber)


@describe("test ethereum log addresses")
def test_eth_getLogs_addr(fxLocalSetup):
    w3 = getWeb3Instance(fxLocalSetup)
    abi, bin = loadContract("SimpleEvent")
    contract = w3.eth.contract(abi=abi, bytecode=bin)
    tx_args = {"from": "0x09b86aa450c61A6ed96824021beFfD32680B8B64"}

    # Deploy the contract twice to get two different contract addresses
    contract_tx = contract.constructor().transact(tx_args)
    contract_txr = w3.eth.waitForTransactionReceipt(contract_tx)
    caddr1 = contract_txr.contractAddress
    contract1 = w3.eth.contract(address=caddr1, abi=abi)

    contract_tx = contract.constructor().transact(tx_args)
    contract_txr = w3.eth.waitForTransactionReceipt(contract_tx)
    caddr2 = contract_txr.contractAddress
    contract2 = w3.eth.contract(address=caddr2, abi=abi)

    # Invoke both contracts to trigger events and generate logs
    func = contract1.get_function_by_name("foo")
    func_tx = func(w3.toInt(hexstr="0x0ff1ce")).transact(tx_args)
    func_txr = w3.eth.waitForTransactionReceipt(func_tx)
    assert func_txr.status == 1

    func = contract1.get_function_by_name("foo")
    func_tx = func(w3.toInt(hexstr="0x0ff1cef001")).transact(tx_args)
    func_txr = w3.eth.waitForTransactionReceipt(func_tx)
    assert func_txr.status == 1

    func = contract2.get_function_by_name("foo")
    func_tx = func(w3.toInt(hexstr="0xba1d0ff1ce")).transact(tx_args)
    func_txr = w3.eth.waitForTransactionReceipt(func_tx)
    assert func_txr.status == 1

    # Get all logs for caddr1
    logs = fxLocalSetup.rpc.getLogs(
        {"fromBlock": "earliest", "address": caddr1})

    assert len(logs) == 2, "Expected two log entries for addr " + caddr1

    filter_out = list(
        filter(lambda l: w3.toChecksumAddress(l["address"]) == caddr1, logs))
    assert filter_out, "Couldn't find logs for addr " + caddr1
    assert len(filter_out) == len(
        logs), "Unexpected logs - only logs from {} expected".format(caddr1)

    # Get all logs for caddr2
    logs = fxLocalSetup.rpc.getLogs(
        {"fromBlock": "earliest", "address": caddr2})

    assert len(logs) == 1, "Expected one log entries for addr " + caddr1
    filter_out = list(
        filter(lambda l: w3.toChecksumAddress(l["address"]) == caddr2, logs))

    assert filter_out, "Couldn't find logs for addr " + caddr2
    assert len(filter_out) == len(
        logs), "Unexpected logs - only logs from {} expected".format(caddr2)


@describe("test block range")
def test_eth_getLogs_block_range(fxLocalSetup):
    w3 = getWeb3Instance(fxLocalSetup)
    abi, bin = loadContract("SimpleEvent")
    contract = w3.eth.contract(abi=abi, bytecode=bin)
    tx_args = {"from": "0x09b86aa450c61A6ed96824021beFfD32680B8B64"}

    # Deploy contract
    contract_tx = contract.constructor().transact(tx_args)
    contract_txr = w3.eth.waitForTransactionReceipt(contract_tx)

    contract = w3.eth.contract(
        address=contract_txr.contractAddress, abi=abi)

    # Issue two transactions (which generate two blocks)
    func = contract.get_function_by_name("foo")
    func_tx = func(w3.toInt(hexstr="0xdeadbeef")).transact(tx_args)
    foo_func_txr = w3.eth.waitForTransactionReceipt(func_tx)
    func_tx = func(w3.toInt(hexstr="0xc0ffee")).transact(tx_args)
    w3.eth.waitForTransactionReceipt(func_tx)
    func_tx = func(w3.toInt(hexstr="0x0ddba11")).transact(tx_args)
    oddball_func_txr = w3.eth.waitForTransactionReceipt(func_tx)

    # Let's get all logs
    logs_all = fxLocalSetup.rpc.getLogs({"fromBlock": "0x0"})
    logs_range = fxLocalSetup.rpc.getLogs(
        {"fromBlock": "earliest", "toBlock": "latest"})

    # Specifying "toBlock" is optional
    assert logs_all == logs_range, "getLogs(0) != getLogs(earliest, latest)"

    expected = [0xdeadbeef, 0xc0ffee, 0x0ddba11]
    for l in logs_all:
        # The first element is the event signature
        # The second is the first argument of the event
        if len(l["topics"]) == 2:
            val = int(l["topics"][1], 16)
            if val in expected:
                expected.remove(val)

    assert not expected, "From all logs: Couldn't find " + str(expected)

    # Let's omit the "0x0ddba11" and don't get everything
    from_block = int(foo_func_txr["blockNumber"])
    to_block = int(oddball_func_txr["blockNumber"]) - 1
    logs = fxLocalSetup.rpc.getLogs(
        {"fromBlock": str(from_block), "toBlock": str(to_block)})

    expected = [0xdeadbeef, 0xc0ffee]
    for l in logs:
        assert not (int(l["blockNumber"], 16) < from_block or int(
            l["blockNumber"], 16) > to_block), "Unexpected block"
        if len(l["topics"]) == 2:
            val = int(l["topics"][1], 16)
            if val in expected:
                expected.remove(val)

    assert not expected, "From log range: Couldn't find " + str(expected)


@describe("test ethereum log topics")
def test_eth_getLogs_topics(fxLocalSetup):
    w3 = getWeb3Instance(fxLocalSetup)
    abi, bin = loadContract("SimpleEvent")
    contract = w3.eth.contract(abi=abi, bytecode=bin)
    tx_args = {"from": "0x0000A12B3F3d6c9B0d3F126a83Ec2dd3Dad15f39"}

    event_signature = w3.sha3(text="Event(int256)").hex()

    # Deploy the contract twice to get two different contract addresses
    contract_tx = contract.constructor().transact(tx_args)
    contract_txr = w3.eth.waitForTransactionReceipt(contract_tx)
    caddr1 = contract_txr.contractAddress
    contract1 = w3.eth.contract(address=caddr1, abi=abi)

    contract_tx = contract.constructor().transact(tx_args)
    contract_txr = w3.eth.waitForTransactionReceipt(contract_tx)
    caddr2 = contract_txr.contractAddress
    contract2 = w3.eth.contract(address=caddr2, abi=abi)

    # Invoke "twoEvents" to trigger two logs in one transaction
    func = contract1.get_function_by_name("twoEvents")
    func_tx = func(w3.toInt(hexstr="0xc0ffee")).transact(tx_args)
    func_txr = w3.eth.waitForTransactionReceipt(func_tx)
    assert func_txr.status == 1
    logs = contract1.events.Event().processReceipt(func_txr)
    assert logs[0].args.value == w3.toInt(hexstr="0xc0ffef")
    assert logs[1].args.value == w3.toInt(hexstr="0xc0fff0")

    # The latest block should contain our events
    logs = fxLocalSetup.rpc.getLogs(
        {"fromBlock": hex(func_txr["blockNumber"]), "topics": [event_signature]})

    assert len(logs) == 2 and logs[0]["topics"][0] == event_signature and logs[1]["topics"][
        0] == event_signature, "Expected two logs in the transaction block with event sig " + event_signature

    # Let's get only one event from the two
    # The "twoEvents" function emits two events one with x+1 and the other with x+2
    param = "0x" + 29 * "00" + "c0ffef"
    logs = fxLocalSetup.rpc.getLogs(
        {"fromBlock": hex(func_txr["blockNumber"]), "topics": [event_signature, param]})

    assert len(
        logs) == 1 and logs[0]["topics"][1] == param, "Expected one log in the transaction block with 124"


@describe("test correct reporting of gas price")
def test_eth_gasPrice(fxLocalSetup):
    '''
    Check that gas price is reported correctly
    '''
    result = fxLocalSetup.rpc.gasPrice()

    errorMessage = "Expected ethereumMode to have 0x... gas price, but found '{}'".format(
        result)
    assert not fxLocalSetup.ethereumMode or (len(result) > 2), errorMessage

    errorMessage = "Expected product to have zero gas price, but found '{}'".format(
        result)
    assert not fxLocalSetup.productMode or (result == "0x0"), errorMessage


@describe("test ethereum storage")
def test_eth_getStorageAt(fxLocalSetup):
    '''
    here we use the Counter contract. We first deploy the Counter contract and
    then call subtract(). All the encoded transaction data
    (contractTransaction and decrementTx) is generated by web3j_3.5.0
    '''
    contractTransaction = "0xf901628085051f4d5c0083419ce08080b9010f608060405234801561001057600080fd5b506104d260005560ea806100256000396000f30060806040526004361060525763ffffffff7c01000000000000000000000000000000000000000000000000000000006000350416634f2be91f811460575780636deebae314606b5780638ada066e14607d575b600080fd5b348015606257600080fd5b50606960a1565b005b348015607657600080fd5b50606960ac565b348015608857600080fd5b50608f60b8565b60408051918252519081900360200190f35b600080546001019055565b60008054600019019055565b600054905600a165627a7a72305820b827241483c0f1a78e00de3ba4a4cb1e67a03bf6fb9f5ecc0491712f7e0aeb8000291ca04a8037443f6f4045acccda71496b9727311bac5ca8c6443c963137f1dadfc38ca056dc2e656776b082962e5452398090a0d0c3a671aca06b61253588334f44bb13"
    decrementTx = "0xf8690185051f4d5c0083419ce094cf98dacbe219c04942a876fff3dc657e731ae9ba80846deebae31ba0de2f19ce91d7abad46a21cb8a017da98f2dd96d32a1b9eb199e0587f89a78dffa024a2b103cd2203b85e1a96b18a153d51d3fd5e1f68b45d3ad31dd22f3de0237d"
    storageLocation = "0x0"
    expectedStartStorage = "0x00000000000000000000000000000000000000000000000000000000000004d2"
    expectedEndStorage = "0x00000000000000000000000000000000000000000000000000000000000004d1"

    txResult = fxLocalSetup.rpc.sendRawTransaction(contractTransaction)
    startBlockNumber = fxLocalSetup.rpc.getBlockNumber()
    assert txResult, "Transaction was not accepted"

    if not fxLocalSetup.productMode:
        log.warn("No verification done in ethereum mode")
    else:
        tx = fxLocalSetup.request.getTransaction(
            fxLocalSetup.blockchainId, txResult)

        assert tx, "No transaction receipt found"
        assert "contract_address" in tx, "No contract_address found. Was this run on an empty cluster?"

    contractAddress = tx["contract_address"]
    txResult = fxLocalSetup.rpc.sendRawTransaction(decrementTx)
    assert txResult, "Transaction was not accepted"

    endBlockNumber = fxLocalSetup.rpc.getBlockNumber()
    startStorage = fxLocalSetup.rpc.getStorageAt(contractAddress, storageLocation,
                                                 startBlockNumber)
    assert startStorage == expectedStartStorage, "start storage does not match expected"

    endStorage = fxLocalSetup.rpc.getStorageAt(contractAddress, storageLocation,
                                               endBlockNumber)
    assert endStorage == expectedEndStorage, "end storage does not match expected"


@describe("test ethereum transaction using hash")
def test_eth_getTransactionByHash(fxLocalSetup):
    '''
    Make sure the API is available and all expected fields are present.
    A proper semantic test should be done in a larger e2e test.
    '''
    block = fxLocalSetup.rpc.getBlockByNumber("latest")
    # The latest block may be a time-update with no transactions in
    # it. Look at an earlier block, if that's the case
    while not block["transactions"]:
        block = fxLocalSetup.rpc.getBlockByNumber(
            hex(int(block["number"], 16) - 1))
    txHash = random.choice(block["transactions"])

    tx = fxLocalSetup.rpc.getTransactionByHash(txHash)

    assert tx, "Failed to get transaction {}".format(txHash)

    dataFields = ["blockHash", "from", "hash", "input", "to", "r", "s"]
    quantityFields = ["blockNumber", "gas", "gasPrice", "nonce",
                      "transactionIndex", "value", "v"]
    expectedFields = dataFields + quantityFields

    (success, field) = util.helper.requireFields(tx, expectedFields)
    assert success, 'Field "{}" not found in getTransactionByHash'.format(
        field)

    (success, field) = requireDATAFields(tx, dataFields)
    assert success, 'DATA expected for field "{}"'.format(field)

    (success, field) = requireQUANTITYFields(tx, quantityFields)
    assert success, 'QUANTITY expected for field "{}"'.format(field)

    # TODO: Better end-to-end test for semantic evaluation of transactions
    assert block["hash"] == tx["blockHash"], "Block hash is wrong in getTransactionByHash: {}:{}".format(
        block["hash"], tx["blockHash"])

    assert txHash == tx["hash"], "Transaction hash is wrong in getTransactionByHash: {}:{}".format(
        txHash, tx["hash"])


@describe("test transaction count")
def test_eth_getTransactionCount(fxLocalSetup):
    '''
    Check that transaction count is updated.
    '''
    caller = fxLocalSetup.productUserConfig["users"][0]["hash"]
    previousBlockNumber = fxLocalSetup.rpc.getBlockNumber()

    txResult = fxLocalSetup.rpc.sendTransaction(caller,
                                                data="0x00",
                                                gas="0x01")

    startNonce = fxLocalSetup.rpc.getTransactionCount(
        caller, previousBlockNumber)

    assert startNonce, "Unable to get starting nonce"
    assert txResult, "Transaction was not accepted"

    endNonce = fxLocalSetup.rpc.getTransactionCount(caller)

    assert endNonce, "Unable to get ending nonce"

    assert int(endNonce, 16) - int(startNonce,
                                   16) == 1, "End nonce '{}' should be exactly one greater than start nonce {})".format(
        endNonce, startNonce)


@describe("test availibility of API and all expected fields")
def test_eth_getTransactionReceipt(fxLocalSetup):
    '''
    Make sure the API is available and all expected fields are present.
    '''
    block = fxLocalSetup.rpc.getBlockByNumber("latest")
    # The latest block may be a time-update with no transactions in
    # it. Look at an earlier block, if that's the case
    while not block["transactions"]:
        block = fxLocalSetup.rpc.getBlockByNumber(
            hex(int(block["number"], 16) - 1))
    txHash = random.choice(block["transactions"])

    tx = fxLocalSetup.rpc.getTransactionReceipt(txHash)

    assert tx, "Failed to get transaction {}".format(txHash)

    dataFields = ["transactionHash", "blockHash", "from", "to",
                  "contractAddress", "logsBloom"]
    quantityFields = ["transactionIndex", "blockNumber", "cumulativeGasUsed",
                      "gasUsed", "status"]
    expectedFields = dataFields + quantityFields + ["logs"]

    (success, field) = util.helper.requireFields(tx, expectedFields)

    assert success, 'Field "{}" not found in getTransactionByHash'.format(
        field)

    (success, field) = requireDATAFields(tx, dataFields)
    if not success:
        assert (field == "contractAddress" and tx["contractAddress"]
                is None), 'DATA expected for field "{}"'.format(field)

    (success, field) = requireQUANTITYFields(tx, quantityFields)
    assert success, 'QUANTITY expected for field "{}"'.format(field)

    assert isinstance(tx["logs"], list), 'Array expected for field "logs"'


@describe("test correct mining status report")
def test_eth_mining(fxLocalSetup):
    '''
    Check that mining status is reported correctly
    '''
    result = fxLocalSetup.rpc.mining()

    assert result == True if fxLocalSetup.ethereumMode is True else "Expected ethereumMode to be mining, but found '{}'".format(
        result)
    assert result == False if fxLocalSetup.productMode is True else "Expected product to not be mining, but found '{}'".format(
        result)


@describe("test correct account creation")
def test_personal_newAccount(fxLocalSetup):
    '''
    Check that account is created correctly
    '''
    user_id = fxLocalSetup.request.getUsers()[0]['user_id']
    user = fxLocalSetup.productUserConfig.get('db_users')[0]
    web3 = Web3(getReverseProxyHttpProvider())
    password = "123456"
    address = web3.personal.newAccount(password)
    wallet = fxLocalSetup.request.getWallet(user_id, address[2:].lower())
    private_key = web3.eth.account.decrypt(wallet, password)
    transaction = {
        'to': '0xF0109fC8DF283027b6285cc889F5aA624EaC1F55',
        'value': 0,
        'gas': 0,
        'gasPrice': 0,
        'nonce': 0,
        'chainId': 1}
    signed = web3.eth.account.signTransaction(transaction, private_key)
    txResult = web3.eth.sendRawTransaction(signed.rawTransaction)
    assert txResult, "Transaction was not accepted"

    if not fxLocalSetup.productMode:
        log.warn("No verification done in ethereum mode")
    else:
        hexstring = txResult.hex()
        tx = fxLocalSetup.request.getTransaction(
            fxLocalSetup.blockchainId, hexstring)
        assert tx, "No transaction receipt found"

        # Note that the there is no leading '0x' for address in wallet
        assert tx["from"][2:] == wallet['address'], "Found from does not match expected from"


@describe("test replay protection")
def test_replay_protection(fxLocalSetup):
    '''
    Check that transactions with incorrect chain IDs
    can't be replayed on blockchain
    '''
    user_id = fxLocalSetup.request.getUsers()[0]['user_id']
    user = fxLocalSetup.productUserConfig.get('db_users')[0]
    web3 = Web3(getReverseProxyHttpProvider())
    password = "123456"
    address = web3.personal.newAccount(password)
    wallet = fxLocalSetup.request.getWallet(user_id, address[2:].lower())
    private_key = web3.eth.account.decrypt(wallet, password)

    # Default VMware Blockchain ID is 1
    # By passing chain ID as 2, this transaction must fail
    # (https://github.com/ethereum/EIPs/blob/master/EIPS/eip-155.md#specification)
    try:
        transaction = {
            'to': '0xF0109fC8DF283027b6285cc889F5aA624EaC1F55',
            'value': 0,
            'gas': 0,
            'gasPrice': 0,
            'nonce': 0,
            'chainId': 2
        }
        signed = web3.eth.account.signTransaction(transaction, private_key)
        txResult = web3.eth.sendRawTransaction(signed.rawTransaction)
        log.debug("**** txResult: {}".format(txResult))
        assert False, "Transaction with incorrect chain ID was replayed"
    except:
        pass

    try:
        transaction = {
            'to': '0xF0109fC8DF283027b6285cc889F5aA624EaC1F55',
            'value': 0,
            'gas': 0,
            'gasPrice': 0,
            'nonce': 0,
            'chainId': 1
        }
        signed = web3.eth.account.signTransaction(transaction, private_key)
        txResult = web3.eth.sendRawTransaction(signed.rawTransaction)
    except:
        assert False, "Unsuccessful transaction with correct chain ID"


@describe("test correct decoding of raw transaction")
def test_eth_sendRawTransaction(fxLocalSetup):
    '''
    Check that a raw transaction gets decoded correctly.
    '''

    # known transaction from public ethereum
    # https://etherscan.io/tx/0x6ab11d26df13bc3b2cb1c09c4d274bfce325906c617d2bc744b45fa39b7f8c68
    rawTransaction = "0xf86b19847735940082520894f6c3fff0b77efe806fcc10176b8cbf71c6dfe3be880429d069189e00008025a0141c8487e4db65457266978a7f8d856b777a51dd9863d31637ccdec8dea74397a07fd0e14d0e3e891882f13acbe68740f1c5bd82a1a254f898cdbec5e9cfa8cf38"
    expectedHash = "0x6ab11d26df13bc3b2cb1c09c4d274bfce325906c617d2bc744b45fa39b7f8c68"
    expectedFrom = "0x42c4f19a097955ff2a013ef8f014977f4e8516c3"
    expectedTo = "0xf6c3fff0b77efe806fcc10176b8cbf71c6dfe3be"
    expectedValue = "0x429d069189e0000"  # 300000000000000000 Wei

    txResult = fxLocalSetup.rpc.sendRawTransaction(rawTransaction)
    assert txResult, "Transaction was not accepted"

    # if this test is re-run on a cluster, we'll see a different
    # hash (or an error once nonce tracking works); don't consider
    # it an error for now

    assert txResult == expectedHash, "Receipt hash != expected hash."

    if not fxLocalSetup.productMode:
        log.warn("No verification done in ethereum mode")
    else:
        tx = fxLocalSetup.request.getTransaction(
            fxLocalSetup.blockchainId, txResult)

        assert tx, "No transaction receipt found"

        # This is the important one: it tells whether signature address
        # recovery works.
        assert tx["from"] == expectedFrom, "Found from does not match expected from"

        # The rest of these are just checking parsing.
        assert tx["to"] == expectedTo, "Found to does not match expectd to"

        assert tx["value"] == expectedValue, "Found value does not match expected value"


@describe("test contract creation by a raw transaction")
def test_eth_sendRawContract(fxLocalSetup):
    '''
    Check that a raw transaction can create a contract. For the contract in use,
    please refer resources/contracts/Counter.sol for the detail.
    '''

    # Compiled abi & bin from contract resources/contracts/Counter.sol
    contract_interface = {
        "abi": "[{\"constant\":false,\"inputs\":[{\"name\":\"x\",\"type\":\"int256\"}],\"name\":\"decrementCounter\",\"outputs\":[],\"payable\":false,\"stateMutability\":\"nonpayable\",\"type\":\"function\"},{\"constant\":false,\"inputs\":[{\"name\":\"x\",\"type\":\"int256\"}],\"name\":\"incrementCounter\",\"outputs\":[],\"payable\":true,\"stateMutability\":\"payable\",\"type\":\"function\"},{\"constant\":true,\"inputs\":[],\"name\":\"getCount\",\"outputs\":[{\"name\":\"\",\"type\":\"int256\"}],\"payable\":false,\"stateMutability\":\"view\",\"type\":\"function\"},{\"inputs\":[],\"payable\":true,\"stateMutability\":\"payable\",\"type\":\"constructor\"},{\"payable\":true,\"stateMutability\":\"payable\",\"type\":\"fallback\"}]",
        "bin": "60806040526000805560e7806100166000396000f30060806040526004361060525763ffffffff7c0100000000000000000000000000000000000000000000000000000000600035041663645962108114605a5780639867b4aa146071578063a87d942c14607a575b6103e8600055005b348015606557600080fd5b50606f600435609e565b005b606f60043560aa565b348015608557600080fd5b50608c60b5565b60408051918252519081900360200190f35b60008054919091039055565b600080549091019055565b600054905600a165627a7a72305820388f79153f456193bb5fb284fa52a73de823a1add68bbf8bf11023fc527ad60d0029"
    }

    wallet = {
        "address": "0000a12b3f3d6c9b0d3f126a83ec2dd3dad15f39",
        "id": "30e15474-1056-4316-b3d9-d2942a1397d6",
        "version": 3,
        "crypto": {
            "cipher": "aes-128-ctr",
            "ciphertext": "47a0f60dab255972bf5bf7f6c57ad119e6e0018df05a997b277b54335736ac21",
            "cipherparams": {"iv": "da55653a91e84b10043860bc3e995c47"},
            "kdf": "scrypt",
            "kdfparams": {"dklen": 32, "n": 262144, "p": 1, "r": 8,
                          "salt": "5aeee57524423766d08c643fca8d23da655614465895d352c57b506130a05ac9"},
            "mac": "4afa549ab91d0a3328df6b28ab62f723f30c699fa40848b1d75a15579b44aebc"
        }
    }
    # Password to decrypt the wallet
    password = "Test123456"
    # will invoke incrementCounter founction, which will increase the counter to 1234
    expectedCount = 1234
    # 10wei (creating) + 10wei(function call)
    expectedBalance = 20

    user = fxLocalSetup.productUserConfig.get('db_users')[0]
    web3 = getWeb3Instance(fxLocalSetup)

    Counter = web3.eth.contract(
        abi=contract_interface['abi'], bytecode=contract_interface['bin'])
    private_key = web3.eth.account.decrypt(wallet, password)
    account = web3.eth.account.privateKeyToAccount(private_key)

    # Currently, we can feed a contract when creating it
    contract_tx = Counter.constructor().buildTransaction({
        'from': account.address,
        'nonce': web3.eth.getTransactionCount(account.address),
        'gas': 2000000,
        'gasPrice': web3.eth.gasPrice,
        'value': 10
    })
    signed = web3.eth.account.signTransaction(contract_tx, private_key)
    txResult = web3.eth.sendRawTransaction(signed.rawTransaction)

    assert txResult, "Transaction was not accepted"

    if not fxLocalSetup.productMode:
        log.warn("No verification done in ethereum mode")
    else:
        hexstring = txResult.hex()
        tx = web3.eth.getTransactionReceipt(hexstring)

        assert tx, "No transaction receipt found"
        assert "contractAddress" in tx, "Contract deployment failed."

        # Test function in the contract, as incrementCounter is payable,
        # we could pass ether to it
        counter = web3.eth.contract(
            address=tx.contractAddress, abi=contract_interface["abi"])
        counter.functions.incrementCounter(1234).transact({
            'from': account.address,
            'value': 10
        })

        count = counter.functions.getCount().call()
        assert count == expectedCount, "incrementCounter does not work, which means contract did not get deployed properly"

        balance = web3.eth.getBalance(tx.contractAddress)
        assert balance == expectedBalance, "Ether balance is incorrect, which means contract did not get deployed properly"


@describe("test correct syncing of state")
def test_eth_syncing(fxLocalSetup):
    '''
    Check that syncing state is reported correctly
    '''
    result = fxLocalSetup.rpc.syncing()

    # TODO: non-false result is also allowed, and indicates that the
    # node knows that it is behind. We don't expect nodes to be
    # running behind in this test right now.

    assert not result, "Expected node to not be syncing, but found '{}'".format(
        result)


@describe("test correct listing of RPC modules")
def test_rpc_modules(fxLocalSetup):
    '''
    Check that available RPC modules are listed correctly
    '''
    result = fxLocalSetup.rpc.modules()

    assert type(
        result) is collections.OrderedDict, "Reply should have been a dict."

    # This means the test is invalid, but let's not blow up because of it
    if len(result) == 0:
        log.warn("No RPC modules returned from rpc request.")

    # Insisting the version is <number>.<number>
    version_re = re.compile("\\d+\\.\\d+")

    for k, v in result.items():
        assert k in ["admin", "eth", "miner", "net", "personal", "rpc",
                     "web3"], "Response included unknown RPC module '{}'".format(k)

        assert version_re.match(
            v), "Module version should be version like, but was '{}'".format(v)


@describe("test expected working of hashing")
def test_web3_sha3(fxLocalSetup):
    '''
    Check that hashing works as expected.
    '''
    # list of (data, expected hash) tuples
    datahashes = [("0x", "0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"),
                  ("0x1234567890abcdef", "0xed8ab4fde4c4e2749641d9d89de3d920f9845e086abd71e6921319f41f0e784f")]

    for (d, h) in datahashes:
        result = fxLocalSetup.rpc.sha3(d)
        errorMessage = "Hash of '{}' did not match expected '{}': actual: '{}'".format(
            d, h, result)

        assert result == h, errorMessage


@describe("test valid client version")
def test_web3_clientVersion(fxLocalSetup):
    '''
    Check that we return a valid version
    '''
    result = fxLocalSetup.rpc.clientVersion()
    assert type(
        result) is str, "Client version should have been a string, but was '{}'".format(result)

    # Insisting version is
    # <name>/v<major>.<minor>.<patch><anything>/
    #    <os>/<language><major>.<minor>.<patch>
    version_re = re.compile("\\w+/v\\d+\\.\\d+\\.\\d+[^/]*/"
                            "[-a-zA-Z0-9]+/\\w+\\d\\.\\d\\.\\d")

    assert version_re.match(result), "Client version doesn't match expected format: '{}'".format(
        result)
