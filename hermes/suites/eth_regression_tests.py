#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Tests the special corner case scenarios which where discovered while
# running ethereum transactions on concord
#########################################################################
import argparse
import collections
import os
import re
import random
import pytest
import util.json_helper
import util.hermes_logging

from util.blockchain import eth as eth_helper
from fixtures.common_fixtures import fxHermesRunSettings, fxProduct, fxBlockchain, fxConnection
from util.auth import getAccessToken
from web3 import Web3, HTTPProvider
from suites.case import describe
from rpc.rpc_call import RPC
log = util.hermes_logging.getMainLogger()

pytest.ethereumMode = None
LocalSetupFixture = collections.namedtuple(
    "LocalSetupFixture", "rpc,productMode,ethrpcApiUrl,product")

# Constants used in the large transaction test.

# A known size to start with as the upper bound for a binary search for the
# point at which EthRPC rejects transactions. Ideally, EthRPC should reject
# transactions of this size, but the test will automatically use a larger size
# if it finds it doesn't.
MIN_ETHRPC_REJECTION_SIZE_TO_START_WITH = 70000

# Size after which to give up on the large transaction test in the event no
# EthRPC rejection can be found without testing above this size. This limit
# exits mostly to attempt the test from trying to generate transactions of
# arbitrary size and possibly running out of memory in the event Concord and
# EthRPC actually handle transactions much larger than Hermes has been given
# memory to handle.
MAX_TESTABLE_TRANSACTION_SIZE = 10000000

# Number of maximally-sized transactions admissible to EthRPC to send during the
# large transaction tests. This number should be designed such that it is
# sufficient to crash the entire Concord cluster or render it unresponsive in
# the event that poorly handled large transactions cause node crashes or
# resource leaks. At the very least, this number should exceed the number of
# nodes in the Concord cluster, the number of SBFT replicas, and the number of
# SBFT client proxies.
LARGE_TRANSACTIONS_TO_SEND = 100

# Contract directory for loadContract function
CONTRACTS_DIR = "resources/contracts"


@pytest.fixture(scope="function")
@describe("fixture; local setup for given test suite")
def fxLocalSetup(request, fxBlockchain, fxHermesRunSettings, fxProduct, fxConnection):
    args = fxHermesRunSettings["hermesCmdlineArgs"]
    if args.ethrpcApiUrl:
        ethrpcApiUrl = args.ethrpcApiUrl
    else:
        ethrpcApiUrl = eth_helper.getEthrpcApiUrl(
            fxConnection.request, fxBlockchain.blockchainId)
    pytest.ethereumMode = args.ethereumMode

    testName = fxConnection.request.testName
    userConfig = fxHermesRunSettings["hermesUserConfig"]
    testLogDir = os.path.join(
        fxHermesRunSettings["hermesTestLogDir"], testName)

    rpc = RPC(testLogDir,
              testName,
              ethrpcApiUrl,
              userConfig)
    return LocalSetupFixture(rpc=rpc, productMode=not args.ethereumMode, ethrpcApiUrl=ethrpcApiUrl,
                             product=fxProduct.product)


def _getWeb3Instance(localSetup):
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


def _loadContract(name):
    '''
    Return contract object to deploy and run queries on.

    Note: We assume that there is only one contract per file.

    Technically, the solidity file isn't required but should be there anyways
    for documentation.
    '''
    sol_path = "{}.sol".format(os.path.join(CONTRACTS_DIR, name))
    abi_path = "{}.abi".format(os.path.join(CONTRACTS_DIR, name))
    hex_path = "{}.hex".format(os.path.join(CONTRACTS_DIR, name))

    assert os.path.isfile(sol_path)
    assert os.path.isfile(abi_path)
    assert os.path.isfile(hex_path)

    with open(abi_path, "r") as f:
        abi = f.read()

    with open(hex_path, "r") as f:
        hex_str = f.read()

    return (abi.strip(), hex_str.strip())


# TODO - This will work once the invocation is direct and not through main.py file
@pytest.mark.skipif(pytest.ethereumMode == True, reason='Skip the test if running in Ethereum mode')
@describe()
def test_nested_contract_creation(fxLocalSetup):
    '''
    Submit a request to create a new contract which itself creates another contract
    '''
    if not fxLocalSetup.productMode:
        pytest.skip()
    else:
        from_addr = "0x61c5e2a298f40dbb2adee3b27c584adad6833bac"
        data = ("60606040525b60405161015b806102a08339018090506040518091039060"
                "00f0600160006101000a81548173ffffffffffffffffffffffffffffffff"
                "ffffffff021916908302179055505b610247806100596000396000f30060"
                "606040526000357c01000000000000000000000000000000000000000000"
                "00000000000000900480632ef9db1314610044578063e376787614610071"
                "57610042565b005b61005b6004803590602001803590602001506100ad56"
                "5b6040518082815260200191505060405180910390f35b61008860048035"
                "906020018035906020015061008a565b005b806000600050600084815260"
                "2001908152602001600020600050819055505b5050565b60006000600084"
                "846040518083815260200182815260200192505050604051809103902091"
                "50610120600160009054906101000a900473ffffffffffffffffffffffff"
                "ffffffffffffffff167f6164640000000000000000000000000000000000"
                "000000000000000000000000846101e3565b905060016000905490610100"
                "0a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffff"
                "ffffffffffffffffffffffffffffffff1681868660405180807f61646400"
                "000000000000000000000000000000000000000000000000000000008152"
                "602001506020018481526020018381526020018281526020019350505050"
                "6000604051808303816000866161da5a03f1915050506000600050600082"
                "81526020019081526020016000206000505492506101db565b5050929150"
                "50565b60004340848484604051808581526020018473ffffffffffffffff"
                "ffffffffffffffffffffffff166c01000000000000000000000000028152"
                "601401838152602001828152602001945050505050604051809103902090"
                "50610240565b9392505050566060604052610148806100136000396000f3"
                "0060606040526000357c0100000000000000000000000000000000000000"
                "00000000000000000090048063471407e614610044578063e37678761461"
                "007757610042565b005b6100616004803590602001803590602001803590"
                "602001506100b3565b6040518082815260200191505060405180910390f3"
                "5b61008e600480359060200180359060200150610090565b005b80600060"
                "00506000848152602001908152602001600020600050819055505b505056"
                "5b6000818301905080506100c684826100d5565b8090506100ce565b9392"
                "505050565b3373ffffffffffffffffffffffffffffffffffffffff168282"
                "60405180807f7265676973746572496e7400000000000000000000000000"
                "000000000000000081526020015060200183815260200182815260200192"
                "5050506000604051808303816000866161da5a03f1915050505b505056")
        gas = "0x47e7c4"
        val = "0000000000000000000000000000000000000000000000000000000000000000"
        txHash = fxLocalSetup.rpc.sendTransaction(
            from_addr, data, gas, value=val)

        assert txHash, "Transaction hash not received"

        txReceipt = fxLocalSetup.rpc.getTransactionReceipt(txHash)
        assert (txReceipt['status'] == '0x1' and txReceipt['contractAddress']
                ), "Transaction was not successful"


# TODO - This will work once the invocation is direct and not through main.py file
@pytest.mark.skipif(pytest.ethereumMode == True, reason='Skip the test if running in Ethereum mode')
@describe()
def test_invalid_addresses(fxLocalSetup):
    '''
    Submit transactions using bad addresses: too long, too short.
    '''
    if not fxLocalSetup.productMode:
        pytest.skip()
    else:
        valid_from = "0x1111111111111111111111111111111111111111"
        valid_to = "0x2222222222222222222222222222222222222222"

        long_from = valid_from + "33"
        long_to = valid_to + "44"

        short_from = valid_from[:len(valid_from)-2]
        short_to = valid_to[:len(valid_to)-2]

        bad_tests = [(valid_from, long_to),
                     (valid_from, short_to),
                     (long_from, valid_to),
                     (short_from, valid_to)]

        for (f, t) in bad_tests:
            try:
                fxLocalSetup.rpc.sendTransaction(
                    f, data="0x00", to=t, value="0x01")

                # This call should fail. If it gets here, we probably
                # silently discarded address bytes.
                assert False, "Invalid address allowed from=%s, to=%s" % (f, t)
            except:
                # Receiving an error message will arrive here. An error is
                # fine - we just need to make sure that concord is still up
                # afterward.
                pass

        # After all of that invalid stuff, a valid transaction should work
        txHash = fxLocalSetup.rpc.sendTransaction(
            valid_from, data="0x00", to=valid_to, value="0x01")
        assert txHash, "No transaction hash was returned"


# TODO - This will work once the invocation is direct and not through main.py file
@pytest.mark.skipif(pytest.ethereumMode == True, reason='Skip the test if running in Ethereum mode')
@describe()
def test_call_writer(fxLocalSetup):
    '''
    Submit an eth_call to a contract that modifies storage. concord
    should properly catch the exception, and stay up to handle
    requests afterward. In ATH-53, it was found that concord would
    exit in this case.
    '''
    if not fxLocalSetup.productMode:
        pytest.skip()
    else:
        from_addr = "0x61c5e2a298f40dbb2adee3b27c584adad6833bac"

        # This creates a contract that just writes the length of the
        # input data to storage when called.
        data = "600480600c6000396000f30036600055"
        gas = "0x47e7c4"
        txHash = fxLocalSetup.rpc.sendTransaction(from_addr, data, gas)

        assert txHash, "Contract transaction hash not received"

        txReceipt = fxLocalSetup.rpc.getTransactionReceipt(txHash)
        assert (txReceipt['status'] == '0x1' and txReceipt['contractAddress']
                ), "Contract creation failed"
        try:
            callData = "0x01"
            callResult = fxLocalSetup.rpc.callContract(txReceipt['contractAddress'],
                                                       callData)
            # this call should actually fail, because the
            # contract tried to write data
            assert False, "Contract call was allowed to write data"
        except:
            pass

        # Before ATH-53, the above call would crash concord, so
        # we now send a transaction to see if concord is still up

        sendData = "0x0102"
        sendHash = fxLocalSetup.rpc.sendTransaction(
            from_addr, sendData, to=txReceipt['contractAddress'])
        assert sendHash, "Contract send transaction failed"
        sendReceipt = fxLocalSetup.rpc.getTransactionReceipt(sendHash)

        assert sendReceipt['status'] == '0x1', "Contract send transaction failed"
        value = fxLocalSetup.rpc.getStorageAt(
            txReceipt['contractAddress'], "0x00")
        # "2" == length of sendData
        assert (value == "0x0000000000000000000000000000000000000000000000000000000000000002"), "Contract did not store correct data"


# TODO - This will work once the invocation is direct and not through main.py file
@pytest.mark.skipif(pytest.ethereumMode == True, reason='Skip the test if running in Ethereum mode')
@describe()
def test_large_transactions(fxLocalSetup):
    '''
    Tests that Concord does not appear to gracelessly handle any large
    transactions in ways that cause unexpected exceptions in EthRPC or crash
    Concord; this test is intended to detect regressions re-introducing a
    class of bugs we have made a fix to prevent, involving Concord partially
    or completely crashing upon being sent transactions in a size range just
    below the point at which EthRPC will reject the requests before they reach
    Concord.

    Specifically, this test expects the behavior that EthRPC will return
    exceptions complaining that requests are too large past a certain point,
    but will never throw an exception below that point, and that submitting a
    large number of transactions just below that point will not cause Concord
    to crash.
    '''
    if not fxLocalSetup.productMode:
        pytest.skip()
    else:
        # Get Web3 instance and load and deploy StringStorage contract.
        # Please see hermes/resources/contracts/StringStorage.sol for details
        # about this contract.
        w3 = _getWeb3Instance(fxLocalSetup)
        abi, bin = _loadContract("StringStorage")
        contract = w3.eth.contract(abi=abi, bytecode=bin)

        # Note that, in this test, we specify explicit values here for every
        # parameter to eth_sendTransaction except data; this is because this
        # test relies on sending specific sizes of transactions to EthRPC by
        # modulating the size of the string parameter it passes to the contract
        # function it is calling; we do not want to allow Web3 to introduce
        # variability in the request size by allowing it to pick things like
        # the gas and gas price.
        txArgs = {"from": "0x61C5E2A298f40DBB2adEE3b27C584AdAD6833BaC",
                  "gas": "0xFFFFFFFF",
                  "gasPrice": "0x1",
                  "value": 0}

        contractTx = contract.constructor().transact(txArgs)
        contractTxReceipt = w3.eth.waitForTransactionReceipt(contractTx)
        contract = w3.eth.contract(address=contractTxReceipt.contractAddress,
                                   abi=abi)

        # Check that the contract appears to be working.
        contract.functions.setString("Example string.").transact(txArgs)
        storedString = contract.functions.storedString().call()

        assert (storedString ==
                "Example string."), "String storage contract failed to store a string."

        # Search for the maximum size of transaction that EthRPC will allow
        # to go through to Concord.
        maximumAdmittedSize = 1
        minimumRejectedSize = MIN_ETHRPC_REJECTION_SIZE_TO_START_WITH

        # This test attempts to make minimal assumptions about the minimum size
        # EthRPC rejects, so it will bump up the limit it starts with if it
        # doesn't find transactions of this size to be rejected.
        confirmedRejection = False
        while ((not confirmedRejection) and (minimumRejectedSize <= MAX_TESTABLE_TRANSACTION_SIZE)):
            stringInput = "A" * minimumRejectedSize
            try:
                result = contract.functions.setString(
                    stringInput).transact(txArgs)

            # Note to detect size rejections from EthRPC, we examine the
            # exception message. This is necessary because Web3 can throw us
            # ValueErrors for several types of errors it gets back from EthRPC,
            # some of which imply non-graceful failures on Concord's part. Note
            # any exception other than the one we expect will be assumed to
            # result from Concord crashing or failing to respond to large
            # requests it gets from EthRPC and therefore grounds for failing
            # this test case.
            except ValueError as e:
                errorMsg = "Unexpected exception when sending transaction with parameter of size " + \
                    str(minimumRejectedSize) + ": " + str(e)
                assert re.search(
                    "[Tt]oo.*(([Ll]arge)|([Bb]ig))", str(e)), errorMsg
                confirmedRejection = True

            if (not confirmedRejection):
                wasMaximal = \
                    (minimumRejectedSize >= MAX_TESTABLE_TRANSACTION_SIZE)
                minimumRejectedSize *= 2
                if ((minimumRejectedSize > MAX_TESTABLE_TRANSACTION_SIZE)
                        and (not wasMaximal)):
                    minimumRejectedSize = MAX_TESTABLE_TRANSACTION_SIZE

        # Note we give up on this test if no transaction within
        # MAX_TESTABLE_TRANSACTION_SIZE is rejected by EthRPC; giving up on the
        # test in this case is interest of not attempting to handle the case
        # or generate transactions of arbitrary size when Concord and EthRPC
        # can handle transactions much larger than Hermes has been given the
        # memory to generate.
        assert confirmedRejection, "Unable to detect EthRPC transaction rejection within test size cap."

        while (maximumAdmittedSize < (minimumRejectedSize - 1)):
            mid = int((maximumAdmittedSize + minimumRejectedSize) / 2)
            rejected = False
            stringInput = "A" * mid
            try:
                result = contract.functions.setString(
                    stringInput).transact(txArgs)

            # For the same reasons as the except clause in the previous loop, we
            # must look at the exception message to determine whether we
            # consider this a graceful EthRPC rejection.
            except ValueError as e:
                errorMsg = "Unexpected exception when sending transaction with parameter of size " + \
                    str(minimumRejectedSize) + ": " + str(e)
                assert re.search(
                    "[Tt]oo.*(([Ll]arge)|([Bb]ig))", str(e)), errorMsg
                rejected = True

            if rejected:
                minimumRejectedSize = mid
            else:
                maximumAdmittedSize = mid

        errorMsg = "Condition: 'Minimum Rejected Size {} = Maximum Admitted Size {} + 1' is not satisfied ".format(
            minimumRejectedSize, maximumAdmittedSize)
        assert(maximumAdmittedSize == (minimumRejectedSize - 1)), errorMsg

        # Send some transactions of the largest size that EthRPC won't reject.
        # Concord may accept or reject these transactions, but it should give a
        # timely response and should not crash as a result of these
        # transactions.
        stringInput = "A" * maximumAdmittedSize
        for i in range(0, LARGE_TRANSACTIONS_TO_SEND):
            result = contract.functions.setString(stringInput).transact(txArgs)

        # Double-check that the Concord cluster still appears to be working.
        # Ideally, this test case should fail if sending the large transactions
        # above cause the Concord cluster to crash.
        contract.functions.setString("Final string.").transact(txArgs)
        storedString = contract.functions.storedString().call()

        assert (storedString == "Final string."), "Concord cluster does not seem to work any more after sending a number of large transactions."


@describe()
def test_zero_exit_code(fxLocalSetup):
    '''
    Stop a concord container, and make sure its exit code is
    zero. This makes sure that:

    1. We're not segfaulting on shutdown.

    2. We are actually catching and handling the TERM signal.

    The second case is tested by the fact that docker gives the
    container only a few seconds to shut down normally, and then sends
    SIGKILL instead of SIGTERM.
    '''
    # We used concord4 because it won't disrupt any other test if
    # this is run out of order.
    containerName = util.blockchain.eth.get_concord_container_name(4)
    assert len(containerName) != 0, "Concord4 was not found"

    assert fxLocalSetup.product.action_on_concord_container(
        containerName, "stop"), "Stop action failed"

    inspect = fxLocalSetup.product.inspect_container(containerName)
    state = inspect[0]['State']

    assert state['Status'] == 'exited', "Container did not exit (Status: {})".format(
        state['Status'])
    assert state['ExitCode'] == 0, "Exit code was non-zero ({})".format(
        state['ExitCode'])
