#########################################################################
# Copyright 2019-2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
# Ethereum tests related helper file
import os
from web3 import Web3, HTTPProvider
from util.auth import getAccessToken
import util.hermes_logging

# Contract directory for loadContract function
CONTRACTS_DIR = "resources/contracts"
log = util.hermes_logging.getMainLogger()


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


def getWeb3Instance(ethrpcApiUrl):
    '''
    Connect the web3 framework to an ethrpc node
    '''
    return Web3(HTTPProvider(
        ethrpcApiUrl,
        request_kwargs={
            'headers': {'Authorization': 'Bearer {0}'.format(getAccessToken())},
            'verify': False,
            'timeout': 60
        }
    ))


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
    Internal function called from requireDATAFields
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
    Validate the data fields
    '''
    for f in fieldList:
        if not isDATA(ob[f]):
            return (False, f)
    return (True, None)


def isQUANTITY(value):
    '''
    Internal function called from requireQUANTITYFields
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
    Validate the quantity fields
    '''
    for f in fieldList:
        if not isQUANTITY(ob[f]):
            return (False, f)
    return (True, None)
