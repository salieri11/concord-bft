#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Tests covering Helen's non-ethereum ReST API.
# (i.e. everything under /api/, excluding /api/concord/eth)
#########################################################################
import logging
import os
import traceback
import pytest
import util.hermes_logging
import collections

from fixtures.common_fixtures import fxHermesRunSettings, fxProduct, fxBlockchain, fxConnection
from suites.case import describe
from rest.request import Request

log = util.hermes_logging.getMainLogger()
LocalSetupFixture = collections.namedtuple(
    "LocalSetupFixture", "testName, args, testLogDir")

contractBytecode_0_5_2 = ('608060405234801561001057600080fd5b5061013980610020600039'
                          '6000f3fe608060405234801561001057600080fd5b50600436106100'
                          '48576000357c01000000000000000000000000000000000000000000'
                          '000000000000009004806319ff1d211461004d575b600080fd5b6100'
                          '556100d0565b60405180806020018281038252838181518152602001'
                          '91508051906020019080838360005b83811015610095578082015181'
                          '84015260208101905061007a565b50505050905090810190601f1680'
                          '156100c25780820380516001836020036101000a0319168152602001'
                          '91505b509250505060405180910390f35b6060604080519081016040'
                          '5280600d81526020017f48656c6c6f2c20576f726c64210000000000'
                          '000000000000000000000000000081525090509056fea165627a7a72'
                          '305820cb955c3e65632e4e81915917d6892eecbbf51d49f3ae2e1aa9'
                          '454017d2a584b60029')

contractBytecode_0_4_19 = ('6060604052341561000f57600080fd5b6101578061001e600039600'
                           '0f300606060405260043610610041576000357c0100000000000000'
                           '000000000000000000000000000000000000000000900463fffffff'
                           'f16806319ff1d2114610046575b600080fd5b341561005157600080'
                           'fd5b6100596100d4565b60405180806020018281038252838181518'
                           '15260200191508051906020019080838360005b8381101561009957'
                           '808201518184015260208101905061007e565b50505050905090810'
                           '190601f1680156100c65780820380516001836020036101000a0319'
                           '16815260200191505b509250505060405180910390f35b6100dc610'
                           '117565b6040805190810160405280600d81526020017f48656c6c6f'
                           '2c20576f726c6421000000000000000000000000000000000000008'
                           '15250905090565b6020604051908101604052806000815250905600'
                           'a165627a7a723058202bf622d6a79b6a1044c23deae4f1c3e7b6fdd'
                           '6df49a8b4a03065078e60cfd06b0029')

contractBytecode_0_4_0 = ('606060405260a18060106000396000f360606040526000357c010000'
                          '00000000000000000000000000000000000000000000000000009004'
                          '806360fe47b11460435780636d4ce63c14605d57603f565b6002565b'
                          '34600257605b60048080359060200190919050506082565b005b3460'
                          '0257606c60048050506090565b604051808281526020019150506040'
                          '5180910390f35b806000600050819055505b50565b60006000600050'
                          '549050609e565b9056')


@pytest.fixture(scope="function")
@describe("fixture; local setup for given test suite")
def fxLocalSetup(request, fxBlockchain, fxHermesRunSettings, fxProduct, fxConnection):
    testName = fxConnection.request.testName
    args = fxHermesRunSettings["hermesCmdlineArgs"]
    testLogDir = os.path.join(
        fxHermesRunSettings["hermesTestLogDir"], testName)
    return LocalSetupFixture(testName=testName, args=args, testLogDir=testLogDir)


# This function will return a request object
def getRequest(localSetup):
    return Request(localSetup.testLogDir, localSetup.testName,
                   localSetup.args.contractCompilerApiBaseUrl, localSetup.args.config)


def contract_compile_util_generic(localSetup, sourceCode, compilerVersion):
    data = {}
    data["sourcecode"] = sourceCode
    data["compilerVersion"] = compilerVersion
    data["isOptimize"] = True
    data["runs"] = 200
    return getRequest(localSetup).compileContract(data)


def contract_compile_util(localSetup, compilerVersion, sourceCode):
    '''
    A helper method to upload simple hello world contract.
    '''
    return contract_compile_util_generic(localSetup, sourceCode, compilerVersion)


def compile_mock_contract(localSetup, compilerVersion=None, sourceCode=None):
    if compilerVersion is None:
        compilerVersion = "v0.5.2+commit.1df8f40c"
    if sourceCode is None:
        contractFile = open("resources/contracts/HelloWorld.sol", 'r')
        sourceCode = contractFile.read()
    result = contract_compile_util(
        localSetup, compilerVersion, sourceCode)
    if "data" in result:
        return result["data"]
    else:
        raise Exception(
            "Contract compilation failed with error '{}'".format(result["error"]))


def contract_verify_util_generic(localSetup, sourceCode, compilerVersion, existingBytecode, selectedContract):
    data = {}
    data["sourcecode"] = sourceCode
    data["compilerVersion"] = compilerVersion
    data["existingBytecode"] = existingBytecode
    data["selectedContract"] = selectedContract

    return getRequest(localSetup).verifyContract(data)


def contract_verify_util(localSetup, compilerVersion, sourceCode, existingBytecode, selectedContract):
    '''
    A helper method to verify a simple contract.
    '''
    return contract_verify_util_generic(localSetup, sourceCode, compilerVersion, existingBytecode, selectedContract)


def verify_mock_contract(localSetup, compilerVersion=None, selectedContract=None, sourceCode=None, existingBytecode=None):
    if compilerVersion is None:
        compilerVersion = "v0.5.2+commit.1df8f40c"
    if selectedContract is None:
        selectedContract = "HelloWorld"
    if sourceCode is None:
        contractFile = open("resources/contracts/HelloWorldMultiple.sol", 'r')
        sourceCode = contractFile.read()
    if existingBytecode is None:
        existingBytecode = contractBytecode_0_5_2
    result = contract_verify_util(localSetup, compilerVersion,
                                  sourceCode, existingBytecode, selectedContract)
    if "data" in result:
        return result["data"]
    else:
        raise Exception(
            "Contract verification failed with error '{}'".format(result["error"]))


# =============================================================================================
# Actual Test Functions
# =============================================================================================


@describe()
def test_contractCompilation(fxLocalSetup):
    result = compile_mock_contract(fxLocalSetup)

    assert "metadata" in result[0], "Contract failed to compile"


@describe()
def test_contractCompilation040(fxLocalSetup):
    # solc outputs are different for compiler versions <= 4.4 as well as <= 4.7.
    # Need to test compilation and response structure
    compilerVersion = "v0.4.0+commit.acd334c9"
    contractFile = open("resources/contracts/SimpleStorage-0.4.0.sol", 'r')
    sourceCode = contractFile.read()

    result = compile_mock_contract(
        fxLocalSetup, compilerVersion, sourceCode)

    assert result[0].get("metadata", {}).get("output", {}).get(
        "abi", False), "Contract failed to compile"


@describe()
def test_contractCompilationFailed(fxLocalSetup):
    compilerVersion = "v0.4.17+commit.bdeb9e52"

    try:
        compile_mock_contract(fxLocalSetup, compilerVersion)
    except Exception as e:
        assert "Source file requires different compiler version" in str(
            e), "Unexpected error when compiling contract"
    else:
        assert 1 == 2, "Failure when compiling contract with invalid compiler version"


@describe()
def test_contractVerification(fxLocalSetup):
    result = verify_mock_contract(fxLocalSetup)

    assert result["verified"] == True, "Bytecode failed to verify"


@describe()
def test_contractVerification0419(fxLocalSetup):
    # bytecode verification is different in compiler versions from 0.4.8 and 0.4.21.
    # Need to test a compiled file in this range
    compilerVersion = "v0.4.19+commit.c4cbbb05"
    result = verify_mock_contract(
        fxLocalSetup, compilerVersion, None, None, contractBytecode_0_4_19)

    assert result["verified"] == True, "Bytecode failed to verify"


@describe()
def test_contractVerification040(fxLocalSetup):
    # bytecode verification is different in compiler versions < 0.4.7.
    # Need to test a compiled file in this range
    compilerVersion = "v0.4.0+commit.acd334c9"
    contractFile = open("resources/contracts/SimpleStorage-0.4.0.sol", 'r')
    sourceCode = contractFile.read()
    selectedContract = "SimpleStorage"
    result = verify_mock_contract(
        fxLocalSetup, compilerVersion, selectedContract, sourceCode, contractBytecode_0_4_0)

    assert result["verified"] == True, "Bytecode failed to verify"


@describe()
def test_contractVerificationFailedMismatchedContract(fxLocalSetup):
    selectedContract = "DummyContract"
    result = verify_mock_contract(fxLocalSetup, None, selectedContract)

    assert result["verified"] == False, "Bytecode verified with mismatched contract"


@describe()
def test_contractVerificationFailedMismatchedCompiler(fxLocalSetup):
    compilerVersion = "v0.4.19+commit.c4cbbb05"
    result = verify_mock_contract(fxLocalSetup, compilerVersion)

    assert result["verified"] == False, "Bytecode verified with mismatched compiler version"


@describe()
def test_contractVerificationFailedToCompile(fxLocalSetup):
    compilerVersion = "v0.4.17+commit.bdeb9e52"

    try:
        verify_mock_contract(fxLocalSetup, compilerVersion)
    except Exception as e:
        assert "Source file requires different compiler version" in str(
            e), "Unexpected error when verifying contract"
    else:
        assert 1 == 2, "Failure when validating contract with invalid compiler version"


@describe()
def test_contractCompilationFailedInvalidFile(fxLocalSetup):
    try:
        compile_mock_contract(fxLocalSetup, None, "")
    except Exception as e:
        assert "Source file does not specify required compiler version" in str(
            e), "Unexpected error when compiling contract"
    else:
        assert 1 == 2, "Failure when validating with invalid source code"


@describe()
def test_contractCompilationFailedInvalidCompilerVersion(fxLocalSetup):
    try:
        compile_mock_contract(fxLocalSetup, "test")
    except Exception as e:
        assert "Error retrieving binary" in str(
            e), "Unexpected error when compiling contract"
    else:
        assert 1 == 2, "Failure when validating with invalid compiler version"


@describe()
def test_contractVerificationFailedInvalidFile(fxLocalSetup):
    try:
        verify_mock_contract(fxLocalSetup, None, None, "")
    except Exception as e:
        assert "Source file does not specify required compiler version" in str(
            e), "Unexpected error when verifying contract"
    else:
        assert 1 == 2, "Failure when validating with invalid source code"


@describe()
def test_contractVerificationFailedInvalidBytecode(fxLocalSetup):
    result = verify_mock_contract(fxLocalSetup, None, None, None, "")

    assert result["verified"] == False, "Bytecode verified with invalid existing bytecode"


@describe()
def test_contractVerificationFailedInvalidCompilerVersion(fxLocalSetup):
    try:
        verify_mock_contract(fxLocalSetup, "test")
    except Exception as e:
        assert "Error retrieving binary" in str(
            e), "Unexpected error when verifying contract"
    else:
        assert 1 == 2, "Failure when validating with invalid compiler version"
