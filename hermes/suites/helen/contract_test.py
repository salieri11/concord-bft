#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import pytest

from suites.case import describe, passed, failed

from fixtures.common_fixtures import fxBlockchain, fxConnection, fxInitializeOrgs, fxProduct
import util.blockchain.eth
import util.helen.validators
import util.helen.validators
import util.numbers_strings

# A user.  This will only work until CSP is implemented and
# concord starts requiring signed transactions.
fromUser = "0x1111111111111111111111111111111111111111"

# The compiler version passed to Helen when uploading contracts.
compilerVersion = "v0.5.2+commit.1df8f40c"

# Automation's contract and version IDs are generated with random_string_generator(),
# which creates strings of six uppercase characters and digits.
nonexistantContractId = "aaaaaaaaaaaaaaaaaaaa"
nonexistantVersionId = "bbbbbbbbbbbbbbbbbbbb"

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# HELEN CONTRACT TESTS
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

@describe()
@pytest.mark.smoke
@pytest.mark.contracts
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

    contract = afterContractList[0]
    # Validate the contract object is what we expect
    util.helen.validators.validateSingleContractFields(contract)

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


@describe()
@pytest.mark.smoke
@pytest.mark.contracts
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

    util.helen.validators.validateContractUploadResponseFields(contractResult, fxBlockchain.blockchainId,
                                                               contractId, contractVersion)

    contract = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)
    util.helen.validators.validateGetContractVersion(contract)

    result = fxConnection.rpc.callContract(contract["address"], data=util.blockchain.eth.helloFunction)
    assert util.blockchain.eth.helloHex in result, "Simple uploaded contract not executed correctly."


@describe()
@pytest.mark.smoke
@pytest.mark.contracts
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
    util.helen.validators.validateContractUploadResponseFields(contractResult, fxBlockchain.blockchainId,
                                                               contractId, contractVersion)

    contract = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)
    util.helen.validators.validateGetContractVersion(contract)

    callContractResult = fxConnection.rpc.callContract(contract["address"], data="0xa87d942c")
    assert int(callContractResult, 16) == 10, "Constructor value was not used."


@describe()
@pytest.mark.smoke
@pytest.mark.contracts
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
    util.helen.validators.validateContractUploadResponseFields(contractResult, fxBlockchain.blockchainId,
                                                               contractId, contractVersion)

    contract = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)
    util.helen.validators.validateGetContractVersion(contract)
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
    util.helen.validators.validateContractUploadResponseFields(contractResult, fxBlockchain.blockchainId,
                                                               contractId, contractVersion)

    contract = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)
    util.helen.validators.validateGetContractVersion(contract)
    optimizedBytecode1Run = contract["bytecode"]
    assert optimizedBytecode1Run != unoptimizedBytecode, "Bytecode was not optimized"


@describe()
@pytest.mark.smoke
@pytest.mark.contracts
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
    util.helen.validators.validateContractUploadResponseFields(contractResult, fxBlockchain.blockchainId,
                                                               contractId, contractVersion)


    contract = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)
    util.helen.validators.validateGetContractVersion(contract)

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
    util.helen.validators.validateContractUploadResponseFields(contractResult, fxBlockchain.blockchainId,
                                                               contractId, contractVersion)

    contract = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)
    util.helen.validators.validateGetContractVersion(contract)

    optimizedBytecode200Runs = contract["bytecode"]

    assert optimizedBytecode200Runs != optimizedBytecode1Run, \
        "Change in runs did not produce different optimized bytecode."


@describe()
@pytest.mark.smoke
@pytest.mark.contracts
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
    util.helen.validators.validateContractUploadResponseFields(contractResult, fxBlockchain.blockchainId,
                                                               contractId, contractVersion)

    contract = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)
    util.helen.validators.validateGetContractVersion(contract)

    assert util.blockchain.eth.helloHex in contract["bytecode"], "HelloWorld! is not present in bytecode."
    assert util.blockchain.eth.howdyHex not in contract["bytecode"], "HowdyWorld! should not be in the bytecode."


@describe()
@pytest.mark.smoke
@pytest.mark.contracts
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
    util.helen.validators.validateContractUploadResponseFields(contractResult, fxBlockchain.blockchainId, contractId, contractVersion)
    contract = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)
    util.helen.validators.validateGetContractVersion(contract)

    assert util.blockchain.eth.howdyHex in contract["bytecode"], "HowdyWorld! is not present in the bytecode."
    assert util.blockchain.eth.helloHex not in contract["bytecode"], "HelloWorld! should not be in the bytecode."


@describe()
@pytest.mark.smoke
@pytest.mark.contracts
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
    util.helen.validators.validateBadRequest(contractResult,
                                             "/api/blockchains/{}/concord/contracts".format(fxBlockchain.blockchainId))


@describe()
@pytest.mark.smoke
@pytest.mark.contracts
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
    util.helen.validators.validateBadRequest(contractResult,
                                             "/api/blockchains/{}/concord/contracts".format(fxBlockchain.blockchainId))


@describe()
@pytest.mark.smoke
@pytest.mark.contracts
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

    expectedPath = "/api/blockchains/{}/concord/contracts/{}/versions/{}".format(fxBlockchain.blockchainId,
                                                                                 contractId, contractVersion)
    contract = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)
    util.helen.validators.validateNotFound(contract,
                                           expectedPath = expectedPath,
                                           testErrorMessage = False)


@describe()
@pytest.mark.smoke
@pytest.mark.contracts
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

    util.helen.validators.validateBadRequest(contractResult,
                                             "/api/blockchains/{}/concord/contracts".format(fxBlockchain.blockchainId))


@describe()
@pytest.mark.smoke
@pytest.mark.contracts
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

    fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)
    util.helen.validators.validateBadRequest(contractResult,
                                             "/api/blockchains/{}/concord/contracts".format(fxBlockchain.blockchainId))


@describe()
@pytest.mark.smoke
@pytest.mark.contracts
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
    util.helen.validators.validateContractUploadResponseFields(contractResult, fxBlockchain.blockchainId,
                                                               contractId, contractVersion)

    contract = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)
    util.helen.validators.validateGetContractVersion(contract)

    result = fxConnection.rpc.callContract(contract["address"], data=util.blockchain.eth.helloFunction)
    assert util.blockchain.eth.helloHex in result, "Simple uploaded contract not executed correctly."


@describe()
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
    # util.helen.validators.validateBadRequest(contractResult,
    #                    "/api/blockchains/{}/concord/contracts".format(fxBlockchain.blockchainId))
    #   assert util.blockchain.eth.helloHex in result, "Simple uploaded contract not executed correctly."


@describe()
@pytest.mark.smoke
@pytest.mark.contracts
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
    util.helen.validators.validateBadRequest(contractResult,
                                             "/api/blockchains/{}/concord/contracts".format(fxBlockchain.blockchainId))


@describe()
@pytest.mark.smoke
@pytest.mark.contracts
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
    util.helen.validators.validateContractUploadResponseFields(contractResult, fxBlockchain.blockchainId,
                                                               contractId, contractVersion)

    contract = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)
    util.helen.validators.validateGetContractVersion(contract)

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


@describe()
@pytest.mark.smoke
@pytest.mark.contracts
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
    util.helen.validators.validateContractUploadResponseFields(contractResult, fxBlockchain.blockchainId,
                                                               contractId, contractVersion)

    contractVersion = util.numbers_strings.random_string_generator(mustNotMatch=contractVersion)
    contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                         "resources/contracts/HelloWorldMultiple.sol",
                                                         "HowdyWorld",
                                                         contractId=contractId,
                                                         contractVersion=contractVersion)
    util.helen.validators.validateContractUploadResponseFields(contractResult, fxBlockchain.blockchainId,
                                                               contractId, contractVersion)


    contract = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)
    util.helen.validators.validateGetContractVersion(contract)

    assert util.blockchain.eth.howdyHex in contract["bytecode"], "HowdyWorld! should be in the bytecode."
    assert util.blockchain.eth.helloHex not in contract["bytecode"], "HelloWorld! should not be in the bytecode."


@describe()
@pytest.mark.smoke
@pytest.mark.contracts
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
    util.helen.validators.validateContractUploadResponseFields(contractResult, fxBlockchain.blockchainId, contractId, contractVersion)

    contractId = util.numbers_strings.random_string_generator(mustNotMatch=contractId)
    contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                         "resources/contracts/HelloWorldMultiple.sol",
                                                         "HowdyWorld",
                                                         contractId=contractId,
                                                         contractVersion=contractVersion)
    util.helen.validators.validateContractUploadResponseFields(contractResult, fxBlockchain.blockchainId, contractId, contractVersion)

    contract = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)
    util.helen.validators.validateGetContractVersion(contract)

    assert util.blockchain.eth.howdyHex in contract["bytecode"], "HowdyWorld! should be in the bytecode."
    assert util.blockchain.eth.helloHex not in contract["bytecode"], "HelloWorld! should not be in the bytecode."


@describe()
@pytest.mark.smoke
@pytest.mark.contracts
def test_getContractById_idInvalid(fxConnection, fxBlockchain):
    '''
    Try to get a contract by ID when the ID is invalid.
    '''
    result = fxConnection.request.getAllContractVersions(fxBlockchain.blockchainId, nonexistantContractId)
    expectedPath = "/api/blockchains/{}/concord/contracts/{}".format(fxBlockchain.blockchainId, nonexistantContractId)

    util.helen.validators.validateNotFound(result,
                                           expectedPath = expectedPath,
                                           errorCode = "NotFoundException",
                                           testErrorMessage = True,
                                           errorMessage = "Contract not found: {}".format(nonexistantContractId))

@describe()
@pytest.mark.smoke
@pytest.mark.contracts
def test_getContractById_oneVersion(fxConnection, fxBlockchain):
    '''
    Upload one version of a contract, get it with /contracts/{id}, and
    verify that it is correct.
    '''
    contractId = util.numbers_strings.random_string_generator()
    contractVersion = util.numbers_strings.random_string_generator()
    contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                         "resources/contracts/HelloWorldMultipleWithDoc.sol",
                                                         "HelloWorld",
                                                         contractId=contractId,
                                                         contractVersion=contractVersion)
    util.helen.validators.validateContractUploadResponseFields(contractResult, fxBlockchain.blockchainId,
                                                               contractId, contractVersion)

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

    util.helen.validators.verifyContractVersionFields(fxConnection.rpc,
                                result["versions"][0],
                                helloExpectedDetails,
                                contractVersion,
                                util.blockchain.eth.helloFunction,
                                util.blockchain.eth.helloHex)


@describe()
@pytest.mark.smoke
@pytest.mark.skip(reason="Unknown intermittent failure BC-4254 & BC-4288")
def test_getContractById_multipleVersions(fxConnection, fxBlockchain):
    '''
    Upload multiple versions of a contract, get all of them in one call with /contracts/{id},
    and verify that they are correct.
    '''
    contractId = util.numbers_strings.random_string_generator()
    helloVersion = util.numbers_strings.random_string_generator()

    # Send Hello.
    contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                         "resources/contracts/HelloWorldMultipleWithDoc.sol",
                                                         "HelloWorld",
                                                         contractId=contractId,
                                                         contractVersion=helloVersion,
                                                         optimize=False)
    util.helen.validators.validateContractUploadResponseFields(contractResult, fxBlockchain.blockchainId,
                                                               contractId, helloVersion)

    helloExpectedDetails = util.json_helper.readJsonFile \
        ("resources/contracts/HelloWorldMultiple_helloExpectedData.json")

    # Send Howdy as a new version of the same contract.
    howdyVersion = util.numbers_strings.random_string_generator(mustNotMatch=helloVersion)
    contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                         "resources/contracts/HelloWorldMultipleWithDoc.sol",
                                                         "HowdyWorld",
                                                         contractId=contractId,
                                                         contractVersion=howdyVersion,
                                                         optimize=True,
                                                         runs=100)
    util.helen.validators.validateContractUploadResponseFields(contractResult, fxBlockchain.blockchainId,
                                                               contractId, howdyVersion)
    howdyExpectedDetails = util.json_helper.readJsonFile \
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
    util.helen.validators.verifyContractVersionFields(fxConnection.rpc,
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
    util.helen.validators.verifyContractVersionFields(fxConnection.rpc,
                                result["versions"][1],
                                howdyExpectedDetails,
                                howdyVersion,
                                util.blockchain.eth.howdyFunction,
                                util.blockchain.eth.howdyHex)

@describe()
@pytest.mark.smoke
@pytest.mark.contracts
def test_getContractVersionById_oneVersion(fxConnection, fxBlockchain):
    '''
    Upload one version of a contract, fetch it with /contract/{id}/version/{id}, and
    verify that the fields are correct.
    '''
    contractId = util.numbers_strings.random_string_generator()
    contractVersion = util.numbers_strings.random_string_generator()
    contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                         "resources/contracts/HelloWorldMultipleWithDoc.sol",
                                                         "HelloWorld",
                                                         contractId=contractId,
                                                         contractVersion=contractVersion,
                                                         optimize=False)
    util.helen.validators.validateContractUploadResponseFields(contractResult, fxBlockchain.blockchainId,
                                                               contractId, contractVersion)
    result = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, contractVersion)
    util.helen.validators.validateGetContractVersion(result)

    # These are in the contract part of the structure when fetching all versions of a contract.
    # When fetching version info, they are included with the version.
    assert result["contract_id"] == contractId, \
        "Contract ID was {}, expected {}".format(result["contract_id"], contractId)
    del result["contract_id"]

    assert result["owner"] == fromUser, \
        "Contract user was {}, expected {}".format(result["owner"], fromUser)
    del result["owner"]

    helloExpectedDetails = util.json_helper.readJsonFile("resources/contracts/HelloWorldMultiple_helloExpectedData.json")

    util.helen.validators.verifyContractVersionFields(fxConnection.rpc,
                                result,
                                helloExpectedDetails,
                                contractVersion,
                                util.blockchain.eth.helloFunction,
                                util.blockchain.eth.helloHex)


@describe()
@pytest.mark.smoke
@pytest.mark.contracts
def test_getContractVersionById_firstVersion(fxConnection, fxBlockchain):
    '''
    Upload multiple versions of a contract, fetch the first with /contract/{id}/version/{id}, and
    verify that the fields are correct.
    '''
    contractId = util.numbers_strings.random_string_generator()
    helloVersion = util.numbers_strings.random_string_generator()
    howdyVersion = util.numbers_strings.random_string_generator(mustNotMatch=helloVersion)

    contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                         "resources/contracts/HelloWorldMultipleWithDoc.sol",
                                                         "HelloWorld",
                                                         contractId=contractId,
                                                         contractVersion=helloVersion,
                                                         optimize=False)
    util.helen.validators.validateContractUploadResponseFields(contractResult, fxBlockchain.blockchainId,
                                                               contractId, helloVersion)

    contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                         "resources/contracts/HelloWorldMultipleWithDoc.sol",
                                                         "HelloWorld",
                                                         contractId=contractId,
                                                         contractVersion=howdyVersion,
                                                         optimize=False)
    util.helen.validators.validateContractUploadResponseFields(contractResult, fxBlockchain.blockchainId,
                                                               contractId, howdyVersion)
    result = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, helloVersion)
    util.helen.validators.validateGetContractVersion(result)

    # These are in the contract part of the structure when fetching all versions of a contract.
    # When fetching version info, they are included with the version.
    assert result["contract_id"] == contractId, \
        "Contract ID was {}, expected {}".format(result["contract_id"], contractId)
    del result["contract_id"]

    assert result["owner"] == fromUser, \
        "Contract user was {}, expected {}".format(result["owner"], fromUser)
    del result["owner"]

    helloExpectedDetails = util.json_helper.readJsonFile("resources/contracts/HelloWorldMultiple_helloExpectedData.json")

    util.helen.validators.verifyContractVersionFields(fxConnection.rpc,
                                result,
                                helloExpectedDetails,
                                helloVersion,
                                util.blockchain.eth.helloFunction,
                                util.blockchain.eth.helloHex)


@describe()
@pytest.mark.smoke
@pytest.mark.contracts
def test_getContractVersionById_lastVersion(fxConnection, fxBlockchain):
    '''
    Upload multiple versions of a contract, fetch the last with /contract/{id}/version/{id}, and
    verify that the fields are correct.
    '''
    contractId = util.numbers_strings.random_string_generator()
    helloVersion = util.numbers_strings.random_string_generator()
    howdyVersion = util.numbers_strings.random_string_generator(mustNotMatch=helloVersion)
    contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                         "resources/contracts/HelloWorldMultipleWithDoc.sol",
                                                         "HelloWorld",
                                                         contractId=contractId,
                                                         contractVersion=helloVersion,
                                                         optimize=False)
    util.helen.validators.validateContractUploadResponseFields(contractResult, fxBlockchain.blockchainId,
                                                               contractId, helloVersion)

    contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                         "resources/contracts/HelloWorldMultipleWithDoc.sol",
                                                         "HowdyWorld",
                                                         contractId=contractId,
                                                         contractVersion=howdyVersion,
                                                         optimize=True,
                                                         runs=100)
    util.helen.validators.validateContractUploadResponseFields(contractResult, fxBlockchain.blockchainId,
                                                               contractId, howdyVersion)

    result = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, howdyVersion)
    util.helen.validators.validateGetContractVersion(result)

    # These are in the contract part of the structure when fetching all versions of a contract.
    # When fetching version info, they are included with the version.
    assert result["contract_id"] == contractId, \
        "Contract ID was {}, expected {}".format(result["contract_id"], contractId)
    del result["contract_id"]

    assert result["owner"] == fromUser, \
        "Contract user was {}, expected {}".format(result["owner"], fromUser)
    del result["owner"]

    howdyExpectedDetails = util.json_helper.readJsonFile("resources/contracts/HelloWorldMultiple_howdyExpectedData.json")

    util.helen.validators.verifyContractVersionFields(fxConnection.rpc,
                                result,
                                howdyExpectedDetails,
                                howdyVersion,
                                util.blockchain.eth.howdyFunction,
                                util.blockchain.eth.howdyHex)


@describe()
@pytest.mark.smoke
@pytest.mark.contracts
def test_getContractVersionById_invalidVersion(fxConnection, fxBlockchain):
    '''
    Pass an invalid version to /contracts/{id}/versions/{id}.
    '''
    contractId = util.numbers_strings.random_string_generator()
    contractVersion = util.numbers_strings.random_string_generator()

    contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                         "resources/contracts/HelloWorldMultipleWithDoc.sol",
                                                         "HelloWorld",
                                                         contractId=contractId,
                                                         contractVersion=contractVersion,
                                                         optimize=False)
    util.helen.validators.validateContractUploadResponseFields(contractResult, fxBlockchain.blockchainId,
                                                               contractId, contractVersion)

    result = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, contractId, nonexistantVersionId)

    expectedPath = "/api/blockchains/{}/concord/contracts/{}/versions/{}". \
        format(fxBlockchain.blockchainId, contractId, nonexistantVersionId)
    expectedMessage = "Contract version not found  {}:{}".format(contractId, nonexistantVersionId)

    util.helen.validators.validateNotFound(result,
                                           expectedPath = expectedPath,
                                           errorCode = "NotFoundException",
                                           testErrorMessage = True,
                                           errorMessage = expectedMessage)

@describe()
@pytest.mark.smoke
@pytest.mark.contracts
def test_getContractVersionById_invalidContract(fxConnection, fxBlockchain):
    '''
    Pass an invalid contract to /contracts/{id}/versions/{id}.
    '''
    contractId = util.numbers_strings.random_string_generator()
    contractVersion = util.numbers_strings.random_string_generator()
    contractResult = util.blockchain.eth.upload_contract(fxBlockchain.blockchainId, fxConnection.request,
                                                         "resources/contracts/HelloWorldMultipleWithDoc.sol",
                                                         "HelloWorld",
                                                         contractId=contractId,
                                                         contractVersion=contractVersion,
                                                         optimize=False)
    util.helen.validators.validateContractUploadResponseFields(contractResult, fxBlockchain.blockchainId,
                                                               contractId, contractVersion)

    expectedPath = "/api/blockchains/{}/concord/contracts/{}/versions/{}". \
        format(fxBlockchain.blockchainId, nonexistantContractId, contractVersion)
    expectedMessage = "Contract version not found  {}:{}".format(nonexistantContractId, contractVersion)

    result = fxConnection.request.getContractVersion(fxBlockchain.blockchainId, nonexistantContractId, contractVersion)
    util.helen.validators.validateNotFound(result,
                                           expectedPath = expectedPath,
                                           errorCode = "NotFoundException",
                                           testErrorMessage = True,
                                           errorMessage = expectedMessage)