#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import collections
import copy
import difflib
import json
from uuid import UUID, uuid4
from uuid import UUID

import util.blockchain.eth
import util.hermes_logging
log = util.hermes_logging.getMainLogger()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# HELEN VALIDATION FUNCTIONS
# Checks to make sure that the response objects don't change.
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# BLOCKCHAIN VALIDATORS
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

def validateBlockchainFields(blockchain):
    '''
    Assert that the blockchain response has the fields we are expecting.
    '''
    assert "id" in blockchain, "No field called id in blockchain"
    assert len(blockchain["id"]) == 36, "Expected UUID of length 36"
    assert type(blockchain["id"]) == str, "Expecting str value for id"

    assert "consortium_id" in blockchain, "No field called consortium_id in blockchain"
    assert type(blockchain["consortium_id"]) == str, "Expecting str value for consortium_id"
    assert len(blockchain["consortium_id"]) == 36, "Expected UUID of length 36"

    assert "blockchain_type" in blockchain, "No field called blockchain_type in blockchain"
    assert type(blockchain["blockchain_type"]) == str, "Expecting str value for blockchain_type"

    assert "blockchain_state" in blockchain, "No field called blockchain_state in blockchain"
    assert type(blockchain["blockchain_state"]) == str, "Expecting str value for blockchain_state"

    assert "version" in blockchain, "No field called version in blockchain"
    assert type(blockchain["version"]) == str, "Expecting str value for version"

    assert "created_by" in blockchain, "No field called created_by in blockchain"
    assert type(blockchain["created_by"]) == str, "Expecting str value for created_by"

    assert "created" in blockchain, "No field called created in blockchain"
    assert type(blockchain["created"]) == int, "Expecting int value for created"

    assert len(blockchain.keys()) == 7, "Expecting 7 fields"

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# CONTRACT VALIDATORS
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

def validateSingleContractFields(contract):
    '''
    Assert that the blockchain response has the fields we are expecting.
    '''
    assert "contract_id" in contract, "No field called contract_id in contract"
    assert type(contract["contract_id"]) == str, "Expecting str value for contract_id"

    assert "owner" in contract, "No field called owner in contract"
    assert type(contract["owner"]) == str, "Expecting str value for owner"

    assert "url" in contract, "No field called url in contract"
    assert type(contract["url"]) == str, "Expecting str value for url"

    assert len(contract.keys()) == 3, "Expecting 3 fields value for created"


def validateContractUploadResponseFields(contract, blockchainId, contractId, contractVersion):
    '''
    testObj: The object to check.
    Other fields: Used as expected values.
    '''
    assert contract["contract_id"] == contractId, "The contract_id field is not correct."
    assert type(contract["contract_id"]) == str, "Expecting str value for contract_id"

    assert contract["version"] == contractVersion, "The contract_version field is not correct."
    assert type(contract["version"]) == str, "Expecting str value for version"

    assert contract["url"] == "/api/blockchains/" + blockchainId + "/concord/contracts/" + \
           contractId + "/versions/" + contractVersion

    assert len(contract.keys()) == 3, "Expecting 3 fields"


def validateGetContractVersion(contractVersionObject):
    '''
    Validates the ContractVersionResponse JSON Array object
    '''
    assert "address" in contractVersionObject, "Field address not found in response"
    assert type(contractVersionObject["address"]) == str, "Expecting str value for address"

    assert "metadata" in contractVersionObject, "Field metadata not found in response"
    assert type(contractVersionObject["metadata"]) == collections.OrderedDict, \
        "Expecting collections.OrderedDict value for metadata"

    assert "version" in contractVersionObject, "Field version not found in response"
    assert type(contractVersionObject["version"]) == str, "Expecting str value for version"

    assert "contract_id" in contractVersionObject, "Field contract_id not found in response"
    assert type(contractVersionObject["contract_id"]) == str, "Expecting str value for contract_id"

    assert "owner" in contractVersionObject, "Field owner not found in response"
    assert type(contractVersionObject["owner"]) == str, "Expecting str value for owner"

    assert "bytecode" in contractVersionObject, "Field bytecode not found in response"
    assert type(contractVersionObject["bytecode"]) == str, "Expecting str value for bytecode"

    assert "sourcecode" in contractVersionObject, "Field sourcecode not found in response"
    assert type(contractVersionObject["sourcecode"]) == str, "Expecting str value for sourcecode"

    assert len(contractVersionObject.keys()) == 7, "Expecting 7 fields"



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



def verifyContractVersionFields(rpc, actualDetails, expectedDetails, expectedVersion,
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

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# CONSORTIUM VALIDATORS
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

def validateConsortiumListGet(consortiumList):
    '''
    Validates a Consortium list response.
    '''
    if len(consortiumList) > 0:
        consortium = consortiumList[0]
        validateConsortiumObject(consortium)

def validateConsortiumObject(consortium):
    '''
    Validates the Consortium GET object.
    '''
    assert "consortium_id" in consortium, "Field consortium_id not found in response"
    assert type(consortium["consortium_id"]) == str, "Expecting str value for consortium_id"
    assert len(consortium["consortium_id"]) == 36, "Expecting UUID of length 36"

    assert "consortium_name" in consortium, "Field consortium_name not found in response"
    assert type(consortium["consortium_name"]) == str, "Expecting str value for consortium_name"

    assert "organization_id" in consortium, "Field organization_id not found in response"
    assert type(consortium["organization_id"]) == str, "Expecting str value for organization_id"
    assert len(consortium["organization_id"]) == 36, "Expecting UUID of length 36"

    assert len(consortium.keys()) == 3, "Expecting 3 fields"

def validateConsortiumPatchResponseObject(consortium):
    '''
    Validates the Consortium PATCH object.
    '''
    assert "consortium_id" in consortium, "Field consortium_id not found in response"
    assert type(consortium["consortium_id"]) == str, "Expecting str value for consortium_id"
    assert len(consortium["consortium_id"]) == 36, "Expecting UUID of length 36"

    assert "consortium_name" in consortium, "Field consortium_name not found in response"
    assert type(consortium["consortium_name"]) == str, "Expecting str value for consortium_name"

    assert "organization_id" in consortium, "Field organization_id not found in response"
    assert type(consortium["organization_id"]) == str, "Expecting str value for organization_id"
    assert len(consortium["organization_id"]) == 36, "Expecting UUID of length 36"

    assert "members" in consortium, "Field members not found in response"
    assert type(consortium["members"]) == list, "Expecting list value for members"
    validateOrgListResponse(consortium["members"])

    assert len(consortium.keys()) == 4, "Expecting 4 fields"

def validateOrgListResponse(orgList):
    '''
    Validates aN Organiation list response.
    '''
    if len(orgList) > 0:
        org = orgList[0]
        validateOrgResponse(org)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ORGANIZATION VALIDATORS
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

def validateOrgResponse(org):
    '''
    Validates the Organization GET object.
    '''
    assert "organization_id" in org, "Field organization_id not found in response"
    assert type(org["organization_id"]) == str, "Expecting str value for organization_id"
    assert len(org["organization_id"]) == 36, "Expecting UUID of length 36"

    assert "organization_name" in org, "Field organization_name not found in response"
    assert type(org["organization_name"]) == str, "Expecting str value for organization_name"

    assert "organization_properties" in org, "Field organization_properties not found in response"
    assert type(org["organization_properties"]) == collections.OrderedDict, \
        "Expecting collections.OrderedDict value for organization_properties"

    assert len(org.keys()) == 3, "Expecting 3 fields"

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# CLIENT VALIDATORS
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

def validateClientListResponse(clientList):
    '''
    Validates a Client list response.
    '''
    if len(clientList) > 0:
        client = clientList[0]
        validateClientResponse(client)

def validateClientResponse(client):
    '''
    Validates the Client GET object.
    '''
    assert "id" in client, "Field id not found in response"
    assert type(client["id"]) == str, "Expecting str value for id"
    assert len(client["id"]) == 36, "Expecting UUID of length 36"

    assert "public_ip" in client, "Field public_ip not found in response"
    if client["public_ip"]:
        assert type(client["public_ip"]) == str, "Expecting str value for public_ip"

    assert "private_ip" in client, "Field private_ip not found in response"
    assert type(client["private_ip"]) == str, "Expecting str value for private_ip"

    assert "name" in client, "Field name not found in response"
    if client["name"]:
        assert type(client["name"]) == str, "Expecting str value for name"

    assert "url" in client, "Field url not found in response"
    assert type(client["url"]) == str, "Expecting str value for url"

    assert "cert" in client, "Field cert not found in response"
    if client["cert"]:
        assert type(client["cert"]) == str, "Expecting str value for cert"

    assert "zone_id" in client, "Field zone_id not found in response"
    assert type(client["zone_id"]) == str, "Expecting str value for zone_id"
    assert len(client["zone_id"]) == 36, "Expecting UUID of length 36"

    assert "auth_url_jwt" in client, "Field zone_id not found in response"
    if client["auth_url_jwt"]:
        assert type(client["auth_url_jwt"]) == str, "Expecting str value for zone_id"

    if client["group_id"]:
        assert type(client["group_id"]) == str, "Expecting str value for zone_id"
        assert len(client["id"]) == 36, "Expecting UUID of length 36"

    if client["group_name"]:
        assert type(client["group_name"]) == str, "Expecting str value for group_name"


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# REPLICA VALIDATORS
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

def validateReplicaListResponse(replicaList):
    '''
    Validates a Replica list response.
    '''
    if len(replicaList) > 0:
        replica = replicaList[0]
        validateReplicaResponse(replica)

def validateReplicaResponse(replica):
    '''
    Validates a Replica GET object.
    '''
    assert "id" in replica, "Field id not found in response"
    assert type(replica["id"]) == str, "Expecting str value for id"
    assert len(replica["id"]) == 36, "Expecting UUID of length 36"

    assert "name" in replica, "Field name not found in response"
    assert type(replica["name"]) == str, "Expecting str value for name"

    assert "public_ip" in replica, "Field public_ip not found in response"
    assert type(replica["public_ip"]) == str, "Expecting str value for public_ip"

    assert "private_ip" in replica, "Field private_ip not found in response"
    assert type(replica["private_ip"]) == str, "Expecting str value for private_ip"

    assert "rpc_url" in replica, "Field rpc_url not found in response"
    assert type(replica["rpc_url"]) == str, "Expecting str value for rpc_url"

    assert "status" in replica, "Field status not found in response"
    assert type(replica["status"]) == str, "Expecting str value for status"

    assert "millis_since_last_message" in replica, "Field millis_since_last_message not found in response"
    assert type(replica["millis_since_last_message"]) == int, "Expecting int value for millis_since_last_message"

    assert "millis_since_last_message_threshold" in replica, \
        "Field millis_since_last_message_threshold not found in response"
    assert type(replica["millis_since_last_message_threshold"]) == int, \
        "Expecting int value for millis_since_last_message_threshold"

    assert "certificate" in replica, "Field certificate not found in response"
    if replica["certificate"]:
        assert type(replica["certificate"]) == str, "Expecting str value for certificate"

    assert "zone_id" in replica, "Field zone_id not found in response"
    assert type(replica["zone_id"]) == str, "Expecting str value for zone_id"
    assert len(replica["zone_id"]) == 36, "Expecting UUID of length 36"

    assert len(replica.keys()) == 10, "Expecting 10 fields"

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ZONE VALIDATORS
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

def validateZoneFieldsList(zoneList):
    '''
    Validates a Zone list response.
    '''
    if len(zoneList) > 0:
        zone = zoneList[0]
        validateZoneFields(zone)

def validateZoneFields(zone):
    '''
    Validates a Zone GET object.
    '''
    assert "id" in zone, "Field id not found in response"
    assert type(zone["id"]) == str, "Expecting str value for id"
    assert len(zone["id"]) == 36, "Expecting UUID of length 36"

    assert "name" in zone, "Field name not found in response"
    assert type(zone["name"]) == str, "Expecting str value for name"

    assert "latitude" in zone, "Field latitude not found in response"
    assert type(zone["latitude"]) == str, "Expecting str value for latitude"

    assert "longitude" in zone, "Field longitude not found in response"
    assert type(zone["longitude"]) == str, "Expecting str value for longitude"

    assert "type" in zone, "Field type not found in response"
    assert type(zone["type"]) == str, "Expecting str value for type"

    if type == "ON_PREM":
        validateOnPremZoneFields(zone)
    else:
        # TODO: when we have the vmc zone...
        pass


def validateOnPremZoneFields(zone):
    '''
    Validate fields on zone objects specific to OnPremZone.
    '''
    assert "org_id" in zone, "Field org_id not found in response"
    assert type(zone["org_id"]) == str, "Expecting str value for org_id"
    assert len(zone["org_id"]) == 36, "Expecting UUID of length 36"

    assert "vcenter" in zone, "Field vcenter not found in response"
    vcenter = zone["vcenter"]
    validateEndpointFields(vcenter)

    assert "resource_pool" in zone, "Field resource_pool not found in response"
    assert type(zone["resource_pool"]) == str, "Expecting str value for resource_pool"

    assert "storage" in zone, "Field storage not found in response"
    assert type(zone["storage"]) == str, "Expecting str value for storage"

    assert "folder" in zone, "Field folder not found in response"
    assert type(zone["folder"]) == str, "Expecting str value for folder"

    assert "network" in zone, "Field network not found in response"
    network = zone["network"]
    if network:
        validateNetworkFields(network)

    assert "outbound_proxy" in zone, "Field outbound_proxy not found in response"
    outbound_proxy = zone["network"]
    if outbound_proxy:
        validateOutboundProxyFields(outbound_proxy)

    assert "container_repo" in zone, "Field container_repo not found in response"
    container_repo = zone["container_repo"]
    if container_repo:
        validateEndpointFields(container_repo)

    assert "wavefront" in zone, "Field wavefront not found in response"
    wavefront = zone["wavefront"]
    if wavefront:
        validateWavefrontFields(wavefront)

    assert "elasticsearch" in zone, "Field wavefront not found in response"
    elasticsearch = zone["wavefront"]
    if elasticsearch:
        validateEndpointFields(elasticsearch)

    assert "log_managements" in zone, "Field wavefront not found in response"
    log_managements = zone["log_managements"]

    validateLogManagementList(log_managements)


def validateEndpointFields(endpoint):
    '''
    Validate Endpoint objects.
    '''
    assert type(endpoint) == collections.OrderedDict, "Expecting collections.OrderedDict value for endpoint"

    assert "url" in endpoint, "Field url not found in endpoint in response"
    assert type(endpoint["url"]) == str, "Expecting str value for url in endpoint"

    assert "username" in vcenter, "Field username not found in endpoint in response"
    assert type(endpoint["username"]) == str, "Expecting str value for username in endpoint"

    assert "password" in vcenter, "Field password not found in endpoint in response"
    assert type(endpoint["password"]) == str, "Expecting str value for password in endpoint"

    assert len(endpoint.keys()) == 3, "Expecting 3 fields in endpoint"


def validateNetworkFields(network):
    '''
    Validate Network objects.
    '''
    assert type(network) == collections.OrderedDict, "Expecting collections.OrderedDict value for network"

    assert "name" in network, "Field name not found in network in response"
    assert type(network["name"]) == str, "Expecting str value for name in network"

    assert "ip_pool" in network, "Field ip_pool not found in network in response"
    assert type(network["ip_pool"]) == list, "Expecting list value for ip_pool in network"
    assert type(network["ip_pool"][0]) == str, "Expecting str value for ip_pool items in network"

    assert "gateway" in network, "Field gateway not found in network in response"
    assert type(network["gateway"]) == str, "Expecting str value for gateway in network"

    assert "subnet" in network, "Field subnet not found in network in response"
    assert type(network["subnet"]) == str, "Expecting str value for subnet in network"

    assert "name_servers" in network, "Field name_servers not found in network in response"
    assert type(network["name_servers"]) == list, "Expecting list value for name_servers in network"
    assert type(network["name_servers"][0]) == str, "Expecting str value for name_servers items in network"

    assert len(network.keys()) == 5, "Expected 5 fields in endpoint."


def validateOutboundProxyFields(outboundProxy):
    '''
    Validate OutboundProxy objects.
    '''
    assert type(outboundProxy) == collections.OrderedDict, "Expecting collections.OrderedDict value for outbound_proxy"

    assert "http_host" in outboundProxy, "Field http_host not found in outbound_proxy in response"
    assert type(outboundProxy["http_host"]) == str, "Expecting str value for http_host in outbound_proxy"

    assert "http_port" in outboundProxy, "Field http_port not found in outbound_proxy in response"
    assert type(outboundProxy["http_port"]) == int, "Expecting int value for http_port in outbound_proxy"

    assert "https_host" in outboundProxy, "Field https_host not found in outbound_proxy in response"
    assert type(outboundProxy["https_host"]) == str, "Expecting str value for https_host in outbound_proxy"

    assert "https_port" in outboundProxy, "Field https_port not found in outbound_proxy in response"
    assert type(outboundProxy["https_port"]) == int, "Expecting int value for https_port in outbound_proxy"

    assert len(outboundProxy.keys()) == 4


def validateWavefrontFields(wavefront):
    '''
    Validate Wavefront objects.
    '''
    assert type(wavefront) == collections.OrderedDict, "Expecting collections.OrderedDict value for wavefront"
    assert "url" in wavefront, "Field url not found in wavefront in response"
    assert type(wavefront["url"]) == str, "Expecting str value for url in wavefront"

    assert "token" in wavefront, "Field token not found in wavefront in response"
    assert type(wavefront["https_port"]) == str, "Expecting str value for token in wavefront"


def validateLogManagementList(logManagementList, zoneType):
    '''
    Validate LogManagement list.
    '''
    if len(logManagementList) > 0:
        logManagement = logManagementList[0]
        if zoneType == "ON_PREM":
            validateLogManagementOnPremFields(logManagement)


def validateLogManagementOnPremFields(logManagement):
    '''
    Validate LogManagement object.
    '''
    assert type(logManagement) == collections.OrderedDict, "Expecting collections.OrderedDict value for log_management"

    assert "destination" in logManagement, "Field destination not found in log_management in response"
    assert type(logManagement["destination"]) == str, "Expecting str value for destination in log_management"

    assert "address" in logManagement, "Field address not found in log_management in response"
    assert type(logManagement["address"]) == str, "Expecting str value for address in log_management"

    assert "port" in logManagement, "Field port not found in log_management in response"
    assert type(logManagement["port"]) == int, "Expecting int value for port in log_management"

    assert "username" in logManagement, "Field username not found in log_management in response"
    assert type(logManagement["username"]) == str, "Expecting str value for username in log_management"

    assert "password" in logManagement, "Field password not found in log_management in response"
    assert type(logManagement["password"]) == str, "Expecting str value for password in log_management"

    assert "log_insight_agent_id" in logManagement, "Field log_insight_agent_id not found in log_management in response"
    assert type(logManagement["log_insight_agent_id"]) == int, "Expecting int value for log_insight_agent_id in log_management"

def validateZoneResponse(origZoneInfo, zoneResponse, orgId):
    '''
    Compares the original zone info to the response.
    The response has two additional fields, org_id and id. Those
    are checked separately, and then everything else is compared
    to the original info.
    '''
    validateZoneFields(zoneResponse)
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

    # TODO: Add test for elasticsearch instead
    if "elasticsearch" not in expected:
        expected["elasticsearch"] = None

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


def validateDeletedZoneResponse(deletedZone):
    '''
    Validates a Zone DELETE response object.
    '''
    assert "id" in deletedZone, "Expecting id in response"
    assert type(deletedZone["id"]) == str, "Expecting str value for id"
    assert len(deletedZone["id"]) == 36, "Expecting UUID of length 36"

def validateNodeSizeTemplateResponse(template):
    '''
    Validate a node size template response object.
    '''
    log.debug(template)

    assert type(template) == collections.OrderedDict, "Expecting collections.OrderedDict value for template"
    # The following statement throws an exception, if id is not of type UUID.
    UUID(template["id"])

    assert "name" in template, "No field called name in node size template"
    assert type(template["name"]) == str, "Expecting str value for name"
    assert len(template["name"]) > 0, "Expected a valid name"

    templates = template["templates"]
    assert len(templates) > 0, "Expecting templates for node sizing"

    for templateItem in templates:
        validateTemplateItem(templateItem)

    range = template["range"]
    assert len(range) > 0, "Expecting range to be non-empty"

    for key in range.keys():
        valMap = range[key]
        assert len(valMap) > 0, "Expecting range values to be non-empty"
        for valKey in valMap:
            assert valMap[valKey] > 0, "Expecting a valid value for " + valKey

def validateTemplateItem(templateItem):
    '''
    Validate a node size template item object.
    '''
    log.debug(templateItem)
    assert type(templateItem) == collections.OrderedDict, "Expecting collections.OrderedDict value for templateItem"

    assert "name" in templateItem, "No field called name in template item"
    assert type(templateItem["name"]) == str, "Expecting str value for name"
    assert len(templateItem["name"]) > 0, "Expected a valid name"

    items = templateItem["items"]
    assert len(items) > 0, "Expecting items for the template item"

    for item in items:
        for key in item.keys():
            assert len(item[key]) > 0, "Expecting a valid value for " + key

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# EXCEPTION VALIDATORS
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

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

def validateNotFound(result, expectedPath,
                     errorCode="NotFoundException",
                     testErrorMessage = True,
                     errorMessage="Bad request (e.g. missing request body)."):
    '''
    Validates the returned result of a Bad Request error.
    The error code, error message, and status are the same.
    Accepts the result to evaluate and the expected value for "path".
    Check that a valid op_id has been returned
    '''
    log.info("Looking for not found error in {}".format(result))

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
    assert result["status"] == 404, "Expected HTTP status 400."

    assert "op_id" in result, "Expected a field called 'op_id'"
    # opid is a uuid string
    assert len(result["op_id"]) == 36
