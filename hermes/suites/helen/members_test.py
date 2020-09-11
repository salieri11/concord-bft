#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import collections
import pytest
import time

from suites.case import describe, passed, failed
from rest.request import Request

from fixtures.common_fixtures import fxBlockchain, fxConnection, fxInitializeOrgs, fxProduct
import util.helper
import util.product

import util.hermes_logging
log = util.hermes_logging.getMainLogger()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# DEPRECATED MEMBERS TESTS
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

@describe()
@pytest.mark.smoke
def test_members_fields(fxConnection):
    blockchains = fxConnection.request.getBlockchains()
    result = fxConnection.request.getMemberList(blockchains[0]["id"])

    assert type(result) is list, "Response was not a list"
    assert len(result) >= 1, "No members returned"

    for m in result:
        (present, missing) = util.helper.requireFields(m, ["hostname", "status", "address",
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


@describe()
@pytest.mark.smoke
def test_members_rpc_url(fxConnection, fxBlockchain, fxHermesRunSettings):
    '''
    Test that the returned value for "rpc_url" is an ethrpc node.
    We'll do that by invoking the API. At the moment, Helen still
    supports the API (it is planned to be removed), so also verify
    that we aren't getting Helen's address back by ensuring a
    Helen-only API call fails.
    '''
    result = fxConnection.request.getMemberList(fxBlockchain.blockchainId)
    ethrpcUrl = None

    for member in result:
        ethrpcUrl = member["rpc_url"]

        # Ensure we have a node that responds to our API.
        # Will throw an exception if not.
        fxConnection.rpc.mining()

        # Ensure that the rpc_url isn't Helen.  This will give a 404
        # and throw an exception.
        userConfig = fxHermesRunSettings["hermesUserConfig"]
        invalidRequest = Request(fxConnection.request.logDir,
                                 fxConnection.request.testName,
                                 ethrpcUrl + "blockchains/local",
                                 userConfig)
        try:
            result = invalidRequest.getBlockList(fxBlockchain.blockchainId)
            assert False, "An exception should have been thrown when asking an ethrpc node for blocks."
        except Exception as e:
            # There are of course various reasons a 404 could be returned.  But let's at least
            # be sure we got back 404 for the given path, indicating this call is not available.
            assert "Not Found" in str(e), "Expected a 404 error about calling 'blocks'."


@describe()
@pytest.mark.smoke
def test_members_hostname(fxConnection):
    '''
    Verify the "hostname" fields are "replica1", "replica2", ...
    '''
    blockchains = fxConnection.request.getBlockchains()
    result = fxConnection.request.getMemberList(blockchains[0]["id"])
    nodeCount = len(result)
    hostNames = []

    for nodeData in result:
        hostNames.append(nodeData["hostname"])

    for i in range(0, nodeCount):
        findMe = "replica" + str(i)
        assert findMe in hostNames, "Could not find host {} in the response.".format(findMe)
        hostNames.remove(findMe)

    assert len(hostNames) == 0, "Hosts not returned in the response: {}".format(hostNames)


@describe()
@pytest.mark.smoke
def test_members_millis_since_last_message(fxConnection, fxBlockchain, fxHermesRunSettings):
    '''
    Pause a node, get sleep time millis, and make sure it is at least as long as we slept.
    Unpause it, and make sure it decreased.
    The numbers are not exact, but we're not testing concord.  We're just
    testing that Helen is receiving/communicating new values, not always
    showing a default, etc...
    '''
    if fxHermesRunSettings["hermesCmdlineArgs"].blockchainLocation != util.helper.LOCATION_LOCAL:
        pytest.skip("Skipping because this test requires pausing a Concord node, and " \
                    "this Concord deployment is on SDDC or on-prem infra.")

    allMembers = fxConnection.request.getMemberList(fxBlockchain.blockchainId)
    nodeData = allMembers[0] # Any will do.
    hostName = nodeData["hostname"]
    concordIndex = int(hostName[len("replica"):]) + 1 # replica0 == concord1
    testTime = 0
    sleepTime = 5
    expectedMinimum = sleepTime * 1000

    # The functionality we need in Product is a bit tied into it, so make
    # a patchy object so we can use what we need.
    HermesArgs = collections.namedtuple("HermesArgs", "resultsDir")
    hermesArgs = HermesArgs(resultsDir = fxHermesRunSettings["hermesCmdlineArgs"].resultsDir)
    product = util.product.Product(hermesArgs,
                                   fxHermesRunSettings["hermesUserConfig"])

    try:
        product.resumeMembers(allMembers)
        log.info("Pausing concord{}".format(concordIndex))
        paused = product.pause_concord_replica(str(concordIndex))
        assert paused, "Unable to pause the container.  Hostname: {}, concord #: {}". \
            format(hostName, concordIndex)
        time.sleep(sleepTime)

        result = fxConnection.request.getMemberList(fxBlockchain.blockchainId)
        for nodeData in result:
            if nodeData["hostname"] == hostName:
                testTime = int(nodeData["millis_since_last_message"])
                break

        assert testTime > expectedMinimum, "Expected millis_since_last_message of " \
                                           "at least {}, got {}.".format(expectedMinimum, testTime)

        log.info("Resuming concord{}".format(concordIndex))
        resumed = product.resume_concord_replica(str(concordIndex))
        assert resumed, "Unable to resume the container.  Hostname: {}, concord #: {}". \
            format(hostName, concordIndex)

        result = fxConnection.request.getMemberList(fxBlockchain.blockchainId)
        assert len(result) > 0, "No members returned"

        for nodeData in result:
            if nodeData["hostname"] == hostName:
                testTimeResumed = int(nodeData["millis_since_last_message"])
                assert testTimeResumed < testTime, "Expected millis_since_last_message " \
                                                   "to be less than {}, received {}.".format(testTime, testTimeResumed)
    finally:
        product.resumeMembers(allMembers)
