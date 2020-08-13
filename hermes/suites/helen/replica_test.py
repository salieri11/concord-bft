#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import pytest

from suites.case import describe, passed, failed

from fixtures.common_fixtures import fxBlockchain, fxConnection, fxHermesRunSettings, fxInitializeOrgs, fxProduct

import util.helen.error_codes
import util.helen.validators

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# HELEN REPLICA TESTS
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

@describe()
@pytest.mark.smoke()
@pytest.mark.replicas()
def test_replicaGet(fxConnection):
    # 4 node blockchain, 4 replicas expected
    blockchain_id = fxConnection.request.getBlockchains()[0]["id"]
    replicas = fxConnection.request.getReplicas(blockchain_id)

    util.helen.validators.validateReplicaListResponse(replicas)
    assert len(replicas) == 4

@describe()
@pytest.mark.smoke()
@pytest.mark.replicas()
def test_replicaGetUnavailableBlockchain(fxConnection):
    # 4 node blockchain, 4 replicas expected
    bid = "00000000-0000-0000-0000-000000000000"
    response = fxConnection.request.getReplicas(bid)
    expectedPath = "/api/blockchains/{}/replicas".format(bid)
    expectedMessage = util.helen.error_codes.BLOCKCHAIN_NOT_FOUND.format(bid)
    util.helen.validators.validateNotFound(response,
                                           expectedPath = expectedPath,
                                           errorCode = "NotFoundException",
                                           testErrorMessage = True,
                                           errorMessage = expectedMessage)