#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import pytest

from suites.case import describe, passed, failed

from fixtures.common_fixtures import fxBlockchain, fxConnection, fxInitializeOrgs, fxProduct
import util.helen.error_codes
import util.helen.validators

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# HELEN CLIENTS TESTS
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

@describe()
@pytest.mark.smoke
@pytest.mark.clients
def test_getClientsNoClientBlockchain(fxConnection):
    '''
    Test for no participants/clients
    '''
    # Take ID of first blockhain in the list
    # Ethereum blockchain, no clients
    blockchain_id = fxConnection.request.getBlockchains()[0]["id"]
    clients = fxConnection.request.get_participant_details(blockchain_id)
    assert len(clients) == 0


@describe()
@pytest.mark.smoke
@pytest.mark.clients
def test_getClientsUnavailableBlockchain(fxConnection, fxBlockchain):
    '''
    Test for invalid blockchain
    '''
    bid = "00000000-0000-0000-0000-000000000000"
    response = fxConnection.request.get_participant_details(bid)
    expectedPath = "/api/blockchains/{}/clients".format(bid)
    expectedMessage = util.helen.error_codes.BLOCKCHAIN_NOT_FOUND.format(bid)
    util.helen.validators.validateNotFound(response,
                                           expectedPath = expectedPath,
                                           errorCode = "NotFoundException",
                                           testErrorMessage = True,
                                           errorMessage = expectedMessage)