#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import pytest
from uuid import UUID, uuid4

from suites.case import describe, passed, failed

from fixtures.common_fixtures import fxBlockchain, fxConnection, fxInitializeOrgs, fxProduct
import util.auth
import util.helen.common
import util.helen.validators
import util.numbers_strings

defaultTokenDescriptor = util.auth.getTokenDescriptor(util.auth.ROLE_CON_ADMIN,
                                                      True,
                                                      util.auth.internal_admin)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# HELEN CONSORTIUM TESTS
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

@describe()
@pytest.mark.smoke
@pytest.mark.consortiums
def test_consortiums_add_basic(fxConnection):
    '''
    Basic consortium creation test.
    Issues:
    - What is the consoritum type?
    - Swagger shows consortiumName, body actually has to be consortium_name.
    - Swagger shows it takes an org, but it just links to the org that the
      user is part of.  So it seems org should not be a parameter.
    '''
    suffix = util.numbers_strings.random_string_generator()
    conName = "con_" + suffix
    req = fxConnection.request.newWithToken(defaultTokenDescriptor)
    con = req.createConsortium(conName)
    util.helen.validators.validateConsortiumObject(con)
    UUID(con["consortium_id"])
    assert con["consortium_name"] == conName
    assert con["organization_id"] == util.auth.orgs["blockchain_dev_service_org"]


@describe()
@pytest.mark.smoke
@pytest.mark.consortiums
def test_consortiums_add_same_name(fxConnection):
    '''
    Multiple consortiums can have the same name, but IDs must be different.
    '''
    suffix = util.numbers_strings.random_string_generator()
    conName = "con_" + suffix
    req = fxConnection.request.newWithToken(defaultTokenDescriptor)
    con1 = req.createConsortium(conName)
    con2 = req.createConsortium(conName)
    UUID(con1["consortium_id"])
    UUID(con2["consortium_id"])
    util.helen.validators.validateConsortiumObject(con1)
    util.helen.validators.validateConsortiumObject(con2)
    assert con1["consortium_id"] != con2["consortium_id"]
    assert con1["consortium_name"] == conName
    assert con2["consortium_name"] == conName


@describe()
@pytest.mark.smoke
@pytest.mark.consortiums
def test_consortiums_empty_name(fxConnection):
    '''
    Create a consortium with an empty string as a name.
    '''
    req = fxConnection.request.newWithToken(defaultTokenDescriptor)
    con = req.createConsortium("")

    util.helen.validators.validateBadRequest(con, "/api/consortiums/", \
                                             errorCode = "MethodArgumentNotValidException", testErrorMessage = False)


@describe()
@pytest.mark.smoke
@pytest.mark.consortiums
def test_consortiums_no_name(fxConnection):
    '''
    Create a consortium with no name field.
    '''
    req = fxConnection.request.newWithToken(defaultTokenDescriptor)
    con = req.createConsortium(None)
    util.helen.validators.validateBadRequest(con, "/api/consortiums/", errorCode = "MethodArgumentNotValidException",
                                             testErrorMessage = False)


@describe()
@pytest.mark.smoke
@pytest.mark.consortiums
def test_consortiums_get_all(fxConnection):
    '''
    Test the API to retrieve all consoritiums.
    '''
    savedCons = []
    req = fxConnection.request.newWithToken(defaultTokenDescriptor)

    for i  in range(3):
        suffix = util.numbers_strings.random_string_generator()
        con = req.createConsortium("con_{}".format(suffix))
        util.helen.validators.validateConsortiumObject(con)
        savedCons.append(con)

    fetchedCons = req.getConsortiums()
    util.helen.validators.validateConsortiumListGet(fetchedCons)

    for saved in savedCons:
        found = False

        for fetched in fetchedCons:
            if saved["consortium_id"] == fetched["consortium_id"] and \
                    saved["consortium_name"] == fetched["consortium_name"]:
                found = True
                break

        assert found, "Created consortium was not retrieved."


@describe()
@pytest.mark.smoke
@pytest.mark.consortiums
def test_consortiums_get_specific(fxConnection):
    '''
    Test the API to retrieve a specific consortium.
    '''
    req = fxConnection.request.newWithToken(defaultTokenDescriptor)
    suffix = util.numbers_strings.random_string_generator()
    savedCon = req.createConsortium("con_{}".format(suffix))
    util.helen.validators.validateConsortiumObject(savedCon)

    for i  in range(3):
        suffix = util.numbers_strings.random_string_generator()
        con = req.createConsortium("con_{}".format(suffix))
        util.helen.validators.validateConsortiumObject(con)

    fetchedCon = fxConnection.request.getConsortium(savedCon["consortium_id"])
    util.helen.validators.validateConsortiumObject(fetchedCon)

    assert savedCon == fetchedCon, "Failed to retrieve the saved consortium."


@describe()
@pytest.mark.smoke
@pytest.mark.consortiums
def test_consortiums_get_nonexistant(fxConnection):
    '''
    Try to retrieve a consortium which does not exist.
    Assumes 865d9e5c-aa7d-4a69-a7b5-1744be8d56f9 won't be generated again.
    Note VB-951, that the column name is included in the error message.  I'm
    not going to exclude this test case for that.
    '''
    oldId = "865d9e5c-aa7d-4a69-a7b5-1744be8d56f9"
    req = fxConnection.request.newWithToken(defaultTokenDescriptor)
    response = req.getConsortium(oldId)

    expectedPath = "/api/consortiums/{}".format(oldId)
    util.helen.validators.validateNotFound(response,
                                           expectedPath = expectedPath,
                                           errorCode = "NotFoundException",
                                           testErrorMessage = False)


@describe()
@pytest.mark.smoke
@pytest.mark.consortiums
def test_consortiums_get_bad_format(fxConnection):
    '''
    Try to retrieve a consortium using an incorrect ID format.
    '''
    req = fxConnection.request.newWithToken(defaultTokenDescriptor)
    result = req.getConsortium("a")
    assert result["error_code"] == "MethodArgumentTypeMismatchException", \
        "Expected different error code"
    msg = "Failed to convert value of type 'java.lang.String' to required type " \
          "'java.util.UUID'"
    assert msg in result["error_message"], "Expected '{}' in the error_message".format(msg)
    assert result["status"] == 400, "Expected status 400"
    assert result["path"] == "/api/consortiums/a", "Expected different path"


@describe()
@pytest.mark.smoke
@pytest.mark.consortiums
def test_patch_consortium_name(fxConnection):
    suffix = util.numbers_strings.random_string_generator()
    conName = "con_" + suffix
    defaultDescriptor = {
        "org": "blockchain_service_dev",
        "user": "vmbc_test_con_admin",
        "role": "consortium_admin"
    }
    tokenDescriptor = util.auth.getTokenDescriptor(util.auth.ROLE_CON_ADMIN,
                                                   True,
                                                   defaultDescriptor)

    req = fxConnection.request.newWithToken(tokenDescriptor)

    conResponse = req.createConsortium(conName)
    util.helen.validators.validateConsortiumObject(conResponse)

    consortiumId = conResponse["consortium_id"]
    renameResponse = req.patchConsortium(consortiumId,
                                         newName="Fred")
    util.helen.validators.validateConsortiumPatchResponseObject(renameResponse)

    renamedCon = req.getConsortium(consortiumId)
    util.helen.validators.validateConsortiumObject(renamedCon)
    assert renameResponse["consortium_name"] == "Fred", \
        "Expected the name to change to Fred"


@describe()
@pytest.mark.smoke
@pytest.mark.consortiums
def test_patch_consortium_add_org(fxConnection, fxInitializeOrgs):
    suffix = util.numbers_strings.random_string_generator()
    conName = "con_" + suffix
    defaultDescriptor = {
        "org": "hermes_org0",
        "user": "vmbc_test_con_admin",
        "role": "consortium_admin"
    }
    tokenDescriptor = util.auth.getTokenDescriptor(util.auth.ROLE_CON_ADMIN,
                                                   True,
                                                   defaultDescriptor)
    originalOrg = tokenDescriptor["org"]
    req = fxConnection.request.newWithToken(tokenDescriptor)
    createResponse = req.createConsortium(conName)
    util.helen.validators.validateConsortiumObject(createResponse)

    patchResponse = req.patchConsortium(createResponse["consortium_id"],
                                        orgsToAdd=[util.auth.orgs["hermes_org1"]])
    util.helen.validators.validateConsortiumPatchResponseObject(patchResponse)

    assert patchResponse["organization_id"] == createResponse["organization_id"], \
        "Adding an org should not have changed the consortium's main organization id."
    assert len(patchResponse["members"]) == 2, "Expected two organizations."

    for m in patchResponse["members"]:
        assert m["organization_id"] in [util.auth.getOrgId(originalOrg),
                                        util.auth.getOrgId("hermes_org1")]
        if m["organization_id"] == util.auth.getOrgId(originalOrg):
            assert m["organization_name"] == originalOrg
        else:
            assert m["organization_name"] == "hermes_org1"


@describe()
@pytest.mark.smoke
@pytest.mark.consortiums
def test_get_consortium_orgs(fxConnection, fxInitializeOrgs):
    '''
    Get the orgs for a consortium.
    It is impossible to have a consortium with no orgs, so we have
    only a positive test case
    '''
    suffix = util.numbers_strings.random_string_generator()
    conName = "con_" + suffix
    originalOrg = "hermes_org0"
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    createResponse = req.createConsortium(conName)
    util.helen.validators.validateConsortiumObject(createResponse)
    req.patchConsortium(createResponse["consortium_id"],
                        orgsToAdd=[util.auth.orgs["hermes_org1"]])
    getOrgsResponse = req.getConsortiumOrgs(createResponse["consortium_id"])
    util.helen.validators.validateOrgListResponse(getOrgsResponse)

    assert len(getOrgsResponse) == 2, "Expected 2 orgs"

    for org in getOrgsResponse:
        assert org["organization_id"] in [util.auth.getOrgId(originalOrg),
                                          util.auth.getOrgId("hermes_org1")]

        if org["organization_id"] == util.auth.getOrgId(originalOrg):
            assert org["organization_name"] == originalOrg
        else:
            assert org["organization_name"] == "hermes_org1"
