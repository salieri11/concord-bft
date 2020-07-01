#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import pytest

from suites.case import describe, passed, failed

from fixtures.common_fixtures import fxBlockchain, fxConnection, fxHermesRunSettings, fxInitializeOrgs, fxProduct
import util.auth
import util.helen.common
import util.helen.validators
import util.helen.zone
import util.helper
import util.numbers_strings

import util.hermes_logging
log = util.hermes_logging.getMainLogger()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# HELEN ZONE TESTS
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

@describe()
@pytest.mark.zones
def test_create_zone(fxConnection, fxBlockchain):
    '''
    Basic test to create a zone.
    '''
    zoneInfo = util.helen.zone.createZoneObject()
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    orgId = util.auth.getOrgId("hermes_org0")
    response = req.createZone(zoneInfo)
    util.helen.validators.validateZoneResponse(zoneInfo, response, orgId)


@describe()
@pytest.mark.zones
@pytest.mark.skip(reason="VB-2082")
def test_create_zone_invalid_field(fxConnection, fxBlockchain):
    '''
    Oops, I misspelled the name of a key.  A bad request error should catch it.
    '''
    zoneInfo = util.helen.zone.createZoneObject()
    zoneInfo["resurse_poule"] = "The Resource Pool"
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    response = req.createZone(zoneInfo)
    util.helen.validators.validateBadRequest(response, "/api/zones")


@describe()
@pytest.mark.zones
def test_create_zone_blank_field(fxConnection, fxBlockchain):
    '''
    Oops, a key is blank. A bad request error should catch it.
    '''
    zoneInfo = util.helen.zone.createZoneObject()
    zoneInfo["name"] = "    "

    errorMessage = "Validation failed for argument [1] in org.springframework.http.ResponseEntity<com.vmware." + \
                   "blockchain.services.blockchains.zones.ZoneController$ZoneResponse> com.vmware.blockchain.services.blockchains" + \
                   ".zones.ZoneController.postZone(com.vmware.blockchain.services.blockchains.zones.Zone$Action,com.vmware." + \
                   "blockchain.services.blockchains.zones.ZoneController$ZoneRequest) throws java.lang.Exception: " + \
                   "[Field error in object 'zoneRequest' on field 'name': rejected value [    ]; codes [NotBlank.zoneRequest." + \
                   "name,NotBlank.name,NotBlank.java.lang.String,NotBlank]; arguments [org.springframework.context.support." + \
                   "DefaultMessageSourceResolvable: codes [zoneRequest.name,name]; arguments []; default message [name]]; " + \
                   "default message [Name cannot be blank]] "

    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    response = req.createZone(zoneInfo)
    util.helen.validators.validateBadRequest \
        (response, "/api/blockchains/zones", errorCode = "MethodArgumentNotValidException", \
         errorMessage = errorMessage)


@describe()
@pytest.mark.zones
def test_create_aws_zone(fxConnection, fxBlockchain):
    '''
    The zone type must be "ON_PREM" or "VMC_AWS".
    '''
    zoneInfo = util.helen.zone.createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    orgId = util.auth.getOrgId("hermes_org0")
    response = req.createZone(zoneInfo)
    util.helen.validators.validateZoneResponse(zoneInfo, response, orgId)


@describe()
@pytest.mark.zones
@pytest.mark.skip(reason="VB-2154")
def test_invalid_zone_type(fxConnection, fxBlockchain):
    '''
    The zone type must be "ON_PREM" or "VMC_AWS".  We cannot deploy
    into a mango.
    '''
    zoneInfo = util.helen.zone.createZoneObject(zoneType="mango")
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    response = req.createZone(zoneInfo)
    util.helen.validators.validateBadRequest(response, "/api/zones")


@describe()
@pytest.mark.zones
@pytest.mark.skip(reason="VB-2089")
def test_create_zone_missing_required_field(fxConnection, fxBlockchain):
    '''
    Per helen/src/main/resources/api-doc/api.yaml, some fields are required.
    e.g. See OnPremZonePost.
    '''
    # Fields which are required for the zone apis.
    requiredZoneFields = {
        "always": {
            "type": {},
            "name": {}
        },
        util.helper.ZONE_TYPE_ON_PREM: {
            "vcenter": {
                "url": {},
                "username": {},
                "password": {}
            },
            "network": {
                "name": {},
                "ip_pool": {},
                "gateway": {},
                "subnet": {},
                "name_servers": {}
            },
            "resource_pool": {},
            "storage": {},
            "folder": {}
        },
        util.helper.ZONE_TYPE_SDDC: {}
    }

    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)

    for zoneType in [util.helper.ZONE_TYPE_ON_PREM, util.helper.ZONE_TYPE_SDDC]:
        zoneObject = util.helen.zone.createZoneObject(zoneType)
        util.helen.zone.checkRequiredZoneFields(req, zoneObject, zoneObject, requiredZoneFields["always"])
        util.helen.zone.checkRequiredZoneFields(req, zoneObject, zoneObject, requiredZoneFields[zoneType])


@describe()
@pytest.mark.zones
def test_get_single_zone(fxConnection, fxBlockchain):
    '''
    Create a zone, then retrieve it.
    '''
    zoneInfo = util.helen.zone.createZoneObject()
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    createZoneResponse = req.createZone(zoneInfo)
    util.helen.validators.validateZoneFields(createZoneResponse)
    zoneId = createZoneResponse["id"]
    getZoneResponse = req.getZone(zoneId)
    util.helen.validators.validateZoneFields(getZoneResponse)
    log.info("getZoneResponse: {}".format(getZoneResponse))
    assert createZoneResponse == getZoneResponse, "Responses should be equal."


@describe()
@pytest.mark.zones
@pytest.mark.skip(reason="VB-2090")
def test_get_missing_zone(fxConnection, fxBlockchain):
    '''
    Try to get a zone which does not exist.
    7d9cea53-33d3-4fe7-8cb3-7d367d2eb30b was a unique zone id seen during testing.
    '''
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    getZoneResponse = req.getZone("7d9cea53-33d3-4fe7-8cb3-7d367d2eb30b")
    log.info("getZoneResponse: {}".format(getZoneResponse))


@describe()
@pytest.mark.zones
def test_get_invalid_zone(fxConnection, fxBlockchain):
    '''
    Try to get a zone with an invalid ID format.
    '''
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    response = req.getZone("3")
    util.helen.validators.validateBadRequest(response, "/api/blockchains/zones/3",
                                             errorCode="MethodArgumentTypeMismatchException",
                                             errorMessage="Failed to convert value of type " \
                                                          "'java.lang.String' to required type 'java.util.UUID'; " \
                                                          "nested exception is java.lang.IllegalArgumentException: " \
                                                          "Invalid UUID string: 3")


@describe()
@pytest.mark.zones
def test_get_all_zones(fxConnection, fxBlockchain):
    '''
    Create and retrieve multiple zones.
    '''
    addedZones = []
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)

    for _ in range(3):
        zoneInfo = util.helen.zone.createZoneObject()
        zoneInfo = req.createZone(zoneInfo)

        # The /api/blockchains/zones api call returns a subset of fields for each zone.
        addedZones.append({
            "type": zoneInfo["type"],
            "name": zoneInfo["name"],
            "latitude": zoneInfo["latitude"],
            "longitude": zoneInfo["longitude"],
            "id": zoneInfo["id"]
        })

    retrievedZones = req.getZones()
    util.helen.validators.util.helen.validators.validateZoneFieldsList(retrievedZones)

    for addedZone in addedZones:
        log.debug("Looking for zone {}".format(addedZone["id"]))
        assert addedZone in retrievedZones, "Zone {} not found in {}".format(addedZone, retrievedZones)


@describe()
@pytest.mark.zones
@pytest.mark.skip(reason="VB-2150")
def test_get_all_no_zones(fxConnection, fxBlockchain):
    '''
    Get all zones when there are none.  Uses the delete api if there are existing zones.
    That does *not* mean this function tests the delete api, since it's possible this
    test gets called before any zones have been added.
    '''
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    retrievedZones = req.getZones()
    util.helen.validators.util.helen.validators.validateZoneFieldsList(retrievedZones)

    if retrievedZones:
        for zoneId in map(lambda x: x["id"], retrievedZones):
            log.debug("Deleting zone {}".format(zoneId))
            deletedZone = req.deleteZone(zoneId)
            util.helen.validators.validateDeletedZoneResponse(deletedZone)

        retrievedZones = req.getZones()
        util.helen.validators.util.helen.validators.validateZoneFieldsList(retrievedZones)

    assert not retrievedZones, "Expected an empty list."


@describe()
@pytest.mark.zones
def test_delete_some_zones(fxConnection, fxBlockchain):
    '''
    Add some zones, then delete them.
    '''
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    zonesToAdd = list(map(lambda _: util.helen.zone.createZoneObject(), range(3)))
    zoneIds = list(map(lambda zone: req.createZone(zone)["id"], zonesToAdd))
    deleteResponses = list(map(lambda zoneId: req.deleteZone(zoneId), zoneIds))
    expectedResponses = list(map(lambda zoneId: {"id": zoneId}, zoneIds))
    assert expectedResponses == deleteResponses, "Zone IDs submitted for deletion, '{}' " \
                                                 "do not match the deletion responses, '{}'.".format(expectedResponses, deleteResponses)

    allZones = req.getZones()
    util.helen.validators.util.helen.validators.validateZoneFieldsList(allZones)
    allZoneIds = list(map(lambda zone: zone["id"], allZones))
    log.info("zoneIds added and deleted: {}".format(zoneIds))
    log.info("zoneIds retrieved after deletion: {}".format(allZoneIds))


@describe()
@pytest.mark.zones
def test_delete_all_zones(fxConnection, fxBlockchain):
    '''
    Add some zones, then delete all zones. Be sure Helen is still responsive
    after doing so.
    '''
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    zonesToAdd = list(map(lambda _: util.helen.zone.createZoneObject(), range(3)))
    [req.createZone(zone) for zone in zonesToAdd]
    allZoneIds = list(map(lambda zone: zone["id"], req.getZones()))

    # Kinda defeats the purpose of a test to delete all zones.  But this will become
    # valid when we stop hard coding some zones.
    allZoneIds, numPrecreatedZonesRemoved = util.helen.zone.removePrecreatedZones(allZoneIds)

    expectedResponses = list(map(lambda zoneId: {"id": zoneId}, allZoneIds))
    deleteResponses = list(map(lambda zoneId: req.deleteZone(zoneId), allZoneIds))
    assert expectedResponses == deleteResponses, "Zone IDs submitted for deletion, '{}' " \
                                                 "do not match the deletion responses, '{}'.".format(expectedResponses, deleteResponses)
    assert len(req.getZones()) == numPrecreatedZonesRemoved, "Expected to have zero zones after deleting them all."

    # Now just check Helen.
    zoneObject = util.helen.zone.createZoneObject()
    newZoneId = req.createZone(zoneObject)["id"]
    util.helen.validators.validateZoneResponse(zoneObject,
                                               req.getZone(newZoneId),
                                               util.auth.getOrgId("hermes_org0"))


@describe()
@pytest.mark.zones
@pytest.mark.skip(reason="VB-2090")
def test_delete_nonexistant_node(fxConnection, fxBlockchain):
    '''
    Try to delete a zone which does not exist.
    7d9cea53-33d3-4fe7-8cb3-7d367d2eb30b was a unique zone id seen during testing.
    '''
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    deleteZoneResponse = req.deleteZone("7d9cea53-33d3-4fe7-8cb3-7d367d2eb30b")
    log.info("getZoneResponse: {}".format(deleteZoneResponse))


@describe()
@pytest.mark.zones
def test_delete_invalid_uuid_node(fxConnection, fxBlockchain):
    '''
    Try to delete a zone with an invalid ID format.
    '''
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    response = req.deleteZone("3")
    util.helen.validators.validateBadRequest(response, "/api/blockchains/zones/3",
                                             errorCode="MethodArgumentTypeMismatchException",
                                             errorMessage="Failed to convert value of type " \
                                                          "'java.lang.String' to required type 'java.util.UUID'; " \
                                                          "nested exception is java.lang.IllegalArgumentException: " \
                                                          "Invalid UUID string: 3")

@describe()
@pytest.mark.zones
def test_no_log_management(fxConnection, fxBlockchain):
    '''
    The log management section is optional.  Remove it and test.
    '''
    zoneInfo = util.helen.zone.createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    orgId = util.auth.getOrgId("hermes_org0")
    del(zoneInfo["log_managements"])
    response = req.createZone(zoneInfo)
    util.helen.validators.validateZoneResponse(zoneInfo, response, orgId)


@describe()
@pytest.mark.zones
def test_log_management_multiple(fxConnection, fxBlockchain):
    '''
    Should be able to specify multiple log management structures.
    '''
    zoneInfo = util.helen.zone.createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    orgId = util.auth.getOrgId("hermes_org0")
    newLogManagementObject = util.helen.zone.createLogManagementObject(util.numbers_strings.random_string_generator(),
                                                                       destination=util.helper.LOG_DESTINATION_LOG_INSIGHT)
    zoneInfo["log_managements"].append(newLogManagementObject)
    response = req.createZone(zoneInfo)
    util.helen.validators.validateZoneResponse(zoneInfo, response, orgId)


@describe()
@pytest.mark.zones
@pytest.mark.skip(reason="VB-2157")
def test_log_management_no_destination(fxConnection, fxBlockchain):
    '''
    Destination is a required field.
    '''
    zoneInfo = util.helen.zone.createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    orgId = util.auth.getOrgId("hermes_org0")
    del(zoneInfo["log_managements"][0]["destination"])
    response = req.createZone(zoneInfo)
    log.info("**** test_log_management_no_destination response: {}".format(response))
    # util.helen.validators.validateBadRequest(...)


@describe()
@pytest.mark.zones
@pytest.mark.skip(reason="VB-2154")
def test_log_management_wrong_destination_type(fxConnection, fxBlockchain):
    '''
    Must be one of [LOG_INTELLIGENCE, LOG_INSIGHT].
    '''
    zoneInfo = util.helen.zone.createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    orgId = util.auth.getOrgId("hermes_org0")
    zoneInfo["log_managements"][0]["destination"] = 3
    response = req.createZone(zoneInfo)
    # util.helen.validators.validateBadRequest(...)


@describe()
@pytest.mark.zones
@pytest.mark.skip(reason="VB-2157")
def test_log_management_no_address(fxConnection, fxBlockchain):
    '''
    Required field.
    '''
    zoneInfo = util.helen.zone.createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    orgId = util.auth.getOrgId("hermes_org0")
    del(zoneInfo["log_managements"][0]["address"])
    response = req.createZone(zoneInfo)
    log.info("**** test_log_management_no_address response: {}".format(response))
    # util.helen.validators.validateBadRequest(...)


@describe()
@pytest.mark.zones
def test_log_management_wrong_address_type(fxConnection, fxBlockchain):
    '''
    Must be a string.
    '''
    zoneInfo = util.helen.zone.createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    orgId = util.auth.getOrgId("hermes_org0")
    zoneInfo["log_managements"][0]["address"] = 3
    response = req.createZone(zoneInfo)
    # The product auto-converts it.  That's fine.
    zoneInfo["log_managements"][0]["address"] = "3"
    util.helen.validators.validateZoneResponse(zoneInfo, response, orgId)


@describe()
@pytest.mark.zones
@pytest.mark.skip(reason="VB-2157")
def test_log_management_no_username(fxConnection, fxBlockchain):
    '''
    Required field.
    '''
    zoneInfo = util.helen.zone.createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    orgId = util.auth.getOrgId("hermes_org0")
    del(zoneInfo["log_managements"][0]["username"])
    response = req.createZone(zoneInfo)
    log.info("**** test_log_management_no_usernames response: {}".format(response))
    # util.helen.validators.validateBadRequest(...)


@describe()
@pytest.mark.zones
def test_log_management_wrong_username_type(fxConnection, fxBlockchain):
    '''
    Must be a string
    '''
    zoneInfo = util.helen.zone.createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    orgId = util.auth.getOrgId("hermes_org0")
    zoneInfo["log_managements"][0]["username"] = 3
    response = req.createZone(zoneInfo)
    # The product auto-converts it.  That's fine.
    zoneInfo["log_managements"][0]["username"] = "3"
    util.helen.validators.validateZoneResponse(zoneInfo, response, orgId)


@describe()
@pytest.mark.zones
@pytest.mark.skip(reason="VB-2157")
def test_log_management_no_password(fxConnection, fxBlockchain):
    '''
    Required field.
    '''
    zoneInfo = util.helen.zone.createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    orgId = util.auth.getOrgId("hermes_org0")
    del(zoneInfo["log_managements"][0]["password"])
    response = req.createZone(zoneInfo)
    log.info("**** test_log_management_no_password response: {}".format(response))
    # util.helen.validators.validateBadRequest(...)


@describe()
@pytest.mark.zones
def test_log_management_wrong_password_type(fxConnection, fxBlockchain):
    '''
    Must be a string
    '''
    zoneInfo = util.helen.zone.createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    orgId = util.auth.getOrgId("hermes_org0")
    zoneInfo["log_managements"][0]["password"] = 3
    response = req.createZone(zoneInfo)
    # The product auto-converts it.  That's fine.
    zoneInfo["log_managements"][0]["password"] = "3"
    util.helen.validators.validateZoneResponse(zoneInfo, response, orgId)


@describe()
@pytest.mark.zones
@pytest.mark.skip(reason="VB-2099 (not supported yet)")
def test_patch_zone_no_change(fxConnection, fxBlockchain):
    '''
    Create a zone, patch it with identical information, and ensure no fields changed.
    '''
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    zoneInfo = util.helen.zone.createZoneObject()
    createResponse = req.createZone(zoneInfo)
    patchResponse = req.patchZone(createResponse["id"], zoneInfo)
    getResponse = req.getZone(createResponse["id"])
    assert createResponse == patchResponse, "Expected the zone patch response, '{}' " \
                                            "to equal the zone creation response, '{}'".format(patchResponse, createResponse)
    assert createResponse == getResponse, "Expected the zone get response, '{}' " \
                                          "to equal the zone creation response, '{}'".format(getResponse, createResponse)


@describe()
@pytest.mark.zones
@pytest.mark.skip(reason="VB-2099 (not supported yet)")
def test_patch_zone_change_fields(fxConnection, fxBlockchain):
    '''
    Create a zone, patch all fields with new values.
    '''
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    zoneInfo = util.helen.zone.createZoneObject()
    createResponse = req.createZone(zoneInfo)
    zoneId = createResponse["id"]

    newZoneInfo = util.helen.zone.createZoneObject()
    patchResponse = req.patchZone(zoneId, newZoneInfo)
    assert patchResponse != createResponse, "Expected the response for creating the " \
                                            "zone, '{}', to differ from the response for patching " \
                                            "the zone, '{}'".format(createResponse, patchResponse)

    getResponse = req.getZone(zoneId)
    assert getResponse == patchResponse, "Expected the zone's new get response, '{}' " \
                                         "to equal the patch response, '{}'".format(getResponse, patchResponse)


@describe()
@pytest.mark.zones
@pytest.mark.skip(reason="VB-2099 (not supported yet)")
def test_patch_zone_clear_required_fields(fxConnection, fxBlockchain):
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    zoneInfo = util.helen.zone.createZoneObject()
    zoneId = req.createZone(zoneInfo)["id"]

    newZoneInfo = util.helen.zone.createZoneObject()
    patchResponse = req.patchZone(zoneId)

    getResponse = req.getZone(zoneId)
    assert getResponse == patchResponse, "Expected the zone's new get response, '{}' " \
                                         "to equal the patch response, '{}'".format(getResponse, patchResponse)


@describe()
@pytest.mark.zones
@pytest.mark.skip(reason="VB-2099 (not supported yet)")
def test_patch_zone_missing_zone(fxConnection, fxBlockchain):
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    req.patchZone("7d9cea53-33d3-4fe7-8cb3-7d367d2eb30b")


@describe()
@pytest.mark.zones
@pytest.mark.skip(reason="VB-2099 (not supported yet)")
def test_patch_zone_invalid_uuid(fxConnection, fxBlockchain):
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    response = req.patchZone("3", util.helen.zone.createZoneObject())
    util.helen.validators.validateBadRequest(response, "/api/blockchains/zones/3",
                                             errorCode="MethodArgumentTypeMismatchException",
                                             errorMessage="Failed to convert value of type " \
                                                          "'java.lang.String' to required type 'java.util.UUID'; " \
                                                          "nested exception is java.lang.IllegalArgumentException: " \
                                                          "Invalid UUID string: 3")


@describe()
@pytest.mark.zones
def test_missing_outbound_proxy(fxConnection, fxBlockchain):
    '''
    The entire outbound_proxy field is missing.
    '''
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    orgId = util.auth.getOrgId("hermes_org0")
    zoneInfo = util.helen.zone.createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
    del(zoneInfo["outbound_proxy"])
    response = req.createZone(zoneInfo)
    util.helen.validators.validateZoneResponse(zoneInfo, response, orgId)


@describe()
@pytest.mark.zones
def test_empty_outbound_proxy_fields(fxConnection, fxBlockchain):
    '''
    All fields of the outbound proxy present, with empty values.
    '''
    zoneInfo = util.helen.zone.createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    orgId = util.auth.getOrgId("hermes_org0")
    zoneInfo["outbound_proxy"] = {
        "http_host": None,
        "http_port": None,
        "https_host": None,
        "https_port": None
    }
    response = req.createZone(zoneInfo)
    util.helen.validators.validateZoneResponse(zoneInfo, response, orgId)


@describe()
@pytest.mark.zones
def test_empty_outbound_http_proxy(fxConnection, fxBlockchain):
    '''
    User defines an https proxy, and nothing for the http proxy.
    '''
    zoneInfo = util.helen.zone.createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    orgId = util.auth.getOrgId("hermes_org0")
    zoneInfo["outbound_proxy"]["http_host"] = None
    zoneInfo["outbound_proxy"]["http_port"] = None
    response = req.createZone(zoneInfo)
    util.helen.validators.validateZoneResponse(zoneInfo, response, orgId)


@describe()
@pytest.mark.zones
def test_empty_outbound_https_proxy(fxConnection, fxBlockchain):
    '''
    User defines an http proxy, and nothing for the https proxy.
    '''
    zoneInfo = util.helen.zone.createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    orgId = util.auth.getOrgId("hermes_org0")
    zoneInfo["outbound_proxy"]["https_host"] = None
    zoneInfo["outbound_proxy"]["https_port"] = None
    response = req.createZone(zoneInfo)
    util.helen.validators.validateZoneResponse(zoneInfo, response, orgId)


@describe()
@pytest.mark.zones
def test_invalid_outbound_host(fxConnection, fxBlockchain):
    '''
    User defines an invalid value for one of the outbound proxy hosts.
    We just take whatever they provide and make it a string.
    '''
    zoneInfo = util.helen.zone.createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    orgId = util.auth.getOrgId("hermes_org0")
    zoneInfo["outbound_proxy"]["https_host"] = 3
    response = req.createZone(zoneInfo)
    util.helen.validators.validateZoneResponse(zoneInfo, response, orgId)


@describe()
@pytest.mark.zones
@pytest.mark.skip(reason="VB-2176")
def test_invalid_outbound_port(fxConnection, fxBlockchain):
    '''
    User defines an invalid value for one of the outbound proxy ports.
    This should be a 400 Bad Request.
    '''
    zoneInfo = util.helen.zone.createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM)
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    orgId = util.auth.getOrgId("hermes_org0")
    zoneInfo["outbound_proxy"]["https_port"] = "a"
    response = req.createZone(zoneInfo)
    util.helen.validators.validateBadRequest(response, "/api/zones")