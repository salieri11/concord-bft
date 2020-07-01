#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import collections
import random
import util.helper

import util.hermes_logging
log = util.hermes_logging.getMainLogger()

def removePrecreatedZones(zones):
    ret = []
    numRemovedZones = 0

    for zone in zones:
        if zone in util.helper.fetch_default_zone_ids():
            numRemovedZones += 1
        else:
            ret.append(zone)

    return ret, numRemovedZones

def createZoneObject(zoneType=util.helper.ZONE_TYPE_ON_PREM):
    '''
    Returns a zone object, filling in all fields with generic info.
    Does not actually send the zone to Helen.

    '''
    uniqueId = util.numbers_strings.random_string_generator()
    gpsPoint = util.numbers_strings.randomGpsPoint()
    return {
        "type": zoneType,
        "name": "Test Zone {}".format(uniqueId),
        "latitude": str(gpsPoint[0]),
        "longitude": str(gpsPoint[1]),
        "vcenter": {
            "url": "https://{}.com".format(uniqueId),
            "username": "admin@{}.com".format(uniqueId),
            "password": "vcenter_pa$$w0rd4{}".format(uniqueId)
        },
        "network": collections.OrderedDict({
            "name": "Blockchain Network for {}".format(uniqueId),
            "ip_pool": [
                "{}-{}".format(util.numbers_strings.randomIP4Address(),util.numbers_strings.randomIP4Address()),
                "{}-{}".format(util.numbers_strings.randomIP4Address(),util.numbers_strings.randomIP4Address()),
                "{}-{}".format(util.numbers_strings.randomIP4Address(),util.numbers_strings.randomIP4Address()),
                "{}".format(util.numbers_strings.randomIP4Address())
            ],
            "gateway": "{}".format(util.numbers_strings.randomIP4Address()),
            "subnet": str(random.randrange(8, 32)),
            "name_servers": [
                "{}".format(util.numbers_strings.randomIP4Address()),
                "{}".format(util.numbers_strings.randomIP4Address())
            ]
        }),
        "outbound_proxy": {
            "http_host": "http://{}".format(util.numbers_strings.randomIP4Address()),
            "http_port": 8080,
            "https_host": "https://{}".format(util.numbers_strings.randomIP4Address()),
            "https_port": 8081
        },
        "resource_pool": "Resource Pool for {}".format(uniqueId),
        "storage": "Datastore for {}".format(uniqueId),
        "folder": "Blockchain Folder for {}".format(uniqueId),
        "container_repo": {
            "url": "https://{}.com".format(uniqueId),
            "username": "user@{}.com".format(uniqueId),
            "password": "container_repo_pa$$w0rd4{}".format(uniqueId)
        },
        "log_managements": [createLogManagementObject(uniqueId)],
        "wavefront": {
            "url": "https://wavefront.com",
            "token": "<test token>"
        }
    }

def checkRequiredZoneFields(req, fullZoneObject, partialZoneObject, requiredFields):
    '''
    For each field name in requiredFields, create a zone object without it and verify
    that we get a Bad Request error when trying to use it.
    '''
    for keyToRemove in requiredFields.keys():
        removedVal = partialZoneObject[keyToRemove]
        del(partialZoneObject[keyToRemove])
        response = req.createZone(fullZoneObject)
        log.debug("Posting a new zone with the '{}' field missing".format(keyToRemove))
        util.helen.validators.validateBadRequest(response, "/api/zones")

        # Put it back.
        partialZoneObject[keyToRemove] = removedVal

        # If there are keys to remove within this key, test those also.
        if requiredFields[keyToRemove]:
            checkRequiredZoneFields(req, fullZoneObject, partialZoneObject[keyToRemove],
                                    requiredFields[keyToRemove])


def createLogManagementObject(uniqueId, destination=util.helper.LOG_DESTINATION_LOG_INTELLIGENCE):
    return {
        "destination": destination,
        "address": "https://example.com/",
        "port": 8080,
        "username": "admin@{}.com".format(uniqueId),
        "password": "logging_pa$$w0rd4{}".format(uniqueId),
        "log_insight_agent_id": 100
    }