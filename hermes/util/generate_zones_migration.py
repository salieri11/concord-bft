#!/usr/bin/env python3
############################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential #
############################################################################

import os
import errno
import argparse
import json
import util.helper as helper
import util.hermes_logging as hermes_logging
import uuid
from typing import Any, Dict
from collections import OrderedDict

log = hermes_logging.getMainLogger()

def _parse_arguments() -> Dict[str, Any]:
    """
    Parse command-line arguments
    :return: Dictionary containing parsed arguments and their associated value
    """
    parser = argparse.ArgumentParser(description="Zones Migrations generator")
    parser.add_argument(
        "--config",
        default=helper.CONFIG_JSON,
        help="Path of the user config json file"
    )
    parser.add_argument(
        "--blockchainLocation",
        default=helper.LOCATION_SDDC,
        choices=[helper.LOCATION_SDDC, helper.LOCATION_ONPREM],
        help="Type of zone, sddc/onprem"
    )
    parser.add_argument(
        "--migrationFile",
        default=helper.MIGRATION_FILE,
        help="Migration file to write into"
    )
    return vars(parser.parse_args())


def _build_migrations_standalone(user_config_file: str, zone_type: str, migration_file: str) -> None:
    """
    Builds the db migrations from standalone invocation of this script
    :param user_config_file: Path to the user_config.json file
    :param zone_type: sddc or onprem
    :param migration_file: Migration file path
    :return: None
    """
    user_config = helper.loadConfigFile(filepath=user_config_file)
    build_migrations(user_config, zone_type, migration_file)


def build_migrations(user_config, zone_type, migration_file):
    """
    Internal build migrations code
    :param user_config: Data from the user_config file, extracted as a dictionary
    :param zone_type: sddc or onprem
    :param migration_file: Migration file path
    :return: True/False if migrations were applied or not
    """
    if os.path.dirname(migration_file) != helper.MIGRATION_BASE_PATH:
        raise Exception("Migrations can only be created in {}".format(helper.MIGRATION_BASE_PATH))

    zones = get_zones(user_config, zone_type)
    if not zones:
        return False

    zones_row_keys_list = write_zones(zones, zone_type, migration_file)
    zones_row_keys_str = ','.join(zones_row_keys_list)
    helper.replace_key(helper.PROPERTIES_TEST_FILE, helper.PROPERTIES_VMBC_ENABLED_VMC_ZONES, zones_row_keys_str)
    return True


def get_zones(config, zone_type):
    """
    Given zone_type, gets zones from config
    :param config: user config dictionary
    :param zone_type:
    :return: List of zones from the user config
    """
    zones = []
    if zone_type.lower() == helper.LOCATION_SDDC:
        zones = config["persephoneTests"]["provisioningService"]["zones"][helper.LOCATION_SDDC]
    elif zone_type.lower() == helper.LOCATION_ONPREM:
        zones = config["persephoneTests"]["provisioningService"]["zones"][helper.LOCATION_ONPREM]
    else:
        log.warning("Migrations are not performed for blockchain location: {}".format(zone_type))

    return zones


def write_zones(zones, zone_type, migration_file):
    """
    Given list of zones, writes them to the sql migration file
    :param zones: List of zones to write into migration file
    :param zone_type: sddc or onprem
    :param migration_file: Migration file to write to/generate
    :return: List of row_key in 'entity' table in Helen db
    """
    if not os.path.exists(os.path.dirname(migration_file)):
        try:
            os.makedirs(os.path.dirname(migration_file))
        except OSError as exc:  # Guard against race condition
            if exc.errno != errno.EEXIST:
                raise

    row_keys = []
    with open(migration_file, 'w') as fp:
        for zone in zones:
            row_key = str(uuid.uuid4())
            write_insert_header(row_key, fp)
            body = build_vmc_body(zone) if zone_type == helper.LOCATION_SDDC else build_onprem_body(zone)
            json.dump(body, fp, indent=2)
            write_insert_footer(fp)
            row_keys.append(row_key)
        fp.close()
    return row_keys


def write_insert_header(row_key, fp):
    """
    Writes the beginning INSERT statement
    :param row_key: row_key in the entity table
    :param fp: file pointer to write into
    :return: None
    """
    insert_header = "INSERT into entity(row_key, column_name, version, body, user_id, user_name) VALUES ('{}', 'helen.zone', 1, '".format(row_key)
    fp.write(insert_header)


def write_insert_footer(fp):
    """
    Writes the closing of the INSERT statement
    :param fp: file pointer to write into
    :return: None
    """
    insert_footer = "'::JSONB, '{}', '{}') ON CONFLICT (row_key,version) DO UPDATE SET body = excluded.body;\n\n\n"\
        .format(helper.MIGRATION_USER_ID, helper.MIGRATION_USER_NAME)
    fp.write(insert_footer)


def build_vmc_body(zone):
    """
    Builds json/dictionary body for the given VMC_AWS zone
    :param zone: VMC_AWS zone dictionary
    :return: Dictionary formatted body
    """
    body = OrderedDict()
    body["name"] = get_name(zone)
    body["latitude"] = get_latitude(zone)
    body["longitude"] = get_longitude(zone)
    body["type"] = helper.ZONE_TYPE_SDDC
    body["csp_url"] = get_csp_url(zone)
    body["vmc_url"] = get_vmc_url(zone)
    body["refresh_token"] = get_refresh_token(zone)
    body["organization"] = get_organization(zone)
    body["datacenter"] = get_datacenter(zone)
    body["resource_pool"] = get_resource_pool(zone)
    body["storage"] = get_storage(zone)
    body["folder"] = get_folder(zone)
    body["network"] = get_network(zone)
    body["wavefront"] = get_wavefront(zone)
    body["log_managements"] = get_vmc_log_managements(zone)
    return body


def build_onprem_body(zone):
    """
    Builds json/dictionary body for the given VMC_AWS zone
    :param zone: VMC_AWS zone dictionary
    :return: Dictionary formatted body
    """
    body = OrderedDict()
    body["name"] = get_name(zone)
    body["latitude"] = get_latitude(zone)
    body["longitude"] = get_longitude(zone)
    body["type"] = helper.ZONE_TYPE_ON_PREM
    body["vcenter"] = get_vcenter(zone)
    body["resource_pool"] = get_resource_pool(zone)
    body["storage"] = get_storage(zone)
    body["folder"] = get_folder(zone)
    body["network"] = get_network(zone)
    body["wavefront"] = get_wavefront(zone)
    body["log_managements"] = get_vmc_log_managements(zone)
    return body


def get_name(zone):
    if "info" in zone and "labels" in zone["info"] and "name" in zone["info"]["labels"]:
        return zone["info"]["labels"]["name"]
    else:
        raise Exception('zone["info"]["labels"]["name"] is not configured in zone')


def get_latitude(zone):
    if "info" in zone and "labels" in zone["info"] and "geo-latitude" in zone["info"]["labels"]:
        return zone["info"]["labels"]["geo-latitude"]
    else:
        raise Exception('zone["info"]["labels"]["geo-latitude"] is not configured in zone')


def get_longitude(zone):
    if "info" in zone and "labels" in zone["info"] and "geo-longitude" in zone["info"]["labels"]:
        return zone["info"]["labels"]["geo-longitude"]
    else:
        raise Exception('zone["info"]["labels"]["geo-longitude"] is not configured in zone')


def get_csp_url(zone):
    if "authentication" in zone and "address" in zone["authentication"]:
        return zone["authentication"]["address"]
    else:
        raise Exception('zone["authentication"]["address"] is not configured in zone')


def get_vmc_url(zone):
    if "api" in zone and "address" in zone["api"]:
        return zone["api"]["address"]
    else:
        raise Exception('zone["api"]["address"] is not configured in zone')


def get_refresh_token(zone):
    if "authentication" in zone and "credential" in zone["authentication"] and "tokenCredential" in \
            zone["authentication"]["credential"] and "token" in zone["authentication"]["credential"]["tokenCredential"]:
        return zone["authentication"]["credential"]["tokenCredential"]["token"]
    else:
        raise Exception('zone["authentication"]["credential"]["tokenCredential"]["token"] is not configured in zone')


def get_organization(zone):
    if "organization" in zone:
        return zone["organization"]
    else:
        raise Exception('zone["organization"] is not configured in zone')


def get_vcenter(zone):
    if "api" in zone:
        vcenter = OrderedDict()
        vcenter["url"] = zone["api"]["address"]
        vcenter["username"] = zone["api"]["credential"]["passwordCredential"]["username"]
        vcenter["password"] = zone["api"]["credential"]["passwordCredential"]["password"]
        return vcenter
    else:
        raise Exception('zone["api"] is not configured in zone')


def get_datacenter(zone):
    if "datacenter" in zone:
        return zone["datacenter"]
    else:
        raise Exception('zone["datacenter"] is not configured in zone')


def get_resource_pool(zone):
    if "vsphere" in zone and "resourcePool" in zone["vsphere"]:
        return zone["vsphere"]["resourcePool"]
    else:
        raise Exception('zone["vsphere"]["resourcePool"] is not configured in zone')


def get_storage(zone):
    if "vsphere" in zone and "datastore" in zone["vsphere"]:
        return zone["vsphere"]["datastore"]
    else:
        raise Exception('zone["vsphere"]["datastore"] is not configured in zone')


def get_folder(zone):
    if "vsphere" in zone and "folder" in zone["vsphere"]:
        return zone["vsphere"]["folder"]
    else:
        raise Exception('zone["vsphere"]["folder"] is not configured in zone')


def get_network(zone):
    network = OrderedDict()
    if "vsphere" in zone and "network" in zone["vsphere"]:
        network["name"] = zone["vsphere"]["network"]["name"]
        network["gateway"] = helper.long2ip(zone["vsphere"]["network"]["gateway"])
        network["subnet"] = str(zone["vsphere"]["network"]["subnet"])
        network["name_servers"] = zone["vsphere"]["network"]["nameServers"]
        return network
    else:
        raise Exception('zone["vsphere"]["network"] is not configured in zone')


def get_wavefront(zone):
    if "wavefront" in zone:
        wavefront = OrderedDict()
        wavefront["url"] = zone["wavefront"]["url"]
        wavefront["token"] = zone["wavefront"]["token"]
        return wavefront
    else:
        raise Exception('zone["wavefront"] is not configured in zone')


def get_vmc_log_managements(zone):
    if "logManagements" in zone:
        log_managements = []
        for lm_entry in zone["logManagements"]:
            log_management = OrderedDict()
            log_management["destination"] = lm_entry["destination"]
            log_management["address"] = lm_entry["endpoint"]["address"]
            log_management["token"] = lm_entry["endpoint"]["credential"]["tokenCredential"]["token"]
            log_managements.append(log_management)
        return log_managements
    else:
        raise Exception('zone["logManagements"] is not configured in zone')


def get_onprem_log_managements(zone):
    if "logManagements" in zone:
        log_managements = []
        for lm_entry in zone["logManagements"]:
            log_management = OrderedDict()
            log_management["destination"] = lm_entry["destination"]
            log_management["address"] = lm_entry["endpoint"]["address"]
            log_management["username"] = lm_entry["endpoint"]["credential"]["passwordCredential"]["username"]
            log_management["password"] = lm_entry["endpoint"]["credential"]["passwordCredential"]["password"]
            log_managements.append(log_management)
        return log_managements
    else:
        raise Exception('zone["logManagements"] is not configured in zone')


def main():
    """
    Main entry point of this script
    :return: None
    """
    args = _parse_arguments()
    _build_migrations_standalone(args["config"], args["blockchainLocation"], args["migrationFile"])


if __name__ == "__main__":
    main()
