"""!/usr/bin/python3
Script to clean up VMC sddc's for blockchain deployments
Cleans up vm's older than n hours
Parses vm name to extract nat and public ip to delete
TODO: Create VMC class to act on related objects
#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
"""
import argparse
import datetime
import ipaddress
import json
import os
import pytz
import re
import sys
import tempfile
import time
import requests
from config import common
from lib import utils, ipam_utils
from lib import vsphere


DC_CONSTANTS = None

logger = utils.setup_logging()


def is_blockchain_vm(uuid_string):
    BC_UUID_PATTERN = re.compile(r'^[\da-f]{8}-([\da-f]{4}-){3}[\da-f]{12}-'\
            '[\da-f]{8}-([\da-f]{4}-){3}[\da-f]{12}$', re.IGNORECASE)
    if BC_UUID_PATTERN.match(uuid_string) is None:
        return False
    else:
        return True


def nwaddress_to_hex(address):
    """
        Convert address to hex
        if cidr return first address on the block
    """
    if address.find("/") == -1:
        return '{:02x}{:02x}{:02x}{:02x}'.format(
                *map(int, address.split(".")))
    else:
        network = ipaddress.IPv4Network(address)
        first_address = network[0]
        return '{:02x}{:02x}{:02x}{:02x}'.format(
                *map(int, first_address.exploded.split(".")))


def get_network_segment(name, dc_data):
    """
        Get segment data given name
    """
    header = utils.get_auth_header(common.CSP_PROD,
                                   DC_CONSTANTS["CSP_API_TOKEN"])
    req = requests.get("%s/vmc/reverse-proxy/api/orgs/%s/sddcs/%s/"
                      "policy/api/v1/infra/tier-1s/cgw/segments/%s" %
                       (dc_data["nsx_mgr"], DC_CONSTANTS["ORG_ID"],
                        dc_data['id'], name), headers=header)
    if req.status_code == 200:
        logger.info("Received network segment data for %s" % name)
        return req.json()
    else:
        logger.error("Error getting network segment data %s" % req.text)
        return None


def get_public_ips(nsx_mgr, org, sddc):
    """
        Get all public ips from vmc nsx mgr
    """
    header = utils.get_auth_header(common.CSP_PROD,
                                   DC_CONSTANTS["CSP_API_TOKEN"])
    req = requests.get('%s/vmc/reverse-proxy/api/orgs/%s/sddcs/%s/'
            'cloud-service/api/v1/infra/public-ips/' %
                       (nsx_mgr, org, sddc), headers=header)
    if req.status_code == 200:
        return req.json()
    else:
        logger.error("Error getting nat rules for sddc %s %s" %
                    (sddc, req.text))


def get_public_ip(nsx_mgr, org, sddc, publicip):
    """
        Get given public ip from vmc nsx mgr
    """
    header = utils.get_auth_header(common.CSP_PROD,
                                   DC_CONSTANTS["CSP_API_TOKEN"])
    req = requests.get('%s/vmc/reverse-proxy/api/orgs/%s/sddcs/%s/'
        'cloud-service/api/v1/infra/public-ips/%s' %
                       (nsx_mgr, org, sddc, publicip), headers=header)
    logger.info(req.json())
    return req


def delete_public_ip(nsx_mgr, org, sddc, publicip):
    """
        Delete public ip from vmc sddc given public ip
    """
    header = utils.get_auth_header(common.CSP_PROD,
                                   DC_CONSTANTS["CSP_API_TOKEN"])
    req = requests.delete('%s/vmc/reverse-proxy/api/orgs/%s/sddcs/%s/'
                          'cloud-service/api/v1/infra/public-ips/%s' %
                          (nsx_mgr, org, sddc, publicip), headers=header)
    if req.status_code == 200:
        logger.info("Public ip %s deleted" % publicip)
    else:
        logger.error("Error deleting public ip %s" % publicip)


def get_nat_rule(nsx_mgr, org, sddc, natrule):
    """
        Get nat rule for vmc sddc
    """
    header = utils.get_auth_header(common.CSP_PROD,
                                   DC_CONSTANTS["CSP_API_TOKEN"])
    req = requests.get("%s/vmc/reverse-proxy/api/orgs/%s/sddcs/%s/"
                      "policy/api/v1/infra/tier-1s/cgw/nat/USER/nat-rules/%s" %
                       (nsx_mgr, org, sddc, natrule), headers=header)
    if req.status_code != 200:
        logger.error("Error getting nat rule %s" % natrule)
        return None
    logger.info(req.json())
    return req.json()


def get_nat_rules(nsx_mgr, org, sddc):
    """
        Get nat rule for sddc
    """
    header = utils.get_auth_header(common.CSP_PROD,
                                   DC_CONSTANTS["CSP_API_TOKEN"])
    req = requests.get("%s/vmc/reverse-proxy/api/orgs/%s/sddcs/%s/"
                      "policy/api/v1/infra/tier-1s/cgw/nat/USER/nat-rules" %
                       (nsx_mgr, org, sddc), headers=header)
    if req.status_code == 200:
        return req.json()
    else:
        logger.error("Error getting nat rules for sddc %s %s" %
                    (sddc, req.text))


def delete_nat_rule(nsx_mgr, org, sddc, natrule):
    """
        Delete nat rule
    """
    header = utils.get_auth_header(common.CSP_PROD,
                                   DC_CONSTANTS["CSP_API_TOKEN"])
    req = requests.delete("%s/vmc/reverse-proxy/api/orgs/%s/sddcs/%s/"
                         "policy/api/v1/infra/tier-1s/cgw/nat/USER/"
                         "nat-rules/%s" %
                          (nsx_mgr, org, sddc, natrule), headers=header)
    if req.status_code == 200:
        logger.info("Nat rule %s deleted" % natrule)
    else:
        logger.error("Error deleting nat rule %s" % natrule)


def get_dated_vms(vcobj, folder, hours):
    """
        Get vm's older than hours hrs for folder
    """
    delta = (datetime.datetime.now(pytz.timezone("UTC")) -
                datetime.timedelta(hours=hours))
    dated = []
    vms = vcobj.get_vms_from_folder(folder)
    for vm in vms:
        task_collector = vcobj.get_vm_task_collector(vm)
        if task_collector is None:
            continue
        for task in task_collector.ReadNext(50):
            if (task.name is not None and
                task.name.info.name == "PowerOn" and task.state == "success"):
                if task.completeTime.replace(tzinfo=pytz.timezone("UTC")) < delta:
                    logger.info("Cleaning up vm %s older than %s hrs" %
                          (vm.name, hours))
                    dated.append(vm)
        task_collector.DestroyCollector()
    return dated


def clean_sddc_folder(vcobj, folder, dc_dict,
                      hours=0, reap_ipam=False):
    """
        Cleanup sddc folder with option of deleting vms
        older than 'hours'
        Should be leaf folder
    """
    cleanup_state = True
    if hours == 0:
        folder_vms = vcobj.get_vms_from_folder(folder)
    else:
        folder_vms = get_dated_vms(vcobj, folder, hours)
    vms = [vm for vm in folder_vms if is_blockchain_vm(vm.name) is True]
    logger.info("Cleaning up %s vm resources" % len(vms))
    metadata = utils.get_network_metadata([vm.name for vm in vms])
    if reap_ipam is True:
        if delete_ipam_resources(vms, dc_dict, metadata) is False:
            cleanup_state = False
    delete_network_resources(metadata, dc_dict)
    vcobj.delete_vms(vms)
    return cleanup_state


def delete_ipam_resources(vms, dc_dict, metadata):
    """
        Retrieve cidr and ipaddr of vm to delete ipam entry
    """
    success = True
    for vm in vms:
        natid = metadata[vm.name]
        data = get_nat_rule(dc_dict["nsx_mgr"],
            DC_CONSTANTS["ORG_ID"], dc_dict['id'], natid)
        if data is None:
            logger.info("Nat rule does not exist, checking for guest ip")
            if vm.runtime.powerState == "poweredOn":
                ipaddr = vm.guest.ipAddress
            else:
                logger.info("Vm not powered on, skipping ipam cleanup")
                continue
        else:
            ipaddr = data["source_network"]
        logger.info("Deleting ipam for %s with address %s" %
                    (vm.name, ipaddr))
        ipaddr_hex = ipam_utils.nwaddress_to_hex(ipaddr)
        nw_segment = vm.network[0].name
        nw_segment_data = get_network_segment(nw_segment, dc_dict)
        cidr_hex = ipam_utils.nwaddress_to_hex(
                                nw_segment_data['subnets'][0]['network'])
        if ipam_utils.delete_ipam_entry(nw_segment,
                    ipaddr_hex, cidr_hex, dc_dict["id"]) is False:
            success = False
    return success


def delete_network_resources(metadata, dc_dict):
    """
        Delete nat rule and public ip given list of ids
    """
    for netid in metadata.values():
        delete_nat_rule(dc_dict["nsx_mgr"],
            DC_CONSTANTS["ORG_ID"], dc_dict['id'], netid)
    for netid in metadata.values():
        delete_public_ip(dc_dict["nsx_mgr"],
                         DC_CONSTANTS["ORG_ID"], dc_dict['id'], netid)


def get_orphaned_nwids(dc_dict, entity, vms):
    """
        Return list of orphaned nat rules
    """
    if entity == "nat":
        natrules = get_nat_rules(dc_dict["nsx_mgr"], DC_CONSTANTS["ORG_ID"],
                                dc_dict['id'])
        auto_ids = [i['display_name'] for i in natrules["results"]
                            if i["action"] == "REFLEXIVE"
                            and utils.validate_uuid4(i["display_name"]) is True]
    elif entity == "eip":
        eips = get_public_ips(dc_dict["nsx_mgr"], DC_CONSTANTS["ORG_ID"],
                                dc_dict['id'])
        auto_ids = [i['display_name'] for i in eips["results"] if
                    utils.validate_uuid4(i["display_name"]) is True]
    orphaned_nw_ids = list(set(auto_ids) - set(vms.values()))
    logger.info("There are %s orphaned network ids" % len(orphaned_nw_ids))
    return orphaned_nw_ids


def reap_orphaned_entities(vcobj, dc_dict, nat=True, eip=True, dryrun=False):
    """
        Reap network entities for sddc
    """
    vms = vcobj.get_all_vms()
    metadata = utils.get_network_metadata([vm.name for vm in vms])
    #logger.info("Virtual machines in the datacenter %s" % vms.keys())
    if nat is True:
        natids = get_orphaned_nwids(dc_dict, "nat", metadata)
        logger.info("List of orphaned nat ids %s" % "\n".join(natids))
        if dryrun is False:
            for natid in natids:
                delete_nat_rule(dc_dict["nsx_mgr"],
                    DC_CONSTANTS["ORG_ID"], dc_dict['id'], natid)
    if eip is True:
        eipids =  get_orphaned_nwids(dc_dict, "eip", metadata)
        logger.info("List of orphaned eips %s" % "\n".join(eipids))
        if dryrun is False:
            for eip in eipids:
                delete_public_ip(dc_dict["nsx_mgr"],
                             DC_CONSTANTS["ORG_ID"], dc_dict['id'], eip)


def reap_network_entities(vcobj, networkname, dc_dict, reap_ipam=False):
    """
        Reap all vm's in a given network
    """
    cleanup_state = True
    network = vcobj.get_network(networkname)
    vms = [vm for vm in network.vm if is_blockchain_vm(vm.name) is True]
    logger.info("Cleaning up %s vm resources for network %s" %
                (len(vms), networkname))
    metadata = utils.get_network_metadata([vm.name for vm in vms])
    if reap_ipam is True:
        if delete_ipam_resources(vms, dc_dict, metadata) is False:
            cleanup_state = False
    delete_network_resources(metadata, dc_dict)
    vcobj.delete_vms(vms)
    return cleanup_state


def setup_arguments():
    """
        Arg setup
    """
    parser = argparse.ArgumentParser(description="Clean up sddc folder")
    parser.add_argument("sddc", type=str,
                    choices=DC_CONSTANTS["SDDCS"].keys(),
                    help="SDDC to clean up")
    subparsers = parser.add_subparsers(help='Subparsers for resource mgmt')
    nwresource = subparsers.add_parser("nwresource", help="Default")
    nwresource.set_defaults(which='nwresource')
    nwresource.add_argument("--networkname", type=str, required=True,
                    help="Network name for cleanup")
    nwresource.add_argument("--reap-ipam", action='store_true', default=False,
                    help="Choose to delete ipam entry for collected vm's")
    resource = subparsers.add_parser("resource-cleanup",
                                    help="Default")
    resource.set_defaults(which='resource')
    resource.add_argument("--folder", type=str, required=True,
                    help="Folder to cleanup; must be a leaf folder")
    resource.add_argument("--reap-ipam", action='store_true', default=False,
                    help="Choose to delete ipam entry for collected vm's")
    resource.add_argument("--olderthan", type=int, default=0,
                    help="Cleanup vm's older than this value in hours")
    orphan = subparsers.add_parser("orphan-cleanup",
                                    help="Default")
    orphan.set_defaults(which='orphan')
    orphan.add_argument("--only-list", action="store_true", default=False,
                    help="Only list orphaned entities")
    orphan.add_argument("--reap-nat", action="store_true", default=False,
                    help="Cleanup orhpaned nat rules")
    orphan.add_argument("--reap-eip", action="store_true", default=False,
                    help="Cleanup orphaned eip's")
    args = parser.parse_args()
    logger.info(args)
    return args


if __name__ == "__main__":
    DC_CONSTANTS = utils.get_vault_constants("VMC")
    args = setup_arguments()
    dc_dict = DC_CONSTANTS["SDDCS"][args.sddc]
    vcenterobj = vsphere.Vsphere(hostname=dc_dict['vcenter'],
                                  username=dc_dict['vc_user'],
                                  password=dc_dict['vc_pwd'])
    if args.which == "orphan":
        reap_orphaned_entities(vcenterobj, dc_dict, args.reap_nat,
                                args.reap_eip, args.only_list)
    elif args.which == "resource":
        if clean_sddc_folder(vcenterobj, args.folder,
                            dc_dict, args.olderthan, args.reap_ipam) is False:
            sys.exit(1)
    elif args.which == "nwresource":
        if reap_network_entities(vcenterobj, args.networkname,
                                dc_dict, args.reap_ipam) is False:
            sys.exit(1)
