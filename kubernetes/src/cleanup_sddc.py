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
import sys
import tempfile
import time
import requests
from config import common
from lib import utils
from lib import vsphere

DC_CONSTANTS = None

logger = utils.setup_logging()

def get_constants():
    """
        Populate constants from vault
    """
    client = utils.get_authenticated_hvac(common.VAULT_ENDPOINT)
    data = client.secrets.kv.v2.read_secret_version(
                        mount_point="kv", path="VMC")["data"]["data"]
    return data


def source_ipam_certificate():
    """
    """
    client = utils.get_authenticated_hvac(common.VAULT_ENDPOINT)
    ipam_data = client.secrets.kv.v2.read_secret_version(
                        mount_point="kv", path="ipam")["data"]["data"]
    server_crt_file = None
    with tempfile.NamedTemporaryFile(delete=False) as server_crt:
        server_crt.write(ipam_data["server_crt"].encode())
        server_crt_file = server_crt.name
    return server_crt_file


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


def get_network_metadata(vms):
    """
        Strip out nat rule id and public ip from vm name
    """
    metadata = {}
    for vm in vms:
        metadata[vm.name] = "-".join(vm.name.split("-")[5:])
    return metadata


def delete_vms(vms):
    """
        Poweroff and delete vm's
    """
    for vm in vms:
        vm.PowerOff()
    logger.info("Powered off vms %s" % vms)
    #Use Taskmanager to poll task completion instead of arbitrary sleep
    time.sleep(5)
    for vm in vms:
        vm.Destroy()
    logger.info("Destroyed vms %s" % vms)


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
    logger.info(req.json())
    return req


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
    logger.info(req.json())
    return req


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
    delta = datetime.datetime.now() - datetime.timedelta(hours=hours)
    dated = []
    vms = vcobj.get_vms_from_folder(folder)
    for vm in vms:
        task_collector = vcobj.get_vm_task_collector(vm)
        if task_collector is None:
            continue
        for task in task_collector.latestPage:
            if (task.name is not None and
                task.name.info.name == "PowerOn" and task.state == "success"):
                if task.completeTime.replace(tzinfo=None) < delta:
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
    if vcobj.check_if_subfolder_exits(folder) is True:
        logger.info("%s has subfolder's , cleanup skipped" % folder)
        sys.exit(1)
    if hours == 0:
        vms = vcobj.get_vms_from_folder(folder)
    else:
        vms = get_dated_vms(vcobj, folder, hours)
    logger.info("Cleaning up %s vm resources" % len(vms))
    metadata = get_network_metadata(vms)
    if reap_ipam is True:
        delete_ipam_resources(vms, dc_dict)
    delete_network_resources(metadata, dc_dict)
    delete_vms(vms)


def delete_ipam_resources(vms, dc_dict):
    """
        Retrieve cidr and ipaddr of vm to delete ipam entry
    """
    for vm in vms:
        ipaddr_hex = nwaddress_to_hex(vm.guest.ipAddress)
        nw_segment = vm.network[0].name
        nw_segment_data = get_network_segment(nw_segment, dc_dict)
        cidr_hex = nwaddress_to_hex(nw_segment_data['subnets'][0]['network'])
        delete_ipam_entry(nw_segment, ipaddr_hex, cidr_hex, dc_dict["id"])


def delete_ipam_entry(nw_name, ipaddr, cidr, sddc_id):
    """
        Delete ipam entry with grpc_curl
    """
    cert_path = source_ipam_certificate()
    data = {"name":
                "blocks/%s-%s/segments/%s/addresses/%s" % (
                sddc_id, nw_name, cidr, ipaddr)}
    release_method = ("vmware.blockchain.deployment."
                       "v1.IPAllocationService.ReleaseAddress")
    cmd =  ("grpcurl -format=json -d='%s' -cacert=%s %s %s" %
                            (json.dumps(data), cert_path,
                            common.IPAM_URL, release_method))
    rc, rv = utils.subproc(cmd, logger=logger, timeout=10)
    os.remove(cert_path)
    if rc == 0:
        logger.info("Successfully removed ipam entry %s" % ipaddr)
    else:
        raise Exception("Error cleaning up ipam entry %s" % rv)


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


def setup_arguments():
    """
        Arg setup
    """
    parser = argparse.ArgumentParser(description="Clean up sddc folder")
    parser.add_argument("--sddc", required=True, type=str,
                    choices=DC_CONSTANTS["SDDCS"].keys(),
                    help="SDDC to clean up")
    parser.add_argument("--folder", type=str, required=True,
                    help="Folder to cleanup; must be a leaf folder")
    parser.add_argument("--reap-ipam", action='store_true', default=False,
                    help="Choose to delete ipam entry for collected vm's")
    parser.add_argument("--olderthan", type=int, default=0,
                    help="Cleanup vm's older than this value in hours")
    args = parser.parse_args()
    logger.info(args)
    return args


if __name__ == "__main__":
    DC_CONSTANTS = get_constants()
    args = setup_arguments()
    dc_dict = DC_CONSTANTS["SDDCS"][args.sddc]
    vcenterobj = vsphere.Vsphere(hostname=dc_dict['vcenter'],
                                 username=dc_dict['vc_user'],
                                 password=dc_dict['vc_pwd'])
    clean_sddc_folder(vcenterobj, args.folder,
                        dc_dict, args.olderthan, args.reap_ipam)
