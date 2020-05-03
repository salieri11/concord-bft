"""!/usr/bin/python3
Script to reap a blockchain
#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
"""

import argparse
import requests
import os
import sys
import time
from pprint import pformat
from config import common
from lib import helen, utils, vmc, vsphere, ipam_utils

logger = utils.setup_logging()

def setup_arguments():
    parser = argparse.ArgumentParser(description=
                    "Reap resources for blockchain with a given id")
    parser.add_argument("--helenurl", type=str, required=True,
                    help="Helen url for environment")
    parser.add_argument("--cspenv", type=str, default="stg",
                    choices=["staging", "production"],
                    help="CSP env associated for the helen deployment")
    parser.add_argument("--blockchainids", required=True, nargs="*", type=str,
                        help="Blockchain id to reap resources")
    parser.add_argument("--only-list", action="store_true", default=False,
                    help="Only list consortium entities")
    args = parser.parse_args()
    logger.info(args)
    return args


def get_blockchain_infra(url, cspenv, bcids):
    """
    Generate blockchain infra data for given blockchain id
    """
    folders = utils.get_vault_constants("SDDC-FOLDER-MAP")[cspenv]
    zone_data = utils.get_vault_constants("ZONE-SDDC-MAP")[cspenv]
    hapi = helen.HelenApi(url, csp_env=cspenv)
    prod_vmc_data = {}
    for sddc in common.PRODUCTION_SDDCS:
        vmcobj = vmc.VmcSddc(sddc)
        prod_vmc_data[sddc] =  {
                            "vmc": vmcobj,
                            "natrules": vmcobj.filter_deployment_rules(),
                            "vms": vmcobj.vc.get_vms_from_folder(folders[sddc])
                            }
    bcvms = {}
    for bcid in bcids:
        bcinfo = hapi.get_blockchain_info(bcid)
        all_nodes = [{'ip':node['ip'], 'zone_id': node['zone_id']}
                    for node in bcinfo["replica_list"]]
        if bcinfo["blockchain_type"] == "DAML":
            client_info = hapi.get_replica_info(bcid)
            all_nodes += [{'ip': node['public_ip'], 'zone_id': node['zone_id']}
                        for node in client_info]
        vms = []
        for node in all_nodes:
            zone = hapi.get_zone_info(node['zone_id'])
            if zone["type"] != "VMC_AWS":
                logger.info("Cannot reap node for given zone type %s" %
                            pformat(zone['type']))
                continue
            node_sddc = zone_data[node["zone_id"]]
            node_rule = [rule for rule in prod_vmc_data[node_sddc]["natrules"]
                        if rule['external_ip'] == node['ip']]
            if len(node_rule) == 0:
                logger.info("Cannot find network info for node with ip %s" %
                             node['ip'])
            elif len(node_rule) == 1:
                nodeinfo = node_rule[0]
                for vm in prod_vmc_data[node_sddc]["vms"]:
                    if nodeinfo['id'] in vm.name:
                        nodeinfo["sddc"] = node_sddc
                        nodeinfo["vmname"] = vm.name
                        nodeinfo["vmobj"] = vm
                        nodeinfo["vmc"] = prod_vmc_data[node_sddc]["vmc"]
                        logger.info("Node info:\n%s" % pformat(nodeinfo))
                        vms.append(nodeinfo)
            else:
                logger.info("More than one entry found with eip %s" % node['ip'])
        bcvms[bcid] = vms
    return bcvms


def delete_nodes(bcresources):
    """
    Delete list blockchain nodes with following dict keys
    { "id": <Natrule/eip id>,
      "sddc": <AWS VMC SDDC>,
      "vmname": <VM NAME>,
      "external_ip": <EXT IP>,
      "internal_ip": <INT IP>,
      "VMC obj for sddc": VMCOBJ,
    }
    """
    error = False
    for bcresource in bcresources:
        logger.info("Releasing resources from blockchain:%s" % bcresource)
        for node in bcresources[bcresource]:
            try:
                vmcobj = node["vmc"]
                vmcobj.delete_ipam_resources([node['vmobj']])
                vmcobj.delete_nat_rule(node['id'])
                vmcobj.delete_public_ip(node['id'])
                vmcobj.vc.delete_vms([node['vmobj']])
                logger.info("Deleted blockchain nodes with external ip %s" %
                            node['external_ip'])
            except Exception as e:
                logger.exception("Error cleaning up blockchain node %s %s" %
                                (node, e))
                error = True
    return error


def deregister_blockchains(helenurl, cspenv, bcids):
    """
        Deregister blockchain ids from helen
    """
    hapi = helen.HelenApi(helenurl, csp_env=cspenv)
    for bcid in bcids:
        hapi.deregister_blockchain(bcid)
    logger.info("Successfully deregistered blockchain ids %s" % bcids)

if __name__ == "__main__":
    args = setup_arguments()
    resource_dict = get_blockchain_infra(args.helenurl, args.cspenv,
                                        args.blockchainids)
    if args.only_list is False:
        fail = delete_nodes(resource_dict)
        if fail is True:
            logger.error("Error deleting blockchain nodes for instance %s"
                        % args.blockchainid)
            sys.exit(1)
        else:
            logger.info("Deleted blockchain nodes for instance %s"
                        % args.blockchainids)
    deregister_blockchains(args.helenurl, args.cspenv, args.blockchainids)