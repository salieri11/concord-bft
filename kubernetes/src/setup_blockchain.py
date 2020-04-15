"""!/usr/bin/python3
Script to create a new blockchain for a new consortium
#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
"""

import argparse
import math
import requests
import os
import sys
import time
from config import common
from lib.helen import HelenApi
from lib import utils

logger = utils.setup_logging()


def setup_arguments():
    parser = argparse.ArgumentParser(description=
                    "Setup blockchain deployment from helen "
                    "for an existing org on available cloud zones")
    parser.add_argument("--helenurl", type=str, required=True,
                    help="Helen url for environment")
    parser.add_argument("--cspenv", type=str, default="stg",
                    choices=["staging", "production"],
                    help="CSP env associated for the helen deployment")
    parser.add_argument("--orgid", type=str, required=True,
                    help="Org id for deploying blockchain")
    parser.add_argument("--zones", nargs="*", type=str,
                    help="Zones to deploy blockchain to")
    parser.add_argument("--zonetype", type=str,
                    default="VMC_AWS", choices=["VMC_AWS", "ON_PREM"],
                    help="Type of zone to deploy")
    parser.add_argument("--slack", action="store_true", default=False,
                    help="Notify on slack")
    parser.add_argument("--email", action="store_true", default=False,
                    help="Notify via email")
    subparsers = parser.add_subparsers(help='Subparsers for '
                        'blockchain deployment')
    committer = subparsers.add_parser("committer",
                                help="Deploy comitter nodes from helen")
    committer.set_defaults(which='committer')
    committer.add_argument("--consortium", type=str, required=True,
                    help="Consortium name to use for deployment")
    committer.add_argument("--nodenumber", type=int, required=True,
                    choices=[4,7],
                    help="Number of nodes required for deployment")
    committer.add_argument("--blockchaintype", type=str, choices=
                    ["DAML", "ETHEREUM"], help="Blockchain type to deploy")
    committer.add_argument("--numparticipants", type=int, default=0,
                    help="Deploy participants for deployed blockchain")
    participant = subparsers.add_parser("participant",
                    help="Deploy participant for given blockchain instance")
    participant.set_defaults(which='participant')
    participant.add_argument("--blockchainid", type=str, required=True,
                    help="Blockchain id for deploying participant")
    participant.add_argument("--participants", type=int, default=1,
                    help="Number of participants to deploy for "
                    "given blockchain")
    args = parser.parse_args()
    return args


if __name__ == "__main__":
    args = setup_arguments()
    logger.info(args)
    helen = HelenApi(args.helenurl, args.orgid, args.cspenv)
    zones = args.zones
    if zones is None:
        helen.logger.info("Using default cloud zones")
        zones = []
    elif helen.validate_zones(zones, args.zonetype) is False:
        helen.logger.error("Invalid zone input")
        sys.exit(1)
    if args.which == "committer":
        c_info = helen.create_consortium(args.consortium , helen.org_id)
        if c_info is None:
            logger.exception("Error creating consortium")
            sys.exit(1)
        blockchainid = helen.create_blockchain(c_info["consortium_id"],
                        args.blockchaintype, args.nodenumber, zones)
        if blockchainid is None:
            logger.error("Unable to deploy blockchain for %s" % helen.org_id)
            sys.exit(1)
        if args.numparticipants > 0:
            helen.deploy_participant(blockchainid, args.numparticipants)
        result = helen.parse_blockchain_nodeinfo(blockchainid)
        version = ("Versions:\n%s\nConcord version:%s\n" %
                        ("-"*12, helen.get_concord_version()))
        result = result + version
        if args.slack is True:
            utils.post_slack_channel(common.SLACK_CHANNELS[0], result)
        logger.info("Succesfully deployed blockchain with id %s" % blockchainid)
        logger.info("Blockchain info %s" % result)
    elif args.which == "participant":
        res = helen.get_blockchain_info(args.blockchainid)
        if res is False:
            logger.exception("Error finding blockchain with id %s"
                                % args.blockchainid)
            sys.exit(1)
        if helen.deploy_participant(args.blockchainid, args.participants,
                                    zones) is True:
            result = helen.parse_blockchain_nodeinfo(args.blockchainid)
            version = ("Versions:\n%s\nConcord version:%s\n" %
                        ("-"*12, helen.get_concord_version()))
            result = result + version
            if args.slack is True:
                utils.post_slack_channel(common.SLACK_CHANNELS[0], result)
            logger.info("Deployed %s participants for blockchain id %s" %
                        (args.participants, args.blockchainid))
            logger.info("Blockchain info %s" % result)
        else:
            logger.error("Unable to deploy participants for blockchainid %s" %
                        args.blockchainid)
