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
from lib import utils

logger = utils.setup_logging()


def setup_arguments():
    parser = argparse.ArgumentParser(description=
                    "Setup blockchain deployment from helen "
                    "for an existing org on available cloud zones")
    parser.add_argument("--helenurl", type=str, required=True,
                    help="Helen url for environment")
    parser.add_argument("--cspenv", type=str, default="stg",
                    choices=["stg", "prod"],
                    help="CSP env associated for the helen deployment")
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
    committer.add_argument("--orgid", type=str, required=True,
                    help="Org id for deploying blockchain")
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

class HelenApi():
    def __init__(self, helen_url, org_id=None, csp_env="stg"):
        self.helen_url = helen_url
        self.csp_env = csp_env
        self.org_id = org_id
        self.vaultinfo = self.get_csp_constants()
        self.auth_header = self.fetch_csp_header()
        if self.org_id is not None:
            if self.validate_orgid() is False:
                logger.error("Org id for given helen service is invalid")
                raise Exception("Org id %s is not found in %s" %
                            (self.org_id, self.helen_url))
            else:
                logger.info("Org id %s exists" % self.org_id)
        self.task_timeout = 600

    def fetch_csp_header(self):
        if self.csp_env == "stg":
            return utils.get_auth_header(common.CSP_STG,
                        self.vaultinfo[common.CSP_STG_KEY])
        elif self.csp_env == "prod":
            return utils.get_auth_header(common.CSP_PROD,
                        self.vaultinfo[common.CSP_PROD_KEY])

    def get_csp_constants(self):
        """
            Populate constants from vault
        """
        client = utils.get_authenticated_hvac(common.VAULT_ENDPOINT)
        data = client.secrets.kv.v2.read_secret_version(
                            mount_point="kv", path="CSP")["data"]["data"]
        return data

    def create_consortium(self, name, orgid):
        """
            Given a consortium name, creates it.
        """
        data = {
            "consortium_name": name,
            "consortium_type": "string",
            "organization": orgid
            }
        req = requests.post("%s/api/consortiums" % (self.helen_url),
                headers=self.auth_header, json=data)
        if req.status_code != 200:
            logger.error("Unable to create consortium %s" % req.text)
            return False
        else:
            logger.error("Succesfully created consortium with id %s" %
                        req.json()['consortium_id'])
            return req.json()

    def get_zones(self):
        """
            Get zones for user
        """
        req = requests.get("%s/api/blockchains/zones" % (self.helen_url),
                headers=self.auth_header)
        return req.json()

    def create_blockchain(self, consortium_id, blockchaintype, nodes=4):
        """
            Create blockchain for given consortium, assumes equal distribution
            among cloud zones
        """
        f = (nodes - 1) / 3
        siteids = self.compute_cloud_zones(nodes)
        url = ("%s/api/blockchains/" % (self.helen_url))
        data = {
            "consortium_id": consortium_id,
            "f_count": int(f),
            "c_count": 0,
            "deployment_type": "FIXED",
            "zone_ids": siteids[:nodes],
            "blockchain_type": blockchaintype
        }
        logger.info("Creating blockchain with specifications %s" % data)
        req = requests.post(url, headers=self.auth_header, json=data)
        taskid = req.json()["task_id"]
        blockchainid, blockchain_link = self.poll_task(taskid)
        if blockchainid is False:
            return None
        else:
            logger.info("Created new blockchain with id %s: resource %s" %
                        (blockchainid, blockchain_link))
            return blockchainid

    def poll_task(self, task_id):
        """
            Polling for helen api task for success
        """
        req = requests.get("%s/api/tasks/%s" %(self.helen_url, task_id),
                        headers=self.auth_header)
        if req.json()['state'] == "SUCCEEDED":
            return req.json()['resource_id']
        else:
            count = 0
            while (req.json()['state'] == "RUNNING" and
                                (count < self.task_timeout)):
                req = requests.get("%s/api/tasks/%s" %
                                (self.helen_url, task_id),
                                headers=self.auth_header)
                logger.info("Polling for task")
                time.sleep(5)
                count += 5
            if req.json()["state"] == "SUCCEEDED":
                return (req.json()['resource_id'], req.json()['resource_link'])
            else:
                logger.error("Task either failed or timed out %s" % req.json())
                return False

    def compute_cloud_zones(self, nodes):
        """
            Get zones for computation
        """
        cloud_zones = []
        zones = self.get_zones()
        cloud_zones = [zone["id"] for zone in zones
                        if zone["type"] == "VMC_AWS"]
        if len(cloud_zones) < nodes:
            return cloud_zones*math.ceil(nodes/len(cloud_zones))
        else:
            return cloud_zones

    def validate_orgid(self):
        """
            Validate orgid for given helen instance
        """
        req = requests.get("%s/api/organizations" % (self.helen_url),
                            headers=self.auth_header)
        if len([True for org in req.json() if
                    org["org_id"] == self.org_id]) == 0:
            return False
        else:
            return True

    def get_blockchain_info(self, blockchain_id):
        """
            Retrieve blockchain info for given blockchainid
        """
        req = requests.get("%s/api/blockchains/%s" % (self.helen_url,
                            blockchain_id), headers=self.auth_header)
        if req.status_code != 200:
            logger.error("Cannot find blockchain with id %s" % (req.text))
            return False
        else:
            return req.json()

    def get_replica_info(self, blockchain_id):
        """
            Retrieve participants for given blockchain id
        """
        req = requests.get("%s/api/blockchains/%s/clients" % (self.helen_url,
                            blockchain_id), headers=self.auth_header)
        if req.status_code != 200:
            logger.error("Cannot find blockchain with id %s" % blockchain_id)
            return False
        else:
            return req.json()

    def deploy_participant(self, blockchain_id, number):
        """
            Deploy participant for given blockchain
        """
        zones = self.compute_cloud_zones(1)
        data = {"zone_ids": zones[:1]}
        for i in range(number):
            url = ("%s/api/blockchains/%s/clients" % (self.helen_url,
                        blockchain_id))
            req = requests.post(url, headers=self.auth_header, json=data)
            logger.info(req.text)
            taskid = req.json()["task_id"]
            participantid, participant_link = self.poll_task(taskid)
            if participantid is False:
                raise Exception("Unable to create participant for blockchain %s"
                                % blockchain_id)
            else:
                logger.info("Created new participant with id %s: resource %s" %
                            (participantid, participant_link))
        return True

    def parse_blockchain_nodeinfo(self, blockchain_id):
        """
            Parse committer and participant node info
        """
        committers = self.get_blockchain_info(blockchain_id)
        participants = self.get_replica_info(blockchain_id)
        msg = ("Node information for new blockchain "
                "deployment\n\nComitters\n%s\n" % ("-"*15))
        for committer in committers["node_list"]:
            msg = msg + ("nodeip: %s\nnodeurl: %s\n\n" %
                        (committer["ip"], committer["url"]))
        msg = msg + "Participants\n%s\n" % ("-"*15)
        for participant in participants:
            msg = msg + ("nodeip: %s\nnodeurl: %s\n\n" %
                        (participant["public_ip"], participant["url"]))
        return msg

if __name__ == "__main__":
    args = setup_arguments()
    logger.info(args)
    if args.which == "committer":
        helen = HelenApi(args.helenurl, args.orgid, args.cspenv)
        c_info = helen.create_consortium(args.consortium , helen.org_id)
        if c_info is None:
            logger.exception("Error creating consortium")
            sys.exit(1)
        blockchainid = helen.create_blockchain(c_info["consortium_id"],
                        args.blockchaintype, args.nodenumber)
        if blockchainid is None:
            logger.error("Unable to deploy blockchain for %s" % helen.org_id)
            sys.exit(1)
        if args.numparticipants > 0:
            helen.deploy_participant(blockchainid, args.numparticipants)
        result = helen.parse_blockchain_nodeinfo(blockchainid)
        if args.slack is True:
            utils.post_slack_channel(common.SLACK_CHANNELS[0], result)
        logger.info("Succesfully deployed blockchain with id %s" % blockchainid)
        logger.info("Blockchain info %s" % result)
    elif args.which == "participant":
        helen = HelenApi(args.helenurl, None, args.cspenv)
        res = helen.get_blockchain(args.blockchainid)
        if res is False:
            logger.exception("Error finding blockchain with id %s" % args.blockchainid)
        if helen.deploy_participant(args.blockchainid, args.participants) is True:
            logger.info("Deployed %s participants for blockchain id %s" %
                        (args.participants, args.blockchainid))
            logger.info(helen.get_replica_info(args.blockchainid))