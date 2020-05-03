
import math
import json
import requests
import time
from config import common
from lib import utils


class HelenApi():
    def __init__(self, helen_url, org_id=None, csp_env="staging"):
        self.helen_url = helen_url
        self.csp_env = csp_env
        self.org_id = org_id
        self.logger = utils.setup_logging()
        self.vaultinfo = self.get_csp_constants()
        self.auth_header = self.fetch_csp_header()
        if self.org_id is not None:
            if self.validate_orgid() is False:
                self.logger.error("Org id for given helen service is invalid")
                raise Exception("Org id %s is not found in %s" %
                            (self.org_id, self.helen_url))
            else:
                self.logger.info("Org id %s exists" % self.org_id)
        self.task_timeout = 900

    def fetch_csp_header(self):
        if self.csp_env == "staging":
            return utils.get_auth_header(common.CSP_STG,
                        self.vaultinfo[common.CSP_STG_KEY])
        elif self.csp_env == "production":
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
            self.logger.error("Unable to create consortium %s" % req.text)
            return False
        else:
            self.logger.error("Succesfully created consortium with id %s" %
                        req.json()['consortium_id'])
            return req.json()

    def get_zone_info(self, zone_id):
        """
            Get zone info for given zone id
        """
        req = requests.get("%s/api/blockchains/zones/%s" % (self.helen_url,
                            zone_id), headers=self.auth_header)
        if req.status_code != 200:
            self.logger.error("Cannot find zone with id %s:%s" %
                                (zone_id, req.text))
            return False
        else:
            return req.json()

    def get_zones(self):
        """
            Get zones for user
        """
        req = requests.get("%s/api/blockchains/zones" % (self.helen_url),
                headers=self.auth_header)
        return req.json()

    def validate_zones(self, zones, zonetype):
        """
            Validate if zones for given zonetype exist
        """
        helen_zones = [zone['id'] for zone in self.get_zones()
                        if zone['type'] == zonetype]
        for zone in zones:
            if zone not in helen_zones:
                self.logger.error("Zone with id %s not found" % zone)
                return False
        return True

    def create_blockchain(self, consortium_id, blockchaintype, nodes=4, zones=[]):
        """
            Create blockchain for given consortium, assumes equal distribution
            among cloud zones
            Note:Does not work for onprem deployments without zone ids specified
        """
        f = (nodes - 1) / 3
        if len(zones) == 0:
            #Assumes cloud deployment
            siteids = self.compute_cloud_zones(nodes)
        else:
            siteids = zones*nodes
        url = ("%s/api/blockchains/" % (self.helen_url))
        data = {
            "consortium_id": consortium_id,
            "f_count": int(f),
            "c_count": 0,
            "deployment_type": "FIXED",
            "zone_ids": siteids[:nodes],
            "blockchain_type": blockchaintype
        }
        self.logger.info("Creating blockchain with specifications %s" % data)
        req = requests.post(url, headers=self.auth_header, json=data)
        taskid = req.json()["task_id"]
        blockchainid, blockchain_link = self.poll_task(taskid)
        if blockchainid is False:
            return None
        else:
            self.logger.info("Created new blockchain with id %s: resource %s" %
                        (blockchainid, blockchain_link))
            return blockchainid

    def poll_task(self, task_id):
        """
            Polling for helen api task for success
        """
        self.logger.info("Polling for task with id:%s" % task_id)
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
                self.logger.info("Polling for task")
                time.sleep(5)
                count += 5
            if req.json()["state"] == "SUCCEEDED":
                return (req.json()['resource_id'], req.json()['resource_link'])
            else:
                self.logger.error("Task either failed or timed out %s" % req.json())
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
            self.logger.error("Cannot find blockchain with id %s" % (req.text))
            return False
        else:
            return req.json()

    def get_consortium_from_blockchain(self, blockchain_id):
        """
            Retrieve consortium id from blockchain
        """
        consortium_id = self.get_blockchain_info(blockchain_id)["consortium_id"]
        url = ("%s/api/consortiums/%s" % (self.helen_url, consortium_id))
        data = utils.parse_request_json(url, header_dict=self.auth_header)
        return data

    def get_replica_info(self, blockchain_id):
        """
            Retrieve participants for given blockchain id
        """
        req = requests.get("%s/api/blockchains/%s/clients" % (self.helen_url,
                            blockchain_id), headers=self.auth_header)
        if req.status_code != 200:
            self.logger.error("Cannot find blockchain with id %s" % blockchain_id)
            return False
        else:
            return req.json()

    def deregister_blockchain(self, blockchain_id):
        """
            Mark blockchain as invalid
        """
        state = self.get_blockchain_info(blockchain_id)["blockchain_state"]
        if state == "INACTIVE":
            self.logger.info("Blockchain %s is already marked inactive" %
                            blockchain_id)
            return True
        else:
            url = ("%s/api/blockchains/deregister/%s" % (self.helen_url,
                    blockchain_id))
            rc, rv = utils.request_url(url, header_dict=self.auth_header,
                                        method="POST")
            if rc == 202:
                self.logger.info("Succesfully de-regsitered blockchain %s" %
                                blockchain_id)
                return True
            else:
                self.logger.error("Unable to de de-regsiter blockchain %s:%s"
                                % (blockchain_id, rv))
                return False

    def deploy_participant(self, blockchain_id, number, zones=[]):
        """
            Deploy participant for given blockchain
        """
        if len(zones) == 0:
            zones = self.compute_cloud_zones(1)
        data = {"zone_ids": zones[:1]}
        for i in range(number):
            url = ("%s/api/blockchains/%s/clients" % (self.helen_url,
                        blockchain_id))
            req = requests.post(url, headers=self.auth_header, json=data)
            self.logger.info(req.text)
            self.logger.info("Deploying participant for blockchain %s" %
                            blockchain_id)
            taskid = req.json()["task_id"]
            participantid, participant_link = self.poll_task(taskid)
            if participantid is False:
                raise Exception("Unable to create participant for blockchain %s"
                                % blockchain_id)
            else:
                self.logger.info("Created new participant with id %s: resource %s" %
                            (participantid, participant_link))
        return True

    def get_concord_version(self, orgid=None):
        """
            Get concord version for given org
            Check helen property if org property is not set
        """
        if orgid is None:
            if self.org_id is None:
                self.logger.error("Default org id not provided")
                return False
            else:
                orgid = self.org_id
        url = ("%s/api/organizations/%s" % (self.helen_url, orgid))
        data = utils.parse_request_json(url, header_dict=self.auth_header)
        version = data["organization_properties"].get(
                    "org_docker_image_override", None)
        if version is None:
            self.logger.info("Org property for concord version not set, "
                            "using default version")
            version = utils.get_default_concord(self.csp_env, "helen")
        return version


    def patch_concord_version(self, concord_version, orgid=None):
        """
            Patch concord version for org
        """
        orgid = self.org_id if orgid is None else orgid
        if orgid is None:
            self.logger.error("Default org is not provided")
            return False
        data = {'add_properties':
                    {'org_docker_image_override': concord_version}
                }
        url = ("%s/api/organizations/%s" % (self.helen_url, orgid))
        rc, rv = utils.request_url(url, header_dict=self.auth_header,
                                    data_dict=json.dumps(data), method="PATCH")
        if rc == 200:
            self.logger.info("Succesfully updated build for org to %s"
                            % concord_version)
            return True
        else:
            self.logger.error("Version update for org %s failed %s" %
                            (orgid, rv))
            return False

    def delete_version_property(self, orgid=None):
        """
            Delete concord version property from org properties
        """
        orgid = self.org_id if orgid is None else orgid
        if orgid is None:
            self.logger.error("Default org is not provided")
            return False
        data = {'delete_properties':
                    {'org_docker_image_override': None}
                }
        url = ("%s/api/organizations/%s" % (self.helen_url, orgid))
        rc, rv = utils.request_url(url, header_dict=self.auth_header,
                                    data_dict=json.dumps(data), method="PATCH")
        if rc == 200:
            self.logger.info("Succesfully delete concord version for org")
            return True
        else:
            self.logger.error("Version update for org %s failed %s" %
                            (orgid, rv))
            return False

    def parse_blockchain_nodeinfo(self, blockchain_id):
        """
            Parse committer and participant node info
        """
        url = ("%s/api/blockchains/%s/replicas" %
                (self.helen_url, blockchain_id))
        committers = utils.parse_request_json(url, header_dict=self.auth_header)
        committer_publicips = []
        committer_privateips = []
        participants = self.get_replica_info(blockchain_id)
        msg = ("Node information for new blockchain "
                "deployment\n\nComitters\n%s\n" % ("-"*15))
        for committer in committers:
            msg = msg + ("publicip: %s\nprivateip: %s\n\n" %
                        (committer["public_ip"], committer["private_ip"]))
            committer_publicips.append(committer["public_ip"])
            committer_privateips.append(committer["private_ip"])
        participant_publicips = []
        participant_privateips = []
        msg = msg + "Participants\n%s\n" % ("-"*15)
        for participant in participants:
            msg = msg + ("nodeip: %s\nprivateip: %s\n\n" %
                        (participant["public_ip"], participant["private_ip"]))
            participant_publicips.append(participant["public_ip"])
            participant_privateips.append(participant["private_ip"])
        return msg
