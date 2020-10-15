#################################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# This class is used by Hermes to create nodes for deployments.
#################################################################################
from collections import OrderedDict
import copy
import logging
import pprint

import util.helper
import util.hermes_logging

log = util.hermes_logging.getMainLogger()

class NodeCreator():
    SAAS_SIZES = ['small', 'medium', 'large']

    def __init__(self, req, hermes_data, zone_ids):
        self.req = req
        self.zone_ids = zone_ids
        self.blockchain_type = hermes_data["hermesCmdlineArgs"].blockchainType
        self.client_size = hermes_data["hermesCmdlineArgs"].clientSize
        self.replica_size = hermes_data["hermesCmdlineArgs"].replicaSize
        self.sizing_overrides = [
            {
                "mem": {
                    "val": hermes_data["hermesCmdlineArgs"].replicaMemory,
                    "saas_field": "memory_in_gigs"
                },
                "cpu": {
                    "val": hermes_data["hermesCmdlineArgs"].replicaCpu,
                    "saas_field": "no_of_cpus"
                },
                "storage": {
                    "val": hermes_data["hermesCmdlineArgs"].replicaStorage,
                    "saas_field": "storage_in_gigs"
                }
            },
            {
                "mem": {
                    "val": hermes_data["hermesCmdlineArgs"].clientMemory,
                    "saas_field": "memory_in_gigs"
                },
                "cpu": {
                    "val": hermes_data["hermesCmdlineArgs"].clientCpu,
                    "saas_field": "no_of_cpus"
                },
                "storage": {
                    "val": hermes_data["hermesCmdlineArgs"].clientStorage,
                    "saas_field": "storage_in_gigs"
                }
            }
        ]

        self.num_groups = int(hermes_data["hermesCmdlineArgs"].numGroups)
        self.num_replicas = int(hermes_data["hermesCmdlineArgs"].numReplicas)
        if self.blockchain_type.lower() == util.helper.TYPE_DAML:
            self.num_clients = int(hermes_data["hermesCmdlineArgs"].numParticipants)
        else:
            self.num_clients = 0


    def get_nodes(self):
        '''
        Returns a (replicas, clients) tuple of data structures the SaaS REST API can use for deployment.
        '''
        replica_size_obj, client_size_obj = self._create_size_objs()
        replica_nodes = self._create_replica_nodes(replica_size_obj)
        client_nodes = None

        if self.blockchain_type.lower() == util.helper.TYPE_DAML:
            client_nodes = self._create_client_nodes(client_size_obj)

        return replica_nodes, client_nodes


    def _create_size_objs(self):
        '''
        Returns: Tuple of (replica size object, client size object)
        Helen returns an object such as:
        "templates": [
            {
              "name": "Small",
              "items": [
                {
                  "type": "replica",
                  "no_of_cpus": "2",
                  "memory_in_gigs": "16",
                  "storage_in_gigs": "64"
                },
                {
                  "type": "client",
                  "no_of_cpus": "2",
                  "memory_in_gigs": "16",
                  "storage_in_gigs": "64"
                }
              ]
            }, ...
        '''
        replica_sizing = None
        client_sizing = None
        templates = None
        resp = self.req.getNodeSizeTemplate()
        templates = resp["templates"]

        for template in templates:
            for item in template["items"]:
                if item["type"] == "replica" and template["name"].lower() == self.replica_size.lower():
                    replica_sizing = copy.copy(item)
                    del(replica_sizing["type"])

                    if client_sizing:
                        break
                if item["type"] == "client" and template["name"].lower() == self.client_size.lower():
                    client_sizing = copy.copy(item)
                    del(client_sizing["type"])

                    if replica_sizing:
                        break

        self._apply_sizing_overrides(replica_sizing, client_sizing)
        log.info("replica_sizing, client_sizing: \n{}\n{}".format(pprint.pformat(replica_sizing, indent=4),
                                                                  pprint.pformat(client_sizing, indent=4)))
        return replica_sizing, client_sizing


    def _apply_sizing_overrides(self, replica_sizing, client_sizing):
        '''
        replica_sizing and client_sizing: Specify the sizing object to be used to create nodes.
        Each has a structure such as:

            OrderedDict([   ('no_of_cpus', '32'),
                    ('memory_in_gigs', '128'),
                    ('storage_in_gigs', '1024')])

        We have the generic settings provided by SaaS (small/medium/large).
        Add any customizations added via --clientMemory, --clientCpu, etc...
        '''
        for i, sizing in enumerate([replica_sizing, client_sizing]):
            overrides = self.sizing_overrides[i]

            for saas_field in sizing:
                for k in overrides:
                    if saas_field == overrides[k]["saas_field"] and \
                       overrides[k]["val"]:
                        sizing[saas_field] = overrides[k]["val"]


    def _create_replica_nodes(self, size_obj):
        '''
        size_obj: A sizing info data structure such as
          {
            "no_of_cpus": "1",
            "storage_in_gigs": "60",
            "memory_in_gigs": "32”
          }
        Returns a list of replica node structures per
        https://confluence.eng.vmware.com/display/BLOC/BC-3521%3A+Node+size+template+support
        such as:
        [
          {
            "zone_id": "84b9a0ed-c162-446a-b8c0-2e45755f3844",
            "sizing_info" :
            {
              "no_of_cpus": "1",
              "storage_in_gigs": "60",
              "memory_in_gigs": "32"
            }
          }, ...
        ]
        '''
        nodes = []
        zone_ids = util.helper.distributeItemsRoundRobin(self.num_replicas, self.zone_ids)

        for zid in zone_ids:
            nodes.append({
                "zone_id": zid,
                "sizing_info": size_obj
            })

        return nodes


    def _create_client_nodes(self, size_obj):
        '''
        num_clients: How many clients to create.
        num_groups: How many groups to create.
        zone_ids: List of unique zone ids.
        sizing: A sizing info data structure such as
          {
            "no_of_cpus": "1",
            "storage_in_gigs": "60",
            "memory_in_gigs": "32”
          }
        Returns a list of client node structures per
        https://confluence.eng.vmware.com/display/BLOC/BC-3521%3A+Node+size+template+support
          {
            "zone_id": "84b9a0ed-c162-446a-b8c0-2e45755f3844",
            "auth_url_jwt": "user@server.com",
            "group_name": “Group 1”,
            "sizing_info" : {
              "no_of_cpus": "1",
              "storage_in_gigs": "100",
              "memory_in_gigs": "128"
            }
          }
        '''
        nodes = []
        if self.num_clients <= 0:
            return nodes

        group_names = []
        for i in range(0, self.num_groups):
            group_names.append("Group {}".format(i))

        current_group_idx = 0
        current_zone_idx = 0

        for _ in range(0, self.num_clients):
            node = {
                "zone_id": self.zone_ids[current_zone_idx],
                "auth_url_jwt": None,
                "group_name": group_names[current_group_idx],
                "sizing_info": size_obj
            }

            if current_zone_idx + 1 < len(self.zone_ids):
                current_zone_idx += 1
            else:
                current_zone_idx = 0

            if current_group_idx + 1 < len(group_names):
                current_group_idx += 1
            else:
                current_group_idx = 0

            nodes.append(node)

        log.debug("client nodes {}".format(nodes))
        return nodes
