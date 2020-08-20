######################################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# This class is a helper file for Persephone Tests and interface with the new workflow
######################################################################################

import sys
import util.helper as helper
import util.hermes_logging
import uuid
from lib.persephone.rpc_helper import RPCHelper
from lib.persephone.vmware.blockchain.deployment.v1 import core_pb2
from lib.persephone.vmware.blockchain.deployment.v1 import orchestration_pb2
from lib.persephone.vmware.blockchain.deployment.v1 import provisioning_service_new_pb2 as ps_apis

sys.path.append('../../')
log = util.hermes_logging.getMainLogger()


class ProvisioningServiceNewRPCHelper(RPCHelper):

    ZONE_TYPE_VMC = helper.LOCATION_SDDC
    ZONE_TYPE_ON_PREM = helper.LOCATION_ONPREM

    def __init__(self, args):
        super().__init__(args)
        self.persephone_config_file = self.get_provisioning_config_file(self.service_name)
        self.channel = self.create_channel(self.service_name)
        self.stub = self.create_stub_new(self.channel)
        self.deployment_info = []
        # Set deploymentComponents to the concord_tag from /docker/.env file
        self.args.deploymentComponents = helper.get_docker_env("concord_tag")

    def __del__(self):
        self.close_channel(self.service_name)

    def create_deployment(self, blockchain_type, zone_type, num_replicas, num_clients, zone_config, num_client_groups = 0,
                          stub=None):
        """
        Main entry point method to create deployment
        :param blockchain_type: Type of blockchain (DAML, ETHEREUM, HLF)
        :param zone_type: Zone/Site type (SDDC or ONPREM)
        :param num_replicas: Number of replica nodes
        :param num_clients: Number of client nodes
        :param zone_config: zone_config.json object
        :param num_client_groups: Number of client groups to create.
        :param stub: Stub for non-default instance (running on other than default provisioning service on port 9002)
        :return: Deployment session ID
        """
        log.info("Creating deployment of blockchain type {} on location {}".format(blockchain_type.upper(),
                                                                                   zone_type.upper()))
        blockchain_type = self.convert_blockchain_type(blockchain_type)

        spec = self.create_deployment_spec(blockchain_type, zone_type, num_replicas, num_clients, zone_config,
                                           num_client_groups)
        deployment_request = self.create_deployment_request(spec)
        deployment_request_response = None
        try:
            if stub is None:
                stub = self.stub
            deployment_request_response = self.call_api(stub.CreateDeployment, deployment_request)

            if deployment_request_response:
                self.deployment_info.append({"deployment_session_id": deployment_request_response, "stub": stub})

        except Exception as e:
            self.handle_exception(e)

        return deployment_request_response

    def stream_deployment_session_events(self, session_id, stub=None):
        """
        Method to stream deployment session events
        :param session_id: Deployment session ID
        :param stub: Stub for non-default instance (running on other than default provisioning service on port 9002)
        :return: DeploymentExecutionEvent
        """
        header = core_pb2.MessageHeader()
        stream_deployment_session_event_request = ps_apis.StreamDeploymentSessionEventRequest(header=header,
                                                                                              session_id=session_id)
        deployment_stream_events = None
        try:
            if stub is None:
                stub = self.stub
            deployment_stream_events = self.call_api(stub.StreamDeploymentSessionEvents,
                                                     stream_deployment_session_event_request, stream=True)
        except Exception as e:
            self.handle_exception(e)
        return deployment_stream_events

    def deprovision_deployment(self, session_id, zone_type, zone_config, stub=None):
        """
        Method to deprovision deployment
        :param session_id: Deployment session ID
        :param zone_type: Zone/Site type (VMC or ONPREM)
        :param zone_config: zone_config.json object
        :param stub: Stub for non-default instance (running on other than default provisioning service on port 9002)
        :return: DeprovisionDeploymentResponse
        """
        header = core_pb2.MessageHeader()
        site_ids, site_infos = self.get_orchestration_site_ids_and_infos(zone_type, zone_config)
        sites = self.create_sites(site_ids, site_infos)
        deployment_stream_events = self.stream_deployment_session_events(session_id)
        resource = []
        for event in deployment_stream_events:
            if event.type == ps_apis.DeploymentExecutionEvent.RESOURCE:
                resource.append(event.resource)
        deprovision_deployment_request = ps_apis.DeprovisionDeploymentRequest(header=header, session_id=session_id,
                                                                              sites=sites, resource=resource)
        deprovision_deployment_response = None
        try:
            if stub is None:
                stub = self.stub
            deprovision_deployment_response = self.call_api(stub.DeprovisionDeployment, deprovision_deployment_request)
        except Exception as e:
            self.handle_exception(e)
        return deprovision_deployment_response

    def create_deployment_request(self, spec):
        """
        Helper method to create DeploymentSpec
        :param spec: DeploymentSpec
        :return: DeploymentRequest
        """
        header = core_pb2.MessageHeader()
        return ps_apis.DeploymentRequest(header=header, spec=spec)

    def create_deployment_spec(self, blockchain_type, zone_type, num_replicas, num_clients, zone_config,
                               num_client_groups=0):
        """
        Helper method to create DeploymentSpec
        :param blockchain_type: Type of the blockchain (DAML, ETHEREUM, HLF)
        :param zone_type: Zone/Site type (SDDC or ONPREM)
        :param num_replicas: Number of replica nodes
        :param num_clients: Number of client nodes
        :param zone_config: zone_config.json object
        :param num_client_groups: Number of client groups to create
        :return: DeploymentSpec
        """
        site_ids, site_infos = self.get_orchestration_site_ids_and_infos(zone_type, zone_config)
        sites = self.create_sites(site_ids, site_infos)
        properties_dict = {'IMAGE_TAG': self.args.deploymentComponents, "ENABLE_BFT_CLIENT": "True" }
        for property_name in helper.DEPLOYMENT_PROPERTIES:
            property_value = helper.DEPLOYMENT_PROPERTIES[property_name]
            properties_dict[property_name.upper()] = property_value
        properties = core_pb2.Properties(values=properties_dict)
        node_assignment = self.create_node_assignment(num_replicas, num_clients, site_ids, num_client_groups)
        spec = ps_apis.DeploymentSpec(consortium_id=str(uuid.uuid4()), blockchain_id=str(uuid.uuid4()),
                                      blockchain_type=blockchain_type, sites=sites, nodeAssignment=node_assignment,
                                      properties=properties)
        log.debug("Deployment Spec:\n{}".format(spec))
        return spec

    def create_sites(self, site_ids, site_infos):
        """
        Helper method to create sites
        :param site_ids: List of OrchestrationSiteIdentifier
        :param site_infos: List of OrchestrationSiteInfo
        :return: Sites
        """
        site_info_list = []
        for site_id, site_info in zip(site_ids, site_infos):
            site_info_list.append(orchestration_pb2.OrchestrationSite(id=site_id, info=site_info))
        return ps_apis.Sites(info_list=site_info_list)

    def create_node_assignment(self, num_replicas, num_clients, site_ids, num_client_groups=0):
        """
        Helper method to create node assignments
        :return: NodeAssignment
        """
        entries = []
        client_groups = [str(uuid.uuid4()) for _ in range(num_client_groups)]

        # Replicas
        site_idx = 0
        for _ in range(num_replicas):
            if site_idx == len(site_ids):
                site_idx = 0
            entries.append(ps_apis.NodeAssignment.Entry(site=site_ids[site_idx], type=core_pb2.REPLICA))
            site_idx += 1

        # Clients
        site_idx = 0
        group_idx = 0
        for _ in range(num_clients):
            # Reset indices if they overflow
            if site_idx == len(site_ids):
                site_idx = 0
            if group_idx == num_client_groups:
                group_idx = 0

            if (num_client_groups > 0):
                properties_dict = {'CLIENT_GROUP_ID': client_groups[group_idx]}
                properties = core_pb2.Properties(values=properties_dict)
                entries.append(ps_apis.NodeAssignment.Entry(site=site_ids[site_idx], type=core_pb2.CLIENT,
                                                            properties=properties))
                group_idx += 1
            else:
                entries.append(ps_apis.NodeAssignment.Entry(site=site_ids[site_idx], type=core_pb2.CLIENT))
            site_idx += 1

        return ps_apis.NodeAssignment(entries=entries)

    def convert_blockchain_type(self, bc_type):
        """
        Given blockchain_type in string, converts it into proto enum
        :param bc_type: daml, ethereum, hlf
        :return: BlockchainType enum
        """
        bc_type = bc_type.lower()
        if bc_type == helper.TYPE_DAML:
            blockchain_type = core_pb2.DAML
        elif bc_type == helper.TYPE_ETHEREUM:
            blockchain_type = core_pb2.ETHEREUM
        elif bc_type == helper.TYPE_HLF:
            blockchain_type = core_pb2.HLF
        else:
            raise Exception("Unsupported blockchain type: {}".format(bc_type))
        return blockchain_type

    def get_orchestration_site_ids_and_infos(self, zone_type, zone_config):
        """
        Helper method to get OrchestrationSites
        :param zone_type: Zone/Site type (SDDC or ONPREM)
        :param zone_config: zone_config.json object
        :return List of OrchestrationSiteIdentifier and list of OrchestrationSiteInfo
        """
        zones = zone_config["zones"][zone_type]

        orchestration_site_ids = []
        orchestration_site_infos = []
        for zone in zones:
            if zone["info"] and "labels" in zone["info"] and "name" in zone["info"]["labels"]:
                log.debug("Fetching Orchestration Site: {}".format(zone["info"]["labels"]["name"]))

            site_id = zone["id"]

            vsphere_datacenter_info = orchestration_pb2.VSphereDatacenterInfo(
                datastore=zone["vsphere"]["datastore"],
                resource_pool=zone["vsphere"]["resourcePool"],
                folder=zone["vsphere"]["folder"],
                network=orchestration_pb2.IPv4Network(
                    name=zone["vsphere"]["network"]["name"],
                    address_allocation=orchestration_pb2.IPv4Network.STATIC,
                    gateway=zone["vsphere"]["network"]["gateway"],
                    name_servers=zone["vsphere"]["network"]["nameServers"],
                    subnet=zone["vsphere"]["network"]["subnet"]
                )
            )
            wavefront = orchestration_pb2.Wavefront(
                url=zone["wavefront"]["url"],
                token=zone["wavefront"]["token"]
            )
            log_managements = self.get_log_managements(zone)

            orchestration_site_id = None
            orchestration_site_info = None
            if zone_type == self.ZONE_TYPE_VMC:
                type = orchestration_pb2.OrchestrationSiteInfo.VMC
                site_info = orchestration_pb2.VmcOrchestrationSiteInfo(
                    authentication=core_pb2.Endpoint(
                        address=zone["authentication"]["address"],
                        credential=core_pb2.Credential(
                            token_credential=core_pb2.BearerTokenCredential(
                                token=zone["authentication"]["credential"]
                                ["tokenCredential"]["token"]
                            )
                        )
                    ),
                    api=core_pb2.Endpoint(address=zone["api"]["address"]),
                    organization=zone["organization"],
                    datacenter=zone["datacenter"],
                    vsphere=vsphere_datacenter_info,
                    wavefront=wavefront,
                    log_managements=log_managements
                )
                orchestration_site_id = orchestration_pb2.OrchestrationSiteIdentifier(id=site_id)
                orchestration_site_info = orchestration_pb2.OrchestrationSiteInfo(type=type, vmc=site_info)

            elif zone_type == self.ZONE_TYPE_ON_PREM:
                type = orchestration_pb2.OrchestrationSiteInfo.VSPHERE
                site_info = orchestration_pb2.VSphereOrchestrationSiteInfo(
                    api=core_pb2.Endpoint(
                        address=zone["api"]["address"],
                        credential=core_pb2.Credential(
                            password_credential=core_pb2.PasswordCredential(
                                username=zone["api"]["credential"]
                                ["passwordCredential"]["username"],
                                password=zone["api"]["credential"]
                                ["passwordCredential"]["password"]
                            )
                        )
                    ),
                    vsphere=vsphere_datacenter_info,
                    wavefront=wavefront,
                    log_managements=log_managements
                )

                orchestration_site_id = orchestration_pb2.OrchestrationSiteIdentifier(id=site_id)
                orchestration_site_info = orchestration_pb2.OrchestrationSiteInfo(type=type, vsphere=site_info)

            orchestration_site_ids.append(orchestration_site_id)
            orchestration_site_infos.append(orchestration_site_info)

        # log.debug("All deployment sites: {}".format(orchestration_site_infos))
        return orchestration_site_ids, orchestration_site_infos

    def get_log_managements(self, zone):
        """
        Gets logManagements section from the provided zone configuration
        :param zone: Zone information
        :return: List of logManagements
        """
        log_managements = []
        for lm in zone["logManagements"]:
            if lm["endpoint"]["credential"]["type"] == helper.CREDENTIAL_BEARER:
                log_management = core_pb2.LogManagement(
                    destination=lm["destination"],
                    endpoint=core_pb2.Endpoint(
                        address=lm["endpoint"]["address"],
                        credential=core_pb2.Credential(
                            token_credential=core_pb2.BearerTokenCredential(
                                token=lm["endpoint"]["credential"]["tokenCredential"]["token"]
                            )
                        )
                    )
                )
            elif lm["endpoint"]["credential"]["type"] == helper.CREDENTIAL_PASSWORD:
                log_management = core_pb2.LogManagement(
                    destination=lm["destination"],
                    endpoint=core_pb2.Endpoint(
                        address=lm["endpoint"]["address"],
                        credential=core_pb2.Credential(
                            password_credential=core_pb2.PasswordCredential(
                                username=lm["endpoint"]["credential"]["passwordCredential"]["username"],
                                password=lm["endpoint"]["credential"]["passwordCredential"]["password"]
                            )
                        )
                    )
                )
            else:
                log_management = core_pb2.LogManagement()

            log_managements.append(log_management)

        return log_managements
