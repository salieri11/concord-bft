#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# This class is a helper file to test provisioning services (deployment services)
#########################################################################

import json
import logging
import sys
from rpc_helper import RPCHelper
from model_service_helper import ModelServiceRPCHelper
from vmware.blockchain.deployment.v1 import core_pb2
from vmware.blockchain.deployment.v1 import orchestration_pb2
from vmware.blockchain.deployment.v1 import provisioning_service_pb2
from vmware.blockchain.ethereum.type import genesis_pb2
sys.path.append('../../')
from util.product import Product as Product
import util.helper
import uuid

import util.hermes_logging
log = util.hermes_logging.getMainLogger()


class ProvisioningServiceRPCHelper(RPCHelper):
   PLACEMENT_TYPE_FIXED = "FIXED"
   PLACEMENT_TYPE_UNSPECIFIED = "UNSPECIFIED"

   ZONE_TYPE_VMC = util.helper.LOCATION_SDDC
   ZONE_TYPE_ON_PREM = util.helper.LOCATION_ONPREM

   UPDATE_DEPLOYMENT_ACTION_NOOP = "NOOP"
   UPDATE_DEPLOYMENT_ACTION_DEPROVISION_ALL = "DEPROVISION_ALL"

   def __init__(self, args):
      super().__init__(args)
      self.service_name = Product.PERSEPHONE_SERVICE_PROVISIONING
      self.service_port = self.get_persephone_service_port(self.service_name)
      self.persephone_config_file = self.get_provisioning_config_file(
         self.service_name)

      if self.args.externalProvisioningServiceEndpoint:
         self.grpc_server = self.args.externalProvisioningServiceEndpoint
      else:
         self.grpc_server = "localhost:{}".format(self.service_port)
      self.channel = self.create_channel(self.service_name)
      self.stub = self.create_stub(self.channel)

   def __del__(self):
      self.close_channel(self.service_name)

   def create_placement_specification(self, cluster_size, zone_type=ZONE_TYPE_VMC):
      '''
      Helper method to create place specification used for create cluster
      :param cluster_size: Number of placement sites
      :return: placement specification
      '''

      entries = []
      deployment_site_ids, deployment_sites = self.get_orchestration_sites(zone_type)

      for placement_count in range(0, cluster_size):
         site_number = placement_count % len(deployment_sites)
         deployment_site = deployment_sites[site_number]
         log.debug("Placing concord[{}] on {}".format(placement_count, deployment_site))
         placement_entry = provisioning_service_pb2.PlacementSpecification.Entry(
            type=provisioning_service_pb2.PlacementSpecification.FIXED,
            site=deployment_site_ids[site_number],
            site_info=deployment_site)
         entries.append(placement_entry)

      placement_specification = provisioning_service_pb2.PlacementSpecification(
         entries=entries
      )
      return placement_specification

   def get_orchestration_sites(self, zone_type):
      '''
      helper method to get Orchestration Sites
      :param zone_type: Zone/Site type (VMC or ON-PREM)
      :return Orchestration sites
      '''
      zones = self.args.zoneConfig["zones"][zone_type]

      deployment_site_ids = []
      deployment_sites = []
      for zone in zones:
         if zone["info"] and "labels" in zone["info"] and "name" in \
               zone["info"]["labels"]:
            log.info("**** Deploying on Orchestration Sites/zones: {}".format(
               zone["info"]["labels"]["name"]))

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
            orchestration_site_info = orchestration_pb2.OrchestrationSiteInfo(
               type=type, vmc=site_info)
            orchestration_site_id = orchestration_pb2.OrchestrationSiteIdentifier(id=site_id)
            deployment_site_ids.append(orchestration_site_id)
            deployment_sites.append(orchestration_site_info)

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
            orchestration_site_info = orchestration_pb2.OrchestrationSiteInfo(
               type=type, vsphere=site_info)
            orchestration_site_id = orchestration_pb2.OrchestrationSiteIdentifier(id=site_id)
            deployment_site_ids.append(orchestration_site_id)
            deployment_sites.append(orchestration_site_info)

      log.debug("All Deployment sites: {}".format(deployment_sites))
      return deployment_site_ids, deployment_sites

   def create_genesis_specification(self):
      '''
      Helper method to create genesis bspec
      :return: genesis spec
      '''
      log.debug("Creating genesis spec")
      genesis_spec=genesis_pb2.Genesis(
          config=genesis_pb2.Genesis.Config(
              chain_id=1,
              homestead_block=0,
              eip155_block=0,
              eip158_block=0
          ),
          nonce="0x0000000000000000",
          difficulty="0x400",
          mixhash="0x0000000000000000000000000000000000000000000000000000000000000000",
          parent_hash="0x0000000000000000000000000000000000000000000000000000000000000000",
          gas_limit="0xf4240",
          alloc={
              "262c0d7ab5ffd4ede2199f6ea793f819e1abb019": genesis_pb2.Genesis.Wallet(balance="12345"),
              "5bb088f57365907b1840e45984cae028a82af934": genesis_pb2.Genesis.Wallet(balance="0xabcdef"),
              "0000a12b3f3d6c9b0d3f126a83ec2dd3dad15f39": genesis_pb2.Genesis.Wallet(balance="0x7fffffffffffffff")
          }
      )
      return genesis_spec

   def create_properties(self, replicas=None):
      '''
      Helper method to create custom properties, like "committers=concord1:50051"
      :param replicas: List of replicas
      :return: properties proto message
      '''
      blockchain_id = "testBlockchain-" + str(uuid.uuid4())
      log.info("Blockchain Id is: " + blockchain_id)
      property_values = {"BLOCKCHAIN_ID": blockchain_id}

      if replicas is not None:
          replicas_with_port = map(lambda x: "{}:50051".format(x), replicas)
          values = ",".join(list(replicas_with_port))
          property_values["COMMITTERS"] = values

      properties = core_pb2.Properties(
         values=property_values
      )
      return properties

   def create_deployment_specification(self, cluster_size, model, placement, genesis_spec, properties):
      '''
      Helper method to create deployment specification
      :param cluster_size: Number of concord members on the cluster cluster
      :param model: Metadata for deployment
      :param placement: placement site/SDDC
      :param genesis_spec: genesis spec
      :param properties: custom properties to be passed to deployment spec
      :return: deployment specification
      '''
      deployment_specification = provisioning_service_pb2.DeploymentSpecification(
         cluster_size=cluster_size, model=model, placement=placement,
         genesis=genesis_spec, consortium=str(uuid.uuid4()), properties=properties)
      return deployment_specification

   def create_cluster_request(self, header, specification):
      '''
      Helper method to create a cluster request
      :param header: concord core header
      :param specification: deployment specification
      :return: cluster request spec
      '''
      create_cluster_request = provisioning_service_pb2.CreateClusterRequest(
         header=header, specification=specification)
      return create_cluster_request

   def create_cluster_deployment_session_event_request(self, header,
                                                       session_id):
      '''
      Helper method to create cluster deployment session event request
      :param header: cocnord core header
      :param session_id: deployment session ID
      :return: event request spec
      '''
      events_request = provisioning_service_pb2.StreamClusterDeploymentSessionEventRequest(
         header=header, session=session_id)
      return events_request

   def create_all_cluster_deployment_session_event_request(self, header):
      '''
      Helper method to create cluster deployment session event request
      :param header: cocnord core header
      :return: Deployment event session request
      '''
      all_events_request = provisioning_service_pb2.StreamAllClusterDeploymentSessionEventRequest(
         header=header)
      return all_events_request

   def update_deployment_session_request(self, header, session_id,
                                         action=UPDATE_DEPLOYMENT_ACTION_NOOP):
      '''
      Helper method to call gRPC UpdateDeploymentSessionRequest
      :param header: concord core header
      :param session_id: Deployment Session ID
      :param action: action to perform like DEPROVISION_ALL
      :return: Update deployment session request
      '''
      if action == self.UPDATE_DEPLOYMENT_ACTION_DEPROVISION_ALL:
         action_obj = provisioning_service_pb2.UpdateDeploymentSessionRequest.DEPROVISION_ALL
      else:
         action_obj = provisioning_service_pb2.UpdateDeploymentSessionRequest.NOOP

      update_deployment_session_request = provisioning_service_pb2.UpdateDeploymentSessionRequest(
         header=header, action=action_obj, session=session_id)
      return update_deployment_session_request

   def rpc_CreateCluster(self, create_cluster_request, stub=None):
      '''
      Helper method to call gRPC CreateCluster
      :param create_cluster_request: Create cluster request spec
      :param stub: Default stub if running default provisioning service on port
      9001, else, stub for the non-default instance
      :return: deployment session ID
      '''
      log.info("createCluster RPC")
      response = None
      try:
         if stub is None:
            stub = self.stub
         response = self.call_api(stub.CreateCluster, create_cluster_request)
      except Exception as e:
         self.handle_exception(e)
      return response

   def rpc_StreamClusterDeploymentSessionEvents(self, get_events_request, stub=None):
      '''
      Helper method to call gRPC rpc_StreamClusterDeploymentSessionEvents
      :param get_events_request: Deployment session ID
      :param stub: Default stub if running default provisioning service on port
      9001, else, stub for the non-default instance
      :return: Deployemtn Event Stream
      '''
      log.info("StreamClusterDeploymentSessionEvents RPC")
      response = None
      try:
         if stub is None:
            stub = self.stub
         # Increasing default stream_timeout to 10 mins due to bug VB-1289
         response = self.call_api(
            stub.StreamClusterDeploymentSessionEvents,
            get_events_request, stream=True, stream_timeout=600)
      except Exception as e:
         self.handle_exception(e)
      return response

   def rpc_StreamAllClusterDeploymentSessionEvents(self, all_events_request):
      '''
      Helper method to call gRPC rpc_StreamAllClusterDeploymentSessionEvents
      This rpc call returns a stream and could be called to run in background
      when provisioning service starts.
      NOTE: Stream timeout is set to 7200 seconds (120 mins) to capture all events
      :param get_events_request: Deployment Session Events Request
      :return: All Deployement Events Stream
      '''
      log.info("StreamAllClusterDeploymentSessionEvents RPC")
      response = None
      try:
         response = self.call_api(
            self.stub.StreamAllClusterDeploymentSessionEvents,
            all_events_request, stream=True, stream_forever=True,
            stream_timeout=7200)
      except Exception as e:
         self.handle_exception(e)
      return response

   def rpc_UpdateDeploymentSession(self, update_deployment_session_request, stub=None):
      '''
      Helper method to call gRPC UpdateDeploymentSession
      :param update_deployment_session_request: Update cluster request spec
      :param stub: Default stub if running default provisioning service on port
      9001, else, stub for the non-default instance
      :return: Empty protobug message {}
      '''
      log.info("UpdateDeploymentSession RPC")
      response = None
      try:
         if stub is None:
            stub = self.stub
         response = self.call_api(stub.UpdateDeploymentSession,
                                  update_deployment_session_request)
      except Exception as e:
         self.handle_exception(e)
      return response

   def get_log_managements(self, zone):
      '''
      Gets logManagements section from the provided zone configuration
      :param zone: Zone information
      :return: List of logManagements
      '''
      log_managements = []
      for lm in zone["logManagements"]:
         if lm["endpoint"]["credential"]["type"] == util.helper.CREDENTIAL_BEARER:
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
         elif lm["endpoint"]["credential"]["type"] == util.helper.CREDENTIAL_PASSWORD:
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
