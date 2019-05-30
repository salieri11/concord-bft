#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# This class is a helper file to test provisioning services (deployment services)
#########################################################################

import sys
import json
sys.path.append('lib/persephone')
from grpc_python_bindings import provisioning_service_pb2
from grpc_python_bindings import orchestration_pb2
from rpc_helper import RPCHelper
from model_service_helper import ModelServiceRPCHelper
from grpc_python_bindings import core_pb2
sys.path.append('../../')
from util.product import Product as Product
import logging

log = logging.getLogger(__name__)


class ProvisioningServiceRPCHelper(RPCHelper):
   PLACEMENT_TYPE_FIXED = "FIXED"
   PLACEMENT_TYPE_UNSPECIFIED = "UNSPECIFIED"

   def __init__(self, cmdlineArgs):
      super().__init__(cmdlineArgs)
      self.service_name = Product.PERSEPHONE_SERVICE_PROVISIONING
      self.service_port = self.get_persephone_service_port(self.service_name)

      self.grpc_server = "localhost:{}".format(self.service_port)
      self.channel = self.create_channel(self.service_name)
      self.stub = self.create_stub(self.channel)

   def __del__(self):
      self.close_channel(self.service_name)

   def create_placement_specification(self, cluster_size, placement_type=PLACEMENT_TYPE_FIXED):
      '''
      Helper method to create place specification used for create cluster
      :param cluster_size: Number of placement sites
      :param placement_type: Placement type FIXED/UNSPECIFIED
      :return: placement specification
      '''
      log.info("Concord node placement type: {}".format(placement_type))

      entries = []
      if placement_type == self.PLACEMENT_TYPE_UNSPECIFIED:
         for placement_count in range(0, cluster_size):
            placement_entry = provisioning_service_pb2.PlacementSpecification.Entry(
               type=provisioning_service_pb2.PlacementSpecification.UNSPECIFIED)
            entries.append(placement_entry)
      else:
         persephone_config_file = self.get_provisioning_config_file(self.service_name)
         with open(persephone_config_file, "r") as confile_fp:
            data = json.load(confile_fp)
         deployment_sites = []
         for site in data["sites"]:
            log.debug("Site ID: {}".format(site["id"]))
            deployment_sites.append(site["id"])

         for placement_count in range(0, cluster_size):
            site_number = placement_count % len(deployment_sites)
            deployment_site = deployment_sites[site_number]
            log.debug("Placing concord[{}] on {}".format(placement_count, deployment_site))
            placement_entry = provisioning_service_pb2.PlacementSpecification.Entry(
               type=provisioning_service_pb2.PlacementSpecification.FIXED,
               site=orchestration_pb2.OrchestrationSiteIdentifier(low=deployment_site["low"],high=deployment_site["high"]))
            entries.append(placement_entry)

      placement_specification = provisioning_service_pb2.PlacementSpecification(
         entries=entries
      )
      return placement_specification

   def create_deployment_specification(self, cluster_size, model, placement):
      '''
      Helper method to create deployment specification
      :param cluster_size: Number of concord members on the cluster cluster
      :param model: Metadata for deployment
      :param placement: placement site/SDDC
      :return: deployment specifcation
      '''
      deployment_specification = provisioning_service_pb2.DeploymentSpecification(
         cluster_size=cluster_size, model=model, placement=placement)
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

   def rpc_CreateCluster(self, create_cluster_request):
      '''
      Helper method to call gRPC CreateCluster
      :param create_cluster_request: Create cluster request spec
      :return: deployment session ID
      '''
      log.info("createCluster RPC")
      response = None
      try:
         response = self.call_api(self.stub.CreateCluster,
                                  create_cluster_request)
      except Exception as e:
         self.handle_exception(e)
      return response

   def rpc_StreamClusterDeploymentSessionEvents(self, get_events_request):
      '''
      Helper method to call gRPC rpc_StreamClusterDeploymentSessionEvents
      :param get_events_request: Deployment session ID
      :return: Deployemtn Event Stream
      '''
      log.info("StreamClusterDeploymentSessionEvents RPC")
      response = None
      try:
         response = self.call_api(self.stub.StreamClusterDeploymentSessionEvents,
                                  get_events_request, stream=True)
      except Exception as e:
         self.handle_exception(e)
      return response
