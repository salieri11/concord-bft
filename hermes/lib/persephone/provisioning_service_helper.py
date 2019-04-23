#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# This class is a helper file to test provisioning services (deployment services)
#########################################################################

import sys
sys.path.append('lib/persephone')
from grpc_python_bindings import provision_service_pb2
from rpc_helper import RPCHelper
from model_service_helper import ModelServiceRPCHelper
from grpc_python_bindings import core_pb2
import logging

log = logging.getLogger(__name__)


class ProvisioningServiceRPCHelper(RPCHelper):
   def __init__(self, persephone_service):
      super().__init__(persephone_service)
      self.grpc_server = "localhost:{}".format(self.service_port)
      self.channel = self.create_channel(self.service_name)
      self.stub = self.create_stub(self.channel)

   def __del__(self):
      self.close_channel(self.service_name)

   def create_placement_specification(self, cluster_size):
      '''
      Helper method to create place specification used for create cluster
      :param cluster_size: Number of placement sites
      :return: placement specification
      '''
      entries = []
      for placement_count in range(0, cluster_size):
         placement_entry = provision_service_pb2.PlacementSpecification.Entry(
            type=provision_service_pb2.PlacementSpecification.UNSPECIFIED)
         entries.append(placement_entry)
      placement_specification = provision_service_pb2.PlacementSpecification(
         entries=entries
      )
      return placement_specification

   def create_deployment_specificaion(self, cluster_size, model, placement):
      '''
      Helper method to create deployment specification
      :param cluster_size: Number of concord members on the cluster cluster
      :param model: Metadata for deployment
      :param placement: placement site/SDDC
      :return: deployment specifcation
      '''
      deployment_specification = provision_service_pb2.DeploymentSpecification(
         cluster_size=cluster_size, model=model, placement=placement)
      return deployment_specification

   def create_cluster_request(self, header, specification):
      '''
      Helper method to create a cluster request
      :param header: concord core header
      :param specification: deployment specification
      :return: cluster request spec
      '''
      create_cluster_request = provision_service_pb2.CreateClusterRequest(
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
      events_request = provision_service_pb2.StreamClusterDeploymentSessionEventRequest(
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
