#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# This class is a helper file to test persephone gRPC
#########################################################################

import sys
import json
sys.path.append('lib/persephone')
from rpc_helper import RPCHelper
from model_service_helper import ModelServiceRPCHelper
from provisioning_service_helper import ProvisioningServiceRPCHelper
from grpc_python_bindings import core_pb2
from google.protobuf.json_format import MessageToJson
sys.path.append('../../')
from util.product import Product as Product
import yaml
import logging

log = logging.getLogger(__name__)


class RPCTestHelper():
   def __init__(self, args):
      self.args = args
      try:
         self.model_rpc_helper = ModelServiceRPCHelper(
            self.args)
         self.provision_rpc_helper = ProvisioningServiceRPCHelper(
            self.args)
         # self.fleet_rpc_helper = FleetServiceRPCHelper(
         #    self.args)

         self.PLACEMENT_TYPE_FIXED = self.provision_rpc_helper.PLACEMENT_TYPE_FIXED
         self.PLACEMENT_TYPE_UNSPECIFIED = self.provision_rpc_helper.PLACEMENT_TYPE_UNSPECIFIED

         self.UPDATE_DEPLOYMENT_ACTION_NOOP = self.provision_rpc_helper.UPDATE_DEPLOYMENT_ACTION_NOOP
         self.UPDATE_DEPLOYMENT_ACTION_DEPROVISION_ALL = \
            self.provision_rpc_helper.UPDATE_DEPLOYMENT_ACTION_DEPROVISION_ALL

         self.persephone_config_file = self.provision_rpc_helper.persephone_config_file
         self.grpc_server = self.provision_rpc_helper.grpc_server

         self.deployed_session_ids = []
      except Exception as e:
         raise Exception(e)

   def rpc_add_model(self):
      '''
      Helper method to call AddModel gRPC
      '''
      header = core_pb2.MessageHeader()
      concord_model_specification = self.model_rpc_helper.create_concord_model_specification()
      add_model_request = self.model_rpc_helper.create_add_model_request(
         header,
         concord_model_specification)

      add_model_response = self.model_rpc_helper.rpc_AddModel(
         add_model_request)
      log.debug("AddModel response:")
      if add_model_response:
         for item in add_model_response:
            log.debug(item)

      return add_model_request, add_model_response

   def rpc_list_models(self):
      '''
      Helper method to call ListModel gRPC
      :return: Metadata
      '''
      metadata = self.model_rpc_helper.rpc_ListModels()

      if metadata:
         for item in metadata:
            log.debug("Metadata: {}".format(item))

      return metadata

   def rpc_create_cluster(self, cluster_size=4, placement_type="FIXED", stub=None):
      '''
      Helper method to call create cluster gRPC
      :param cluster_size: cluster size
      :param placement_type: FIXED/UNSPECIFIED to place the concord memebers on site
      :param stub: Default stub if running default provisioning service on port 9001,
      else, stub for the non-default instance
      :return: deployment session ID
      '''
      header = core_pb2.MessageHeader()
      concord_model_specification = self.model_rpc_helper.create_concord_model_specification()
      placement_specification = self.provision_rpc_helper.create_placement_specification(
         cluster_size, placement_type=placement_type)
      genesis_spec = self.provision_rpc_helper.create_genesis_specification()
      deployment_specification = self.provision_rpc_helper.create_deployment_specification(
         cluster_size, concord_model_specification, placement_specification,
         genesis_spec)
      create_cluster_request = self.provision_rpc_helper.create_cluster_request(
         header, deployment_specification)

      session_id = self.provision_rpc_helper.rpc_CreateCluster(
         create_cluster_request, stub=stub)

      if session_id:
         self.deployed_session_ids.append((session_id,stub))
         log.debug("Session ID: ")
         for item in session_id:
            log.debug(item)

      return session_id

   def rpc_stream_cluster_deployment_session_events(self, session_id, stub=None):
      '''
      Helper method to stream deployment session events
      :param session_id: deployment session ID
      :param stub: Default stub if running default provisioning service on port
      9001, else, stub for the non-default instance
      :return: deployment events
      '''
      header = core_pb2.MessageHeader()
      get_events_request = self.provision_rpc_helper.create_cluster_deployment_session_event_request(
         header, session_id)
      events = self.provision_rpc_helper.rpc_StreamClusterDeploymentSessionEvents(
         get_events_request, stub=stub)

      if events:
         for event in events:
            log.debug("Event: {}".format(event))

      return events

   def rpc_stream_all_cluster_deployment_session_events(self):
      '''
      Helper method to stream All deployment session events
      :return: All deployment Session events
      '''
      header = core_pb2.MessageHeader()
      all_events_request = self.provision_rpc_helper.create_all_cluster_deployment_session_event_request(
         header)
      events = self.provision_rpc_helper.rpc_StreamAllClusterDeploymentSessionEvents(
         all_events_request)

      if events:
         for event in events:
            log.debug("Event: {}".format(event))

      return events

   def rpc_update_deployment_session(self, session_id, action, stub=None):
      '''
      Helper method to call undeploy rpc
      :param session_id: deployment session ID
      :param action: action to be performed
      :param stub: Default stub if running default provisioning service on port
      9001, else, stub for the non-default instance
      :return: session ID
      '''
      header = core_pb2.MessageHeader()
      update_deployment_session_request = self.provision_rpc_helper.update_deployment_session_request(
         header, session_id, action=action)

      session_id = self.provision_rpc_helper.rpc_UpdateDeploymentSession(
         update_deployment_session_request, stub=stub)

      if session_id:
         log.debug("Session ID: ")
         for item in session_id:
            log.debug(item)

      return session_id
