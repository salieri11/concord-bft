#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# This class is a helper file to test persephone gRPC
#########################################################################

import json
import logging
import os
import sys
import threading
import time
import traceback
import types
from google.protobuf.json_format import MessageToJson
from model_service_helper import ModelServiceRPCHelper
from provisioning_service_helper import ProvisioningServiceRPCHelper
from rpc_helper import RPCHelper
from vmware.blockchain.deployment.v1 import core_pb2

sys.path.append('../../')
from util.product import Product as Product
import util.helper


log = logging.getLogger(__name__)


def startMonitoringBlockchainDeployments(args, logDir):
   '''
   Helper method outside the RPCTestHelper class which sets up initial configuration
   for collecting deployment events.
   Returns a structure that should be passed to stopCollectingDeploymentData() when
   tests are done.
   '''
   os.makedirs(logDir, exist_ok=True)
   cleanupData = {}
   args.cancel_stream = False
   args.fileRoot = logDir
   cleanupData["rpcTestHelper"] = RPCTestHelper(args)
   cleanupData["deploymentMonitoringThread"] = threading.Thread(
      target=_thread_get_completed_session_ids,
      name="StreamAllDeploymentEventsInHelen",
      args=(cleanupData,))
   cleanupData["deploymentMonitoringThread"].start()

   return cleanupData


def _thread_get_completed_session_ids(cleanupData):
   '''
   Call gRPC to stream all deployment events since the start of provisioning service
   Stores all event data in cleanupData.
   '''
   allEventsObject = cleanupData["rpcTestHelper"].rpc_stream_all_cluster_deployment_session_events()
   allEventsProto = []

   for e in allEventsObject:
      # NOTE: str(e) shows the "type" field as containing the string "COMPLETED".  But actually
      # the field is an enum, and the value is the number 2.
      # (See hermes/lib/persephone/grpc_python_bindings/provisioning_service_pb2.py.)
      # We aren't going to use the enums in that file because they are marked as private.
      # So str() it and look for "COMPLETED".
      lines = str(e).split("\n")
      for line in lines:
         if line.startswith("type") and "COMPLETED" in line:
            allEventsProto.append(e.session)

   cleanupData["eventsProto"] = allEventsProto


def stopCollectingDeploymentData(cleanupData, deleteBlockchains=False):
   '''
   Clean up resources for the given cleanupData structure, which by this time contains
   a list of session IDs to clean up.
   '''
   cleanupData["rpcTestHelper"].args.cancel_stream = True
   cleanupData["deploymentMonitoringThread"].join()

   if deleteBlockchains:
      if "eventsProto" in cleanupData and cleanupData["eventsProto"]:
         cleanupData["rpcTestHelper"].deleteSessionIdList(cleanupData["eventsProto"])
      else:
         log.info("No blockchain deletion data was created by the blockchain deployment " \
                  "monitoring thread. No blockchain resources, if any were created, will be removed.")


class RPCTestHelper():
   def __init__(self, args):
      self.args = args
      try:
         self.model_rpc_helper = ModelServiceRPCHelper(self.args)
         self.provision_rpc_helper = ProvisioningServiceRPCHelper(self.args)
         # self.fleet_rpc_helper = FleetServiceRPCHelper(self.args)

         self.CONCORD_TYPE_ETHEREUM = self.model_rpc_helper.CONCORD_TYPE_ETHEREUM
         self.CONCORD_TYPE_DAML = self.model_rpc_helper.CONCORD_TYPE_DAML
         self.NODE_TYPE_COMMITTER = self.model_rpc_helper.NODE_TYPE_COMMITTER
         self.NODE_TYPE_PARTICIPANT = self.model_rpc_helper.NODE_TYPE_PARTICIPANT

         self.PLACEMENT_TYPE_FIXED = self.provision_rpc_helper.PLACEMENT_TYPE_FIXED
         self.PLACEMENT_TYPE_UNSPECIFIED = self.provision_rpc_helper.PLACEMENT_TYPE_UNSPECIFIED

         self.UPDATE_DEPLOYMENT_ACTION_NOOP = self.provision_rpc_helper.UPDATE_DEPLOYMENT_ACTION_NOOP
         self.UPDATE_DEPLOYMENT_ACTION_DEPROVISION_ALL = \
            self.provision_rpc_helper.UPDATE_DEPLOYMENT_ACTION_DEPROVISION_ALL

         self.ZONE_TYPE_ON_PREM = self.provision_rpc_helper.ZONE_TYPE_ON_PREM
         self.ZONE_TYPE_VMC = self.provision_rpc_helper.ZONE_TYPE_VMC

         self.persephone_config_file = self.provision_rpc_helper.persephone_config_file
         self.grpc_server = self.provision_rpc_helper.grpc_server

         self.deployment_info = []
      except Exception as e:
         traceback.print_stack()
         raise Exception(e)

   def rpc_add_model(self):
      '''
      Helper method to call AddModel gRPC
      '''
      header = core_pb2.MessageHeader()
      concord_model_specification = self.model_rpc_helper.create_concord_model_specification(
         deployment_components=self.args.deploymentComponents)
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

   def rpc_create_cluster(self, cluster_size=4,
                          placement_type=ProvisioningServiceRPCHelper.PLACEMENT_TYPE_FIXED,
                          stub=None,
                          concord_type=ModelServiceRPCHelper.CONCORD_TYPE_ETHEREUM,
                          node_type=None,
                          zone_type=ProvisioningServiceRPCHelper.ZONE_TYPE_VMC):
      '''
      Helper method to call create cluster gRPC
      :param cluster_size: cluster size
      :param placement_type: FIXED/UNSPECIFIED to place the concord memebers on site
      :param stub: Default stub if running default provisioning service on port 9002
      else, stub for the non-default instance
      :param concord_type: Concord type (ethereum, DAML, etc)
      :return: deployment session ID
      '''
      log.info("**** Deployment type: {}".format(zone_type))

      header = core_pb2.MessageHeader()
      concord_model_specification = self.model_rpc_helper.create_concord_model_specification(
         deployment_components=self.args.deploymentComponents,
         concord_type=concord_type, node_type=node_type)
      placement_specification = self.provision_rpc_helper.create_placement_specification(
         cluster_size, zone_type=zone_type)
      genesis_spec = self.provision_rpc_helper.create_genesis_specification()
      deployment_specification = self.provision_rpc_helper.create_deployment_specification(
         cluster_size, concord_model_specification, placement_specification,
         genesis_spec)
      create_cluster_request = self.provision_rpc_helper.create_cluster_request(
         header, deployment_specification)

      session_id = None
      session_id = self.provision_rpc_helper.rpc_CreateCluster(
         create_cluster_request, stub=stub)

      if session_id:
         self.deployment_info.append(
            {"deployment_session_id": session_id,
             "stub": stub
             }
         )

         log.debug("Session ID: ")
         for item in session_id:
            log.debug(item)

      return session_id

   def rpc_stream_cluster_deployment_session_events(self, session_id, stub=None):
      '''
      Helper method to stream deployment session events
      :param session_id: deployment session ID
      :param stub: Default stub if running default provisioning service on port
      9002, else, stub for the non-default instance
      :return: deployment events
      '''
      header = core_pb2.MessageHeader()
      get_events_request = self.provision_rpc_helper.create_cluster_deployment_session_event_request(
         header, session_id)
      events = self.provision_rpc_helper.rpc_StreamClusterDeploymentSessionEvents(
         get_events_request, stub=stub)

      # if events:
      #    for event in events:
      #       log.debug("Event: {}".format(event))

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

      # if events:
      #    for event in events:
      #       log.debug("Event: {}".format(event))

      return events

   def rpc_update_deployment_session(self, session_id, action, stub=None):
      '''
      Helper method to call undeploy rpc
      :param session_id: deployment session ID
      :param action: action to be performed
      :param stub: Default stub if running default provisioning service on port
      9002, else, stub for the non-default instance
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

   def deleteSessionIdList(self, sessionIds):
      '''
      Remove all resources belonging to the passed in session IDs.
      '''
      for sessionId in sessionIds:
         log.info("deleteSessionIdList sending request to deprovision session ID " \
                  "{}".format(sessionId))
         response = self.rpc_update_deployment_session(
            sessionId,
            action=self.UPDATE_DEPLOYMENT_ACTION_DEPROVISION_ALL)

         maxTimeout = 120
         sleepTime = 15
         startTime = time.time()
         cleanedUp = False

         while ((time.time() - startTime) < maxTimeout) and not cleanedUp:
            events = self.rpc_stream_cluster_deployment_session_events(sessionId)

            # Converts the object to a Python dict.
            responseEvents = util.helper.protobuf_message_to_json(events)

            for eventObject in responseEvents:
               if "RESOURCE_DEPROVISIONING" in eventObject["type"]:
                  cleanedUp = True

            if cleanedUp:
               log.info("Cleaned up!")
            else:
               log.info("Sleeping for {} seconds and retrying".format(sleepTime))
               time.sleep(sleepTime)

         if not cleanedUp:
            log.info("Failed to deprovision {}.  Blockchain resources may need to be cleaned up manually.".format(sessionId))
