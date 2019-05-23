#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# This class file covers the tests related to persephone (deployment services)
#########################################################################
import os
import sys
import json
import time
import logging
import traceback
import util.helper as helper
from . import test_suite
sys.path.append('lib')

log = logging.getLogger(__name__)


class PersephoneTests(test_suite.TestSuite):
   '''
   This class is deployment service testsuite, covering testcases for metadata,
   provisioning microservices
   '''
   _args = None
   _resultFile = None
   _unintentionallySkippedFile = None
   default_cluster_size = 4

   def __init__(self, passedArgs):
      super().__init__(passedArgs)

   def getName(self):
      return "PersephoneTests"

   def run(self):
      ''' Runs all of the tests. '''
      try:
         log.info("Launching Persephone Server...")
         self.launchPersephone(self._args,
                               self._userConfig)
      except Exception as e:
         log.error(traceback.format_exc())
         return self._resultFile

      try:
         from persephone.rpc_test_helper import RPCTestHelper
      except ImportError as e:
         log.error(
            "****gRPC Python bindings not generated. Execute util/generate_gRPC_bindings_for_py3.sh")
         raise Exception("gRPC Python bindings not generated")

      self.rpc_test_helper = RPCTestHelper(self._args)
      tests = self._get_tests()

      for (testName, testFun) in tests:
         testLogDir = os.path.join(self._testLogDir, testName)
         try:
            result, info = self._runTest(testName,
                                         testFun)
         except Exception as e:
            result = False
            info = str(e)
            traceback.print_tb(e.__traceback__)
            log.error("Exception running test: '{}'".format(info))

         if info:
            info += "  "
         else:
            info = ""

         relativeLogDir = self.makeRelativeTestPath(testLogDir)
         info += "Log: <a href=\"{}\">{}</a>".format(relativeLogDir,
                                                     testLogDir)
         self.writeResult(testName, result, info)

      log.info("Tests are done.")
      return self._resultFile

   def _runTest(self, testName, testFun):
      log.info("Starting test '{}'".format(testName))
      fileRoot = os.path.join(self._testLogDir, testName)
      os.makedirs(fileRoot, exist_ok=True)

      return testFun()

   def _get_tests(self):
      return [
         ("add_model", self._test_add_model),
         ("list_models", self._test_list_models),
         ("4_Node_Blockchain_UNSPECIFIED_Site", self._test_create_blockchain_4_node_unspecified_site),
         ("get_deployment_events", self._test_stream_deployment_events),
         # ("7_Node_Blockchain_FIXED_Site", self._test_create_blockchain_7_node_fixed_site)
      ]

   def get_json_object(self, message_obj):
      from google.protobuf.json_format import MessageToJson
      if isinstance(message_obj, (list,)):
         list_of_json_objects = []
         for message in message_obj:
            json_object = json.loads(MessageToJson(message))
            list_of_json_objects.append(json_object)
         return list_of_json_objects
      else:
         json_object = json.loads(MessageToJson(message_obj))
         return json_object

   def validate_cluster_deployment_events(self, cluster_size,
                                          response_events_json):
      '''
      Validates the deployment events response
      :param cluster_size: No. of concord nodes in the cluster
      :param response_events_json: event response in json format
      :return: validation status (True/False)
      '''
      events_to_monitor = [{"ACKNOWLEDGED": 1},
                           {"NODE_DEPLOYED": cluster_size},
                           {"CLUSTER_DEPLOYED": 1},
                           {"COMPLETED": 1}]
      skip_events_from_monitoring = ["RESOURCE"]

      log.info("Validating Deployment Events...")

      event_item_to_monitor = 0
      event_response_count = 1
      for event in response_events_json:
         eventset_to_monitor = events_to_monitor[event_item_to_monitor]
         event_to_monitor = list(eventset_to_monitor.keys())[0]
         event_count = eventset_to_monitor[event_to_monitor]

         if event["type"] == event_to_monitor:
            if event_response_count == event_count:
               event_item_to_monitor += 1
               event_response_count = 1
            else:
               event_response_count += 1
         elif event["type"] in skip_events_from_monitoring:
            log.debug("Skipping EVENT type: {}".format(event["type"]))
         else:
            log.error(
               "Expected Event '{}' {} time(s) in order, but received '{}'".format(
                  event_to_monitor, event_count, event["type"]))
            log.error("Expected order of events: {}".format(events_to_monitor))
            return False

      return True

   def perform_post_deployment_validations(self, events, cluster_size):
      '''
      Perform post deploy validation, including validating EVENTS, Get ethrpc
      endpoints, SSH connection to concord nodes, verify docker container names
      :param events: Deployment events
      :param cluster_size: No. of nodes deployed on the cluster
      :return: Validation status, Message
      '''
      log.info("Performing Post deployment validations...")
      expected_docker_containers = ["concord", "ethrpc", "agent"]
      response_events_json = self.get_json_object(events)
      if self.validate_cluster_deployment_events(cluster_size,
                                                 response_events_json):
         log.info("Deployment Events validated")

         ethrpc_endpoints = self.get_ethrpc_endpoints(response_events_json)
         if len(ethrpc_endpoints) == cluster_size:
            log.info("Fetched ethrpc endpoints successfully: {}".format(
               ethrpc_endpoints))

            concord_memeber_credentials = \
               self._userConfig["persephoneTests"]["provisioningService"][
                  "concordNode"]
            concord_username = concord_memeber_credentials["username"]
            concord_password = concord_memeber_credentials["password"]
            command_to_run = "docker ps --format '{{.Names}}'"
            log.info("Initiating SSH verification on all concord nodes...")
            for ethrpc_endpoint in ethrpc_endpoints:
               concord_ip = ethrpc_endpoint.split('//')[1].split(':')[0]
               log.info("Concord IP: {}".format(concord_ip))
               ssh_output = helper.ssh_connect(concord_ip,
                                               concord_username,
                                               concord_password,
                                               command_to_run)
               log.debug(ssh_output)
               for container_name in expected_docker_containers:
                  if container_name not in ssh_output:
                     log.error(
                        "Container {} not up and running on concord node '{}'".format(
                           container_name, concord_ip))
                     return (False,
                             "Not all containers are up and running on concord node")
                  else:
                     log.debug(
                        "Container {} found in Concord node '{}'".format(
                           container_name, concord_ip))
            log.info("SSH Verification on all concord nodes are successful")
            return (True, None)
         else:
            log.error("{} ethrpc endpoint not fetched".format(cluster_size))
            return (False, "ethrpc endpoint not fetched")

   def get_ethrpc_endpoints(self, response_events_json):
      '''
      Get ethrpc endpoints from deployment events response
      :param response_events_json: deployment events (JSON)
      :return: ethrpc endpoints (list)
      '''
      ethrpc_endpoints = []
      try:
         for event in response_events_json:
            if event["type"] == "CLUSTER_DEPLOYED":
               for member in event["cluster"]["info"]["members"]:
                  ethrpc_endpoints.append(
                     member["hostInfo"]["endpoints"]["ethereum-rpc"]["url"])
      except KeyError as e:
         log.error("ERROR fetching ethrpc endpoint: {}".format(e))

      log.debug("ethrpc Endpoints: {}".format(ethrpc_endpoints))
      return ethrpc_endpoints

   def _test_add_model(self):
      '''
      Test to add metadata and validate AddModel RPC
      '''
      request, response = self.rpc_test_helper.rpc_add_model()
      response_add_model_json = self.get_json_object(response[0])
      if "id" in response_add_model_json:
         self.request_add_model = self.get_json_object(request)
         self.response_add_model_id = response_add_model_json["id"]
         return (True, None)
      return (False, "AddModel RPC Call Failed")

   def _test_list_models(self):
      '''
      Test to list the metadata that was added using AddModel RPC
      '''
      time.sleep(2)
      response = self.rpc_test_helper.rpc_list_models()
      response_list_models_json = self.get_json_object(response)
      for metadata in response_list_models_json:
         if self.response_add_model_id == metadata["id"]:
            if self.request_add_model["specification"] == metadata[
               "specification"]:
               return (True, None)
      return (False,
              "ListModels does not contain the added metadata: {}".format(
                 self.response_add_model_id))

   def _test_create_blockchain_4_node_unspecified_site(self,
                                           cluster_size=default_cluster_size):
      '''
      Test to Create a blockchain cluster with 4 nodes on UNSPECIFIED sites
      :param cluster_size: No. of concord members in the cluster
      '''
      response = self.rpc_test_helper.rpc_create_cluster(
         cluster_size=cluster_size,
         placement_type=self.rpc_test_helper.PLACEMENT_TYPE_UNSPECIFIED)
      if response:
         response_session_id_json = self.get_json_object(response[0])
         if "low" in response_session_id_json:
            self.response_deployment_session_id = response[0]
            return (True, None)
      return (False, "Failed to get a valid deployment session ID")

   def _test_stream_deployment_events(self, cluster_size=default_cluster_size):
      '''
      Test to get deployment events stream
      :param cluster_size: No. of concord memebers in the cluster
      '''
      events = self.rpc_test_helper.rpc_stream_cluster_deployment_session_events(
         self.response_deployment_session_id)
      if events:
         status, msg = self.perform_post_deployment_validations(events,
                                                                cluster_size)
         return (status, msg)
      else:
         return (False, "Failed to fetch Deployment Events")


   def _test_create_blockchain_7_node_fixed_site(self, cluster_size=7):
      '''
      Test to create a blockchain cluster with 7 nodes on FIXED sites
      :param cluster_size: No. of concord nodes on the cluster
      '''

      response = self.rpc_test_helper.rpc_create_cluster(cluster_size=cluster_size)
      if response:
         response_session_id_json = self.get_json_object(response[0])
         if "low" in response_session_id_json:
            response_deployment_session_id = response[0]

            events = self.rpc_test_helper.rpc_stream_cluster_deployment_session_events(
               response_deployment_session_id)
            if events:
               status, msg = self.perform_post_deployment_validations(events,
                                                                      cluster_size)
               return (status, msg)
            return (False, "Failed to fetch Deployment Events")

      return (False, "Failed to get a valid deployment session ID")

