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
      fileRoot = os.path.join(self._testLogDir, testName);
      os.makedirs(fileRoot, exist_ok=True)

      return testFun()

   def _get_tests(self):
      return [
         ("add_model", self._test_add_model),
         ("list_models", self._test_list_models),
         ("4 Node Blockchain-UNSPECIFIED Site", self._test_create_blockchain_4_node_unspecified_site),
         ("get_deployment_events", self._test_stream_deployment_events),
         ("7 Node Blockchain-FIXED Site", self._test_create_blockchain_7_node_fixed_site)
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
         else:
            log.error(
               "Expected Event '{}' {} time(s) in order, but received '{}'".format(
                  event_to_monitor, event_count, event["type"]))
            log.error("Expected order of events: {}".format(events_to_monitor))
            return False

      return True

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
      # TODO: ListModel fails to fetch the added metadata if it's right after
      # addModel. Check with James if it's expected not to respond immediately
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
         response_events_json = self.get_json_object(events)
         if self.validate_cluster_deployment_events(cluster_size,
                                                    response_events_json):
            # TODO:
            # 1. Fetch concord IPs & check the validity of for other fields
            # 2. Ping/SSH those IPs
            # 3. Save IPs to be used for extensive tesing by running existing testsuites
            return (True, None)
      return (False, "Failed to fetch Deployment Events")


   # TODO: Change cluster_size to 7, once product is ready to handle
   def _test_create_blockchain_7_node_fixed_site(self, cluster_size=4):
      '''
      Test to create a blockchain cluster with 7 nodes on FIXED sites
      :param cluster_size: Np. of concord nodes on the cluster
      '''

      response = self.rpc_test_helper.rpc_create_cluster(cluster_size=cluster_size)
      if response:
         response_session_id_json = self.get_json_object(response[0])
         if "low" in response_session_id_json:
            response_deployment_session_id = response[0]

            events = self.rpc_test_helper.rpc_stream_cluster_deployment_session_events(
               response_deployment_session_id)
            if events:
               response_events_json = self.get_json_object(events)
               if self.validate_cluster_deployment_events(cluster_size,
                                                          response_events_json):
                  return (True, None)
            return (False, "Failed to fetch Deployment Events")

            return (True, None)
      return (False, "Failed to get a valid deployment session ID")

