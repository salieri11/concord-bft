#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# This class file covers the tests related to persephone (deployment services)
#########################################################################
import json
import logging
import os
import queue
import struct
import socket
import shutil
import subprocess
import sys
import tempfile
import threading
import time
import traceback
from util import auth, helper, infra, hermes_logging
from util.daml import daml_helper
from util.product import Product as Product
from . import test_suite
from suites.case import describe, passed, failed, getStackInfo
sys.path.append('lib')


log = hermes_logging.getMainLogger()


class PersephoneTests(test_suite.TestSuite):
   '''
   This class is deployment service testsuite, covering testcases for metadata,
   provisioning microservices
   '''
   _args = None
   _resultFile = None
   _unintentionallySkippedFile = None

   def __init__(self, cmdlineArgs, product):
      super().__init__(cmdlineArgs, product)
      self.args = self._args
      self.args.userConfig = self._userConfig
      self.args.zoneConfig = self._zoneConfig
      self.concord_ips = []
      self.session_ids_to_retain = []


   def _get_tests(self):
      if self.args.tests and self.args.tests.lower() == "smoke":
         return [
            ("7_Node_DAML_committer_participant_ON-PREM",
             self._test_daml_committer_participant_7_node_onprem),
            ("Ethereum-on-cloud-with-IPAM-test",
             self.deploy_and_verify_ipam_on_4_node_fixed_site),
         ]
      elif self.args.tests and self.args.tests.lower() == "all_tests":
         return [
            ("7_Node_DAML_committer_participant_ON-PREM",
             self._test_daml_committer_participant_7_node_onprem),
            ("4_Node_DAML_committer_participant_CLOUD",
             self._test_daml_committer_participant_deployment),
            ("4_Node_Blockchain_FIXED_Site",
             self._test_create_blockchain_4_node_fixed_site),
            ("7_Node_Blockchain_FIXED_Site",
             self._test_create_blockchain_7_node_fixed_site),
            ("concurrent_deployments_fixed_site",
             self._test_concurrent_deployments_fixed_site),
            # ("concurrent_DAML_deployments_fixed_site",
            #  self._test_concurrent_daml_deployments_fixed_site),
         ]
      elif self.args.blockchainType.lower() == helper.TYPE_ETHEREUM:
         return [
            ("4_Node_ethereum_on-cloud",
             self._test_create_blockchain_4_node_fixed_site),
         ]
      elif self.args.blockchainType.lower() == helper.TYPE_DAML:
         return [
            ("7_Node_DAML_committer_participant_ON-PREM",
             self._test_daml_committer_participant_7_node_onprem),
         ]



   def getName(self):
      return "PersephoneTests"


   def run(self):
      ''' Runs all of the tests. '''
      try:
         self.update_provisioning_config_file()

         log.info("Launching Persephone Server...")
         self.launchPersephone()
      except Exception as e:
         log.error(traceback.format_exc())
         return self._resultFile

      helper.checkRpcTestHelperImport()
      from persephone.rpc_test_helper import RPCTestHelper
      self.rpc_test_helper = RPCTestHelper(self.args)

      log.info("****************************************")
      if self.args.tests is None or self.args.tests.lower() == "smoke":
         log.info("**** Running SMOKE tests ****")
         log.info(
            "**** To run all Persephone tests, pass the command line arg '--tests all_tests'")
      else:
         log.info("**** Running All tests ****")

      if self.args.deploymentComponents is None:
         log.info("****")
         log.info("**** And, to deploy using specific concord/ethrpc Docker images, ")
         log.info(
            "**** pass the command line arg '--deploymentComponents "
            "<repo/agent:tag>,<repo/concord-core:tag>,<repo/ethrpc:tag>, etc...'")
      log.info("****************************************")

      # Call gRPC to Stream Al Deployment Events in a background thread
      self.args.cancel_stream = False
      self.logs_for_stream_all_deploy_events = os.path.join(self._testLogDir,
                                                            "stream_all_deploy_events")
      os.makedirs(self.logs_for_stream_all_deploy_events, exist_ok=True)
      self.args.fileRoot = self.logs_for_stream_all_deploy_events
      self._stream_all_deployment_events()

      tests = self._get_tests()
      for (testName, testFun) in tests:
         testLogDir = os.path.join(self._testLogDir, testName)
         try:
            result, info, stackInfo = self._runTest(testName,
                                         testFun)
         except Exception as e:
            result = False
            info = str(e)
            stackInfo = getStackInfo(e)
            traceback.print_tb(e.__traceback__)
            log.error("Exception running test: '{}'".format(info))

         if info:
            info += "  "
         else:
            info = ""

         relativeLogDir = self.makeRelativeTestPath(testLogDir)
         info += "Log: <a href=\"{}\">{}</a>".format(relativeLogDir,
                                                     testLogDir)
         self.writeResult(testName, result, info, stackInfo)

      if self.rpc_test_helper.deployment_info:
         fileRoot = os.path.join(self._testLogDir, "undeploy_all_clusters")
         os.makedirs(fileRoot, exist_ok=True)
         self.args.fileRoot = fileRoot
         self.undeploy_blockchain_cluster()

      # Trigger to cancel stream
      self.args.fileRoot = self.logs_for_stream_all_deploy_events
      self.args.cancel_stream = True
      self.background_thread.join()

      # Replace provisioning application-test.properties to default, if modified.
      self.update_provisioning_config_file(mode="RESET")

      log.info("Tests are done.")
      log.info("")
      log.info("############################################################")
      log.info("##########          Deployment Summary            ##########")
      log.info("############################################################")
      for deployment_info in self.rpc_test_helper.deployment_info:
         session_id = deployment_info["deployment_session_id"]
         log.info("Blockchain type: {} (deployment {})".format(
            deployment_info["concord_type"], session_id[0]))
         log.info("Replicas (public IP)/Private IP")
         for index, public_ip in enumerate(deployment_info["replicas"]):
            log.info("{}) {:>15}/{}".format(index + 1, public_ip,
                                        deployment_info["private_ips"][index]))
         log.info("")
      log.info("############################################################")

      return super().run()


   def _runTest(self, testName, testFun):
      log.info("****************************************")
      log.info("**** Starting test '{}' ****".format(testName))
      fileRoot = os.path.join(self._testLogDir, testName)
      os.makedirs(fileRoot, exist_ok=True)
      self.args.fileRoot = fileRoot
      if self.args.externalProvisioningServiceEndpoint:
         log.info("**** Using External Provisioning Service Endpoint: {}".format(
            self.args.externalProvisioningServiceEndpoint))

      return testFun()




   #=============================================================================================
   #
   #  Helper Functions
   #
   #=============================================================================================

   def update_provisioning_config_file(self, mode="UPDATE"):
      '''
      Helper method to update provisioning service application-test.properties with local
      config-service and use security None for local testing
      '''
      try:
         if self._args.useLocalConfigService:
            persephone_config_file = helper.get_deployment_service_config_file(
               self.args.dockerComposeFile,
               Product.PERSEPHONE_SERVICE_PROVISIONING)
            persephone_config_file_orig = "{}.orig".format(persephone_config_file)

            if mode == "UPDATE":
               log.info("****************************************")
               log.info(
                  "**** Updating provisioning service config file to use local config-service...")
               log.info("  config file: {}".format(persephone_config_file))
               log.info("  [backup: {}]".format(persephone_config_file_orig))
               log.debug("Config file: {}".format(persephone_config_file))
               shutil.copy(persephone_config_file, persephone_config_file_orig)

               ports = helper.get_docker_compose_value(self.args.dockerComposeFile,
                                                       Product.PERSEPHONE_CONFIG_SERVICE,
                                                       "ports")
               config_service_port = ports[0].split(':')[0]

               command = ["/sbin/ifconfig", "ens160"]
               ifconfig_output = subprocess.run(command, stdout=subprocess.PIPE,
                                                stderr=subprocess.STDOUT)
               ifconfig_output.check_returncode()
               if ifconfig_output.stderr:
                  log.error("stderr: {}".format(ifconfig_output.stderr))
               log.debug(
                  "ipconfig output: {}".format(ifconfig_output.stdout.decode()))
               for line in ifconfig_output.stdout.decode().split('\n'):
                  fields = line.split()
                  log.info("***** fields: {}".format(fields))
                  if fields[0] == 'inet':
                     if ":" in fields[1]:
                        host_ip = fields[1].split(':')[1]
                     else:
                        host_ip = fields[1]

                     break

               log.info(
                  "  Updating configService[\"address\"] to: {}:{}".format(host_ip,
                                                                           config_service_port))

               provisioning_config_service_address = "{}:{}".format(host_ip,
                                                                    config_service_port)
               helper.set_props_file_value(persephone_config_file,
                                           'provisioning.config.service.address',
                                           provisioning_config_service_address)
               helper.set_props_file_value(persephone_config_file,
                                           'provisioning.config.service.transportSecurity.type',
                                           "NONE")

               log.info("Update completed!")
               log.info("****************************************")

            if mode == "RESET":
               log.info("****************************************")
               log.info(
                  "Reverting changes made to provisioning service config file")
               modified_config_file = persephone_config_file.replace("properties", "")
               modified_config_file += time.strftime('%Y-%m-%d_%H-%M-%S', time.gmtime(time.time()))
               modified_config_file += ".properties"
               shutil.copy(persephone_config_file, modified_config_file)
               shutil.move(persephone_config_file_orig, persephone_config_file)
               log.info("  Updated config file for this run: {}".format(
                  modified_config_file))
               log.info("****************************************")

      except Exception as e:
         log.error(traceback.format_exc())
         raise


   def verify_ethrpc_block_0(self, concord_ip, ethrpc_port=443):
      '''
      Helper method to validate hitting ethrpc endpoint
      :param concord_ip: Concord IP
      :param ethrpc_port: ethrpc port, defaulting to 443 (workaround as 8545 is
      blocked on vmware network)
      :return: Verification status (True/False)
      '''
      log.info("Validating ethrpc (get Block 0) on port '{}'".format(ethrpc_port))
      from rpc.rpc_call import RPC
      tokenDescriptor = auth.getTokenDescriptor(auth.ROLE_CON_ADMIN,
                                                     True,
                                                     auth.internal_admin)
      rpc = RPC(self.args.fileRoot,
                "verify_ethrpc_block_0",
                "http://{}:{}".format(concord_ip, ethrpc_port),
                self._userConfig,
                tokenDescriptor)
      attempt = 0
      max_tries = 5
      while attempt < max_tries:
         attempt += 1
         log.info("Verifying ethrpc connectivity (attempt: {}/{})...".format(
            attempt, max_tries))
         try:
            blockZero = rpc.getBlockByNumber(0)
            if blockZero and blockZero["number"] == "0x0":
               return True
            else:
               log.error("Invalid response getting block zero: {}".format(blockZero))
         except Exception as e:
            if attempt == max_tries:
               log.error(e)
            else:
               sleep_time = 30  # seconds
               log.info("Retry after {} seconds...".format(sleep_time))
               time.sleep(sleep_time)

      return False


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


   def add_ethrpc_port_forwarding(self, concord_ip, concord_username,
                                     concord_password, src_port=443, dest_port=8545):
      '''
      This is a workaround to enable ethrpc to listen on port 443, so tests and
      other users like performance team can hit ethrpc over vmware network.
      Bug/Story: VB-1170
      :param concord_ip: Concord node IP
      :param concord_username: concord node login - username
      :param concord_password: concord node login - password
      :return: Port forwarding Status (True/False)
      '''
      port_forwarding_status = helper.add_ethrpc_port_forwarding(concord_ip,
                                                                 concord_username,
                                                                 concord_password,
                                                                 src_port=src_port,
                                                                 dest_port=dest_port)
      if port_forwarding_status:
         log.info("Port 443 forwarded to {}:8545 - Successful".format(concord_ip))
      else:
         log.warning("Port 443 forwarding to {}:8545 - Failed".format(concord_ip))


   def mark_node_as_logged_in(self, concord_ip, concord_username,
                                 concord_password, mode=None):
      '''
      Helper method to call a node as "logged in" by touching a file /tmp/<publicIP>
      :param concord_ip: Concord IP
      :param concord_username: Concord node login - username
      :param concord_password: Concord node login - password
      :param mode: Nonde for marking node as "logged in". Any other mode,
      indicates check for the marker file
      :return: Status. True if marker file created/found, else False
      '''
      marker_file = "/tmp/{}".format(concord_ip)
      if mode is None:
         command_to_run = "touch {} ; ls {}".format(marker_file, marker_file)
         log.debug(
            "Marking concord node as logged in: {}".format(command_to_run))
         validation_message_success = "Marker file '{}' created".format(
            marker_file)
         validation_message_fail = "Failed to create marker file '{}'".format(
            marker_file)
      else:
         command_to_run = "ls {}".format(marker_file)
         log.debug(
            "Verifying if concord node is marked as logged in: {}".format(
               command_to_run))
         validation_message_success = "Marker file '{}' found".format(
            marker_file)
         validation_message_fail = "Cannot find marker file '{}'".format(
            marker_file)

      ssh_output = helper.ssh_connect(concord_ip,
                                      concord_username,
                                      concord_password,
                                      command_to_run)
      log.debug(ssh_output)
      if ssh_output:
         if ssh_output.rstrip() == marker_file:
            log.debug(validation_message_success)
            if mode is None:
               self.concord_ips.append(concord_ip)
            return True
      log.error(validation_message_fail)
      return False


   def get_ethrpc_endpoints(self, response_events_json, concord_type, node_type=None):
      '''
      Get ethrpc endpoints from deployment events response
      :param response_events_json: deployment events (JSON)
      :param concord_type: Concord type (ethereum, DAML, etc)
      :return: ethrpc endpoints (list)
      '''

      endpoint_id = "ethereum-rpc"
      if concord_type == self.rpc_test_helper.CONCORD_TYPE_DAML:
         endpoint_id = "daml-ledger-api"

      ethrpc_endpoints = []
      private_ips = []
      node_details = []
      try:
         for event in response_events_json:
            if event["type"] == "CLUSTER_DEPLOYED":
               for member in event["cluster"]["info"]["members"]:
                  # TODO: Add another validation to check "blockchainType"
                  # e.g. full rpc/url = "https://10.70.30.237:6865/path"
                  endpoint_full_url = member["hostInfo"]["endpoints"][endpoint_id]["url"]
                  ethrpc_endpoints.append(endpoint_full_url)
                  endpoint_ip = endpoint_full_url.split('//')[1].split(':')[0]

                  private_ip_in_decimal = list(member["hostInfo"]["ipv4AddressMap"].values())[0]
                  private_ip = socket.inet_ntoa(struct.pack('!L', private_ip_in_decimal))
                  private_ips.append(private_ip)
                  public_ip = endpoint_ip if endpoint_ip != private_ip else None
                  isParticipant = (node_type == self.rpc_test_helper.NODE_TYPE_PARTICIPANT)
                  node_details.append({
                    "id": member["id"]["id"], # node_id
                    "public_ip": public_ip, # can be null
                    "private_ip": private_ip, # strictly private/internal
                    "endpoint_ip": endpoint_ip,
                    "password": member["info"]["nodePassword"], # strong password
                    "type_name": helper.TYPE_DAML_PARTICIPANT if isParticipant else helper.TYPE_DAML_COMMITTER,
                    "url": endpoint_full_url, # (optional)
                    "model": member["info"]["model"], # (optional), info containing: model, blockchainType,
                  })
      except KeyError as e:
         log.error("ERROR fetching ethrpc endpoint: {}".format(e))

      log.debug("ethrpc Endpoints: {}".format(ethrpc_endpoints))
      return (ethrpc_endpoints, private_ips, node_details)

   
   def _stream_all_deployment_events(self):
      '''
      Stream all deployment events on a background thread
      :return:
      '''
      log.info("****************************************")
      log.info("**** Starting StreamAllDeploymentEvents Thread ****")

      self.background_thread = threading.Thread(
         target=self._thread_stream_all_deployment_events,
         name="StreamAllDeploymentEvents")
      self.background_thread.start()
   




   #=============================================================================================
   #
   #  Advanced Helper Functions
   #
   #=============================================================================================

   def parse_test_status(self, status, msg, stackInfo, deployment_session_id=None):
      '''
      Parse the test status to determine Blockchain node/replica's retention policy
      :param status: test status
      :param msg: test status description
      :param deployment_session_id: deployment session ID (if exists)
      :return: test status, test status description
      '''
      if deployment_session_id:
         if (helper.KEEP_BLOCKCHAINS_ALWAYS in self.args.keepBlockchains) or (
               self.args.keepBlockchains == helper.KEEP_BLOCKCHAINS_ON_FAILURE
               and (not status)):
            log.info("Adding Session ID to preserve list: \n{}".format(
               deployment_session_id))
            self.session_ids_to_retain.append(deployment_session_id)

         # Create support bundle
         if not status:
            for deployment_info in self.rpc_test_helper.deployment_info:
               if deployment_info["deployment_session_id"][0] == deployment_session_id:
                  if "replicas" in deployment_info and deployment_info["replicas"]:
                     log.debug(
                        "Call to create support-bundle for session ID: {}".format(
                           deployment_session_id))
                     helper.create_concord_support_bundle(
                                             deployment_info["replicas"],
                                             deployment_info["concord_type"],
                                             deployment_info["log_dir"])
                  else:
                     log.info("No replicas found to get support logs")

      return (status, msg, stackInfo)


   def undeploy_blockchain_cluster(self):
      '''
      Undeploy all deployed blockchain clusters in PersephoneTests run
      '''
      log.info("****************************************")
      log.info("**** Undeploy all created blockchain clusters ****")

      undeployed_status = None
      stackInfo = getStackInfo()
      for deployment_info in self.rpc_test_helper.deployment_info:
         session_id = deployment_info["deployment_session_id"]
         stub = deployment_info["stub"]

         cleaned_up = False
         if session_id[0] not in self.session_ids_to_retain:
            # gRPC call to Undeploy resources
            log.info("Undeploying Session ID:\n{}".format(session_id[0]))
            response = self.rpc_test_helper.rpc_update_deployment_session(
               session_id[0],
               action=self.rpc_test_helper.UPDATE_DEPLOYMENT_ACTION_DEPROVISION_ALL,
               stub=stub)

            max_timeout = 480 # seconds
            sleep_time = 15 # seconds
            start_time = time.time()
            while ((time.time() - start_time) < max_timeout) and not cleaned_up:
               events = self.rpc_test_helper.rpc_stream_cluster_deployment_session_events(
                  session_id[0], stub=stub)
               response_events_json = helper.protobuf_message_to_json(events)
               for event in response_events_json:
                  if "RESOURCE_DEPROVISIONING" in event["type"]:
                     # TODO: Add more validation to API response
                     cleaned_up = True
               log.info("Sleep for {} seconds and retry".format(sleep_time))
               time.sleep(sleep_time)

            if cleaned_up:
               log.info("**** Deprovisioned Successfully")
               if undeployed_status is None:
                  undeployed_status = True
            if not cleaned_up:
               undeployed_status = False
               log.info("**** Deprovisioning Failed!")
            log.info("")
         else:
            log.info("Preserving Session ID:\n{}".format(session_id[0]))

      if undeployed_status is None:
         status_message = "No Session IDs to undeploy"
         stackInfo = getStackInfo()
         log.info(status_message)
      elif undeployed_status:
         status_message = "Undeployed all sessions Successfully!"
         stackInfo = getStackInfo()
         log.info(status_message)
      else:
         status_message = "Failed to Undeploy all Sessions"
         stackInfo = getStackInfo()
         log.error(status_message)
      self.writeResult("Undeploy", undeployed_status, status_message, stackInfo)


   def deploy_and_verify_ipam_on_4_node_fixed_site(self, cluster_size=4):
      '''
      Start another instance of provisioning service on a non-default port, and
      perform a new deployment. After a successful deployment, SSH into all
      concord nodes deployed via both instances of provisioning services and
      check for the marker file (/tmp/<publicIP>)
      :param cluster_size: Cluster size
      :return: Status of IPAM verification
      '''
      log.info("****************************************")
      log.info("**** Deploy & Verify concord nodes for IPAM ****")

      status = False
      msg = "Test Failed"
      stackInfo = getStackInfo()
      response_deployment_session_id = None
      if self.concord_ips:
         try:
            import grpc
            from vmware.blockchain.deployment.v1 import provisioning_service_pb2
            from vmware.blockchain.deployment.v1 import provisioning_service_pb2_grpc

            service_name = Product.PERSEPHONE_SERVICE_PROVISIONING_2
            ports = helper.get_docker_compose_value(
               self.args.dockerComposeFile, service_name, "ports")
            port = ports[0].split(':')[0]
            grpc_server = "localhost:{}".format(port)
            channel = grpc.insecure_channel(grpc_server)
            stub = provisioning_service_pb2_grpc.ProvisioningServiceStub(channel)
            log.info("Created gRPC channel/stub for 2nd instance of provisioning "
                     "service: {}".format(grpc_server))

            start_time = time.time()
            log.info("Deployment Start Time: {}".format(start_time))
            response = self.rpc_test_helper.rpc_create_cluster(cluster_size=cluster_size, stub=stub)
            if response:
               response_session_id_json = helper.protobuf_message_to_json(response[0])
               if "id" in response_session_id_json:
                  response_deployment_session_id = response[0]

                  events = self.rpc_test_helper.rpc_stream_cluster_deployment_session_events(
                     response_deployment_session_id, stub=stub)
                  end_time = time.time()
                  log.info("Deployment End Time: {}".format(end_time))
                  log.info("**** Time taken for this deployment: {} mins".format(
                     (end_time - start_time) / 60))

                  if events:
                     deployment_status, msg, stackInfo = self.perform_post_deployment_validations(
                        events, cluster_size, response_deployment_session_id)
                     if deployment_status:
                        status, msg, stackInfo = self.verify_IPAM_configuration()
                  else:
                     msg = "Failed to fetch Deployment Events"
                     stackInfo = getStackInfo()
                     log.error(msg)
            else:
               msg = "Failed to get a valid deployment session ID"
               stackInfo = getStackInfo()
               log.error(msg)
         except Exception as e:
            log.info("Error: {}".format(e))
            stackInfo = getStackInfo()
            msg = e
      else:
         msg = "No Deployments found from default provisioning service"
         stackInfo = getStackInfo()
         log.error(
            "No Deployments found from default provisioning service. To verify "
            "IPAM, enable one of the deployment tests hitting default "
            "provisioning service (9001)")

      self.parse_test_status(status, msg, stackInfo,
                             deployment_session_id=response_deployment_session_id)
      # self.writeResult("ethereum_deployment", status, msg, stackInfo)
      return (status, msg, stackInfo)


   def verify_IPAM_configuration(self):
      '''
      Helper method to verify IPAM configuration by doing SSH into all concord
      IPs and verify if /tmp/<publicIP> is present
      :return: True if "marker file" is present, else False
      '''
      log.info("")
      log.info("**** Verifying IPAM configuration on all concord nodes: {}".format(
         self.concord_ips))
      concord_memeber_credentials = \
         self._userConfig["persephoneTests"]["provisioningService"][
            "concordNode"]
      concord_username = concord_memeber_credentials["username"]
      concord_password = concord_memeber_credentials["password"]
      for concord_ip in self.concord_ips:
         log.info("Verifying node: {}".format(concord_ip))
         if not self.mark_node_as_logged_in(concord_ip, concord_username,
                                            concord_password,
                                            mode="VERIFY_IPAM"):
            status_message = "IPAM test - Failed"
            log.error(status_message)
            return failed(status_message)

      status_message = "IPAM test - PASS"
      log.info(status_message)
      return passed(status_message)


   def perform_post_deployment_validations(self, events, cluster_size,
                                           session_id=None,
                                           concord_type=None,
                                           node_type=None):
      '''
      Perform post deploy validation, including validating EVENTS, Get ethrpc
      endpoints, SSH connection to concord nodes, verify docker container names
      :param events: Deployment events
      :param cluster_size: No. of nodes deployed on the cluster
      :param session_id: Deployment Session ID
      :param concord_type: Concord type (ethereum, DAML, etc)
      :return: Validation status, Message
      '''
      log.info("Performing Post deployment validations...")
      if concord_type is None:
         concord_type = self.rpc_test_helper.CONCORD_TYPE_ETHEREUM

      if node_type is None:
         concord_type_to_deploy = concord_type
      else:
         concord_type_to_deploy = node_type
      expected_docker_containers = list(self._userConfig["persephoneTests"][
         "modelService"]["defaults"]["deployment_components"][
         concord_type_to_deploy].values())

      response_events_json = helper.protobuf_message_to_json(events)
      if self.validate_cluster_deployment_events(cluster_size,
                                                 response_events_json):
         log.info("Deployment Events validated")

         ethrpc_endpoints, private_ips, node_details = self.get_ethrpc_endpoints(
                                                      response_events_json, concord_type, node_type)
         concord_memeber_credentials = \
            self._userConfig["persephoneTests"]["provisioningService"][
               "concordNode"]
         concord_username = concord_memeber_credentials["username"]
         concord_password = concord_memeber_credentials["password"]

         if session_id: # has reponse
            replicas = []
            for node_info in node_details: replicas.append(node_info["endpoint_ip"])

            log.info("Annotating VMs with deployment context...")
            log.info("Engine Type: {}".format(concord_type))
            log.info("Deployed Nodes: {}".format(json.dumps(node_details, indent=4)))
            fatal_errors = infra.giveDeploymentContext({
              "id": "None",
              "consortium_id": "None",
              "blockchain_type": concord_type,
              "nodes_list": node_details,
              "deployed_from": "Persephone, V1"
            })
            if fatal_errors: # e.g. IP conflicts
                infra.save_fatal_errors_to_summary(fatal_errors)

            for deployment_info in self.rpc_test_helper.deployment_info:
               if deployment_info["deployment_session_id"][0] == session_id:
                  log.debug(
                     "Updating more info for deployment ID: {}".format(session_id))
                  deployment_info["replicas"] = replicas
                  deployment_info["private_ips"] = private_ips
                  deployment_info["concord_username"] = concord_username
                  deployment_info["concord_password"] = concord_password
                  deployment_info["docker_containers"] = expected_docker_containers
                  deployment_info["concord_type"] = concord_type_to_deploy
                  deployment_info["log_dir"] = self.args.fileRoot

         if len(ethrpc_endpoints) == cluster_size:
            log.info("Fetched ethrpc endpoints successfully: {}".format(
               ethrpc_endpoints))
            log.info("Initiating SSH verification on all concord nodes...")
            for ethrpc_endpoint in ethrpc_endpoints:
               concord_ip = ethrpc_endpoint.split('//')[1].split(':')[0]
               log.info("**** Concord IP: {}".format(concord_ip))

               start_time = time.time()
               max_timeout = 180 # 3 mins
               verify_ssh_connection = None
               log.info(
                  "Waiting for SSH to be enabled ({}) within {} mins".format(
                     concord_ip, max_timeout / 60))
               while (time.time() - start_time) <= max_timeout and \
                     verify_ssh_connection is None:
                  verify_ssh_connection = helper.ssh_connect(concord_ip,
                                                             concord_username,
                                                             concord_password,
                                                             "hostname",
                                                             log_mode="WARNING")
                  if verify_ssh_connection:
                     log.info("SSH enabled within {} mins".format(
                        (time.time() - start_time) / 60))
                     break
                  else:
                     sleep_time = 30 # sec
                     log.info("Sleep {} secs and try again...".format(sleep_time))
                     time.sleep(sleep_time)
               if not verify_ssh_connection:
                  log.error(
                     "SSH not enabled within {} mins".format(max_timeout / 60))
                  return failed("SSH not enabled within {} mins".format(
                     max_timeout / 60))

               if self.mark_node_as_logged_in(concord_ip, concord_username, concord_password):
                  log.info("Marked node as logged in (/tmp/{})".format(concord_ip))
               else:
                  return failed(
                          "Failed creating marker file on node '{}'".format(
                             concord_ip))

               count = 0
               max_timeout = 600 # 10 mins
               start_time = time.time()
               docker_images_found = False
               command_to_run = "docker ps --format '{{.Names}}'"
               log.info(
                  "Waiting for all docker containers to be up on '{}' within {} mins".format(
                     concord_ip, max_timeout / 60))
               while not docker_images_found:
                  count += 1
                  log.debug(
                     "Verifying docker containers (attempt: {})...".format(
                        count))
                  ssh_output = helper.ssh_connect(concord_ip,
                                                  concord_username,
                                                  concord_password,
                                                  command_to_run)
                  log.debug("SSH output: {}".format(ssh_output))
                  for container_name in expected_docker_containers:
                     if container_name not in ssh_output:
                        docker_images_found = False
                        if (time.time() - start_time) > max_timeout:
                           log.info("SSH output:\n{}".format(ssh_output))
                           log.error(
                              "Container '{}' not up and running on node '{}'".format(
                                 container_name, concord_ip))
                           return failed(
                                   "Not all containers are up and running on node")
                        else:
                           log.warning(
                              "Container '{}' not up and running on node '{}'".format(
                                 container_name, concord_ip))
                           time.sleep(30)
                           break
                     else:
                        docker_images_found = True
                        log.debug(
                           "Container {} found in node '{}'".format(
                              container_name, concord_ip))
               log.info("Docker containers verified on {}".format(concord_ip))

               if concord_type is self.rpc_test_helper.CONCORD_TYPE_ETHEREUM:
                  # This is a workaround to enable ethrpc to listen on port 443, so
                  # tests and other users like performance team can hit ethroc over
                  # vmware network. Bug/Story: VB-1170
                  self.add_ethrpc_port_forwarding(concord_ip,
                                                  concord_username,
                                                  concord_password)

                  if self.verify_ethrpc_block_0(concord_ip):
                     log.info("Ethrpc (get Block 0) Validation - PASS")
                  else:
                     log.error(
                        "Ethrpc (get Block 0) Validation ({})- FAIL".format(
                           concord_ip))
                     return failed("Ethrpc (get Block 0) Validation - FAILED")

               if concord_type is self.rpc_test_helper.CONCORD_TYPE_DAML:
                  if node_type is None or node_type == self.rpc_test_helper.NODE_TYPE_PARTICIPANT:
                     src_port = helper.FORWARDED_DAML_LEDGER_API_ENDPOINT_PORT
                     self.add_ethrpc_port_forwarding(concord_ip, concord_username, concord_password,
                                                     src_port=src_port, dest_port=6865)

                     if helper.verify_connectivity(concord_ip, src_port):
                        log.info("DAML Connectivity - PASS")

                        try:
                           log.info("dar upload/verification...")
                           daml_helper.upload_test_tool_dars(host=concord_ip, port=str(src_port))
                           daml_helper.verify_ledger_api_test_tool(host=concord_ip, port=str(src_port))
                           log.info("dar upload/verification passed.")
                        except Exception as e:
                           log.error(e)
                           return failed("dar upload/verification failed")
                     else:
                        log.error("DAML Connectivity ({})- FAILED".format(concord_ip))
                        return failed("DAML Connectivity - FAILED")

            log.info("All post deployment sanity checks are successful")
            return passed()
         else:
            log.error("{} ethrpc endpoint not fetched".format(cluster_size))
            return failed("ethrpc endpoint not fetched")
      else:
         return failed("Deployment Event validation Failed")


   def _thread_deploy_blockchain_cluster(self, cluster_size, placement_type,
                                         result_queue, concord_type=None):
      '''
      This method is to support concurrent deployments and and do post deploy
      validations, and save the status in result queue.
      :param cluster_size: Blockchain cluster size
      :param placement_type: Node placement type
      :param result_queue: Result queue to save the status for each thread
      :param concord_type: Concord type (ethereum, DAML, etc)
      :return: Result status
      '''
      status = None
      thread_name = threading.current_thread().name
      log.info("Thread: {}".format(thread_name))
      start_time = time.time()
      log.info("Deployment Start Time: {}".format(start_time))
      response = self.rpc_test_helper.rpc_create_cluster(cluster_size=cluster_size,
                                                         placement_type=placement_type,
                                                         concord_type=concord_type)
      if response:
         response_session_id_json = helper.protobuf_message_to_json(response[0])
         if "id" in response_session_id_json:
            response_deployment_session_id = response[0]

            events = self.rpc_test_helper.rpc_stream_cluster_deployment_session_events(
               response_deployment_session_id)
            end_time = time.time()
            log.info(
               "**** Thread '{}' Time taken for this deployment: {} mins".format(
                  thread_name, (end_time - start_time) / 60))
            if events:
               status, msg, stackInfo = self.perform_post_deployment_validations(events,
                                                                      cluster_size,
                                                                      response_deployment_session_id,
                                                                      concord_type=concord_type)
               if status:
                  log.info(
                     "Thread {}: Deployment & Validation Completed Successfully".format(
                        thread_name))
               else:
                  log.info(
                     "Thread {}: Post Deployment Failed".format(thread_name))

               self.parse_test_status(status, msg, stackInfo,
                                      deployment_session_id=response_deployment_session_id)
               result_queue.put([status, msg])
            else:
               self.parse_test_status(status, msg, getStackInfo(),
                                      deployment_session_id=response_deployment_session_id)
               result_queue.put([False, "Failed to fetch Deployment Events", getStackInfo()])

      else:
         result_queue.put([False, "Failed to get a valid deployment session ID", getStackInfo()])

      log.info("Thread {}: Deployment Status '{}' put in request queue".format(
         thread_name, status))


   def _thread_stream_all_deployment_events(self):
      '''
      Call gRPC to stream all deployment events since the start of provisioning
      service
      '''
      status = None
      status_message = "Skipped"
      stackInfo = getStackInfo()
      all_events = self.rpc_test_helper.rpc_stream_all_cluster_deployment_session_events()
      # sleep before parsing the stream
      time.sleep(2)

      if all_events:
         all_deployment_session_ids_from_stream = []
         all_events = helper.protobuf_message_to_json(all_events)
         for event in all_events:
            if "COMPLETED" in event["type"]:
               all_deployment_session_ids_from_stream.append(event["session"])
         log.info("List of all session IDs from stream: {}".format(
            all_deployment_session_ids_from_stream))

         status = False
         for deployment_info in self.rpc_test_helper.deployment_info:
            session_id = deployment_info["deployment_session_id"]
            stub = deployment_info["stub"]

            if stub is None: # check for default channel/stub only
               if helper.protobuf_message_to_json(
                     session_id[0]) in all_deployment_session_ids_from_stream:
                  status = True
                  status_message = "Fetched all deployment Events Successfully!"
                  stackInfo = getStackInfo()
               else:
                  status = False
                  status_message = "Failed to fetch All Deployment Events"
                  stackInfo = getStackInfo()
                  break
      self.writeResult("StreamAllDeploymentEvents", status, status_message, stackInfo)


   def deploy_daml_committer_node(self, cluster_size=4, zone_type=helper.LOCATION_SDDC):
      '''
      Deploy DAML committer nodes
      :param cluster_size: No. of nodes on the cluster
      '''
      log.info("")
      log.info("**** Deploying committer node(s)...")
      concord_type = self.rpc_test_helper.CONCORD_TYPE_DAML
      node_type = self.rpc_test_helper.NODE_TYPE_COMMITTER
      replicas = None
      stackInfo = getStackInfo()

      start_time = time.time()
      log.info("Deployment Start Time: {}".format(start_time))
      response = self.rpc_test_helper.rpc_create_cluster(
         cluster_size=cluster_size,
         concord_type=concord_type,
         node_type=node_type,
         zone_type=zone_type)
      if response:
         response_session_id_json = helper.protobuf_message_to_json(response[0])
         if "id" in response_session_id_json:
            response_deployment_session_id = response[0]

            events = self.rpc_test_helper.rpc_stream_cluster_deployment_session_events(
               response_deployment_session_id)
            end_time = time.time()
            log.info("Deployment End Time: {}".format(end_time))
            log.info("**** Time taken for this deployment: {} mins".format(
               (end_time - start_time) / 60))

            if events:
               status, msg, stackInfo = self.perform_post_deployment_validations(events,
                                                                      cluster_size,
                                                                      response_deployment_session_id,
                                                                      concord_type=concord_type,
                                                                      node_type=node_type)
               if status:
                  for deployment_info in self.rpc_test_helper.deployment_info:
                     if deployment_info["deployment_session_id"][
                        0] == response_deployment_session_id:
                        if "private_ips" in deployment_info and deployment_info[
                           "private_ips"]:
                           replicas = deployment_info["private_ips"]

               status, msg, stackInfo = self.parse_test_status(status, msg, stackInfo,
                                             deployment_session_id=response_deployment_session_id)
               return (status, msg, stackInfo, replicas, response_deployment_session_id)
            status, msg, stackInfo = self.parse_test_status(False, "Failed to fetch Deployment Events", getStackInfo(),
                                          deployment_session_id=response_deployment_session_id)
            return (status, msg, stackInfo, replicas, None)

      status, msg, stackInfo = self.parse_test_status(False, "Failed to get a valid deployment session ID", getStackInfo())
      return (status, msg, stackInfo, replicas, None)


   def deploy_daml_participant_node(self, cluster_size=1, committers_session_id=None,
                                    replicas=None, zone_type=helper.LOCATION_SDDC):
      '''
      Deploy DAML participant node
      :param cluster_size: No. of nodes on the cluster
      :param replicas: List of replicas (committers)
      '''
      log.info("")
      log.info("**** Deploying participant node...")
      concord_type = self.rpc_test_helper.CONCORD_TYPE_DAML
      node_type = self.rpc_test_helper.NODE_TYPE_PARTICIPANT

      start_time = time.time()
      log.info("Deployment Start Time: {}".format(start_time))
      response = self.rpc_test_helper.rpc_create_cluster(
         cluster_size=cluster_size,
         concord_type=concord_type,
         node_type=node_type,
         zone_type=zone_type,
         replicas=replicas)
      if response:
         response_session_id_json = helper.protobuf_message_to_json(response[0])
         if "id" in response_session_id_json:
            response_deployment_session_id = response[0]

            events = self.rpc_test_helper.rpc_stream_cluster_deployment_session_events(
               response_deployment_session_id)
            end_time = time.time()
            log.info("Deployment End Time: {}".format(end_time))
            log.info("**** Time taken for this deployment: {} mins".format(
               (end_time - start_time) / 60))

            if events:
               status, msg, stackInfo = self.perform_post_deployment_validations(events,
                                                                      cluster_size,
                                                                      response_deployment_session_id,
                                                                      concord_type=concord_type,
                                                                      node_type=node_type)
               if not status:
                  log.info(
                     "Collecting support logs from committer nodes : {}".format(
                        replicas))
                  self.parse_test_status(status, "Support logs from committers", stackInfo,
                                      deployment_session_id=committers_session_id)
               return self.parse_test_status(status, msg, getStackInfo(),
                                             deployment_session_id=response_deployment_session_id)
            return self.parse_test_status(False, "Failed to fetch Deployment Events", getStackInfo(),
                                          deployment_session_id=response_deployment_session_id)

      return self.parse_test_status(False, "Failed to get a valid deployment session ID", getStackInfo())



   

   #=============================================================================================
   #
   #  Actual Test Functions
   #
   #=============================================================================================

   @describe()
   def _test_create_blockchain_4_node_fixed_site(self, cluster_size=4):
      '''
      Test to create a blockchain cluster with 4 nodes on FIXED sites
      :param cluster_size: No. of concord nodes on the cluster
      '''

      start_time = time.time()
      log.info("Deployment Start Time: {}".format(start_time))
      response = self.rpc_test_helper.rpc_create_cluster(cluster_size=cluster_size)
      if response:
         response_session_id_json = helper.protobuf_message_to_json(response[0])
         if "id" in response_session_id_json:
            response_deployment_session_id = response[0]

            events = self.rpc_test_helper.rpc_stream_cluster_deployment_session_events(
               response_deployment_session_id)
            end_time = time.time()
            log.info("Deployment End Time: {}".format(end_time))
            log.info("**** Time taken for this deployment: {} mins".format(
               (end_time - start_time) / 60))

            if events:
               status, msg, stackInfo = self.perform_post_deployment_validations(events,
                                                                      cluster_size,
                                                                      response_deployment_session_id)
               return self.parse_test_status(status, msg, stackInfo,
                                             deployment_session_id=response_deployment_session_id)
            return self.parse_test_status(False,
                                          "Failed to fetch Deployment Events",
                                          getStackInfo(),
                                          deployment_session_id=response_deployment_session_id)

      return self.parse_test_status(False, "Failed to get a valid deployment session ID", getStackInfo())


   @describe()
   def _test_create_daml_blockchain_7_node_fixed_site(self, cluster_size=7):
      '''
      Test to create a blockchain cluster with 7 DAML nodes on FIXED sites
      :param cluster_size: No. of concord nodes on the cluster
      '''
      concord_type = self.rpc_test_helper.CONCORD_TYPE_DAML

      start_time = time.time()
      log.info("Deployment Start Time: {}".format(start_time))
      response = self.rpc_test_helper.rpc_create_cluster(
         cluster_size=cluster_size,
         concord_type=concord_type)
      if response:
         response_session_id_json = helper.protobuf_message_to_json(response[0])
         if "id" in response_session_id_json:
            response_deployment_session_id = response[0]

            events = self.rpc_test_helper.rpc_stream_cluster_deployment_session_events(
               response_deployment_session_id)
            end_time = time.time()
            log.info("Deployment End Time: {}".format(end_time))
            log.info("**** Time taken for this deployment: {} mins".format(
               (end_time - start_time) / 60))

            if events:
               status, msg, stackInfo = self.perform_post_deployment_validations(events,
                                                                      cluster_size,
                                                                      response_deployment_session_id,
                                                                      concord_type=concord_type)
               return self.parse_test_status(status, msg, stackInfo,
                                             deployment_session_id=response_deployment_session_id)
            return self.parse_test_status(False, "Failed to fetch Deployment Events", getStackInfo(),
                                          deployment_session_id=response_deployment_session_id)

      return self.parse_test_status(False, "Failed to get a valid deployment session ID", getStackInfo())


   @describe()
   def _test_create_blockchain_7_node_fixed_site(self, cluster_size=7):
      '''
      Test to create a blockchain cluster with 7 nodes on FIXED sites
      :param cluster_size: No. of concord nodes on the cluster
      '''

      start_time = time.time()
      log.info("Deployment Start Time: {}".format(start_time))
      response = self.rpc_test_helper.rpc_create_cluster(cluster_size=cluster_size)
      if response:
         response_session_id_json = helper.protobuf_message_to_json(response[0])
         if "id" in response_session_id_json:
            response_deployment_session_id = response[0]

            events = self.rpc_test_helper.rpc_stream_cluster_deployment_session_events(
               response_deployment_session_id)
            end_time = time.time()
            log.info("Deployment End Time: {}".format(end_time))
            log.info("**** Time taken for this deployment: {} mins".format(
               (end_time - start_time) / 60))

            if events:
               status, msg, stackInfo = self.perform_post_deployment_validations(events,
                                                                      cluster_size,
                                                                      response_deployment_session_id)
               return self.parse_test_status(status, msg, stackInfo,
                                             deployment_session_id=response_deployment_session_id)
            return self.parse_test_status(False,
                                          "Failed to fetch Deployment Events",
                                          getStackInfo(),
                                          deployment_session_id=response_deployment_session_id)

      return self.parse_test_status(False, "Failed to get a valid deployment session ID", getStackInfo())


   @describe()
   def _test_concurrent_deployments_fixed_site(self, cluster_1_size=4,
                                               cluster_2_size=7):
      '''
      Test to perform concurrent deployments, both 4 node and 7 node
      :param cluster_1_size: Cluster 1 size
      :param cluster_2_size: Cluster 2 size
      :return: Test Status
      '''
      log.info("Performing concurrent deployments")

      result_queue = queue.Queue()
      cluster_1 = threading.Thread(target=self._thread_deploy_blockchain_cluster,
                                   name="Deployment_1", args=(
         cluster_1_size, self.rpc_test_helper.PLACEMENT_TYPE_FIXED,
         result_queue))
      cluster_2 = threading.Thread(target=self._thread_deploy_blockchain_cluster,
                                   name="Deployment_2", args=(
         cluster_2_size, self.rpc_test_helper.PLACEMENT_TYPE_FIXED,
         result_queue))

      log.info("Starting Deployment 1")
      cluster_1.start()
      log.info("Starting Deployment 2")
      cluster_2.start()

      cluster_1.join()
      cluster_2.join()

      overall_status = False
      while not result_queue.empty():
         deployment_status = result_queue.get()[0]
         result_queue.task_done()

         if deployment_status:
            overall_status = True
         else:
            overall_status = False
            break

      log.info("Overall status : {}".format(overall_status))

      if overall_status:
         log.info("Concurrent Deployments: Completed Successfully")
         return passed()
      else:
         log.error("Concurrent Deployments: Failed")
         return failed("Failed to deploy concurrent Clusters")


   @describe()
   def _test_concurrent_daml_deployments_fixed_site(self, cluster_1_size=4,
                                               cluster_2_size=7):
      '''
      Test to perform concurrent deployments, both 4 DAML nodes and 7 DAML nodes
      :param cluster_1_size: Cluster 1 size
      :param cluster_2_size: Cluster 2 size
      :return: Test Status
      '''
      log.info("Performing concurrent deployments")

      concord_type = self.rpc_test_helper.CONCORD_TYPE_DAML
      result_queue = queue.Queue()
      cluster_1 = threading.Thread(target=self._thread_deploy_blockchain_cluster,
                                   name="Deployment_1", args=(
         cluster_1_size, self.rpc_test_helper.PLACEMENT_TYPE_FIXED,
         result_queue), kwargs={'concord_type': concord_type})
      cluster_2 = threading.Thread(target=self._thread_deploy_blockchain_cluster,
                                   name="Deployment_2", args=(
         cluster_2_size, self.rpc_test_helper.PLACEMENT_TYPE_FIXED,
         result_queue), kwargs={'concord_type': concord_type})

      log.info("Starting Deployment 1")
      cluster_1.start()
      log.info("Starting Deployment 2")
      cluster_2.start()

      cluster_1.join()
      cluster_2.join()

      overall_status = False
      while not result_queue.empty():
         deployment_status = result_queue.get()[0]
         result_queue.task_done()

         if deployment_status:
            overall_status = True
         else:
            overall_status = False
            break

      log.info("Overall status : {}".format(overall_status))

      if overall_status:
         log.info("Concurrent Deployments: Completed Successfully")
         return passed()
      else:
         log.error("Concurrent Deployments: Failed")
         return failed("Failed to deploy concurrent Clusters")


   @describe()
   def _test_create_daml_blockchain_7_node_onprem(self, cluster_size=7):
      '''
      Test to create a blockchain cluster with 7 DAML nodes on-prem
      :param cluster_size: No. of concord nodes on the cluster
      '''
      concord_type = self.rpc_test_helper.CONCORD_TYPE_DAML

      start_time = time.time()
      log.info("Deployment Start Time: {}".format(start_time))
      response = self.rpc_test_helper.rpc_create_cluster(
         cluster_size=cluster_size,
         concord_type=concord_type,
         zone_type=self.rpc_test_helper.ZONE_TYPE_ON_PREM)
      if response:
         response_session_id_json = helper.protobuf_message_to_json(response[0])
         if "id" in response_session_id_json:
            response_deployment_session_id = response[0]

            events = self.rpc_test_helper.rpc_stream_cluster_deployment_session_events(
               response_deployment_session_id)
            end_time = time.time()
            log.info("Deployment End Time: {}".format(end_time))
            log.info("**** Time taken for this deployment: {} mins".format(
               (end_time - start_time) / 60))

            if events:
               status, msg, stackInfo = self.perform_post_deployment_validations(events,
                                                                      cluster_size,
                                                                      response_deployment_session_id,
                                                                      concord_type=concord_type)
               return self.parse_test_status(status, msg, stackInfo,
                                             deployment_session_id=response_deployment_session_id)
            return self.parse_test_status(False, "Failed to fetch Deployment Events", getStackInfo(),
                                          deployment_session_id=response_deployment_session_id)

      return self.parse_test_status(False, "Failed to get a valid deployment session ID", getStackInfo())


   @describe()
   def _test_daml_committer_participant_4_node_onprem(self,
                                                      committer_nodes=4,
                                                      participant_nodes=1,
                                                      zone_type=helper.LOCATION_ONPREM):
      '''
      Test to create DAML committer & participant nodes on-prem
      :param cluster_size: No. of concord nodes on the cluster
      '''
      status, msg, stackInfo, replicas, committers_session_id = self.deploy_daml_committer_node(
         cluster_size=committer_nodes, zone_type=zone_type)
      if status and replicas:
         log.info("**** Committer nodes deployed Successfully\n")
         status, msg, stackInfo = self.deploy_daml_participant_node(
            cluster_size=participant_nodes,
            committers_session_id=committers_session_id, replicas=replicas,
            zone_type=zone_type)
         if status:
            log.info("**** Participant node(s) deployed Successfully\n")
         else:
            log.error("Failed to deploy participant node(s)")
      else:
         log.error("Failed to deploy committer nodes")

      return (status, msg, stackInfo)


   @describe()
   def _test_daml_committer_participant_7_node_onprem(self,
                                                      committer_nodes=7,
                                                      participant_nodes=1,
                                                      zone_type=helper.LOCATION_ONPREM):
      '''
      Test to create DAML committer & participant nodes on-prem
      :param cluster_size: No. of concord nodes on the cluster
      '''
      status, msg, stackInfo, replicas, committers_session_id = self.deploy_daml_committer_node(
         cluster_size=committer_nodes, zone_type=zone_type)
      if status and replicas:
         log.info("**** Committer nodes deployed Successfully\n")
         status, msg, stackInfo = self.deploy_daml_participant_node(
            cluster_size=participant_nodes,
            committers_session_id=committers_session_id, replicas=replicas,
            zone_type=zone_type)
         if status:
            log.info("**** Participant node(s) deployed Successfully\n")
         else:
            log.error("Failed to deploy participant node(s)")
      else:
         log.error("Failed to deploy committer nodes")

      return (status, msg, stackInfo)


   @describe()
   def _test_daml_committer_participant_deployment(self, committer_nodes=7,
                                                     participant_nodes=1,
                                                     zone_type=helper.LOCATION_SDDC):
      '''
      Test to create DAML committer & participant nodes on-prem
      :param cluster_size: No. of concord nodes on the cluster
      '''
      status, msg, stackInfo, replicas, committers_session_id = self.deploy_daml_committer_node(
         cluster_size=committer_nodes, zone_type=zone_type)
      if status and replicas:
         log.info("**** Committer nodes deployed Successfully\n")
         status, msg, stackInfo = self.deploy_daml_participant_node(
            cluster_size=participant_nodes,
            committers_session_id=committers_session_id, replicas=replicas,
            zone_type=zone_type)
         if status:
            log.info("**** Participant node(s) deployed Successfully\n")
         else:
            log.error("Failed to deploy participant node(s)")
      else:
         log.error("Failed to deploy committer nodes")

      return (status, msg, stackInfo)

