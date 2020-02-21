#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# This class is a helper file to test persephone gRPC
#########################################################################

import grpc
import json
import logging
import os
import queue
import sys
import threading
import time
from google.protobuf.json_format import MessageToJson
from vmware.blockchain.deployment.v1 import provisioning_service_pb2
from vmware.blockchain.deployment.v1 import provisioning_service_pb2_grpc

sys.path.append('../../')
from util.product import Product as Product
from util import helper


log = logging.getLogger(__name__)


class RPCHelper():
   def __init__(self, args):
      self.args = args
      self.channel_connect_timeout = 5  # seconds
      self.channel_connect_status = False

   def get_persephone_service_port(self, service_name):
      '''
      Helper method to get the port number for the passed persephone service
      :param service_name: microservice name
      :return: port number
      '''
      ports = helper.get_docker_compose_value(
         self.args.dockerComposeFile, service_name, "ports")
      try:
         port = ports[0].split(':')[0]
      except Exception as e:
         raise
      return port

   def get_provisioning_config_file(self, service_name):
      '''
      Helper method to get the provisioning config file
      :param service_name: service name (provisioning)
      :return: config file
      '''
      return helper.get_deployment_service_config_file(
         self.args.dockerComposeFile, service_name)

   def create_channel(self, service_name):
      '''
      Helper method to create a gRPC channel conneting to a gRPC server
      :param service_name: Service name like persephone-provisioning-service, etc
      :return: gRPC channel
      '''
      log.info("Creating channel to microservice '{}'".format(service_name))
      self.channel = grpc.insecure_channel(self.grpc_server)
      max_try = 2
      for i in range(1, max_try + 1):
         log.info("  Check if server is up in {} seconds [{}/{}]...".format(
            self.channel_connect_timeout, i, max_try))
         try:
            grpc.channel_ready_future(self.channel).result(
               timeout=self.channel_connect_timeout)
         except grpc.FutureTimeoutError:
            if i == max_try:
               log.error("****Error connecting to server")
               sys.exit(1)
         else:
            break
      self.channel_connect_status = True
      return self.channel

   def create_stub(self, channel):
      '''
      Helper method to create a gRPC stup to the created channel
      :param channel: gRPC channel
      :return: gRPC stub to the created channel
      '''
      log.info("Creating stub...")
      stub = None
      log.info("  Creating stub for {}".format(self.service_name))
      if self.service_name is Product.PERSEPHONE_SERVICE_PROVISIONING:
         stub = provisioning_service_pb2_grpc.ProvisioningServiceStub(channel)

      if stub is None:
         raise Exception(
            "Stub creation failed for service {}".format(self.service_name))
      return stub

   def close_channel(self, service_name):
      '''
      Helper method to gracefully close a created channel
      :param service_name: Service name (for logging)
      '''
      log.info("*** Closing channel to microservice '{}'".format(service_name))
      if self.channel and self.channel_connect_status:
         self.channel.close()
      else:
         log.error("Channel not open to be closed")

   def collect_responses(self, response_iterator, response_queue):
      '''
      Collects the items/events from a gRPC stream and adds to a queue
      :param response_iterator: gRPC stream
      :param response_queue: response queue
      '''
      try:
         for response in response_iterator:
            #log.debug("Adding to response queue: {}".format(response))
            response_queue.put(response)
      except Exception as e:
         pass

   def call_api(self, rpc, rpc_request=None, stream=False, stream_forever=False,
                stream_timeout=300):
      '''
      Helper method to call the actual gRPC using python bindings
      :param rpc: gRPC
      :param rpc_request: gRPC request
      :param stream: boolean to expect a stream/non-stream
      :param stream_forever: stream open until cancelled (Default False)
      :param stream_timeout: Max timeout when the stream would be CANCELLED
      :return: gRPC response
      '''
      response_list = []
      log.info("Calling rpc {}/[stream: {} / Run forever: {}] ****".format(rpc,
                                                                           stream,
                                                                           stream_forever))
      response_stream = rpc(rpc_request, stream_timeout)
      if stream:
         if stream_forever:
            response_queue = queue.Queue()
            thread = threading.Thread(target=self.collect_responses,
                                      args=(response_stream, response_queue))
            thread.start()

            sleep_time = 30
            time_slept = 0
            while time_slept < stream_timeout and not self.args.cancel_stream:
               log.debug("Trigger status to cancel stream: {}".format(
                  self.args.cancel_stream))
               log.debug(
                  "Sleep for {} secs ({}/{}) and check if 'cancel background "
                  "stream collection' trigger is enabled.".format(
                     sleep_time, time_slept, stream_timeout))
               time.sleep(sleep_time)
               time_slept += sleep_time

            log.info("**** Trigger received to Cancel Stream. This will take a while.")

            # The thread in Persephone which supplies events has a one minute initial wait,
            # and then wakes up every thirty seconds.  For very fast test runs, we can end
            # up with no events and resources being left on SDDC. So sleep one more sleep_time.
            time.sleep(sleep_time)
            response_stream.cancel()
            thread.join()

            while not response_queue.empty():
               rsp = response_queue.get()
               response_list.append(rsp)
         else:
            for rsp in response_stream:
               response_list.append(rsp)
      else:
         response_list.append(response_stream)

      #log.debug("gRPC Response from server: {}".format(response_list))

      request_file = os.path.join(self.args.fileRoot,
                                  "{}_request.json".format(
                                     time.time()))
      log.info("gRPC Request: {}".format(request_file))
      rpc_request_json = helper.protobuf_message_to_json(rpc_request)
      with open(request_file, "w") as f:
         json.dump(rpc_request_json, f, indent=4, sort_keys=True)

      response_file = os.path.join(self.args.fileRoot,
                                   "{}_response.json".format(
                                      time.time()))
      log.info("gRPC Response: {}".format(response_file))
      response_json = helper.protobuf_message_to_json(response_list)
      with open(response_file, "w") as f:
         json.dump(response_json,f, indent=4, sort_keys=True)

      return response_list

   def handle_exception(self, e):
      '''
      Helper method to handle various HTTP status and other error codes from
      gRPC server
      :param e: Exception
      '''
      log.error("Error: {}".format(e))
      try:
         status_code = e.code()
         if status_code == grpc.StatusCode.DEADLINE_EXCEEDED:
            log.error("gRPC Call TIMED OUT")
      except Exception as err:
         pass


