#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# This class is a helper file to test persephone gRPC
#########################################################################

import yaml
import json
import grpc
from grpc_python_bindings import model_service_pb2
from grpc_python_bindings import model_service_pb2_grpc
from grpc_python_bindings import provision_service_pb2
from grpc_python_bindings import provision_service_pb2_grpc
from grpc_python_bindings import fleet_service_pb2
from grpc_python_bindings import fleet_service_pb2_grpc
from google.protobuf.json_format import MessageToJson
import sys

sys.path.append('../../')
from util.product import Product as Product
from util import helper
import logging

log = logging.getLogger(__name__)


class RPCHelper():
   def __init__(self, cmdlineArgs):
      self.cmdlineArgs = cmdlineArgs
      self.channel_connect_timeout = 5  # seconds
      self.channel_connect_status = False

   def get_persephone_service_port(self, service_name):
      '''
      Helper method to get the port number for the passed persephone service
      :param service_name: microservice name
      :return: port number
      '''
      ports = helper.get_docker_compose_value(
         self.cmdlineArgs.dockerComposeFile, service_name, "ports")
      try:
         port = ports[0].split(':')[0]
      except Exception as e:
         raise
      return port

   def get_provisioning_config_file(self, service_name):
      '''
      Helper method to get the provisioning config file
      :param service_name: service name (provisioning)
      :return: configl file
      '''
      config_file = helper.get_docker_compose_value(
         self.cmdlineArgs.dockerComposeFile, service_name, "volumes")
      try:
         config_file = config_file[0].split(':')[0]
      except Exception as e:
         raise
      return config_file

   def create_channel(self, service_name):
      '''
      Helper method to create a gRPC channel conneting to a gRPC server
      :param service_name: Service name like persephone-metadata, etc
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
      if self.service_name is Product.PERSEPHONE_SERVICE_METADATA:
         stub = model_service_pb2_grpc.ConcordModelServiceStub(channel)
      if self.service_name is Product.PERSEPHONE_SERVICE_PROVISIONING:
         stub = provision_service_pb2_grpc.ProvisionServiceStub(channel)
      # if self.service_name is Product.PERSEPHONE_SERVICE_FLEET:
      #    stub = fleet_service_pb2_grpc.FleetServiceStub(channel)

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

   def call_api(self, rpc, rpc_request=None, stream=False):
      '''
      Helper method to call the acutal gRPC using python bindings
      :param rpc: gRPC name
      :param rpc_request: gRPC request
      :param stream: boolean to expect a stream/non-stream
      :return: gRPC response
      '''
      response_list = []
      log.info(
         "**** Calling rpc {}/[response=stream: {}] ****".format(rpc, stream))
      # TODO: Introduce thread and MAX TIMEOUT when waiting for stream
      response = rpc(rpc_request)
      log.info("**** Response:")
      if stream:
         if response:
            for rsp in response:
               response_list.append(rsp)
         else:
            log.error("Error reading stream")
      else:
         response_list.append(response)

      log.debug("gRPC Response from server: {}".format(response_list))
      # TODO: Copy the response to a json file for logging
      return response_list

   def handle_exception(self, e):
      '''
      Helper method to handle various HTTP status and other error codes from
      gRPC server
      :param e: Exception
      '''
      log.error("Error: {}".format(e))
