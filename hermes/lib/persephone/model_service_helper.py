#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# This class is a helper file to test model services (deployment services)
#########################################################################

import sys
sys.path.append('lib/persephone')
from grpc_python_bindings import model_service_pb2
from grpc_python_bindings import model_service_pb2_grpc
from rpc_helper import RPCHelper
from grpc_python_bindings import concord_model_pb2
import logging

log = logging.getLogger(__name__)


class ModelServiceRPCHelper(RPCHelper):
   def __init__(self, persephone_service):
      super().__init__(persephone_service)
      self.grpc_server = "localhost:{}".format(self.service_port)
      try:
         self.channel = self.create_channel(self.service_name)
         self.stub = self.create_stub(self.channel)
      except Exception as e:
         raise Exception(e)

   def __del__(self):
      self.close_channel(self.service_name)

   def create_add_model_request(self, header, concord_model_specification):
      add_model_request = model_service_pb2.AddModelRequest(
         header=header,
         specification=concord_model_specification)
      return add_model_request

   def create_concord_model_specification(self):
      '''
      RPC Helper to create concord model specification
      '''
      model_specification = concord_model_pb2.ConcordModelSpecification(
         version=self.get_concord_model_version(),
         template=self.get_concord_model_template_name(),
         components=self.get_concord_components())
      return model_specification

   def get_concord_model_version(self, version="test_version"):
      '''
      concord model version to be used for choosing a specific metadata
      # TODO: Once product uses version as an option to choose a template,
      Read version from config json outside code
      '''
      return version

   def get_concord_model_template_name(self, template_name="test_template"):
      '''
      concord model temolate to be used for choosing a specific metadata
      # TODO: Once product uses template name as an option to choose a template,
      Read template name from config json outside code
      '''
      return template_name

   def get_concord_components(self):
      '''
      concord model components (docker images) to be used for picking a
      specific metadata
      # TODO: Read component names from config json outside code
      '''
      concord_components = ["vmwblockchain/concord-core:latest",
                            "vmwblockchain/ethrpc:latest"]
      components = []
      for name in concord_components:
         component = concord_model_pb2.ConcordComponent(
            type=concord_model_pb2.ConcordComponent.DOCKER_IMAGE,
            name=name
         )
         components.append(component)
      return components

   def rpc_AddModel(self, add_model_request):
      '''
      Call to AddModel RPC
      '''
      log.info("AddModel RPC")
      response = None
      try:
         response = self.call_api(self.stub.AddModel, add_model_request)
      except Exception as e:
         self.handle_exception(e)
      return response

   def rpc_ListModels(self):
      '''
      Call to ListModels RPC
      '''
      log.info("ListModels RPC")
      response = None
      try:
         response = self.call_api(self.stub.ListModels,
                                  model_service_pb2.ListModelsRequest(),
                                  stream=True)
      except Exception as e:
         self.handle_exception(e)
      return response
