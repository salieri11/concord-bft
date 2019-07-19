#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# This class is a helper file to test model services (deployment services)
#########################################################################

import sys
sys.path.append('lib/persephone')
from grpc_python_bindings import metadata_service_pb2
from grpc_python_bindings import metadata_service_pb2_grpc
from rpc_helper import RPCHelper
from grpc_python_bindings import concord_model_pb2
sys.path.append('../../')
from util.product import Product as Product

import logging

log = logging.getLogger(__name__)


class ModelServiceRPCHelper(RPCHelper):
   def __init__(self, args):
      super().__init__(args)
      self.service_name = Product.PERSEPHONE_SERVICE_METADATA
      self.service_port = self.get_persephone_service_port(self.service_name)

      self.grpc_server = "localhost:{}".format(self.service_port)
      try:
         self.channel = self.create_channel(self.service_name)
         self.stub = self.create_stub(self.channel)
      except Exception as e:
         raise Exception(e)

      # TODO: Read these default values from config json outside code or
      #  directly from persephone's config
      self.DEFAULT_CONCORD_MODEL_VERSION = "1.0"
      self.DEFAULT_CONCORD_MODEL_TEMPLATE = "8abc7fda-9576-4b13-9beb-06f867cf2c7c"
      self.DEFAULT_CONCORD_COMPONENTS = [
         (concord_model_pb2.ConcordComponent.CONCORD, "vmwblockchain/concord-core:latest"),
         (concord_model_pb2.ConcordComponent.ETHEREUM_API, "vmwblockchain/ethrpc:latest")
      ]

   def __del__(self):
      self.close_channel(self.service_name)

   def create_add_model_request(self, header, concord_model_specification):
      add_model_request = metadata_service_pb2.AddModelRequest(
         header=header,
         specification=concord_model_specification)
      return add_model_request

   def create_concord_model_specification(self, version=None, template=None,
                                             concord_components=None):
      '''
      RPC Helper to create concord model specification
      :param version: Model Specification version
      :param template: Model Specification template name (UUID)
      :param concord_components: Concord components in the spec
      '''

      if version is None:
         version = self.DEFAULT_CONCORD_MODEL_VERSION
      if template is None:
         template = self.DEFAULT_CONCORD_MODEL_TEMPLATE
      if concord_components is None:
         concord_components = self.DEFAULT_CONCORD_COMPONENTS

      model_specification = concord_model_pb2.ConcordModelSpecification(
         version=version,
         template=template,
         components=self.get_concord_components(concord_components))
      return model_specification

   def get_concord_components(self, concord_components):
      '''
      concord model components (docker images) to be used for picking a
      specific metadata
      '''
      components = []
      for service_type, name in concord_components:
         component = concord_model_pb2.ConcordComponent(
            type=concord_model_pb2.ConcordComponent.CONTAINER_IMAGE,
            service_type=service_type,
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
                                  metadata_service_pb2.ListModelsRequest(),
                                  stream=True)
      except Exception as e:
         self.handle_exception(e)
      return response
