#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# This class is a helper file to test model services (deployment services)
#########################################################################

import logging
import sys
from rpc_helper import RPCHelper
from vmware.blockchain.deployment.v1 import concord_model_pb2
from vmware.blockchain.deployment.v1 import metadata_service_pb2
from vmware.blockchain.deployment.v1 import metadata_service_pb2_grpc

sys.path.append('../../')
from util.product import Product as Product


log = logging.getLogger(__name__)


class ModelServiceRPCHelper(RPCHelper):
   CONCORD_TYPE_DAML = "daml"
   NODE_TYPE_COMMITTER = "committer"
   NODE_TYPE_PARTICIPANT = "participant"
   CONCORD_TYPE_ETHEREUM = "ethereum"
   CONCORD_TYPE_HLF = "hlf"


   def __init__(self, args):
      super().__init__(args)
      self.args = args
      self.service_name = Product.PERSEPHONE_SERVICE_METADATA
      self.service_port = self.get_persephone_service_port(self.service_name)

      self.grpc_server = "localhost:{}".format(self.service_port)
      try:
         self.channel = self.create_channel(self.service_name)
         self.stub = self.create_stub(self.channel)
      except Exception as e:
         raise Exception(e)

      self.AGENT_ID = self.args.userConfig["persephoneTests"]["modelService"][
         "deployment_component_ids"]["AGENT"]
      self.FLUENTD_ID = self.args.userConfig["persephoneTests"]["modelService"][
         "deployment_component_ids"]["FLUENTD"]

      self.CONCORD_ID = self.args.userConfig["persephoneTests"]["modelService"][
         "deployment_component_ids"]["CONCORD"]
      self.ETHRPC_ID = self.args.userConfig["persephoneTests"]["modelService"][
         "deployment_component_ids"]["ETHRPC"]

      self.DAML_EXECUTION_ENGINE_ID = \
      self.args.userConfig["persephoneTests"]["modelService"][
         "deployment_component_ids"]["DAML_EXECUTION_ENGINE"]
      self.DAML_LEDGER_API_ID = \
      self.args.userConfig["persephoneTests"]["modelService"][
         "deployment_component_ids"]["DAML_LEDGER_API"]
      self.DAML_INDEX_DB_ID = \
         self.args.userConfig["persephoneTests"]["modelService"][
            "deployment_component_ids"]["DAML_INDEX_DB"]

      self.HLF_ORDERER = \
      self.args.userConfig["persephoneTests"]["modelService"][
         "deployment_component_ids"]["HLF_ORDERER"]
      self.HLF_PEER = \
      self.args.userConfig["persephoneTests"]["modelService"][
         "deployment_component_ids"]["HLF_PEER"]
      self.HLF_TOOLS = \
         self.args.userConfig["persephoneTests"]["modelService"][
            "deployment_component_ids"]["HLF_TOOLS"]

   def __del__(self):
      self.close_channel(self.service_name)

   def create_add_model_request(self, header, concord_model_specification):
      add_model_request = metadata_service_pb2.AddModelRequest(
         header=header,
         specification=concord_model_specification)
      return add_model_request

   def create_concord_model_specification(self, version=None, template=None,
                                          deployment_components=None,
                                          concord_type=CONCORD_TYPE_ETHEREUM,
                                          node_type=None):
      '''
      RPC Helper to create concord model specification
      :param version: Model Specification version
      :param template: Model Specification template name (UUID)
      :param deployment_components: Deployment components to be used in concord spec
      :param concord_type: Concord type (ethereum, DAML, HLF etc)
      '''

      if version is None:
         version = \
         self.args.userConfig["persephoneTests"]["modelService"]["defaults"][
            "concord_model_version"]
      if template is None:
         template = \
         self.args.userConfig["persephoneTests"]["modelService"]["defaults"][
            "concord_model_template_id"]

      if concord_type is ModelServiceRPCHelper.CONCORD_TYPE_DAML:
         blockchain_type = concord_model_pb2.ConcordModelSpecification.DAML
      elif concord_type is ModelServiceRPCHelper.CONCORD_TYPE_HLF:
         blockchain_type = concord_model_pb2.ConcordModelSpecification.HLF
      else:
         concord_type = ModelServiceRPCHelper.CONCORD_TYPE_ETHEREUM
         blockchain_type = concord_model_pb2.ConcordModelSpecification.ETHEREUM
      log.info("**** Deploying Blockchain Type: {}".format(concord_type))

      if deployment_components:
         deployment_components = deployment_components.split(',')
      else:
         if node_type is None:
            concord_type_to_deploy = concord_type
         else:
            concord_type_to_deploy = "{}-{}".format(concord_type, node_type)

         deployment_components = \
            self.args.userConfig["persephoneTests"]["modelService"]["defaults"][
               "deployment_components"][concord_type_to_deploy].keys()

      concord_components = []
      components_for_this_deployment = []
      if concord_type is ModelServiceRPCHelper.CONCORD_TYPE_DAML:
         for component in deployment_components:
            if self.AGENT_ID in component:
               concord_components.append(
                  (concord_model_pb2.ConcordComponent.GENERIC, component))
            if self.FLUENTD_ID in component:
               concord_components.append(
                  (concord_model_pb2.ConcordComponent.LOGGING, component))
            if self.CONCORD_ID in component:
               if node_type is None or node_type == self.NODE_TYPE_COMMITTER:
                  concord_components.append((
                                          concord_model_pb2.ConcordComponent.DAML_CONCORD,
                                          component))
            if self.DAML_EXECUTION_ENGINE_ID in component:
               if node_type is None or node_type == self.NODE_TYPE_COMMITTER:
                  concord_components.append((
                                          concord_model_pb2.ConcordComponent.DAML_EXECUTION_ENGINE,
                                          component))
            if self.DAML_LEDGER_API_ID in component:
               if node_type is None or node_type == self.NODE_TYPE_PARTICIPANT:
                  concord_components.append((
                     concord_model_pb2.ConcordComponent.DAML_LEDGER_API,
                     component))
            if self.DAML_INDEX_DB_ID in component:
               if node_type is None or node_type == self.NODE_TYPE_PARTICIPANT:
                  concord_components.append((
                     concord_model_pb2.ConcordComponent.DAML_INDEX_DB,
                     component))
      elif concord_type is ModelServiceRPCHelper.CONCORD_TYPE_HLF:
         for component in deployment_components:
            if self.AGENT_ID in component:
               concord_components.append(
                  (concord_model_pb2.ConcordComponent.GENERIC, component))
            if self.CONCORD_ID in component:
               concord_components.append(
                  (concord_model_pb2.ConcordComponent.HLF_CONCORD, component))
            if self.HLF_ORDERER in component:
               concord_components.append((
                                         concord_model_pb2.ConcordComponent.HLF_ORDERER,
                                         component))
            if self.HLF_PEER in component:
               concord_components.append((
                                         concord_model_pb2.ConcordComponent.HLF_PEER,
                                         component))
            if self.HLF_TOOLS in component:
               concord_components.append((
                  concord_model_pb2.ConcordComponent.HLF_TOOLS,
                  component))
      else:
         for component in deployment_components:
            if self.AGENT_ID in component:
               concord_components.append(
                  (concord_model_pb2.ConcordComponent.GENERIC, component))
            if self.FLUENTD_ID in component:
               concord_components.append(
                  (concord_model_pb2.ConcordComponent.LOGGING, component))
            if self.CONCORD_ID in component:
               concord_components.append(
                  (concord_model_pb2.ConcordComponent.CONCORD, component))
            if self.ETHRPC_ID in component:
               concord_components.append(
                  (concord_model_pb2.ConcordComponent.ETHEREUM_API, component))

      log.info(
         "**** Using Deployment components: {}".format(concord_components))

      model_specification = concord_model_pb2.ConcordModelSpecification(
         version=version,
         template=template,
         components=self.get_concord_components(concord_components),
         blockchain_type=blockchain_type
      )
      log.debug("Model Specification: {}".format(model_specification))
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
