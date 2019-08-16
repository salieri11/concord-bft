#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# This class is a helper file to test orchestration services (deployment services)
#########################################################################

import sys
sys.path.append('lib/persephone')
from grpc_python_bindings import orchestration_service_pb2
from grpc_python_bindings import orchestration_service_pb2_grpc
from rpc_helper import RPCHelper
sys.path.append('../../')
from util.product import Product as Product

import logging

log = logging.getLogger(__name__)


class OrchestrationServiceRPCHelper(RPCHelper):
   def __init__(self, args):
      super().__init__(args)
      self.args = args
      self.provisioning_service_name = Product.PERSEPHONE_SERVICE_PROVISIONING
      self.service_name = Product.PERSEPHONE_SERVICE_ORCHESTRATION
      self.service_port = self.get_persephone_service_port(self.provisioning_service_name)

      self.grpc_server = "localhost:{}".format(self.service_port)
      try:
         self.channel = self.create_channel(self.service_name)
         self.stub = self.create_stub(self.channel)
      except Exception as e:
         raise Exception(e)

   def __del__(self):
      self.close_channel(self.service_name)


   def rpc_ListOrchestrationSites(self):
      '''
      Call to ListOrchestrationSites RPC
      '''
      log.info("ListOrchestrationSites RPC")
      response = None
      try:
         response = self.call_api(self.stub.ListOrchestrationSites,
                                  orchestration_service_pb2.ListOrchestrationSitesRequest())
      except Exception as e:
         self.handle_exception(e)
      return response
