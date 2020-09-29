#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import pytest

from suites.case import describe

from fixtures.common_fixtures import fxBlockchain, fxConnection, fxInitializeOrgs, fxProduct
import util.auth
import util.helen.common
import util.helen.error_codes
import util.helen.validators
import util.helen.zone
import util.helper
import util.product
import math
from util import json_helper
import util.hermes_logging
import collections

log = util.hermes_logging.getMainLogger()

defaultTokenDescriptor = util.auth.getTokenDescriptor(util.auth.ROLE_CON_ADMIN,
                                                      True,
                                                      util.auth.internal_admin)

LocalSetupfixture = collections.namedtuple("LocalSetupfixture",["flag", "vm_size","warning"])

@pytest.fixture
@describe("fixture; Initial Setup")
def fxLocalSetup(fxHermesRunSettings):
   warning = None
   blockchain_type = fxHermesRunSettings["hermesCmdlineArgs"].blockchainType.lower()
   blockchain_location = fxHermesRunSettings["hermesCmdlineArgs"].blockchainLocation.lower()
   vm_size_configs = fxHermesRunSettings["hermesCmdlineArgs"].vmSizeConfig
   if vm_size_configs:
      size_config = json_helper.readJsonFile(vm_size_configs)
   else:
      size_config = None
   if (blockchain_type == util.helper.TYPE_DAML and
         blockchain_location in [util.helper.LOCATION_SDDC, util.helper.LOCATION_ONPREM] and
         size_config):
      flag = True
   else:
      warning = ("blockchainType must be {} and blockchainLocation must be onprem or sddc and VM size must be "
                  "specified- All VM Test skipped".format(blockchain_type, blockchain_location))
      flag = False

   return LocalSetupfixture(flag=flag,vm_size=size_config,warning = warning)

def validateSize(nodes,vm_node_size):
   for node in nodes:
      if node['public_ip'] == None:
         ip = node['private_ip']
      else:
         ip = node['public_ip']

      if node['name'] == None:
         node['name'] = node['type_name']

      log.info("checking {} IP Address - {}".format(node['name'], ip))
      vm_actual_size = getVMSize(ip)
      assert vm_node_size.get("storage_in_gigs") == vm_actual_size[
         'storage'], "Storage of {} IP Address-{} is not {}GB but {}GB".format(node['name'], ip,
                                                                    vm_node_size.get("storage_in_gigs"),vm_actual_size["storage"])
      assert vm_node_size.get("no_of_cpus") == vm_actual_size[
         'vcpu'], "CPU of {} IP Address-{} is not {} ".format(node['name'], ip, vm_node_size.get("no_of_cpus"),vm_actual_size["vcpu"])
      assert vm_node_size.get("memory_in_gigs") == vm_actual_size[
         'memory'], "Memory of {} IP Address-{} is not {}".format(node['name'], ip,
                                                                  vm_node_size.get("memory_in_gigs"),vm_actual_size["memory"])


def getVMSize(ip):
   '''
    Get VM size details
    '''
   user_config = util.json_helper.readJsonFile(util.helper.CONFIG_USER_FILE)
   username = user_config["persephoneTests"]["provisioningService"]["concordNode"]["username"]
   password = user_config["persephoneTests"]["provisioningService"]["concordNode"]["password"]
   vm_actual_size = {}
   cmd_vm_memory = "grep MemTotal /proc/meminfo"
   cmd_vm_cpu = "lscpu|grep 'CPU(s):'"
   cmd_vm_disk = "fdisk -l | grep Disk | grep /dev/sdb"

   final_command = cmd_vm_memory +"\n"+ cmd_vm_cpu +"\n"+ cmd_vm_disk
   output=util.helper.ssh_connect(ip, username, password, final_command)
   assert output, "Unable to connect with IP:{}".format(ip)
   log.debug ("*********Total*********\n{}".format(output))
   output=output.split("\n")
   memory = (output[0].split()[1])
   vm_actual_size["memory"] = str(math.ceil(int(memory) / (1024 * 1024)))
   vm_actual_size["vcpu"] = (output[1].split()[1])
   vm_actual_size["storage"] = (output[3].split()[2])
   return vm_actual_size

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# HELEN BLOCKCHAIN SIZE TESTS
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=d-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
@describe()
@pytest.mark.smoke
@pytest.mark.blockchains
def test_vm_range(fxConnection, fxLocalSetup):
   '''
    Verify VM size passed is as per the node-size-template range
    '''
   if not fxLocalSetup.flag:
      pytest.skip(fxLocalSetup.warning)

   log.info("PERFORMING RANGE TEST")

   validate_range = util.helen.validators.validateRange(fxConnection.request, fxLocalSetup.vm_size)
   assert validate_range == True, validate_range


@describe()
@pytest.mark.smoke
@pytest.mark.blockchains
def test_size_blockchain(fxConnection, fxLocalSetup):
   '''
    Verify Blockchain created successfully when size parameter is passed
   '''
   if not fxLocalSetup.flag:
      pytest.skip(fxLocalSetup.warning)

   log.info("Performing deployed Blockchain with size parameters validation")

   blockchains = fxConnection.request.getBlockchains()
   assert len(blockchains) == 1, "Expected one blockchain to be returned"
   blockchain = blockchains[0]
   util.helen.validators.validateBlockchainFields(blockchain)

@describe()
@pytest.mark.smoke
@pytest.mark.blockchains
def test_replica_size(fxConnection, fxBlockchain, fxLocalSetup):
   '''
    Verify VM size of Replicas
    '''
   if not fxLocalSetup.flag:
      pytest.skip(fxLocalSetup.warning)

   vm_replica_size = fxLocalSetup.vm_size.get("replica")

   if not vm_replica_size:
      pytest.skip("Replica size is not specified")

   log.info("Performing VM size verification of replicas")

   all_committers = fxBlockchain.replicas[util.helper.TYPE_DAML_COMMITTER]

   validateSize(all_committers,vm_replica_size)


@describe()
@pytest.mark.smoke
@pytest.mark.blockchains
def test_client_size(fxConnection, fxBlockchain, fxLocalSetup):
   '''
    Verify VM size of Clients
    '''
   if not fxLocalSetup.flag:
      pytest.skip(fxLocalSetup.warning)

   vm_client_size = fxLocalSetup.vm_size.get("replica")

   if not vm_client_size:
      pytest.skip("Client size is not specified")

   log.info("Performing VM size verification of replicas")

   all_participants = fxBlockchain.replicas[util.helper.TYPE_DAML_PARTICIPANT]

   validateSize(all_participants,vm_client_size)
