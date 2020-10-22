#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import pytest
from suites.case import describe
from fixtures.common_fixtures import fxBlockchain, fxConnection, fxInitializeOrgs, fxProduct
from util import auth, helper, product, hermes_logging, json_helper
from util.helen import common, error_codes, validators, zone
import collections

log = hermes_logging.getMainLogger()

defaultTokenDescriptor = auth.getTokenDescriptor(auth.ROLE_CON_ADMIN, True, auth.internal_admin)

LocalSetupfixture = collections.namedtuple("LocalSetupfixture", ["flag", "node_size", "participant_nodes",
                                                                 "committer_nodes", "warning"])


@pytest.fixture
@describe("fixture; Initial Setup")
def fxLocalSetup(fxHermesRunSettings, fxBlockchain, fxConnection):
    warning = None
    blockchain_type = fxHermesRunSettings["hermesCmdlineArgs"].blockchainType.lower()
    blockchain_location = fxHermesRunSettings["hermesCmdlineArgs"].blockchainLocation.lower()

    if blockchain_type == helper.TYPE_DAML and blockchain_location in [helper.LOCATION_SDDC, helper.LOCATION_ONPREM]:
        flag = True
    else:
        warning = ("blockchainType must be {} and blockchainLocation must be onprem or sddc and VM size must be "
                   "specified- All VM Test skipped".format(blockchain_type, blockchain_location))
        flag = False
    node_sizing = get_node_size(fxHermesRunSettings, fxConnection)
    all_participants = fxBlockchain.replicas[helper.TYPE_DAML_PARTICIPANT]
    all_committers = fxBlockchain.replicas[helper.TYPE_DAML_COMMITTER]

    participant_nodes = []
    committer_nodes = []

    for node in (all_participants + all_committers):
        ip = node['private_ip'] if node['public_ip'] is None else node['public_ip']

        # as the client node has name as None
        if node['name'] is None:
            node['name'] = "Client"

        # get vm size for only Client and Replica, there may be other types of node as well
        if node['name'] == "Client":
            vm_size = get_vm_size(ip)
            node_info = {
                        "ip": ip,
                        "size_info": vm_size,
                        "name": node["name"]
            }
            participant_nodes.append(node_info)
        elif node['name'][0:7] == "Replica":
            vm_size = get_vm_size(ip)
            node_info = {
                        "ip": ip,
                        "size_info": vm_size,
                        "name": node["name"]
            }
            committer_nodes.append(node_info)

        log.info("checking {} IP Address - {}".format(node['name'], ip))

    log.debug("Specified sizing details {}".format(node_sizing))
    log.debug("Participant nodes {}".format(participant_nodes))
    log.debug("Committer nodes {}".format(committer_nodes))
    return LocalSetupfixture(flag=flag, node_size=node_sizing, participant_nodes=participant_nodes,
                             committer_nodes=committer_nodes, warning=warning)


def get_node_size(fxHermesRunSettings, fxConnection):
    '''
    creating a node size json consisting of replica and client size from command line argument
    then modifying it with available details if command line argument is None
    '''
    node_sizing = [
        {
            "type": "replica",
            "name": fxHermesRunSettings["hermesCmdlineArgs"].replicaSize,
            "mem": fxHermesRunSettings["hermesCmdlineArgs"].replicaMemory,
            "storage": fxHermesRunSettings["hermesCmdlineArgs"].replicaStorage,
            "cpu": fxHermesRunSettings["hermesCmdlineArgs"].replicaCpu
        },
        {
            "type": "client",
            "name": fxHermesRunSettings["hermesCmdlineArgs"].clientSize,
            "mem": fxHermesRunSettings["hermesCmdlineArgs"].clientMemory,
            "storage": fxHermesRunSettings["hermesCmdlineArgs"].clientStorage,
            "cpu": fxHermesRunSettings["hermesCmdlineArgs"].clientCpu
        }]
    log.debug("Before modification {}".format(node_sizing))
    res = fxConnection.request.getNodeSizeTemplate()
    log.debug("Node size template {}".format(res))
    templates = res.get("templates")
    for node_size in node_sizing:
        for template in templates:
            if template.get("name").lower() == node_size.get("name").lower():
                for item in template.get("items"):
                    if item.get("type") == node_size.get("type"):
                        node_size["mem"] = item.get("memory_in_gigs") \
                            if node_size["mem"] is None or node_size["mem"] == '' else node_size["mem"]
                        log.debug("node memory {}".format(node_size["mem"]))
                        node_size["storage"] = item.get("storage_in_gigs") \
                            if node_size["storage"] is None or node_size["storage"] == '' else node_size["storage"]
                        node_size["cpu"] = item.get("no_of_cpus") \
                            if node_size["cpu"] is None or node_size["cpu"] == '' else node_size["cpu"]
                        break
                    else:
                        log.debug("type {} {}".format(item.get("type"), node_size.get("type")))
                break
            else:
                log.debug("name match failed {} {}".format(template.get("name").lower(), node_size.get("name").lower()))
    return node_sizing


def validate_size(actual_nodes_size, vm_node_size):
    '''
    Validates actual node size with template/entered size
    :param actual_nodes_size: Actual VM size of the nodes
    :param vm_node_size:  Node size from template/command line argument
    '''
    for node in actual_nodes_size:
        assert float(vm_node_size.get("storage")) == float(node[
            'size_info']['storage']), "Storage of {} IP Address-{} is not {}GB but {}GB". \
            format(node['name'], node['ip'], vm_node_size.get('storage_in_gigs'), node["size_info"]['storage'])

        assert vm_node_size.get("cpu") == node['size_info']['cpu'], \
            "CPU of {} IP Address-{} is not {} ".\
            format(node['name'], node['ip'], vm_node_size.get('no_of_cpus'), node['size_info']['cpu'])

        # Actual memory of VM wont match exactly to the specified json value
        # memory details obtained are in KB
        # obtaining two limits for comparison with specified value by dividing actual value by 1000*1000 and 1024*1024
        # we can ensure that the actual value is within +3.5% or -3.5% from the specified value
        assert (float(node['size_info']['memory']) / (1000 * 1000)) >= float(vm_node_size.get("mem")) \
               >= (float(node['size_info']['memory']) / (1024 * 1024)), "Memory of {} IP Address-{} is not " \
                                                                        "{}".format(node['name'], node["ip"],
                                                                                    vm_node_size.get('memory_in_gigs'),
                                                                                    node['size_info']['memory'])


def validate_range(fxConnection, actual_nodes_size):
    '''
     Validates the sizing template provided for the system against available node size template range
     '''
    res = fxConnection.request.getNodeSizeTemplate()
    range_template = res.get("range")
    max_cpu = range_template.get("no_of_cpus").get("max")
    min_cpu = range_template.get("no_of_cpus").get("min")
    max_memory = range_template.get("memory_in_gigs").get("max")
    min_memory = range_template.get("memory_in_gigs").get("min")
    max_storage = range_template.get("storage_in_gigs").get("max")
    min_storage = range_template.get("storage_in_gigs").get("min")
    for node in actual_nodes_size:
        cpu = int(node["size_info"].get("cpu"))
        storage = node["size_info"].get("storage")
        memory = int(node["size_info"].get("memory"))

        assert cpu <= max_cpu or cpu >= min_cpu, \
            "CPU of {} with IP: {} is not in the range {} - {}".format(node["name"], node["ip"], max_cpu, min_cpu)

        assert memory <= max_memory or memory >= min_memory, \
            "Memory of {} with IP: {} is not in the range {} - {}".format(node["name"], node["ip"],
                                                                          max_memory, min_memory)

        assert float(storage) <= float(max_storage) or float(storage) >= float(min_storage), \
            "Storage of {} with IP: {} is not in the range {} - {}".format(node["name"], node["ip"],
                                                                           max_storage, min_storage)


def get_vm_size(ip):
    '''
     Get VM size details from the vm of given IP
    '''
    user_config = json_helper.readJsonFile(helper.CONFIG_USER_FILE)
    username = user_config["persephoneTests"]["provisioningService"]["concordNode"]["username"]
    password = user_config["persephoneTests"]["provisioningService"]["concordNode"]["password"]
    vm_actual_size = {}
    cmd_vm_memory = "grep MemTotal /proc/meminfo"
    cmd_vm_cpu = "lscpu|grep 'CPU(s):'"
    cmd_vm_disk = "fdisk -l | grep Disk | grep /dev/sdb"

    final_command = cmd_vm_memory + "\n" + cmd_vm_cpu + "\n" + cmd_vm_disk
    output = helper.ssh_connect(ip, username, password, final_command)
    assert output, "Unable to connect with IP:{}".format(ip)
    log.debug("*********Size Details*********\n{}".format(output))
    output = output.split("\n")
    vm_actual_size["memory"] = (output[0].split()[1])
    vm_actual_size["cpu"] = (output[1].split()[1])

    # cpu command may result in one or two line output. hence checking the length of the output
    # when storage is greater than 1024 GB, the output will be in-terms of TB. changing it to equivalent GB value
    index = 2 if len(output) == 3 else 3
    log.debug("VM storage: {}".format(output[index]))
    vm_actual_size["storage"] = str(float(output[index].split()[2])*1024) \
        if "TiB" in output[index].split()[3] else output[index].split()[2]

    return vm_actual_size


# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# HELEN BLOCKCHAIN SIZE TESTS
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=d-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
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
    validators.validateBlockchainFields(blockchain)


@describe()
@pytest.mark.smoke
@pytest.mark.blockchains
def test_replica_size(fxConnection, fxBlockchain, fxLocalSetup):
    '''
     Verify VM size of Replicas
     '''
    if not fxLocalSetup.flag:
        pytest.skip(fxLocalSetup.warning)

    replica_size_info = fxLocalSetup.node_size[0] if fxLocalSetup.node_size[0].get("type") is "replica" \
        else fxLocalSetup.node_size[1]

    log.info("Performing VM size verification of replicas")
    committer_nodes_size = fxLocalSetup.committer_nodes
    validate_size(committer_nodes_size, replica_size_info)


@describe()
@pytest.mark.smoke
@pytest.mark.blockchains
def test_client_size(fxConnection, fxBlockchain, fxLocalSetup):
    '''
     Verify VM size of Clients
     '''
    if not fxLocalSetup.flag:
        pytest.skip(fxLocalSetup.warning)

    client_size_info = fxLocalSetup.node_size[1] if fxLocalSetup.node_size[1].get("type") is "client" \
        else fxLocalSetup.node_size[0]

    log.info("Performing VM size verification of clients")
    participant_nodes_size = fxLocalSetup.participant_nodes
    validate_size(participant_nodes_size, client_size_info)


@describe()
@pytest.mark.smoke
@pytest.mark.blockchains
def test_vm_range(fxConnection, fxLocalSetup):
    '''
     Verify created VM size is as per the node-size-template range
     '''
    if not fxLocalSetup.flag:
        pytest.skip(fxLocalSetup.warning)
    log.info("Performing range test")
    validate_range(fxConnection, fxLocalSetup.participant_nodes + fxLocalSetup.committer_nodes)
