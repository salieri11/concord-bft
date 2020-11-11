# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential

# Castor tests are run after the docker-compose-castor.yml file is launched. That
# docker-compose file launches the deployment. This test further validates that
# the deployment went through successfully.
#
# main.py
#         --dockerComposeFiles=../docker/docker-compose-castor.yml,
#                              ../docker/docker-compose-castor-prereqs.yml
#         CastorDeploymentTests


import pytest
import atexit
import os
import fnmatch
import re
import json
import subprocess
import util.helper as helper
import util.hermes_logging
from util.product import Product
from suites.case import describe
import tempfile
import sys
import io
import time
from datetime import datetime, timezone
import string
import grpc
from lib.persephone.rpc_helper import RPCHelper
from lib.persephone.provisioning_service_new_helper import ProvisioningServiceNewRPCHelper
from lib.persephone.vmware.blockchain.deployment.v1 import core_pb2
from lib.persephone.vmware.blockchain.deployment.v1 import orchestration_pb2
from lib.persephone.vmware.blockchain.deployment.v1 import provisioning_service_new_pb2 as ps_apis



log = util.hermes_logging.getMainLogger()

# Read by the fxProduct fixture.
# Set this to no-op so the framework does not wait for Provisioning service to
# start up. It will be started by this test itself.
_productType = helper.TYPE_NO_VERIFY

# NOTE: These need to match the docker-compose-castor.yml file
_ORCHESTRATOR_DESCRIPTORS_DIR_KEY = "ORCHESTRATOR_DESCRIPTORS_DIR"
_ORCHESTRATOR_DESCRIPTORS_DIR_VALUE = "../docker/config-castor/descriptors"
_INFRA_DESC_FILENAME_KEY = "INFRA_DESC_FILENAME"
_INFRA_DESC_FILENAME_VALUE = "test01_infrastructure_descriptor.json"
_DEPLOY_DESC_FILENAME_KEY = "DEPLOY_DESC_FILENAME"
_DEPLOY_DESC_FILENAME_VALUE = "test01_deployment_descriptor.json"
_ORCHESTRATOR_OUTPUT_DIR_KEY = "ORCHESTRATOR_OUTPUT_DIR"
_CONFIG_SERVICE_IP = "CONFIG_SERVICE_IP"

_COMPOSE_CASTOR_LOG = "docker-compose-castor.log"
_DEPLOYMENT_SUCCESS_MSG = "Deployment completed with status: SUCCESS"
_CASTOR_OUTPUT_SUCCESS_MSG_PATTERN = "Deployment finished at .* with status SUCCESS"
_CASTOR_OUTPUT_NODE_LOGIN_PATTERN = "NODE_LOGIN"
# These must match the infra and deployment descriptor files
_CONSORTIUM_NAME = "hermes-castor-consortium"
_CASTOR_OUTPUT_CLIENT_NODE_PATTERN = "CLIENT_GROUP_ID"
_COMPOSE_CONFIG_SERVICE_LOG = "docker-compose-config-service.log"

"""
All fixtures
"""


@pytest.fixture
def product(fxHermesRunSettings):
    cmdLineArgs = fxHermesRunSettings['hermesCmdlineArgs']
    userConfig = fxHermesRunSettings['hermesUserConfig']
    product = Product(cmdLineArgs, userConfig)
    return product


@pytest.fixture
def upPrereqsDocker(fxHermesRunSettings, product):
    """
    Launch docker-compose-castor-prereqs.yml to start the product
    """
    dockerComposeFiles = fxHermesRunSettings['hermesCmdlineArgs'].dockerComposeFile
    castorOutputDir = fxHermesRunSettings["hermesTestLogDir"]
    log.info(castorOutputDir)
    os.makedirs(castorOutputDir, exist_ok=True)
    prereqsComposeFile = None
    for dcf in dockerComposeFiles:
        if "prereqs.yml" in dcf:
            prereqsComposeFile = dcf
            
    # Update provisioning service application-test.properties with local docker base image versions.
    # This is needed because further DAML validation below requires the image versions to be updated to their latest.
    tags_info = helper.get_agent_pulled_tags_info()
    concord_current_tag = tags_info["tags"]["concord"]["tag"]
    log.info("concord_current_tag: %s" % concord_current_tag)
    log.info("command line docker compose files: %s" % dockerComposeFiles)
    persephone_config_file = helper.get_deployment_service_config_file(dockerComposeFiles,
                                                                       Product.PERSEPHONE_SERVICE_PROVISIONING)
    log.info("Updating provisioning service application properties file: %s with concord_current_tag: %s" %
             (persephone_config_file, concord_current_tag))
    helper.set_props_file_value(persephone_config_file, 'docker.image.base.version', concord_current_tag)

    if not product.validatePaths(dockerComposeFiles):
        raise Exception("Docker compose file is not present: %s" % dockerComposeFiles)

    cmd = ["docker-compose"]
    cmd += ["--file", prereqsComposeFile]
    cmd += ["up"]
    log.info("Launching docker compose: {}".format(cmd))

    # Setup config service to launch the product
    newEnv = os.environ.copy()
    newEnv[_CONFIG_SERVICE_IP] = helper.getNetworkIPAddress()
    prereqsComposeOutputLogFilePath = os.path.join(castorOutputDir, _COMPOSE_CONFIG_SERVICE_LOG)
    grpc_server_status = False
    with io.open(prereqsComposeOutputLogFilePath, "w") as prereqsLogFile, \
            io.open(prereqsComposeOutputLogFilePath, 'r', 1) as reader:
        subprocess.Popen(cmd, stdout=prereqsLogFile, stderr=prereqsLogFile, env=newEnv)
        reader.seek(0, 2)
        while grpc_server_status is not True:
            line = reader.readline()
            if not line:
                time.sleep(0.5)
                if "gRPC Server started, listening on port" in reader.read():
                    grpc_server_status = True

    assert grpc_server_status, "Config and Provisioning service couldn't started"
    log.info("Config service and provisioning service is started")
    log.info("docker-compose-castor-prereqs.yml launched")

    # Take the backup of existing deployment and infrastructure descriptor
    with open(os.path.join(_ORCHESTRATOR_DESCRIPTORS_DIR_VALUE, _INFRA_DESC_FILENAME_VALUE)) as from_file, \
            open(os.path.join(_ORCHESTRATOR_DESCRIPTORS_DIR_VALUE, "infra_backup.json"), "w+") as to:
        to.write(from_file.read())


@pytest.fixture
def upCastorDockerCompose(request, fxHermesRunSettings, product):
    """
    Launch docker-compose-castor.yml, and wait for it to finish.
    The whole test is predicated on the successful launch and completion of the castor docker process:
    It is a self-contained product that reads the descriptors, calls the provisioning service to deploy
    the blockchain, and exits.

    :return success or failure of launching the compose file:
    """

    num_replicas, num_clients, deployment_descriptor, infra_descriptor, is_multiple_zone = request.param
    if deployment_descriptor is not _DEPLOY_DESC_FILENAME_VALUE and infra_descriptor is not _INFRA_DESC_FILENAME_VALUE:
        log.info("Not updating infrastructure and deployment descriptor. customized ones are {} and {}".
                 format(infra_descriptor, deployment_descriptor))
    else:
        add_remove_nodes_from_deploy_descriptor(num_replicas, num_clients)
        _populateInfraDescriptorFile(fxHermesRunSettings, is_multiple_zone)
    dockerComposeFiles = fxHermesRunSettings['hermesCmdlineArgs'].dockerComposeFile
    castorOutputDir = fxHermesRunSettings["hermesTestLogDir"]
    os.makedirs(castorOutputDir, exist_ok=True)
    castorComposeFile = None
    for dcf in dockerComposeFiles:
        if "docker-compose-castor.yml" in dcf:
            castorComposeFile = dcf

    if not product.validatePaths(dockerComposeFiles):
        raise Exception("Docker compose file is not present: %s" % dockerComposeFiles)

    cmd = ["docker-compose"]
    cmd += ["--file", castorComposeFile]
    cmd += ["up"]
    log.info("Launching docker compose: {}".format(cmd))

    # Set up the descriptor and output directories as env variables picked up by docker-compose
    # NOTE: These need to match the docker-compose-castor.yml file
    newEnv = os.environ.copy()
    newEnv[_ORCHESTRATOR_DESCRIPTORS_DIR_KEY] = _ORCHESTRATOR_DESCRIPTORS_DIR_VALUE
    newEnv[_ORCHESTRATOR_OUTPUT_DIR_KEY] = castorOutputDir

    newEnv[_INFRA_DESC_FILENAME_KEY] = infra_descriptor
    newEnv[_DEPLOY_DESC_FILENAME_KEY] = deployment_descriptor

    castorComposeOutputLogFilePath = os.path.join(castorOutputDir, _COMPOSE_CASTOR_LOG)
    deployment_success = False

    # measuring time castor takes for the deployment
    start_time = datetime.now(timezone.utc).astimezone()
    log.info("Deployment start time: {}".format(start_time.strftime(helper.TIME_FMT_TIMEZONE)))

    with io.open(castorComposeOutputLogFilePath, "a") as composeOutputLogFile, io.open(castorComposeOutputLogFilePath,
                                                                                       'r', 1) as reader:
        # Wait until the docker-compose process terminates
        subprocess.Popen(cmd, stdout=composeOutputLogFile, stderr=composeOutputLogFile, env=newEnv)
        reader.seek(0, 2)
        while deployment_success is not True:
            line = reader.readline()
            if _DEPLOYMENT_SUCCESS_MSG in line:
                log.info("Got deployment success msg")
                deployment_success = True
            if "Deployment completed with status: FAILURE" in line:
                deployment_success = False
                break
            if "docker_castor_1 exited with code 1" in line:
                deployment_success = False
                break

    # get the end time after the deployment
    end_time = datetime.now(timezone.utc).astimezone()
    log.info("Deployment end time: {}".format(end_time.strftime(helper.TIME_FMT_TIMEZONE)))
    deployment_time = end_time - start_time
    m, s = divmod(deployment_time.seconds, 60)
    log.info("Deployment time taken: {} minutes and {} seconds".format(m, s))

    assert m <= 15, "Deployment took more than 15 minutes"
    log.info("docker-compose-castor.yml launched")
    return deployment_success


@pytest.fixture
def castor_teardown(fxHermesRunSettings):
    """
    This is a teardown function which will deprovision the deployed blockchain and revert the infra and deployment descriptor
    """
    yield True
    log.info("Starting teardown")
    deprovision_blockchain(fxHermesRunSettings)

    # Revert modified descriptors
    with open(os.path.join(_ORCHESTRATOR_DESCRIPTORS_DIR_VALUE, "infra_backup.json")) as from_file, \
            open(os.path.join(_ORCHESTRATOR_DESCRIPTORS_DIR_VALUE, _INFRA_DESC_FILENAME_VALUE), "w+") as to:
        to.write(from_file.read())


"""
Generic Functions
"""


def match_pattern(lines, pattern):
    matches = 0
    for line in lines:
        if pattern.search(line):
            matches += 1
    return matches


def add_remove_nodes_from_deploy_descriptor(num_replicas, num_clients):
    """
    This function is to prepare deployment descriptor
    """
    if num_replicas > 0 or num_clients > 0:
        with open(os.path.join(_ORCHESTRATOR_DESCRIPTORS_DIR_VALUE, _DEPLOY_DESC_FILENAME_VALUE)) as deploy_desc:
            data = json.load(deploy_desc)

        if len(data["replicas"]) > num_replicas:
            data["replicas"] = data["replicas"][:-(len(data["replicas"]) - num_replicas)]

        if len(data["clients"]) > num_clients:
            data["clients"] = data["clients"][:-(len(data["clients"]) - num_clients)]

        if len(data["replicas"]) < num_replicas:
            no_of_committers_req = len(data["replicas"]) - num_replicas
            clone_committers = [data["replicas"][0]] * abs(no_of_committers_req)
            data["replicas"] = [*data["replicas"], *clone_committers]

        if len(data["clients"]) < num_clients:
            no_of_clients_req = len(data["clients"]) - num_clients
            clone_clients = [data["clients"][0]] * abs(no_of_clients_req)
            data["clients"] = [*data["clients"], *clone_clients]

        with open(os.path.join(_ORCHESTRATOR_DESCRIPTORS_DIR_VALUE, _DEPLOY_DESC_FILENAME_VALUE), 'w') as w_deploy_desc:
            json.dump(data, w_deploy_desc, indent=4)

        log.info("Deployment descriptor is updated")

    else:
        return


def _populateInfraDescriptorFile(fxHermesRunSettings, is_multiple_zone=False):
    """
    This function is to prepare infrastructure descriptor
    """
    available_zones = list(string.ascii_uppercase)

    with open(os.path.join(_ORCHESTRATOR_DESCRIPTORS_DIR_VALUE, _INFRA_DESC_FILENAME_VALUE)) as infraFile:
        infraDescriptor = json.load(infraFile)
    default_zone_path = fxHermesRunSettings['hermesZoneConfig']['zones']['onprem']
    len_default_zone_path = len(default_zone_path)
    log.info("Length of default zone is {}".format(len_default_zone_path))
    if is_multiple_zone is True:
        if len(default_zone_path) > len(infraDescriptor['zones']):
            no_of_zones_req = len(default_zone_path) - len(infraDescriptor['zones'])
            log.info("Number of zone required {}".format(no_of_zones_req))
            clone_zones = [infraDescriptor['zones'][0]] * abs(no_of_zones_req)
            infraDescriptor['zones'] = [*infraDescriptor['zones'], *clone_zones]
        with open(os.path.join(_ORCHESTRATOR_DESCRIPTORS_DIR_VALUE, _INFRA_DESC_FILENAME_VALUE), "w") as infraFile:
            json.dump(infraDescriptor, infraFile, indent=4)
        time.sleep(5)

    else:
        len_default_zone_path = 1
    with open(os.path.join(_ORCHESTRATOR_DESCRIPTORS_DIR_VALUE, _INFRA_DESC_FILENAME_VALUE)) as infra_modified_File:
        infraDescriptor = json.load(infra_modified_File)

    for item in range(len_default_zone_path):
        url = default_zone_path[item]['api']['address']
        userName = default_zone_path[item]['api']['credential']['passwordCredential']['username']
        password = default_zone_path[item]['api']['credential']['passwordCredential']['password']
        resource = default_zone_path[item]['vsphere']['resourcePool']
        storage = default_zone_path[item]['vsphere']['datastore']
        folder = default_zone_path[item]['vsphere']['folder']
        networkName = default_zone_path[item]['vsphere']['network']['name']
        gateway = default_zone_path[item]['vsphere']['network']['gateway']
        subnet = default_zone_path[item]['vsphere']['network']['subnet']
        nameServers = default_zone_path[item]['vsphere']['network']['nameServers']

        # update infra keys
        infraDescriptor['zones'][item]["name"] = "hermes-castor-zone-1 - {}".format(available_zones[item])
        log.info("Logging zone {}".format(infraDescriptor['zones'][item]["name"]))
        infraDescriptor['zones'][item]['vCenter']['url'] = url
        infraDescriptor['zones'][item]['vCenter']['userName'] = userName
        infraDescriptor['zones'][item]['vCenter']['password'] = password
        infraDescriptor['zones'][item]['vCenter']['resourcePool'] = resource
        infraDescriptor['zones'][item]['vCenter']['storage'] = storage
        infraDescriptor['zones'][item]['vCenter']['folder'] = folder

        infraDescriptor['zones'][item]['network']['name'] = networkName
        infraDescriptor['zones'][item]['network']['gateway'] = gateway
        infraDescriptor['zones'][item]['network']['subnet'] = subnet
        infraDescriptor['zones'][item]['network']['nameServers'] = nameServers
        time.sleep(5)

    with open(os.path.join(_ORCHESTRATOR_DESCRIPTORS_DIR_VALUE, _INFRA_DESC_FILENAME_VALUE), "w") as infraFile:
        json.dump(infraDescriptor, infraFile, indent=4)

    log.info("Infrastructure descriptor is updated")
    log.info(infraFile)


def downCastorDockerCompose(dockerComposeFiles):
    log.info("Stopping docker containers for: %s" % dockerComposeFiles)


def _get_all_nodes(castor_output_file):
    """
    Get the private IP from the output file for each node
    """
    all_nodes = []
    node_values = []
    cof = open(castor_output_file)
    lines = cof.readlines()[3:]
    for line in lines:
        if _CASTOR_OUTPUT_CLIENT_NODE_PATTERN in line:
            client_node_login_pattern = re.search(r'(key: )([A-Z])(\w+), (value: )(.*$)', line)
            node_values.append(client_node_login_pattern.group(5))

    # based on the node info get the client node IP
    client_node_ips = []
    ip_pattern = re.compile(r'[0-9]+(?:\.[0-9]+){3}')
    for node in node_values:
        for line in lines:
            if node in line and re.search(ip_pattern, line):
                client_node_ips.append(ip_pattern.findall(line)[0])

    for line in lines:
        if "PRIVATE_IP" in line:
            all_nodes.append(ip_pattern.findall(line)[0])

    cof.close()
    return all_nodes, client_node_ips



def _get_node_info_list(all_nodes, client_nodes):
    """
    Create a dictionary of the nodes to identify which is committer and which is participant
    """
    node_info_list = {}
    for node in all_nodes:
        if node in client_nodes:
            node_info_list[node] = "daml_participant"
        else:
            node_info_list[node] = "daml_committer"

    return node_info_list


def _get_docker_containers_by_node_type(user_config, node_type):
    """
    Get the list of docker containers that should be available in each node type
    """
    deployDescFilePath = os.path.join(_ORCHESTRATOR_DESCRIPTORS_DIR_VALUE, _DEPLOY_DESC_FILENAME_VALUE)
    with open(deployDescFilePath, "r") as deployFile:
        data = deployFile.read()

    deployDescriptor = json.loads(data)

    blockchain_type = deployDescriptor["blockchain"]["blockchainType"]
    blockchain_type = blockchain_type.lower()
    if blockchain_type == helper.TYPE_DAML:
        if node_type == "daml_committer":  # Replica node
            container_type = helper.TYPE_DAML_COMMITTER
        else:  # Client node
            container_type = helper.TYPE_DAML_PARTICIPANT
    else:
        container_type = blockchain_type

    docker_containers = list(user_config["persephoneTests"]["modelService"]["defaults"]["deployment_components"]
                             [container_type].values())

    return docker_containers


def _verify_docker_containers_in_each_node(fxHermesRunSettings, node_info_list):
    """
    Verify if the necessary docker containers are running on each the nodes
    :param fxHermesRunSettings: Fixture for Hermes settings
    :param node_info_list: List of NodeInfo
    :return: True/False
    """
    status = False
    userConfig = fxHermesRunSettings["hermesUserConfig"]
    # get default username and password of the node from user config
    username, password = helper.getNodeCredentials()

    error_msg = "Error verifying docker containers"
    for node, node_type in node_info_list.items():
        containers_to_verify = _get_docker_containers_by_node_type(userConfig, node_type)
        log.info("Containers to verify for node {} and node type {}".format(node, node_type))

        # Verify SSH connection
        ssh_status = _verify_ssh_connectivity(node, username, password)
        if not ssh_status:
            error_msg = "Could not SSH to {}. Aborting docker container verification".format(node)
            break

        # Verify docker containers
        count = 0
        max_timeout = 1200  # 20 mins; containers total size 10 GB+ now
        start_time = time.time()
        docker_images_found = False
        command_to_run = "docker ps --format '{{.Names}}'"
        log.info("Waiting for all docker containers to be up on {} within {} mins".format(node, max_timeout / 60))
        while (time.time() - start_time) <= max_timeout and not docker_images_found:
            count += 1
            log.debug("Verifying docker containers (attempt: {})".format(count))
            ssh_output = helper.ssh_connect(node, username, password, command_to_run)
            log.debug("SSH output: {}".format(ssh_output))
            for container_name in containers_to_verify:
                if container_name not in ssh_output:
                    if container_name == "wavefront-proxy":
                        continue
                    docker_images_found = False
                    log.warning("Container '{}' not up and running on node '{}'".format(container_name, node))
                    time.sleep(30)
                    break  # break out of container_name in containers_to_verify for-loop
                else:
                    docker_images_found = True
                    log.debug("Container {} found in node {}".format(container_name, node))

        if not docker_images_found:
            error_msg = "Not all containers are up and running on node '{}'".format(node)
            break  # break out of node in node_info_list for-loop
        else:
            log.info("Docker containers verified on {}".format(node))
            status = True

    return status, error_msg


def _verify_ssh_connectivity(ip, username, password, mode=None):
    """
    Establish SSH connection to node
    :param ip: IP to SSH into
    :param username: Username of the node
    :param password: Password of the node
    :param mode: None for marking node as "logged in". Any other mode, indicates check for the marker file
    :return: True/False
    """
    log.debug("Establishing SSH connection to {}".format(ip))
    ssh_result = None
    status = False
    max_timeout = 180  # 3 mins
    start_time = time.time()
    while (time.time() - start_time) <= max_timeout and ssh_result is None:
        ssh_result = helper.ssh_connect(ip, username, password, "hostname", log_mode="WARNING")
        if ssh_result:
            log.debug("SSH enabled within {} mins".format((time.time() - start_time) / 60))
            status = True
            break
        else:
            sleep_time = 30
            log.debug("Sleep for {} seconds and retry".format(sleep_time))
            time.sleep(sleep_time)

    if not status:
        log.error("SSH connection to {} could not be established within {} mins".format(ip, max_timeout / 60))
    else:  # Mark node as logged in
        status = False  # setting status to False for this portion
        marker_file = "/tmp/{}".format(ip)
        if mode is None:
            command_to_run = "touch {} ; ls {}".format(marker_file, marker_file)
            log.debug("Marking concord node as logged in: {}".format(command_to_run))
            validation_message_success = "Marker file '{}' created".format(marker_file)
            validation_message_fail = "Failed to create marker file '{}'".format(marker_file)
        else:
            command_to_run = "ls {}".format(marker_file)
            log.debug("Verifying if concord node is marked as logged in: {}".format(command_to_run))
            validation_message_success = "Marker file '{}' found".format(marker_file)
            validation_message_fail = "Cannot find marker file '{}'".format(marker_file)

        ssh_output = helper.ssh_connect(ip, username, password, command_to_run)
        log.debug("SSH output: {}".format(ssh_output))
        if ssh_output:
            if ssh_output.rstrip() == marker_file:
                log.debug(validation_message_success)
                status = True
        if not status:
            log.error(validation_message_fail)

    return status

def validate_castor_output_msg(castorOutputDir, num_of_nodes):
    
    files = os.listdir(castorOutputDir)

    filePatternStr = _CONSORTIUM_NAME + "*"
    outputFileBases = fnmatch.filter(files, filePatternStr)
    assert len(outputFileBases) == 1, "Zero/Multiple matches found for consortium: %s, %s" % (
        _CONSORTIUM_NAME, outputFileBases)

    outputFileBase = outputFileBases[0]
    castorOutputFile = os.path.join(castorOutputDir, outputFileBase)
    log.info("Processing castor output file: {}".format(castorOutputFile))

    # Check that the output file says "SUCCESS"
    outputSuccessMatchPattern = re.compile(_CASTOR_OUTPUT_SUCCESS_MSG_PATTERN)
    with open(castorOutputFile) as cof:
        lines = cof.readlines()
        matchingLines = match_pattern(lines, outputSuccessMatchPattern);
        assert matchingLines == 1, "Castor output file does not have success marker: %s" % _CASTOR_OUTPUT_SUCCESS_MSG_PATTERN

        # Match 2 clients and 4 committers
        nodeLoginMatchPattern = re.compile(_CASTOR_OUTPUT_NODE_LOGIN_PATTERN)
        matchingNodeLogins = match_pattern(lines, nodeLoginMatchPattern)
    
    return matchingNodeLogins, castorOutputFile



def create_resources(site_id, castor_output_file):
    '''
    parse the output file to retrieve resource properties and create resource instances
    return the array of Resource
    '''
    with open(castor_output_file, 'r') as cof:
        lines = cof.readlines()
    patternString = "^Node Id:.*, name: (.*), key: (.*), value: (.*)$"
    pattern = re.compile(patternString)
    seennames = set()
    resources = []
    for line in lines:
        m = pattern.match(line)
        if m:
            resname = m.group(1)
            if resname.startswith('/'):
                if resname not in seennames:
                    seennames.add(resname)
                    resource = ps_apis.DeployedResource(type='NETWORK_RESOURCE', name=resname, site_id=site_id)
                    resources.append(resource)
            elif resname.startswith('http'):
                if resname not in seennames:
                    seennames.add(resname)
                    resource = ps_apis.DeployedResource(type='COMPUTE_RESOURCE', name=resname, site_id=site_id)
                    resources.append(resource)

    return resources


def get_deployed_node_size(ip):
    '''
     Get Node size details
    '''
    
    # get default username and password of the node from user config
    username, password = helper.getNodeCredentials()
    
    deployed_vm_size = {}
    cmd_vm_memory = "grep MemTotal /proc/meminfo"
    cmd_vm_cpu = "lscpu|grep 'CPU(s):'"
    cmd_vm_disk = "fdisk -l | grep Disk | grep /dev/sdb"

    vm_size_command = cmd_vm_memory + "\n" + cmd_vm_cpu + "\n" + cmd_vm_disk
    ssh_output = helper.ssh_connect(ip, username, password, vm_size_command)
    assert ssh_output, "Unable to connect to VM :{}".format(ip)
    ssh_output = ssh_output.split("\n")
    deployed_vm_size["memoryGb"] = (ssh_output[0].split()[1])
    deployed_vm_size["cpuCount"] = (ssh_output[1].split()[1])

    # cpu command may result in one or two line output. hence checking the length of the output
    # when storage is greater than 1024 GB, the output will be in-terms of TB. changing it to equivalent GB value
    index = 2 if len(ssh_output) == 3 else 3
    deployed_vm_size["diskSizeGb"] = str(float(ssh_output[index].split()[2])*1024) \
        if "TiB" in ssh_output[index].split()[3] else ssh_output[index].split()[2]

    return deployed_vm_size


def validate_replica_node_size(deployment_descriptor, deployed_vm_size, ip):
    '''
    Validate deployed node size with defined node size in descriptor
    '''
    deployment_descriptor_path = os.path.join(_ORCHESTRATOR_DESCRIPTORS_DIR_VALUE, deployment_descriptor)
    with open(deployment_descriptor_path) as ds:
        deployDescriptor = json.load(ds)
    
    assert deployDescriptor["replicaNodeSpec"]["cpuCount"] == int(deployed_vm_size["cpuCount"]), "CPU count of replica node IP {} is {} which is not matching with replica node CPU defined {}".format(ip, deployed_vm_size["cpuCount"], deployDescriptor["replicaNodeSpec"]["cpuCount"])
    assert (float(deployed_vm_size["memoryGb"]) / (1000 * 1000)) >= float(deployDescriptor["replicaNodeSpec"]["memoryGb"]) \
               >= (float(deployed_vm_size["memoryGb"]) / (1024 * 1024)), "Memory of replica node IP {} is not matching with replica node memory defined {}".format(ip, deployDescriptor["replicaNodeSpec"]["memoryGb"])
    #assert deployDescriptor["replicaNodeSpec"]["memoryGb"] == int(deployed_vm_size["memoryGb"]), "Memory of replica node IP {} is {} which is not matching with replica node memory defined {}".format(ip, deployed_vm_size["memoryGb"], deployDescriptor["replicaNodeSpec"]["memoryGb"])
    assert deployDescriptor["replicaNodeSpec"]["diskSizeGb"] == float(deployed_vm_size["diskSizeGb"]), "Disk size of replica node IP {} is {} which is not matching with replica node disk size defined {}".format(ip, deployed_vm_size["diskSizeGb"], deployDescriptor["replicaNodeSpec"]["diskSizeGb"])


def validate_client_node_size(deployment_descriptor, deployed_vm_size, ip):
    '''
    Validate deployed node size with defined node size in descriptor
    '''
    deployment_descriptor_path = os.path.join(_ORCHESTRATOR_DESCRIPTORS_DIR_VALUE, deployment_descriptor)
    with open(deployment_descriptor_path) as ds:
        deployDescriptor = json.load(ds)
    
    assert deployDescriptor["clientNodeSpec"]["cpuCount"] == int(deployed_vm_size["cpuCount"]), "CPU count of replica node IP {} is {} which is not matching with client node CPU defined {}".format(ip, deployed_vm_size["cpuCount"], deployDescriptor["clientNodeSpec"]["cpuCount"])
    assert (float(deployed_vm_size["memoryGb"]) / (1000 * 1000)) >= float(deployDescriptor["clientNodeSpec"]["memoryGb"]) \
               >= (float(deployed_vm_size["memoryGb"]) / (1024 * 1024)), "Memory of client node IP {} is not matching with client node memory defined {}".format(ip, deployDescriptor["clientNodeSpec"]["memoryGb"])    
    assert deployDescriptor["clientNodeSpec"]["diskSizeGb"] == float(deployed_vm_size["diskSizeGb"]), "Disk size of replica node IP {} is {} which is not matching with client node disk size defined {}".format(ip, deployed_vm_size["diskSizeGb"], deployDescriptor["clientNodeSpec"]["diskSizeGb"])


def deprovision_blockchain(fxHermesRunSettings):
    """
    De-provisioning the deployment
    """

    session_id = None
    castorOutputDir = fxHermesRunSettings["hermesTestLogDir"]
    files = os.listdir(castorOutputDir)

    filePatternStr = _CONSORTIUM_NAME + "*"
    outputFileBases = fnmatch.filter(files, filePatternStr)

    outputFileBase = outputFileBases[0]
    castorOutputFile = os.path.join(castorOutputDir, outputFileBase)
    with open(castorOutputFile) as cof:
        lines = cof.readlines()
        for line in lines:
            if "Deployment Request Id" in line:
                request_id_pattern = re.search(r'(Id: )(.*$)', line)
                session_id = request_id_pattern.group(2)

    log.info("De-provisioning the deployment using session id - {}".format(session_id))
    cmdlineArgs = fxHermesRunSettings["hermesCmdlineArgs"]
    cmdlineArgs.cancel_stream = True
    logs_for_stream_all_deploy_events = os.path.join(castorOutputDir, "stream_deployment_events")
    os.makedirs(logs_for_stream_all_deploy_events, exist_ok=True)
    cmdlineArgs.fileRoot = logs_for_stream_all_deploy_events

    ps_helper = ProvisioningServiceNewRPCHelper(cmdlineArgs)

    zone_type = "onprem"
    zone_config = fxHermesRunSettings["hermesZoneConfig"]

    site_ids, site_infos = ps_helper.get_orchestration_site_ids_and_infos(zone_type, zone_config)
    sites = ps_helper.create_sites(site_ids, site_infos)

    deployment_stream_events = ps_helper.stream_deployment_session_events(session_id)

    log.info("Streaming deployment events {}".format(deployment_stream_events))
    if not deployment_stream_events:
        raise Exception("Error while streaming deployment events")

    resource = []
    for event in deployment_stream_events:
        if event.type == ps_apis.DeploymentExecutionEvent.RESOURCE:
            resource.append(event.resource)

    log.info("This is resource {}".format(resource))
    site_id = site_ids[0].id
    resources = create_resources(site_id, castorOutputFile)

    log.info("Deprovisioning session id: {}".format(session_id))
    
    header = core_pb2.MessageHeader()

    cleaned_up = False
    max_timeout = 480  # seconds
    sleep_time = 15  # seconds
    start_time = time.time()
    while ((time.time() - start_time) < max_timeout) and not cleaned_up:
        deprovision_deployment_request = ps_apis.DeprovisionDeploymentRequest(header=header, session_id=session_id,
                                                                              sites=sites, resource=resources)
        log.info("deprovision deployment request {}".format(deprovision_deployment_request))
        deprovision_deployment_response = ps_helper.call_api(ps_helper.stub.DeprovisionDeployment,
                                                             deprovision_deployment_request)

        if deprovision_deployment_response:
            log.debug("Deprovisioning response: {}".format(deprovision_deployment_response))
            cleaned_up = True
            break
        else:
            log.info("Sleep for {} seconds and retry".format(sleep_time))
            time.sleep(sleep_time)

    if cleaned_up:
        log.info("Deprovisioning successful")
    else:
        log.info("Deprovisioning failed")

    return cleaned_up

"""
TEST CASES
"""




@describe("Deploy 7 node daml blockchain and verify blockchain health")
@pytest.mark.parametrize('upCastorDockerCompose',
                         [(7, 2, _DEPLOY_DESC_FILENAME_VALUE, _INFRA_DESC_FILENAME_VALUE, False)], indirect=True)
def test_castor_deployment_7_node(upPrereqsDocker, upCastorDockerCompose, fxHermesRunSettings, castor_teardown):
    """
    The startup/shutdown of the Castor containers is verified by the upPrereqsDocker, upCastorDockerCompose.
    Once the provisioning and config service is up then the castor deployment is initiated
    If that fails, there is no point in continuing onward with this test. The test verifies the following:
    1. the output file contains all the information about clients and committers specified in the descriptors files
    2. ssh connectivity on all the nodes
    3. Docker containers in each node
    4. After the above 3 are verified then the Daml sanity is executed for the participant nodes
    """
    # Check if deployment started
    assert upCastorDockerCompose, "Deployment did not start"

    # start the test
    num_of_nodes = 9
    log.info("Starting test test_castor_deployment")
    castorOutputDir = fxHermesRunSettings["hermesTestLogDir"]
    matchingNodeLogins, castorOutputFile = validate_castor_output_msg(castorOutputDir, num_of_nodes)

    # validate success message in the output file
    assert matchingNodeLogins == num_of_nodes, "%s expected lines: %s, found: %s" % (
            _CASTOR_OUTPUT_NODE_LOGIN_PATTERN, num_of_nodes, matchingNodeLogins)

        
    # # -------------post deployment validations -----------------
    # get all nodes and client node IPs from output file
    all_nodes, client_nodes = _get_all_nodes(castorOutputFile)

    # create a dictionary to segregate all the nodes by Replica and Client
    node_info_list = _get_node_info_list(all_nodes, client_nodes)

    # verify docker containers on each node
    status, error = _verify_docker_containers_in_each_node(fxHermesRunSettings, node_info_list)
    log.info(status, error)
    assert status, "Unable to verify all the docker containers in each node, refer to error - {}".format(error)

    # run daml sanity
    log.info("Running daml sanity tests to validate health of the deployed blockchain")
    daml_sanity_status = helper.run_daml_sanity(ledger_api_hosts=client_nodes, results_dir=castorOutputDir,
                                                run_all_tests=False)
    assert daml_sanity_status, "Daml Sanity test did not pass, deployment is failed"

    # validate teardown
    assert castor_teardown, "Teardown failed"


@describe("Deploy 4 node daml blockchain and verify blockchain health")
@pytest.mark.smoke
@pytest.mark.parametrize('upCastorDockerCompose',
                         [(4, 1, _DEPLOY_DESC_FILENAME_VALUE, _INFRA_DESC_FILENAME_VALUE, False)],
                         indirect=True)
def test_castor_4_node_deployment(upPrereqsDocker, upCastorDockerCompose, fxHermesRunSettings, castor_teardown):
    """
    The startup/shutdown of the Castor containers is verified by the upPrereqsDocker, upCastorDockerCompose.
    Once the provisioning and config service is up then the castor deployment is initiated
    If that fails, there is no point in continuing onward with this test. The test verifies the following:
    1. the output file contains all the information about clients and committers specified in the descriptors files
    2. ssh connectivity on all the nodes
    3. Docker containers in each node
    4. Check VM Sizing
    5. After the above 3 are verified then the Daml sanity is executed for the participant nodes
    """
    # Check if deployment started
    assert upCastorDockerCompose, "Deployment did not start"

    # start the test
    num_of_nodes = 5
    castorOutputDir = fxHermesRunSettings["hermesTestLogDir"]
    matchingNodeLogins, castorOutputFile = validate_castor_output_msg(castorOutputDir, num_of_nodes)

    # validate success message in the output file
    assert matchingNodeLogins == num_of_nodes, "%s expected lines: %s, found: %s" % (
            _CASTOR_OUTPUT_NODE_LOGIN_PATTERN, num_of_nodes, matchingNodeLogins)

    # -------------post deployment validations -----------------
    # get all nodes and client node IPs from output file
    all_nodes, client_nodes = _get_all_nodes(castorOutputFile)

    # create a dictionary to segregate all the nodes by Replica and Client
    node_info_list = _get_node_info_list(all_nodes, client_nodes)

    # validate node size
    for node_ip, node_type in node_info_list.items():
        vm_size = get_deployed_node_size(node_ip)
        if "committer" in node_type:
            validate_replica_node_size(_DEPLOY_DESC_FILENAME_VALUE, vm_size, node_ip)
        if "client" in node_type:
            validate_client_node_size(_DEPLOY_DESC_FILENAME_VALUE, vm_size, node_ip)

    # verify docker containers on each node
    status, error = _verify_docker_containers_in_each_node(fxHermesRunSettings, node_info_list)
    log.info(status, error)
    assert status, "Unable to verify all the docker containers in each node, refer to error - {}".format(error)

    # run daml sanity
    log.info("Running daml sanity tests to validate health of the deployed blockchain")
    daml_sanity_status = helper.run_daml_sanity(ledger_api_hosts=client_nodes, results_dir=castorOutputDir,
                                                run_all_tests=False)
    assert daml_sanity_status, "Daml Sanity test did not pass, deployment is failed"

    # validate teardown
    assert castor_teardown, "Teardown failed"


@describe("Test multiple zone deployment")
@pytest.mark.smoke
@pytest.mark.parametrize('upCastorDockerCompose', [(4, 1, "test02_deployment_descriptor_multiple_zone.json",
                                                    _INFRA_DESC_FILENAME_VALUE, True)],
                         indirect=True)
def test_multiple_zone_deployment(upPrereqsDocker, upCastorDockerCompose, fxHermesRunSettings, castor_teardown):
    """
    The startup/shutdown of the Castor containers is verified by the upPrereqsDocker, upCastorDockerCompose.
    Once the provisioning and config service is up then the castor deployment is initiated
    If that fails, there is no point in continuing onward with this test. The test verifies the following:
    1. the output file contains all the information about clients and committers specified in the descriptors files
    2. ssh connectivity on all the nodes
    3. Docker containers in each node
    4. After the above 3 are verified then the Daml sanity is executed for the participant nodes
    """
    # check if deployment started
    assert upCastorDockerCompose, "Deployment did not start"

    # start the test
    num_of_nodes = 5
    log.info("Starting test test_castor_deployment")
    castorOutputDir = fxHermesRunSettings["hermesTestLogDir"]
    matchingNodeLogins, castorOutputFile = validate_castor_output_msg(castorOutputDir, num_of_nodes)

    # validate success message in the output file
    assert matchingNodeLogins == num_of_nodes, "%s expected lines: %s, found: %s" % (
            _CASTOR_OUTPUT_NODE_LOGIN_PATTERN, num_of_nodes, matchingNodeLogins)

    # -------------post deployment validations -----------------
    # get all nodes and client node IPs from output file
    all_nodes, client_nodes = _get_all_nodes(castorOutputFile)

    # create a dictionary to segregate all the nodes by Replica and Client
    node_info_list = _get_node_info_list(all_nodes, client_nodes)

    # verify docker containers on each node
    status, error = _verify_docker_containers_in_each_node(fxHermesRunSettings, node_info_list)
    log.info(status, error)
    assert status, "Unable to verify all the docker containers in each node, refer to error - {}".format(error)

    # run daml sanity
    log.info("Running daml sanity tests to validate health of the deployed blockchain")
    daml_sanity_status = helper.run_daml_sanity(ledger_api_hosts=client_nodes, results_dir=castorOutputDir,
                                                run_all_tests=False)
    assert daml_sanity_status, "Daml Sanity test did not pass, deployment is failed"

    # validate teardown
    assert castor_teardown, "Teardown failed"


@describe("Deployment on invalid host")
@pytest.mark.smoke
@pytest.mark.parametrize('upCastorDockerCompose', [(4, 1, "test03_deployment_descriptor_invalid_host.json",
                                                    "test03_infrastructure_descriptor_invalid_host.json", False)],
                         indirect=True)
def test_deployment_invalid_host(upPrereqsDocker, upCastorDockerCompose, fxHermesRunSettings):
    """
    This is a negative test. We don't need teardown here
    Objective - Validate if castor startup is failed because of invalid host end point
    """
    assert upCastorDockerCompose == False, "Somehow Castor docker compose started the deployment. " \
                                           "It should have been failed"


@describe("Verify Max VM Sizing")
@pytest.mark.smoke
@pytest.mark.parametrize('upCastorDockerCompose',
                         [(4, 1, "test04_deployment_descriptor_vm_sizing.json", _INFRA_DESC_FILENAME_VALUE, False)],
                         indirect=True)
def test_castor_deployment_max_vm_sizing(upPrereqsDocker, upCastorDockerCompose, fxHermesRunSettings, castor_teardown):
    """
    The startup/shutdown of the Castor containers is verified by the upPrereqsDocker, upCastorDockerCompose.
    Once the provisioning and config service is up then the castor deployment is initiated
    If that fails, there is no point in continuing onward with this test. The test verifies the following:
    1. the output file contains all the information about clients and committers specified in the descriptors files
    2. ssh to all the VMs and check if the node size is configured as per deployment descriptor
    """
    # Check if deployment started
    assert upCastorDockerCompose, "Deployment did not start"

    # start the test
    num_of_nodes = 5
    castorOutputDir = fxHermesRunSettings["hermesTestLogDir"]
    matchingNodeLogins, castorOutputFile = validate_castor_output_msg(castorOutputDir, num_of_nodes)

    # validate success message in the output file
    assert matchingNodeLogins == num_of_nodes, "%s expected lines: %s, found: %s" % (
            _CASTOR_OUTPUT_NODE_LOGIN_PATTERN, num_of_nodes, matchingNodeLogins)

    # -------------post deployment validations -----------------
    # get all nodes and client node IPs from output file
    all_nodes, client_nodes = _get_all_nodes(castorOutputFile)

    # create a dictionary to segregate all the nodes by Replica and Client
    node_info_list = _get_node_info_list(all_nodes, client_nodes)

    # validate node size
    for node_ip, node_type in node_info_list.items():
        vm_size = get_deployed_node_size(node_ip)
        if "committer" in node_type:
            validate_replica_node_size("test04_deployment_descriptor_vm_sizing.json", vm_size, node_ip)
        if "client" in node_type:
            validate_client_node_size("test04_deployment_descriptor_vm_sizing.json", vm_size, node_ip)