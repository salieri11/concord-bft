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
_NUM_NODES = 9
_CASTOR_OUTPUT_CLIENT_NODE_PATTERN = "CLIENT_GROUP_ID"
_COMPOSE_CONFIG_SERVICE_LOG = "docker-compose-config-service.log"


def match_pattern(lines, pattern):
    matches = 0
    for line in lines:
        if pattern.search(line):
            matches += 1
    return matches


def _populateDescriptorFiles(fxHermesRunSettings):
    """
    This function is to populate infrastructure descriptor file from hermes configs
    """

    default_zone_path = fxHermesRunSettings['hermesZoneConfig']['zones']['onprem'][0]

    url = default_zone_path['api']['address']
    userName = default_zone_path['api']['credential']['passwordCredential']['username']
    password = default_zone_path['api']['credential']['passwordCredential']['password']
    resource = default_zone_path['vsphere']['resourcePool']
    storage = default_zone_path['vsphere']['datastore']
    folder = default_zone_path['vsphere']['folder']
    networkName = default_zone_path['vsphere']['network']['name']
    gateway = default_zone_path['vsphere']['network']['gateway']
    subnet = default_zone_path['vsphere']['network']['subnet']
    nameServers = default_zone_path['vsphere']['network']['nameServers']

    # Read the infra descriptor file
    infraFilePath = os.path.join(_ORCHESTRATOR_DESCRIPTORS_DIR_VALUE, _INFRA_DESC_FILENAME_VALUE)

    with open(infraFilePath, "r") as infraFile:
        data = infraFile.read()

    infraDescriptor = json.loads(data)

    infraDescriptor['zones'][0]['vCenter']['url'] = url
    infraDescriptor['zones'][0]['vCenter']['userName'] = userName
    infraDescriptor['zones'][0]['vCenter']['password'] = password
    infraDescriptor['zones'][0]['vCenter']['resourcePool'] = resource
    infraDescriptor['zones'][0]['vCenter']['storage'] = storage
    infraDescriptor['zones'][0]['vCenter']['folder'] = folder

    infraDescriptor['zones'][0]['network']['name'] = networkName
    infraDescriptor['zones'][0]['network']['gateway'] = gateway
    infraDescriptor['zones'][0]['network']['subnet'] = subnet
    infraDescriptor['zones'][0]['network']['nameServers'] = nameServers

    # Write back the updated model
    with open(infraFilePath, "w") as infraFile:
        json.dump(infraDescriptor, infraFile, indent=4)

    infraJson = json.dumps(infraDescriptor, indent=4)
    log.info("Infra descriptor file was updated with: ")
    log.info(infraJson)


def downCastorDockerCompose(dockerComposeFiles):
    log.info("Stopping docker containers for: %s" % dockerComposeFiles)


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
    _populateDescriptorFiles(fxHermesRunSettings)

    dockerComposeFiles = fxHermesRunSettings['hermesCmdlineArgs'].dockerComposeFile
    castorOutputDir = fxHermesRunSettings["hermesTestLogDir"]
    log.info(castorOutputDir)
    os.makedirs(castorOutputDir, exist_ok=True)
    prereqsComposeFile = None
    for dcf in dockerComposeFiles:
        if "prereqs.yml" in dcf:
            prereqsComposeFile = dcf

    atexit.register(downCastorDockerCompose, prereqsComposeFile)

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


@pytest.fixture
def upCastorDockerCompose(fxHermesRunSettings, product):
    """
    Launch docker-compose-castor.yml, and wait for it to finish.
    The whole test is predicated on the successful launch and completion of the castor docker process:
    It is a self-contained product that reads the descriptors, calls the provisioning service to deploy
    the blockchain, and exits.

    :return success or failure of launching the compose file:
    """
    _populateDescriptorFiles(fxHermesRunSettings)

    dockerComposeFiles = fxHermesRunSettings['hermesCmdlineArgs'].dockerComposeFile
    castorOutputDir = fxHermesRunSettings["hermesTestLogDir"]
    os.makedirs(castorOutputDir, exist_ok=True)
    castorComposeFile = None
    for dcf in dockerComposeFiles:
        if "docker-compose-castor.yml" in dcf:
            castorComposeFile = dcf

    atexit.register(downCastorDockerCompose, castorComposeFile)

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

    newEnv[_INFRA_DESC_FILENAME_KEY] = _INFRA_DESC_FILENAME_VALUE
    newEnv[_DEPLOY_DESC_FILENAME_KEY] = _DEPLOY_DESC_FILENAME_VALUE

    castorComposeOutputLogFilePath = os.path.join(castorOutputDir, _COMPOSE_CASTOR_LOG)
    deployment_success = False
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

    assert deployment_success, "Castor docker-compose startup failed, output does not contain SUCCESS"
    log.info("docker-compose-castor.yml launched")


@describe("Deploy 7 node daml blockchain and verify blockchain health")
def test_castor_deployment(upPrereqsDocker, upCastorDockerCompose, fxHermesRunSettings):
    """
    The startup/shutdown of the Castor containers is verified by the upPrereqsDocker, upCastorDockerCompose fixture.
    If that fails, there is no point in continuing onward with this test. The test verifies the following:
    1. the output file contains all the information about clients and committers specified in the descriptors files
    2. ssh connectivity on all the nodes
    3. Docker containers in each node
    4. After the above 3 are verified then the Daml sanity is executed for the participant nodes
    """
    log.info("Starting test test_castor_deployment")
    castorOutputDir = fxHermesRunSettings["hermesTestLogDir"]
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
        assert matchingNodeLogins == _NUM_NODES, "%s expected lines: %s, found: %s" % (
            _CASTOR_OUTPUT_NODE_LOGIN_PATTERN, _NUM_NODES, matchingNodeLogins)

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
                                                run_all_tests=True)
    assert daml_sanity_status, "Daml Sanity test did not pass, deployment is failed"


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