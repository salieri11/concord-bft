# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential

# Castor tests are run after the docker-compose-castor.yml file is launched. That
# docker-compose file launches the deployment. This test further validates that
# the deployment went through successfully.
#
# main.py
#         --dockerComposeFiles=../docker/docker-compose-orchestrator.yml,
#                              ../docker/docker-compose-orchestrator-prereqs.yml
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
from util import cert
import tempfile
import sys
import io
import time
from datetime import datetime, timezone
import string
import grpc
import ast
from lib.persephone.rpc_helper import RPCHelper
from lib.persephone.provisioning_service_new_helper import ProvisioningServiceNewRPCHelper
from lib.persephone.vmware.blockchain.deployment.v1 import core_pb2
from lib.persephone.vmware.blockchain.deployment.v1 import orchestration_pb2
from lib.persephone.vmware.blockchain.deployment.v1 import provisioning_service_new_pb2 as ps_apis

#import hermes_info


log = util.hermes_logging.getMainLogger()

# Read by the fxProduct fixture.
# Set this to no-op so the framework does not wait for Provisioning service to
# start up. It will be started by this test itself.
_productType = helper.TYPE_NO_VERIFY

# NOTE: These need to match the docker-compose-orchestrator.yml file
_ORCHESTRATOR_DESCRIPTORS_DIR_KEY = "ORCHESTRATOR_DESCRIPTORS_DIR"
_ORCHESTRATOR_DESCRIPTORS_DIR_VALUE = "../docker/config-castor/descriptors"
_INFRA_DESC_FILENAME_KEY = "INFRA_DESC_FILENAME"
_INFRA_DESC_FILENAME_VALUE = "test01_infrastructure_descriptor.json"
_DEPLOY_DESC_FILENAME_KEY = "DEPLOY_DESC_FILENAME"
_DEPLOY_DESC_FILENAME_VALUE = "test01_deployment_descriptor.json"
_ORCHESTRATOR_OUTPUT_DIR_KEY = "ORCHESTRATOR_OUTPUT_DIR"
_CONFIG_SERVICE_IP = "CONFIG_SERVICE_IP"

_COMPOSE_CASTOR_LOG = "docker-compose-orchestrator.log"
_DEPLOYMENT_SUCCESS_MSG = "Deployment completed with status: SUCCESS"
_CASTOR_OUTPUT_SUCCESS_MSG_PATTERN = "Deployment finished at .* with status SUCCESS"
_CASTOR_OUTPUT_NODE_LOGIN_PATTERN = "NODE_LOGIN"
# These must match the infra and deployment descriptor files
_CONSORTIUM_NAME = "hermes-castor-consortium"
_CASTOR_OUTPUT_CLIENT_NODE_PATTERN = "CLIENT_GROUP_ID"
_CASTOR_OUTPUT_RO_PATTERN = "OBJECT_STORE_BUCKET_NAME"
_COMPOSE_CONFIG_SERVICE_LOG = "docker-compose-config-service.log"
_CONCORD_TYPE = "DAML"

"""
All fixtures
"""


# @pytest.fixture(scope="module")
# def product(hermes_info):
#     cmdLineArgs = hermes_info['hermesCmdlineArgs']
#     userConfig = hermes_info['hermesUserConfig']
#     product = Product(cmdLineArgs, userConfig)
#     return product


@pytest.fixture(scope="module")
def upPrereqsDocker(hermes_info):
    """
    Fixture to launch the product
    """
    if hermes_info["hermesCmdlineArgs"].deploymentTool != "Castor":
        return
    launch_castor_product(hermes_info)


def launch_castor_product(hermes_info):
    '''
    Launch docker-compose-orchestrator-prereqs.yml to start the product
    '''
    cmdLineArgs = hermes_info['hermesCmdlineArgs']
    userConfig = hermes_info['hermesUserConfig']
    product = Product(cmdLineArgs, userConfig)

    dockerComposeFiles = hermes_info['hermesCmdlineArgs'].dockerComposeFile
    log.info(dockerComposeFiles)
    castorOutputDir = hermes_info["hermesTestLogDir"]
    os.chmod(castorOutputDir, 0o777)
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
    log.info("docker-compose-orchestrator-prereqs.yml launched")

    # Take the backup of existing deployment and infrastructure descriptor
    with open(os.path.join(_ORCHESTRATOR_DESCRIPTORS_DIR_VALUE, _INFRA_DESC_FILENAME_VALUE)) as from_file, \
            open(os.path.join(_ORCHESTRATOR_DESCRIPTORS_DIR_VALUE, "infra_backup.json"), "w+") as to:
        to.write(from_file.read())


@pytest.fixture(scope="module")
def upCastorDockerCompose(request, hermes_info):
    """
    Launch docker-compose-castor.yml, and wait for it to finish.
    The whole test is predicated on the successful launch and completion of the castor docker process:
    It is a self-contained product that reads the descriptors, calls the provisioning service to deploy
    the blockchain, and exits.

    :return success or failure of launching the compose file:
    """
    if hermes_info["hermesCmdlineArgs"].deploymentTool != "Castor":
        return False
    create_castor_blockchain(request, hermes_info)


def create_castor_blockchain(request, hermes_info):
    cmdLineArgs = hermes_info['hermesCmdlineArgs']
    userConfig = hermes_info['hermesUserConfig']
    product = Product(cmdLineArgs, userConfig)

    is_request_param = True
    try:
        request_param = getattr(request, "param")
    except AttributeError:
        is_request_param = False
    if is_request_param:
        num_replicas, num_clients, deployment_descriptor, infra_descriptor, is_multiple_zone = request.param
        if deployment_descriptor is not _DEPLOY_DESC_FILENAME_VALUE and infra_descriptor is not _INFRA_DESC_FILENAME_VALUE:
            log.info("Not updating infrastructure and deployment descriptor. customized ones are {} and {}".
                    format(infra_descriptor, deployment_descriptor))
        else:
            add_remove_nodes_from_deploy_descriptor(num_replicas, num_clients)
            _populateInfraDescriptorFile(hermes_info, is_multiple_zone)
    else:
        infra_descriptor = _INFRA_DESC_FILENAME_VALUE
        deployment_descriptor = _DEPLOY_DESC_FILENAME_VALUE
        _populateInfraDescriptorFile(hermes_info)
        prepare_deployment_descriptor_from_cmdline(hermes_info)
    
    dockerComposeFiles = hermes_info['hermesCmdlineArgs'].dockerComposeFile
    castorOutputDir = hermes_info["hermesTestLogDir"]
    os.makedirs(castorOutputDir, exist_ok=True)
    castorComposeFile = None
    for dcf in dockerComposeFiles:
        if "docker-compose-orchestrator.yml" in dcf:
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

    assert m <= 20, "Deployment took more than 20 minutes"
    log.info("docker-compose-orchestrator.yml launched")
    return deployment_success


@pytest.fixture(scope="module")
def castor_teardown(hermes_info, request):
    """
    This is a teardown function which will deprovision the deployed blockchain and revert the infra and deployment descriptor
    """
    yield True
    log.info("Starting teardown")
    if request.session.testsfailed:
        collect_concord_support_bundle(hermes_info)
    deprovision_blockchain(hermes_info)

    # Revert modified descriptors
    with open(os.path.join(_ORCHESTRATOR_DESCRIPTORS_DIR_VALUE, "infra_backup.json")) as from_file, \
            open(os.path.join(_ORCHESTRATOR_DESCRIPTORS_DIR_VALUE, _INFRA_DESC_FILENAME_VALUE), "w+") as to:
        to.write(from_file.read())

@pytest.fixture(scope="module")
def run_on_failure(request, hermes_info):
    yield True
    if request.session.testsfailed:
        collect_concord_support_bundle(hermes_info)
        deprovision_blockchain(hermes_info)


"""
Generic Functions
"""


def match_pattern(lines, pattern):
    matches = 0
    for line in lines:
        if pattern.search(line):
            matches += 1
    return matches


def prepare_deployment_descriptor_from_cmdline(hermes_info):
    """
    This test is only for on demand castor deployment
    """

    blockchain_type = "DAML"
    num_replicas = int(hermes_info["hermesCmdlineArgs"].numReplicas)
    num_clients = int(hermes_info["hermesCmdlineArgs"].numParticipants)
    client_memory = 16 if not hermes_info["hermesCmdlineArgs"].clientMemory else int(hermes_info["hermesCmdlineArgs"].clientMemory)
    client_cpu = 2 if not hermes_info["hermesCmdlineArgs"].clientCpu else int(hermes_info["hermesCmdlineArgs"].clientCpu)
    client_storage = 64 if not hermes_info["hermesCmdlineArgs"].clientStorage else int(hermes_info["hermesCmdlineArgs"].clientStorage)
    replica_memory = 16 if not hermes_info["hermesCmdlineArgs"].replicaMemory else int(hermes_info["hermesCmdlineArgs"].replicaMemory)
    replica_cpu = 2 if not hermes_info["hermesCmdlineArgs"].replicaCpu else int(hermes_info["hermesCmdlineArgs"].replicaCpu)
    replica_storage = 64 if not hermes_info["hermesCmdlineArgs"].replicaStorage else int(hermes_info["hermesCmdlineArgs"].replicaStorage)

    # get ro replica details
    ro_replica = hermes_info["hermesCmdlineArgs"].readOnlyReplicas

    # prepare descriptor files based on the command line args
    log.info("Updating descriptor")
    add_remove_nodes_from_deploy_descriptor(num_replicas, num_clients)

    with open(os.path.join(_ORCHESTRATOR_DESCRIPTORS_DIR_VALUE, _DEPLOY_DESC_FILENAME_VALUE)) as deploy_desc:
            data = json.load(deploy_desc)

    # update blockchain type
    data["blockchain"]["blockchainType"] = blockchain_type
    
    if "replicaNodeSpec" not in data:
        data["replicaNodeSpec"] = {}
    
    data["replicaNodeSpec"]["cpuCount"] = replica_cpu
    data["replicaNodeSpec"]["memoryGb"] = replica_memory
    data["replicaNodeSpec"]["diskSizeGb"] = replica_storage

    if "clientNodeSpec" not in data:
        data["clientNodeSpec"] = {}
    
    data["clientNodeSpec"]["cpuCount"] = client_cpu
    data["clientNodeSpec"]["memoryGb"] = client_memory
    data["clientNodeSpec"]["diskSizeGb"] = client_storage

    if ro_replica:
        ro_replicas = get_ro_replica_nodes(ro_replica)
        data["readonlyReplicas"] = add_ro_replica_nodes_in_deployment_descriptor(ro_replicas)
        
        ro_replica_memory = 16 if not hermes_info["hermesCmdlineArgs"].roReplicaMemory else int(hermes_info["hermesCmdlineArgs"].roReplicaMemory)
        ro_replica_cpu = 2 if not hermes_info["hermesCmdlineArgs"].roReplicaCpu else int(hermes_info["hermesCmdlineArgs"].roReplicaCpu)
        ro_replica_storage = 64 if not hermes_info["hermesCmdlineArgs"].roReplicaStorage else int(hermes_info["hermesCmdlineArgs"].roReplicaStorage)
        data["readonlyReplicaNodeSpec"] = {}
        data["readonlyReplicaNodeSpec"]["cpuCount"] = ro_replica_cpu
        data["readonlyReplicaNodeSpec"]["memoryGb"] = ro_replica_memory
        data["readonlyReplicaNodeSpec"]["diskSizeGb"] = ro_replica_storage

    tls_enabled = hermes_info["hermesCmdlineArgs"].tlsEnabledClient
    if tls_enabled:
        for i in range(num_clients):
            tls_crt = cert.tlsCreateLoadSignedCrt('server')
            server_crt = tls_crt.serverCrt
            server_key = tls_crt.serverKey
            root_crt = tls_crt.rootCaCrt
            data["clients"][i]["tlsLedgerData"] = {}
            data["clients"][i]["tlsLedgerData"]["crt"] = server_crt
            data["clients"][i]["tlsLedgerData"]["pem"] = server_key
            data["clients"][i]["tlsLedgerData"]["cacrt"] = root_crt
            data["clients"][i]["tlsLedgerData"]["clientAuth"] = "REQUIRE"


        
    with open(os.path.join(_ORCHESTRATOR_DESCRIPTORS_DIR_VALUE, _DEPLOY_DESC_FILENAME_VALUE), 'w') as w_deploy_desc:
            json.dump(data, w_deploy_desc, indent=4) 
    log.info("updated deployment descriptor")


def add_remove_nodes_from_deploy_descriptor(num_replicas, num_clients):
    """
    This function is to modify deployment descriptor
    :param num_replicas: number of replicas to be added
    :param num_clients: number of participants to be added
    :return: True/False
    """
    with open(os.path.join(_ORCHESTRATOR_DESCRIPTORS_DIR_VALUE, _DEPLOY_DESC_FILENAME_VALUE)) as deploy_desc:
            data = json.load(deploy_desc)

    # update zone name from infrastructure descriptor
    with open(os.path.join(_ORCHESTRATOR_DESCRIPTORS_DIR_VALUE, _INFRA_DESC_FILENAME_VALUE)) as infraFile:
        infraDescriptor = json.load(infraFile)
    
    get_zones = [zone["name"] for zone in infraDescriptor["zones"]]
    node = {}
    counter = 0
    data["replicas"] = []
    for _ in range(num_replicas):
        node["zoneName"] = get_zones[counter]
        data["replicas"].append(node.copy())
        counter+=1
        if counter>=len(get_zones):
            counter = 0
    data["clients"] = []
    for _ in range(num_clients):
        node["zoneName"] = get_zones[counter]
        data["clients"].append(node.copy())
        counter+=1
        if counter>=len(get_zones):
            counter = 0

    with open(os.path.join(_ORCHESTRATOR_DESCRIPTORS_DIR_VALUE, _DEPLOY_DESC_FILENAME_VALUE), 'w') as w_deploy_desc:
        json.dump(data, w_deploy_desc, indent=4)

    log.info("Deployment descriptor is updated")


def _populateInfraDescriptorFile(hermes_info, is_multiple_zone=False):
    """
    Populate infrastructure descriptor from zone config
    :param hermes_info: fixture to get the zone config
    :param is_multiple_zone: Flag if multiple zone is required
    :return:
    """
    available_zones = list(string.ascii_uppercase)
    log.info(os.path.join(_ORCHESTRATOR_DESCRIPTORS_DIR_VALUE, _INFRA_DESC_FILENAME_VALUE))
    with open(os.path.join(_ORCHESTRATOR_DESCRIPTORS_DIR_VALUE, _INFRA_DESC_FILENAME_VALUE)) as infraFile:
        infraDescriptor = json.load(infraFile)
    default_zone_path = hermes_info['hermesZoneConfig']['zones']['onprem']
    len_default_zone_path = len(default_zone_path)
    log.info("Length of default zone is {}".format(len_default_zone_path))

    #Clone zone
    if len(default_zone_path) > len(infraDescriptor['zones']):
            no_of_zones_req = len(default_zone_path) - len(infraDescriptor['zones'])
            log.info("Number of zone required {}".format(no_of_zones_req))
            clone_zones = [infraDescriptor['zones'][0]] * abs(no_of_zones_req)
            infraDescriptor['zones'] = [*infraDescriptor['zones'], *clone_zones]
    with open(os.path.join(_ORCHESTRATOR_DESCRIPTORS_DIR_VALUE, _INFRA_DESC_FILENAME_VALUE), "w") as infraFile:
        json.dump(infraDescriptor, infraFile, indent=4)

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
        
        wavefront_url = default_zone_path[item]['wavefront']['url']
        wavefront_token = default_zone_path[item]['wavefront']['token']
        log_insight_type = "LOG_INSIGHT"
        log_insight_endpoint = "10.202.69.231"
        log_insight_port = "9543"
        log_insight_username = "admin"
        log_insight_password = "VMw@re@2o2o"
        log_insight_agent_id = "0"
        

        # update infra keys

        dcr = _get_docker_container_registry_settings()
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
        
        infraDescriptor['zones'][item]['containerRegistry'] = {}
        infraDescriptor['zones'][item]['containerRegistry']['url'] = dcr['container_address']
        infraDescriptor['zones'][item]['containerRegistry']['userName'] = dcr['container_user']
        infraDescriptor['zones'][item]['containerRegistry']['password'] = dcr['container_pwd']

        enable_notary = hermes_info["hermesCmdlineArgs"].enableNotaryServer
        if enable_notary is True:
            # get notary server from zone config
            notary_server_url = default_zone_path[item]['notaryServer']['url']
            notary_server_tlsdata = default_zone_path[item]['notaryServer']['tlsCertificateData']

            # update in infra descriptor
            infraDescriptor['zones'][item]['notaryServer'] = {}
            infraDescriptor['zones'][item]['notaryServer']["url"] = notary_server_url
            infraDescriptor['zones'][item]['notaryServer']["tlsCertificateData"] = notary_server_tlsdata

        infraDescriptor['zones'][item]['wavefront'] = {}
        infraDescriptor['zones'][item]['wavefront']["url"] = wavefront_url
        infraDescriptor['zones'][item]['wavefront']["token"] = wavefront_token
        infraDescriptor['zones'][item]["logManagement"] = []
        logmanagement_dict = {}
        logmanagement_dict["type"] = log_insight_type
        logmanagement_dict["address"] = log_insight_endpoint
        logmanagement_dict["port"] = log_insight_port
        logmanagement_dict["userName"] = log_insight_username
        logmanagement_dict["password"] = log_insight_password
        logmanagement_dict["logInsightAgentId"] = log_insight_agent_id
        infraDescriptor['zones'][item]["logManagement"].append(logmanagement_dict)

   
    #Pull metrics endpoint
    pull_metrics_info = hermes_info["hermesCmdlineArgs"].pullMetricsInfo
    if pull_metrics_info:
        infraDescriptor['zones'][item]["pullMetricsEndpoint"] = {}
        infraDescriptor['zones'][item]["pullMetricsEndpoint"] = ast.literal_eval(get_pull_metrics_details(pull_metrics_info))

    # if deployment properties are defined
    deployment_properties = hermes_info["hermesCmdlineArgs"].propertiesString
    if deployment_properties:
        infraDescriptor["organization"]["advancedFeatures"] = helper.get_deployment_properties(deployment_properties)

    #enable bft client
    enable_bft_client = hermes_info["hermesCmdlineArgs"].enableBftClient
    if enable_bft_client:
        infraDescriptor["organization"]["enableBftClient"] = enable_bft_client

    #generate daml db password flag
    generate_daml_db_password = hermes_info["hermesCmdlineArgs"].generateDamlDbPassword
    if generate_daml_db_password:
        infraDescriptor["organization"]["generateDamlDbPassword"] = generate_daml_db_password

    
    with open(os.path.join(_ORCHESTRATOR_DESCRIPTORS_DIR_VALUE, _INFRA_DESC_FILENAME_VALUE), "w") as infraFile:
        json.dump(infraDescriptor, infraFile, indent=4)

    log.info("Infrastructure descriptor is updated")
    log.info(infraFile)


def _get_docker_container_registry_settings():
    # We need the docker information.  Currently, we put this in the Persephone properties
    # file when we are testing.  Let's keep that as the source of truth for now.
    with open(util.product.persephoneConfigFile, "r") as f:
        for line in f.readlines():
            if line.startswith("provisioning.container.registry.address="):
                container_address = line.split("=")[1].strip()
                log.info("docker container registry address: %s" % container_address)
            elif line.startswith("provisioning.container.registry.username="):
                container_user = line.split("=")[1].strip()
                log.info("docker container registry user: %s" % container_user)
            elif line.startswith("provisioning.container.registry.password="):
                container_pwd = line.split("=")[1].strip()
                log.info("docker container registry key exists, of length: %s" % len(container_pwd))

    if container_pwd.startswith("<") and container_pwd.endswith(">"):
        raise Exception("Ensure that usernames, passwords, etc... are valid in " \
                        "{}".format(util.product.persephoneConfigFile))

    return {
        'container_address': container_address,
        'container_user': container_user,
        'container_pwd': container_pwd
    }


def downCastorDockerCompose(dockerComposeFiles):
    log.info("Stopping docker containers for: %s" % dockerComposeFiles)


def _get_all_nodes(castor_output_file):
    """
    Get all the nodes information from Castor output file
    :param castor_output_file:
    :return: list of all nodes and IPs of client nodes
    """
    all_nodes = []
    client_node_values = []
    ro_node_values = []
    cof = open(castor_output_file)
    lines = cof.readlines()[3:]
    for line in lines:
        if _CASTOR_OUTPUT_CLIENT_NODE_PATTERN in line:
            client_node_login_pattern = re.search(
                r'(key: )([A-Z])(\w+), (value: )(.*$)', line)
            client_node_values.append(client_node_login_pattern.group(5))

        if _CASTOR_OUTPUT_RO_PATTERN in line:
            ro_node_login_pattern = re.search(r'(Node Id: )(.*$)', line)
            ro_node_values.append(ro_node_login_pattern.group(2).split(',')[0])

    # based on the node info get the client node IP
    client_node_ips = []
    ro_node_ips = []
    ip_pattern = re.compile(r'[0-9]+(?:\.[0-9]+){3}')
    
    for node in client_node_values:
        for line in lines:
            if node in line and re.search(ip_pattern, line) and "PRIVATE_IP" in line:
                client_node_ips.append(ip_pattern.findall(line)[0])

    for node in ro_node_values:
        for line in lines:
            if node in line and re.search(ip_pattern, line) and "PRIVATE_IP" in line:
                ro_node_ips.append(ip_pattern.findall(line)[0])

    for line in lines:
        if "PRIVATE_IP" in line:
            all_nodes.append(ip_pattern.findall(line)[0])

    cof.close()
    return all_nodes, client_node_ips, ro_node_ips


def get_consortium_id(castor_output_file):
    with open(castor_output_file) as cof:
        lines = cof.readlines()

    for line in lines:
        if "Consortium Id:" in line:
            consortium_id_pattern = re.search(r'(Consortium Id: )(.*$)', line)
            consortium_id = consortium_id_pattern.group(2)
    
    return consortium_id


def get_blockchain_id(castor_output_file):
    with open(castor_output_file) as cof:
        lines = cof.readlines()

    for line in lines:
        if "Blockchain Id:" in line:
            blockchain_id_pattern = re.search(r'(Blockchain Id: )(.*$)', line)
            blockchain_id = blockchain_id_pattern.group(2)
            break
   
    return blockchain_id


def _get_node_info_list(all_nodes, client_nodes):
    """
    Create dictionary of nodes based on node type
    :param all_nodes: list of all node IPs
    :param client_nodes: list of all client node IPs
    :return:
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
    :param user_config: User config from hermes/resources
    :param node_type: participant/committer
    :return: list of docker containers in each node
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


def _verify_docker_containers_in_each_node(hermes_info, node_info_list, blockchain_id):
    """
    Verify if the necessary docker containers are running on each the nodes
    :param hermes_info: Fixture for Hermes settings
    :param node_info_list: List of NodeInfo
    :return: True/False
    """
    status = False
    userConfig = hermes_info["hermesUserConfig"]

    error_msg = "Error verifying docker containers"
    for node, node_type in node_info_list.items():
        username, password = helper.getNodeCredentials(blockchain_id, node)
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


def validate_castor_output_msg(castorOutputDir):
    """
    Validate Success message in Castor output file
    :param castorOutputDir: Location of the output file to read the file
    :return: number of matching node logins, castor output file
    """
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
        matchingLines = match_pattern(lines, outputSuccessMatchPattern)
        assert matchingLines == 1, "Castor output file does not have success marker: %s" % _CASTOR_OUTPUT_SUCCESS_MSG_PATTERN

        # Match 2 clients and 4 committers
        nodeLoginMatchPattern = re.compile(_CASTOR_OUTPUT_NODE_LOGIN_PATTERN)
        matchingNodeLogins = match_pattern(lines, nodeLoginMatchPattern)

    return matchingNodeLogins, castorOutputFile


def create_resources(site_id, castor_output_file):
    """
    parse the output file to retrieve resource properties and create resource instances
    return the array of Resource
    :param site_id: site id is obtained from get_orchestration_site_ids_and_infos
    :param castor_output_file: Castor output file
    :return: list of resources
    """
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


def get_deployed_node_size(blockchain_id, ip):
    """
    Get the size of the deployed node
    :param ip: IP of the node
    :return: Dictionary of node configuration which contains - memory, storage and CPU count
    """

    # get default username and password of the node from user config
    username, password = helper.getNodeCredentials(blockchain_id, ip)

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
    deployed_vm_size["diskSizeGb"] = str(float(ssh_output[index].split()[2]) * 1024) \
        if "TiB" in ssh_output[index].split()[3] else ssh_output[index].split()[2]

    return deployed_vm_size


def validate_node_size(deployment_descriptor, deployed_vm_size, ip, node):
    """
   Validate deployed client node configuration with defined node configuration in deployment descriptor
   :param deployment_descriptor: deployment descriptor file path
   :param deployed_vm_size: dictionary of configuration from deployed VM
   :param ip: IP of the VM
   """
    deployment_descriptor_path = os.path.join(_ORCHESTRATOR_DESCRIPTORS_DIR_VALUE, deployment_descriptor)
    with open(deployment_descriptor_path) as ds:
        deployDescriptor = json.load(ds)

    assert deployDescriptor[f"{node}NodeSpec"]["cpuCount"] == int(
        deployed_vm_size["cpuCount"]), "CPU count of {} node IP {} is {} which is not matching with client node " \
                                       "CPU defined {}".format(
        node, ip, deployed_vm_size["cpuCount"], deployDescriptor[f"{node}NodeSpec"]["cpuCount"])

    assert (float(deployed_vm_size["memoryGb"]) / (1000 * 1000)) >= float(
        deployDescriptor[f"{node}NodeSpec"]["memoryGb"]) >= (float(deployed_vm_size["memoryGb"]) / (1024 * 1024)), \
        "Memory of client node IP {} is not matching with client node memory defined {}".format(
        ip, deployDescriptor[f"{node}NodeSpec"]["memoryGb"])

    assert deployDescriptor[f"{node}NodeSpec"]["diskSizeGb"] == float(
        deployed_vm_size["diskSizeGb"]), "Disk size of {} node IP {} is {} which is not matching with client " \
                                         "node disk size defined {}".format(
        node, ip, deployed_vm_size["diskSizeGb"], deployDescriptor[f"{node}NodeSpec"]["diskSizeGb"])


def deprovision_blockchain(hermes_info):
    """
    De-provisioning the deployment
    """

    session_id = None
    castorOutputDir = hermes_info["hermesTestLogDir"]
    files = os.listdir(castorOutputDir)

    filePatternStr = _CONSORTIUM_NAME + "*"
    outputFileBases = fnmatch.filter(files, filePatternStr)
    if len(outputFileBases) < 1:
        log.info("No output file available. No need to deprovision")
        return

    outputFileBase = outputFileBases[0]
    castorOutputFile = os.path.join(castorOutputDir, outputFileBase)
    with open(castorOutputFile) as cof:
        lines = cof.readlines()
        for line in lines:
            if "Deployment Request Id" in line:
                request_id_pattern = re.search(r'(Id: )(.*$)', line)
                session_id = request_id_pattern.group(2)

    log.info("De-provisioning the deployment using session id - {}".format(session_id))
    cmdlineArgs = hermes_info["hermesCmdlineArgs"]
    cmdlineArgs.cancel_stream = True
    logs_for_stream_all_deploy_events = os.path.join(castorOutputDir, "stream_deployment_events")
    os.makedirs(logs_for_stream_all_deploy_events, exist_ok=True)
    cmdlineArgs.fileRoot = logs_for_stream_all_deploy_events

    ps_helper = ProvisioningServiceNewRPCHelper(cmdlineArgs)

    zone_type = "onprem"
    zone_config = hermes_info["hermesZoneConfig"]

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


def collect_concord_support_bundle(hermes_info):
    """
    Collect concord support bundle
    """
    try:
        castorOutputDir = hermes_info["hermesTestLogDir"]
        castor_output_file = validate_castor_output_msg(castorOutputDir)[1]
        all_nodes = _get_all_nodes(castor_output_file)[0]
        helper.create_concord_support_bundle(all_nodes, _CONCORD_TYPE.lower(), castorOutputDir)
    except FileNotFoundError:
        log.info("No output file found to collect support bundle. Skipping.........")


def get_ro_replica_nodes(ro_replica_info):
    evaluate_string = ast.literal_eval(ro_replica_info)
    json_data = json.dumps(evaluate_string)
    return json_data


def add_ro_replica_nodes_in_deployment_descriptor(ro_replica_info):
    ro_replica_nodes = get_ro_replica_nodes(ro_replica_info)

    with open(os.path.join(_ORCHESTRATOR_DESCRIPTORS_DIR_VALUE, _INFRA_DESC_FILENAME_VALUE)) as infraFile:
            infraDescriptor = json.load(infraFile)
        
    get_zones = [zone["name"] for zone in infraDescriptor["zones"]]
        
    ro_replica = []
    ro_replica.append(ast.literal_eval(ro_replica_nodes))

    counter = 0
    for i in range(len(ro_replica)):
        ro_replica[i]["zoneName"] = get_zones[counter]
        counter+=1
        if counter>=len(ro_replica_nodes):
            counter = 0

    return ro_replica


def get_notary_server_details(notary_server_info):
    evaluate_string = ast.literal_eval(notary_server_info)
    notary_dict = json.dumps(evaluate_string)
    return notary_dict


def check_ro_replica_enabled(castor_output_file):
    with open(castor_output_file) as cof:
        lines = cof.readlines()
    object_store_status = False
    for line in lines:
        if "OBJECT_STORE_ENABLED" in line:
            object_store_status_pattern = re.search(r'(value: )(.*$)', line)
            object_store_status = object_store_status_pattern.group(2)
    
    return True if object_store_status == "True" else False


def get_ro_replica_details(castor_output_file):
    ro_replicas = []
    with open(castor_output_file) as cof:
        lines = cof.readlines()
    ro_replica_nodes = []
    for line in lines:
        if "OBJECT_STORE_ACCESS_KEY" in line:
            # get the node id
            ans = line.split(',', 1)[0]  # maxsplit = 1; 
            object_store_node_id_pattern = re.search(r'(Node Id: )(.*$)', ans)
            node_id = object_store_node_id_pattern.group(2)
            ro_replica_nodes.append(node_id)

    repica_data = {}
    for node in ro_replica_nodes:
        for line in lines:
            if node in line:
                # split the line and get the last two elements of the line
                a = line.split(',')[-2:]

                # get the key and its value
                key = re.search(r'(key: )(.*$)', a[0]).group(2)
                value = re.search(r'(value: )(.*$)', a[1]).group(2)
                repica_data[key] = value
        ro_replicas.append(repica_data)
    
    return ro_replicas
        

def get_pull_metrics_details(pull_metrics_info):
    evaluate_string = ast.literal_eval(pull_metrics_info)
    pull_metrics_dict = json.dumps(evaluate_string)
    return pull_metrics_dict


def read_castor_output(castorOutputDir):
    log.info("This is castor output dir...... {}".format(castorOutputDir))
    files = os.listdir(castorOutputDir)

    filePatternStr = _CONSORTIUM_NAME + "*"
    outputFileBases = fnmatch.filter(files, filePatternStr)
    assert len(outputFileBases) == 1, "Zero/Multiple matches found for consortium: %s, %s" % (
        _CONSORTIUM_NAME, outputFileBases)

    outputFileBase = outputFileBases[0]
    castorOutputFile = os.path.join(castorOutputDir, outputFileBase)
    log.debug("Processing castor output file: {}".format(castorOutputFile))

    all_nodes, client_nodes, ro_nodes = _get_all_nodes(castorOutputFile)
    log.debug("All nodes {}".format(all_nodes))
    log.debug("Client nodes {}".format(client_nodes))
    log.debug("RO nodes {}".format(ro_nodes))

    replica_nodes = [node for node in all_nodes if (node not in client_nodes and node not in ro_nodes)]
    log.debug("Replica nodes {}".format(replica_nodes))

    replicas = {
        "daml_committer": [],
        "daml_participant": [],
        "daml_ro_committer": []
    }

    for client in client_nodes: replicas["daml_participant"].append({"ip":client})
    for ro_replica in ro_nodes: replicas["daml_ro_committer"].append({"ip":ro_replica})
    for regular_replica in replica_nodes: replicas["daml_committer"].append({"ip":regular_replica})

    log.debug("Replicas structure {}".format(replicas))

    consortium_id = get_consortium_id(castorOutputFile)
    log.debug("Consortium id is {}".format(consortium_id))
    blockchain_id = get_blockchain_id(castorOutputFile)
    log.debug("Blockchain id is {}".format(blockchain_id))
    ro_replicas = get_ro_replica_details(castorOutputFile)
    log.debug("ro replicas are {}".format(ro_replicas))

    return blockchain_id, consortium_id, replicas, client_nodes, ro_replicas, castorOutputDir
