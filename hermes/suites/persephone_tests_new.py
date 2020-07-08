############################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# This test file covers the tests related to Persephone (Deployment Service)
############################################################################
import os
import pytest
import shutil
import subprocess
import time
import traceback
from dataclasses import dataclass
from datetime import datetime, timezone

from lib.persephone.provisioning_service_new_helper import ProvisioningServiceNewRPCHelper
from suites.case import describe
from util import helper, hermes_logging, infra, auth
from util.daml import daml_helper
from util.product import Product

log = hermes_logging.getMainLogger()
session_ids_to_retain = []


@dataclass
class NodeInfo:
    node_id: str
    private_ip: str = ""
    public_ip: str = ""
    username: str = "root"
    password: str = "c0nc0rd"
    node_type: int = 0  # 0 for replica, 1 for client


@dataclass
class DeploymentParams:
    blockchain_type: str
    zone_type: str
    num_replicas: int
    num_clients: int


@pytest.fixture
def ps_setup(request, hermes_settings):
    """
    Sets up provisioning service docker instance
    :param request: Default Pytest request param
    :param hermes_settings: hermes_settings fixture from conftest.py
    :return: None
    """
    log.info("Provisioning Service setup fixture for {}".format(request.node.name))

    # Clear list
    session_ids_to_retain.clear()

    # Parameters
    cmdline_args = hermes_settings["cmdline_args"]
    user_config = hermes_settings["user_config"]
    try:
        # Update provisioning service application properties file
        update_provisioning_service_application_properties(cmdline_args)

        # Launch provisioning service
        product_mode = not cmdline_args.ethereumMode
        no_launch = cmdline_args.noLaunch
        if product_mode and not no_launch:
            product = Product(cmdline_args, user_config)
            product.launchPersephone()
    except Exception as e:
        log.error("Provisioning Service did not start successfully for {}".format(request.node.name))
        log.error(traceback.format_exc())
        raise Exception(e)


@pytest.fixture
def ps_helper(request, hermes_settings, ps_setup):
    """
    Returns instance of ProvisioningServiceNewRPCHelper
    :param request: Default Pytest request param
    :param hermes_settings: hermes_settings fixture from conftest.py
    :param ps_setup: Fixture that launches provisioning service. Don't need the results from it, but need to start it.
    :return: Instance of ProvisioningServiceNewRPCHelper
    """
    log.info("Provisioning Service gRPC Helper fixture for {}".format(request.node.name))
    args = hermes_settings["cmdline_args"]
    try:
        return ProvisioningServiceNewRPCHelper(args)
    except Exception as e:
        log.error("Unable to create Provisioning Service gRPC Helper")
        raise Exception(e)


@pytest.fixture
def file_root(request, hermes_settings):
    """
    Sets the file_root path for a test case
    :param request: Default Pytest request param
    :param hermes_settings: hermes_settings fixture from conftest.py
    :return: file_root directory path
    """
    test_log_dir = os.path.join(hermes_settings["cmdline_args"].resultsDir, "test_logs")
    file_root = os.path.join(test_log_dir, request.node.name)
    log.debug("File root for test: {}".format(file_root))
    return file_root


@pytest.fixture
def teardown(hermes_settings, ps_helper):
    """
    This fixture is used for cleanup activities, which is why it yields a dummy return value.
    It needs to be a parameter in every test case.
    :return: True
    """
    yield True  # yield dummy value to run teardown code below after test case is completed
    log.info("Starting teardown")
    cmdline_args = hermes_settings["cmdline_args"]
    # Reset provisioning service application properties file
    update_provisioning_service_application_properties(cmdline_args, mode="RESET")
    deprovision(hermes_settings, ps_helper)


def create_deployment(ps_helper, deployment_params, zone_config):
    """
    Fixture to run combinations of deployment
    :param ps_helper: Instance of ProvisioningServiceNewRPCHelper
    :param deployment_params: Object of dataclass DeploymentParams
    :param zone_config: Zone config object from zone_config.json
    :return: deployment_session_id, deployment_stream_events
    """
    # Input parameters
    blockchain_type = deployment_params.blockchain_type
    zone_type = validate_blockchain_location(deployment_params.zone_type)
    num_replicas = deployment_params.num_replicas
    num_clients = deployment_params.num_clients
    log.info("Create deployment with following parameters: Blockchain type: {}; Zone type: {}; "
             "Number of Replica nodes: {}; Number of Client nodes: {}"
             .format(blockchain_type.upper(), zone_type.upper(), num_replicas, num_clients))

    # Deploy
    start_time = datetime.now(timezone.utc).astimezone()
    log.info("Deployment start time: {}".format(start_time.strftime(helper.TIME_FMT_TIMEZONE)))
    deployment_response = ps_helper.create_deployment(blockchain_type, zone_type, num_replicas, num_clients,
                                                      zone_config)

    # Validate deployment response
    deployment_session_id = get_deployment_session_id(deployment_response)
    deployment_stream_events = None
    if deployment_session_id:
        log.info("Deployment session id: {}".format(deployment_session_id))
        deployment_stream_events = ps_helper.stream_deployment_session_events(deployment_session_id)
        end_time = datetime.now(timezone.utc).astimezone()
        log.info("Deployment end time: {}".format(end_time.strftime(helper.TIME_FMT_TIMEZONE)))
        deployment_time = end_time - start_time
        m, s = divmod(deployment_time.seconds, 60)
        log.info("Deployment time taken: {} minutes and {} seconds".format(m, s))

    return deployment_session_id, deployment_stream_events


def post_deployment(hermes_settings, ps_helper, deployment_session_id, deployment_stream_events, deployment_params,
                    file_root):
    """
    Post deployment validations
    :param hermes_settings: Fixture for Hermes settings
    :param ps_helper: Instance of ProvisioningServiceNewRPCHelper
    :param deployment_session_id: Deployment session id
    :param deployment_stream_events: Deployment session stream events
    :param deployment_params: Object of dataclass DeploymentParams
    :param file_root: Root directory to save test log files
    :return: List of NodeInfo objects
    """
    log.info("Post deployment for deployment session id: {}".format(deployment_session_id))

    # Input parameters
    blockchain_type = deployment_params.blockchain_type
    zone_type = validate_blockchain_location(deployment_params.zone_type)
    num_replicas = deployment_params.num_replicas
    num_clients = deployment_params.num_clients
    num_nodes = num_replicas + num_clients

    node_info_list = None
    if deployment_session_id and deployment_stream_events:
        # Validate streaming events
        execution_events_json = helper.protobuf_message_to_json(deployment_stream_events)
        if validate_stream_deployment_session_events(execution_events_json, num_nodes):
            log.info("Stream session events validation successful")
            blockchain_id, consortium_id = get_blockchain_and_consortium_ids(execution_events_json)
            # Get Node Info list
            node_info_list = get_node_info_list(execution_events_json, blockchain_type)
            # Save details to infra and for adding information to deployment_info
            save_details_to_infra(hermes_settings, ps_helper, deployment_session_id, node_info_list, consortium_id,
                                  blockchain_type, zone_type, file_root)
            if len(node_info_list) == num_nodes:
                # Verify containers running on each node
                status = verify_docker_containers(hermes_settings, node_info_list, blockchain_type, zone_type)
            else:
                log.error("Received {} number of node information in stream events. Expected {}"
                          .format(len(node_info_list), num_nodes))
                status = False

        else:
            log.error("Stream session events validation failed")
            status = False

    else:
        log.error("Deployment session id or stream session events unavailable")
        status = False

    return node_info_list if status else None


def deploy_and_post_deploy_wrapper(hermes_settings, ps_helper, deployment_params, file_root):
    """
    Wrapper called from every test case to run deployment and post deployment operations
    :param hermes_settings: Fixture for Hermes settings
    :param ps_helper: Instance of ProvisioningServiceNewRPCHelper
    :param deployment_params: Object of dataclass DeploymentParams
    :param file_root: Root directory to save test log files
    :return: Deployment session id, List of NodeInfo
    """

    cmdline_args = hermes_settings["cmdline_args"]
    zone_config = hermes_settings["zone_config"]
    deployment_session_id, deployment_stream_events = create_deployment(ps_helper, deployment_params, zone_config)
    node_info_list = post_deployment(hermes_settings, ps_helper, deployment_session_id, deployment_stream_events,
                                     deployment_params, file_root)
    status = True if node_info_list is not None else False
    retention_check_and_support_bundle(cmdline_args, ps_helper, status, deployment_session_id)
    return deployment_session_id, node_info_list


def update_provisioning_service_application_properties(cmdline_args, mode="UPDATE"):
    """
    Helper method to update provisioning service application-test.properties with local config-service
    :param cmdline_args: Hermes cmdline arguments
    :param mode: UPDATE/RESET
    :return: None
    """
    try:
        if cmdline_args.useLocalConfigService:
            persephone_config_file = helper.get_deployment_service_config_file(cmdline_args.dockerComposeFile,
                                                                               Product.PERSEPHONE_SERVICE_PROVISIONING)
            persephone_config_file_orig = "{}.orig".format(persephone_config_file)

            if mode == "UPDATE":
                log.info("Updating provisioning service config file to use local config-service")
                log.info("Config file: {}".format(persephone_config_file))
                log.info("[backup: {}]".format(persephone_config_file_orig))
                shutil.copy(persephone_config_file, persephone_config_file_orig)

                ports = helper.get_docker_compose_value(cmdline_args.dockerComposeFile, Product.PERSEPHONE_CONFIG_SERVICE,
                                                        "ports")
                config_service_port = ports[0].split(':')[0]
                command = ["/sbin/ifconfig", "ens160"]
                ifconfig_output = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
                ifconfig_output.check_returncode()
                if ifconfig_output.stderr:
                    log.error("ifconfig stderr: {}".format(ifconfig_output.stderr))
                log.debug("ifconfig output: {}".format(ifconfig_output.stdout.decode()))
                host_ip = None
                for line in ifconfig_output.stdout.decode().split('\n'):
                    fields = line.split()
                    log.info("***** fields: {}".format(fields))
                    if fields[0] == 'inet':
                        if ":" in fields[1]:
                            host_ip = fields[1].split(':')[1]
                        else:
                            host_ip = fields[1]
                        break

                log.info("Updating configService[\"address\"] to: {}:{}".format(host_ip, config_service_port))

                provisioning_config_service_address = "{}:{}".format(host_ip, config_service_port)
                helper.set_props_file_value(persephone_config_file, 'provisioning.config.service.address',
                                            provisioning_config_service_address)
                helper.set_props_file_value(persephone_config_file,
                                            'provisioning.config.service.transportSecurity.type', "NONE")
                log.info("Update completed!")

            if mode == "RESET":
                log.info("Reverting changes made to provisioning service config file")
                modified_config_file = persephone_config_file.replace("properties", "")
                modified_config_file += time.strftime('%Y-%m-%d_%H-%M-%S', time.gmtime(time.time()))
                modified_config_file += ".properties"
                shutil.copy(persephone_config_file, modified_config_file)
                shutil.move(persephone_config_file_orig, persephone_config_file)
                log.info("Updated config file for this run: {}".format(modified_config_file))

    except Exception as e:
        log.error(traceback.format_exc())
        raise


def validate_blockchain_location(location):
    """
    Helper method to get a valid location for Persephone Tests
    :param location: SDDC or ONPREM
    :return: Validated location 'sddc' or 'onprem'
    """
    blockchain_location = location.lower()
    if blockchain_location not in [helper.LOCATION_SDDC, helper.LOCATION_ONPREM]:
        raise Exception("Unsupported location to deploy blockchain: {}".format(blockchain_location))
    return blockchain_location


def get_deployment_session_id(deployment_response):
    """
    Method to validate deployment_reponse and get session ID
    :param deployment_response: Response from create_deployment
    :return: Deployment session ID
    """
    deployment_session_id = None
    if deployment_response:
        log.debug("Deployment response: {}".format(deployment_response))
        deployment_response_json = helper.protobuf_message_to_json(deployment_response)
        if "id" in deployment_response_json[0]:
            deployment_session_id = deployment_response_json[0]["id"]

    if deployment_session_id is None:
        log.error("Deployment session id not found")

    return deployment_session_id


def validate_stream_deployment_session_events(execution_events, num_nodes):
    """
    Validates the stream output of StreamDeploymentSessionEvents
    :param execution_events: Stream of deployment execution events to be validated in json format
    :param num_nodes: Total number of nodes (client + replica)
    :return: True or False
    """
    events_to_monitor = {"ACKNOWLEDGED": 1,
                         "COMPUTE_RESOURCE": num_nodes,
                         "COMPLETED": 1}
    for event in execution_events:
        event_type = event["type"]
        if event_type == "RESOURCE":
            if event["resource"]["type"] == "COMPUTE_RESOURCE":
                events_to_monitor["COMPUTE_RESOURCE"] -= 1
            else:
                continue

        if event_type in events_to_monitor:
            events_to_monitor[event_type] -= 1

    validation_status = True
    for event_type, count in events_to_monitor.items():
        if count:
            log.error("Stream session events validation failed for event: {}. Number of events not received: {}"
                      .format(event_type, count))
            validation_status = False

    return validation_status


def get_blockchain_and_consortium_ids(execution_events):
    """
    Fetch Blockchain ID and Consortium ID from stream execution events
    :param execution_events: Stream of deployment execution events to be validated in json format
    :return: Blockchain ID and Consortium ID
    """
    blockchain_id = None
    consortium_id = None
    for event in execution_events:
        if event["type"] == "ACKNOWLEDGED":
            blockchain_id = event["blockchainId"]
            consortium_id = event["consortiumId"]
            break

    log.debug("Blockchain ID: {}".format(blockchain_id))
    log.debug("Consortium ID: {}".format(consortium_id))
    return blockchain_id, consortium_id


def get_node_info_list(execution_events, blockchain_type):
    """
    Looks through the stream execution events to parse and get node details
    :param execution_events: Stream of deployment execution events to be validated in json format
    :param blockchain_type: DAML, ETHEREUM, HLF
    :return: List of NodeInfo objects
    """
    node_map = {}
    for event in execution_events:
        event_type = event["type"]
        if event_type != "RESOURCE":
            continue

        # Only RESOURCE events
        resource = event["resource"]
        node_id = resource["nodeId"]
        node = node_map.setdefault(node_id, NodeInfo(node_id))

        # NETWORK_RESOURCE resource type
        if resource["type"] == "NETWORK_RESOURCE":
            if "additionalInfo" in resource and "values" in resource["additionalInfo"] \
                    and "PRIVATE_IP" in resource["additionalInfo"]["values"]:
                node.private_ip = resource["additionalInfo"]["values"]["PRIVATE_IP"]
            elif "additionalInfo" in resource and "values" in resource["additionalInfo"] \
                    and "PUBLIC_IP" in resource["additionalInfo"]["values"]:
                node.public_ip = resource["additionalInfo"]["values"]["PUBLIC_IP"]

        # COMPUTE_RESOURCE resource type
        elif resource["type"] == "COMPUTE_RESOURCE":
            if "additionalInfo" in resource and "values" in resource["additionalInfo"] \
                    and "CLIENT_ENDPOINT" in resource["additionalInfo"]["values"]:
                if blockchain_type == helper.TYPE_DAML:
                    node.node_type = 1
            if "additionalInfo" in resource and "values" in resource["additionalInfo"] \
                    and "NODE_LOGIN" in resource["additionalInfo"]["values"]:
                node.password = resource["additionalInfo"]["values"]["NODE_LOGIN"]

    log.debug("Node Info")
    node_info_list = list(node_map.values())
    for node in node_info_list:
        log.debug("{}".format(node))

    return node_info_list


def get_docker_containers_by_blockchain_type(user_config, blockchain_type):
    """
    Get list of docker containers by blockchain type
    :param user_config: user_config.json JSON object
    :param blockchain_type: DAML, Ethereum, HLF
    :return: List of docker containers for the blockchain type
    """
    return list(user_config["persephoneTests"]["modelService"]["defaults"]["deployment_components"]
                [blockchain_type].values())


def get_docker_containers_by_node_type(user_config, blockchain_type, node_type):
    """
    Get list of docker containers by node type
    :param user_config: user_config.json JSON object
    :param blockchain_type: DAML, Ethereum, HLF
    :param node_type: Replica/Client (0 or 1)
    :return: List of docker containers for the node type
    """
    if blockchain_type == helper.TYPE_DAML:
        if node_type == 0:  # Replica node
            container_type = helper.TYPE_DAML_COMMITTER
        else:  # Client node
            container_type = helper.TYPE_DAML_PARTICIPANT
    else:
        container_type = blockchain_type
    return list(user_config["persephoneTests"]["modelService"]["defaults"]["deployment_components"]
                [container_type].values())


def save_details_to_infra(hermes_settings, ps_helper, deployment_session_id, node_info_list, consortium_id,
                          blockchain_type, zone_type, file_root):
    """
    Saves deployment information to infra
    :param hermes_settings: Fixture for Hermes settings
    :param ps_helper: Instance of ProvisioningServiceNewRPCHelper
    :param deployment_session_id: Deployment session ID
    :param node_info_list: List of NodeInfo objects
    :param consortium_id: Consortium ID
    :param blockchain_type: Type of blockchain (DAML, Ethereum, HLF)
    :param zone_type: sddc or onprem
    :param file_root: Root directory to save test log files
    :return: None
    """
    nodes = []
    for node in node_info_list:
        vm_handle = infra.findVMByInternalIP(node.private_ip)
        if vm_handle:
            nodes.append({"ip": node.private_ip,
                          "replica_id": vm_handle["replicaId"] if vm_handle else node.node_id
                          })

    log.info("Annotating VMs with deployment context")
    try:
        infra.giveDeploymentContext({
            "id": "None",
            "consortium_id": consortium_id,
            "blockchain_type": blockchain_type,
            "nodes_type": infra.PRETTY_TYPE_REPLICA,
            "replica_list": nodes
        })

        for deployment_info in ps_helper.deployment_info:
            if get_deployment_session_id(deployment_info["deployment_session_id"]) == deployment_session_id:
                log.debug("Updating more info for deployment session ID: {}".format(deployment_session_id))
                replicas = [node.public_ip if zone_type == helper.LOCATION_SDDC else node.private_ip
                            for node in node_info_list]
                deployment_info["replicas"] = replicas
                deployment_info["private_ips"] = [node.private_ip for node in node_info_list]
                deployment_info["concord_username"] = "root"
                deployment_info["concord_password"] = [node.password for node in node_info_list]
                deployment_info["docker_containers"] = \
                    get_docker_containers_by_blockchain_type(hermes_settings["user_config"], blockchain_type)
                deployment_info["concord_type"] = blockchain_type
                deployment_info["log_dir"] = file_root
                break
    except Exception as e:
        log.error("Exception in saving details to infra: {}".format(e))


def verify_docker_containers(hermes_settings, node_info_list, blockchain_type, zone_type):
    """
    Verify if the necessary docker containers are running on all the nodes
    :param hermes_settings: Fixture for Hermes settings
    :param node_info_list: List of NodeInfo
    :param blockchain_type: DAML, Ethereum, HLF
    :param zone_type: sddc or onprem
    :return: True/False
    """
    status = True
    user_config = hermes_settings["user_config"]
    for node in node_info_list:
        containers_to_verify = get_docker_containers_by_node_type(user_config, blockchain_type, node.node_type)
        # Verify SSH connection
        ip = node.public_ip if zone_type == helper.LOCATION_SDDC else node.private_ip
        ssh_status = ssh_connect_node(ip, node.username, node.password)
        if not ssh_status:
            log.error("Could not SSH to {}. Aborting docker container verification".format(ip))
            status = False
            break

        # Verify docker containers
        count = 0
        max_timeout = 600  # 10 mins
        start_time = time.time()
        docker_images_found = False
        command_to_run = "docker ps --format '{{.Names}}'"
        log.info("Waiting for all docker containers to be up on {} within {} mins".format(ip, max_timeout / 60))
        while (time.time() - start_time) <= max_timeout and not docker_images_found:
            count += 1
            log.debug("Verifying docker containers (attempt: {})".format(count))
            ssh_output = helper.ssh_connect(ip, node.username, node.password, command_to_run)
            log.debug("SSH output: {}".format(ssh_output))
            for container_name in containers_to_verify:
                if container_name not in ssh_output:
                    docker_images_found = False
                    log.warning("Container '{}' not up and running on node '{}'".format(container_name, ip))
                    time.sleep(30)
                    break  # break out of container_name in containers_to_verify for-loop
                else:
                    docker_images_found = True
                    log.debug("Container {} found in node {}".format(container_name, ip))

        if not docker_images_found:
            log.error("Not all containers are up and running on node '{}'".format(ip))
            status = False
            break  # break out of node in node_info_list for-loop
        else:
            log.info("Docker containers verified on {}".format(ip))

    return status


def ssh_connect_node(ip, username, password, mode=None):
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


def verify_ethrpc_block_0(hermes_settings, ps_helper, ip, username, password, file_root, deployment_session_id):
    """
    Method to verify hitting ethrpc endpoint and getting block 0
    :param hermes_settings: Fixture for Hermes settings
    :param ps_helper: Instance of ProvisioningServiceNewRPCHelper
    :param ip: IP of the node
    :param username: username
    :param password: password
    :param file_root: Root directory to save test log files
    :param deployment_session_id: Deployment session ID
    :return: True/False
    """
    # Setup port forwarding
    src_port = 443
    helper.add_ethrpc_port_forwarding(ip, username, password, src_port=src_port)

    log.info("Validating ethrpc (get Block 0) on port {}".format(src_port))
    from rpc.rpc_call import RPC
    token_descriptor = auth.getTokenDescriptor(auth.ROLE_CON_ADMIN, True, auth.internal_admin)
    rpc = RPC(file_root, "verify_ethrpc_block_0", "http://{}:{}".format(ip, src_port),
              hermes_settings["user_config"], token_descriptor)
    attempt = 0
    max_tries = 5
    status = False
    while attempt < max_tries:
        attempt += 1
        log.debug("Verifying ethrpc connectivity (attempt: {}/{})".format(attempt, max_tries))
        try:
            block_zero = rpc.getBlockByNumber(0)
            if block_zero and block_zero["number"] == "0x0":
                status = True
                break
            elif block_zero:
                log.error("Invalid response getting block zero: {}".format(block_zero))
                break
            else:
                raise Exception("Block zero not found. Will attempt to retry.")
        except Exception as e:
            if attempt == max_tries:
                log.error(e)
            else:
                sleep_time = 30  # seconds
                log.debug("Retry after {} seconds".format(sleep_time))
                time.sleep(sleep_time)

    retention_check_and_support_bundle(hermes_settings["cmdline_args"], ps_helper, status, deployment_session_id)
    return status


def verify_dar_upload(hermes_settings, ps_helper, ip, username, password, deployment_session_id):
    """
    Method to verify connectivity to client, upload DAR, and run tests
    :param hermes_settings: Fixture for Hermes settings
    :param ps_helper: Instance of ProvisioningServiceNewRPCHelper
    :param ip: IP of the node
    :param username: username
    :param password: password
    :param deployment_session_id: Deployment session ID
    :return: True/False
    """
    # Setup port forwarding
    src_port = helper.FORWARDED_DAML_LEDGER_API_ENDPOINT_PORT
    dest_port = 6865
    log.debug("Sleep 60 seconds to ensure ledger_api is fully up")
    time.sleep(60)
    helper.add_ethrpc_port_forwarding(ip, username, password, src_port=src_port, dest_port=dest_port)

    status = False
    if helper.verify_connectivity(ip, src_port):
        log.info("Verified connectivity to client {} on port {}".format(ip, src_port))
        try:
            log.info("Starting DAR upload on {}:{}".format(ip, src_port))
            daml_helper.upload_test_tool_dars(host=ip, port=str(src_port))
            log.info("Starting DAML verification tests on {}:{}".format(ip, src_port))
            daml_helper.verify_ledger_api_test_tool(host=ip, port=str(src_port))
            status = True
        except Exception as e:
            log.error("DAR upload/verification failed")
    else:
        log.error("Could not verify connectivity to client {} on port {}".format(ip, src_port))

    retention_check_and_support_bundle(hermes_settings["cmdline_args"], ps_helper, status, deployment_session_id)
    return status


def retention_check_and_support_bundle(cmdline_args, ps_helper, status, deployment_session_id=None):
    """
    Parse the test status to determine Blockchain node/replica's retention policy
    :param cmdline_args: Cmdline args
    :param ps_helper: Instance of ProvisioningServiceNewRPCHelper
    :param status: Test status
    :param deployment_session_id: Deployment session ID (if exists)
    :return: None
    """
    log.info("Checking deployment retention for deployment session id: {}".format(deployment_session_id))
    if deployment_session_id:
        if (cmdline_args.keepBlockchains == helper.KEEP_BLOCKCHAINS_ALWAYS) or \
                (cmdline_args.keepBlockchains == helper.KEEP_BLOCKCHAINS_ON_FAILURE and (not status)):
            log.info("Adding deployment session id to retention list: {}".format(deployment_session_id))
            session_ids_to_retain.append(deployment_session_id)
        else:
            log.debug("Retention is not needed for deployment session id: {}".format(deployment_session_id))

    deployment_info_found = False
    if not status:
        log.info("Create support bundle for deployment session id: {}".format(deployment_session_id))
        for deployment_info in ps_helper.deployment_info:
            if get_deployment_session_id(deployment_info["deployment_session_id"]) == deployment_session_id:
                deployment_info_found = True
                if "replicas" in deployment_info and deployment_info["replicas"]:
                    log.info("Create support bundle for session id: {}".format(deployment_session_id))
                    helper.create_concord_support_bundle(deployment_info["replicas"], deployment_info["concord_type"],
                                                         deployment_info["log_dir"])
                else:
                    log.info("No replicas found to get support logs")
                break

        if not deployment_info_found:
            log.info("Deployment session id not found in deployment_info list")


def deprovision(hermes_settings, ps_helper):
    """
    Deprovision/Teardown function
    :param hermes_settings: Fixture for Hermes settings
    :param ps_helper: Instance of ProvisioningServiceNewRPCHelper
    :return: None
    """
    log.info("Deprovision all deployments")

    zone_type = hermes_settings["cmdline_args"].blockchainLocation.lower()
    zone_config = hermes_settings["zone_config"]
    deprovision_status = None
    for deployment_info in ps_helper.deployment_info:
        session_id = get_deployment_session_id(deployment_info["deployment_session_id"])
        stub = deployment_info["stub"]

        cleaned_up = False
        if session_id not in session_ids_to_retain:
            # Deprovision call
            log.info("Deprovisioning session id: {}".format(session_id))
            max_timeout = 480  # seconds
            sleep_time = 15  # seconds
            start_time = time.time()
            while ((time.time() - start_time) < max_timeout) and not cleaned_up:
                response = ps_helper.deprovision_deployment(session_id, zone_type, zone_config, stub)
                if response:
                    log.debug("Deprovisioning response: {}".format(response))
                    cleaned_up = True
                    break
                else:
                    log.info("Sleep for {} seconds and retry".format(sleep_time))
                    time.sleep(sleep_time)

            if cleaned_up:
                log.info("Deprovisioning successful")
                if deprovision_status is None:
                    deprovision_status = True
            else:
                deprovision_status = False
                log.info("Deprovisioning failed")
        else:
            log.info("Retaining session id: {}".format(session_id))

    if deprovision_status is None:
        log.warning("No session ids to deprovision")
    elif deprovision_status:
        log.info("Deprovisioned all sessions")
    else:
        log.error("Failed to deprovision")


"""
# TEST CASES
# Set deployment_params as per the test requirement. Rest of the body template should remain the same 
# for Ethereum and DAML respectively.
"""


@describe("Test to run 4 node Ethereum deployment on VMC")
@pytest.mark.smoke
def test_ethereum_4_node_vmc(request, hermes_settings, ps_helper, file_root, teardown):

    # Set the deployment params for this test case
    deployment_params = DeploymentParams(helper.TYPE_ETHEREUM, helper.LOCATION_SDDC, 4, 0)
    zone_type = helper.LOCATION_SDDC

    # Rest of the code below shouldn't change between different combinations of Ethereum test cases
    # Update zone_type in hermes_settings fixture, since it is used downstream in deprovision function
    hermes_settings["cmdline_args"].blockchainLocation = zone_type

    # Call the deploy and post deploy wrapper
    deployment_session_id, node_info_list = deploy_and_post_deploy_wrapper(hermes_settings, ps_helper,
                                                                           deployment_params, file_root)
    assert node_info_list is not None

    # Verify ethrpc block 0 on all nodes
    for node in node_info_list:
        ip = node.public_ip if zone_type == helper.LOCATION_SDDC else node.private_ip
        assert verify_ethrpc_block_0(hermes_settings, ps_helper, ip, node.username, node.password, file_root,
                                     deployment_session_id)

    log.info("Test {} completed successfully".format(request.node.name))


@describe("Test to run a DAML ONPREM deployment (7 replicas + 1 client)")
@pytest.mark.smoke
def test_daml_7_node_onprem(request, hermes_settings, ps_helper, file_root, teardown):

    # Set the deployment params for this test case
    deployment_params = DeploymentParams(helper.TYPE_DAML, helper.LOCATION_ONPREM, 7, 1)
    zone_type = helper.LOCATION_ONPREM

    # Rest of the code below shouldn't change between different combinations of DAML test cases
    # Update zone_type in hermes_settings fixture, since it is used downstream in deprovision function
    hermes_settings["cmdline_args"].blockchainLocation = zone_type

    # Call the deploy and post deploy wrapper
    deployment_session_id, node_info_list = deploy_and_post_deploy_wrapper(hermes_settings, ps_helper,
                                                                           deployment_params, file_root)
    assert node_info_list is not None

    # Verify DAR upload on all client nodes
    for node in node_info_list:
        if node.node_type == 1:
            ip = node.public_ip if zone_type == helper.LOCATION_SDDC else node.private_ip
            assert verify_dar_upload(hermes_settings, ps_helper, ip, node.username, node.password,
                                     deployment_session_id)

    log.info("Test {} completed successfully".format(request.node.name))


@describe("Use this test to test Persephone with desired cmdline arguments")
@pytest.mark.cmdline
def test_cmdline_driven(request, hermes_settings, ps_helper, file_root, teardown):

    blockchain_type = hermes_settings["cmdline_args"].blockchainType
    zone_type = validate_blockchain_location(hermes_settings["cmdline_args"].blockchainLocation)
    num_replicas = int(hermes_settings["cmdline_args"].numReplicas)
    num_clients = int(hermes_settings["cmdline_args"].numParticipants)
    deployment_params = DeploymentParams(blockchain_type, zone_type, num_replicas, num_clients)

    # Call the deploy and post deploy wrapper
    deployment_session_id, node_info_list = deploy_and_post_deploy_wrapper(hermes_settings, ps_helper,
                                                                           deployment_params, file_root)
    assert node_info_list is not None

    # Verify DAR upload for DAML, or ethrpc block 0 for Ethereum
    for node in node_info_list:
        ip = node.public_ip if zone_type == helper.LOCATION_SDDC else node.private_ip
        if blockchain_type.lower() == helper.TYPE_DAML:
            if node.node_type == 1:
                assert verify_dar_upload(hermes_settings, ps_helper, ip, node.username, node.password,
                                         deployment_session_id)
        else:
            assert verify_ethrpc_block_0(hermes_settings, ps_helper, ip, node.username, node.password, file_root,
                                         deployment_session_id)

    log.info("Test {} completed successfully".format(request.node.name))


@describe("Use this test to run a manual deployment from Persephone with the desired cmdline arguments")
@pytest.mark.deployment_only
def test_deployment_only(request, hermes_settings, ps_helper):

    blockchain_type = hermes_settings["cmdline_args"].blockchainType
    zone_type = validate_blockchain_location(hermes_settings["cmdline_args"].blockchainLocation)
    num_replicas = int(hermes_settings["cmdline_args"].numReplicas)
    num_clients = int(hermes_settings["cmdline_args"].numParticipants)
    zone_config = hermes_settings["zone_config"]
    deployment_params = DeploymentParams(blockchain_type, zone_type, num_replicas, num_clients)

    deployment_session_id, deployment_stream_events = create_deployment(ps_helper, deployment_params, zone_config)
    assert deployment_session_id is not None

    log.info("Test {} completed successfully".format(request.node.name))
