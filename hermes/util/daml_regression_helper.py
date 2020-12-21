from daml_requests import simple_request, get_daml_url, DAML_LEDGER_API_PORT, continuous_daml_request_submission
import json
from datetime import datetime, timedelta
from pathlib import Path
import os
import sys
import random
from . import helper, hermes_logging, blockchain_ops, wavefront
from . import node_interruption_helper as intr_helper
import multiprocessing
from asyncio import set_event_loop, new_event_loop, get_event_loop
from itertools import zip_longest
import time
import util.daml.daml_helper as daml_helper

# Do not add any import after this
sys.path.append(os.path.abspath('../daml'))

log = hermes_logging.getMainLogger()

PARTICIPANT_GENERIC_ERROR_MSG = "DAML request submission/verification failed "
COMMITTER_POWER_ON_ERROR_MSG = "Failed to power on committer node "
COMMITTER_POWER_OFF_ERROR_MSG = "Failed to power off committer node "
PARTICIPANT_POWER_ON_ERROR_MSG = "Failed to power on participant node "
PARTICIPANT_POWER_OFF_ERROR_MSG = "Failed to power off participant node "
CHECKPOINT_ERROR_MESSAGE = "Failed to trigger new checkpoint"
DAML_ATTEMPTS = 2


def format_hosts_structure(all_replicas):
    '''
    Currently the structure of replicas is different when blockchain is deployed 
    and when replicasConfig argument is provided.
    Once the utility functions are corrected to allow only single format, 
    this function would be removed.
    '''
    client_hosts = all_replicas["daml_participant"]
    concord_hosts = all_replicas["daml_committer"]

    client_hosts_list, concord_hosts_list = [], []

    # Parse through the participants and committers
    for client_host, concord_host in list(zip_longest(client_hosts, concord_hosts)):
        # Participant hosts
        if client_host:
            if (isinstance(client_host, dict)):
                client_host = client_host["private_ip"] if client_host[
                    "private_ip"] is not None else client_host["public_ip"]
            client_hosts_list.append(client_host)

        # Committer hosts
        if concord_host:
            if (isinstance(concord_host, dict)):
                concord_host = concord_host["private_ip"] \
                    if concord_host["private_ip"] is not None else concord_host["public_ip"]
            concord_hosts_list.append(concord_host)

    client_hosts = client_hosts_list if len(
        client_hosts_list) else client_hosts
    concord_hosts = concord_hosts_list if len(
        concord_hosts_list) else concord_hosts

    return client_hosts, concord_hosts


def get_block_id(blockchain_id, ip):
    '''
    Function to get last block_id of a replica
    Args:
        ip: IP of replica
    Returns:
        int: Last Block ID of replica 
    '''
    username, password = helper.getNodeCredentials(blockchain_id, ip)

    # Find concord-core container image
    cmd_for_container_image = 'docker images | grep -m1 concord-core'
    container_image = helper.ssh_connect(
        ip, username, password, cmd_for_container_image)
    container_image_list = [i for i in container_image.split(" ") if i != ""]
    concord_core_param = container_image_list[0] + \
        ':' + container_image_list[1]

    params = "type=bind,source=/mnt/data/rocksdbdata/,target=/concord/rocksdbdata"
    tool_path = "/concord/sparse_merkle_db_editor /concord/rocksdbdata"
    cmd = ' '.join([params, concord_core_param, tool_path])

    # Find last block ID
    cmd_for_block_id = 'docker run -it --entrypoint="" --mount {0} getLastBlockID'.format(
        cmd)
    log.debug("\nFinding last block ID")
    result = helper.ssh_connect(
        ip, username, password, cmd_for_block_id)
    if ("Error" in str(result)):
        params = "type=bind,source=/config/concord/rocksdbdata/,target=/concord/rocksdbdata"
        cmd = ' '.join([params, concord_core_param, tool_path])
        cmd_for_block_id = 'docker run -it --entrypoint="" --mount {0} getLastBlockID'.format(
            cmd)
        result = helper.ssh_connect(
            ip, username, password, cmd_for_block_id)
    block_id = (result.split(': \"')[1]).split('\"')[0]
    log.debug("Block ID : {}".format(block_id))
    return int(block_id)


def install_sdk_deploy_daml(client_host):
    '''
    Function to install DAML SDK and deploy the dar file
    on the client_host where Ledger API is running.
    Args:
        client_host: Host where ledger API is running
    Returns:
        None
    '''
    client_port = '6861' if client_host == 'localhost' else DAML_LEDGER_API_PORT
    daml_sdk_path = None
    is_daml_deployed = False
    for i in range(0, DAML_ATTEMPTS):
        try:
            if i > 0:
                time.sleep(pow(2, i))
            if not daml_sdk_path:
                daml_sdk_path = daml_helper.install_daml_sdk()
                log.debug("\nDaml sdk path is {}".format(daml_sdk_path))
            if not is_daml_deployed:
                cmd = [daml_sdk_path, "deploy", "--host",
                       client_host, "--port", client_port]
                party_project_dir = "util/daml/request_tool"
                is_daml_deployed, output = helper.execute_ext_command(
                    cmd, timeout=240, working_dir=party_project_dir, verbose=False)
                log.debug("\nSuccess and Output are {} and {}".format(
                    is_daml_deployed, output))
                assert is_daml_deployed, "DAML Error: Unable to deploy DAML app for one/more parties"
            log.info("\n*** Daml sdk installation and deployment successful ***")
        except Exception as e:
            log.error(
                "Error in DAML SDK installation or deployment for {}: {}".format(client_host, e))


def interrupt_node(fxHermesRunSettings, fxBlockchain, node, node_type, interruption_type,
                   mode, custom_params=None):
    '''
    Function to create node_interruption_details object based on input.
    This will be passed to node interruption utility function.
    Args:
        fxHermesRunSettings: Hermes command line arguments
        node: Node to be interrupted
        node_type: Type of node to be interrupted
        interruption_type: Type of interruption
        mode: Subtype of interruption
        custom_params: Custom parameters to be passed, if any.
    Returns:
        True/False based on the action outcome.
    '''
    scenario_details = {
        intr_helper.NODE_TYPE_TO_INTERRUPT: node_type,
        intr_helper.NODE_INTERRUPTION_DETAILS: [
            {
                intr_helper.NODE_INTERRUPTION_TYPE: interruption_type,
                intr_helper.SKIP_MASTER_REPLICA: False,
                intr_helper.CUSTOM_INTERRUPTION_PARAMS: {
                }
            }
        ]
    }
    if custom_params:
        scenario_details[intr_helper.NODE_INTERRUPTION_DETAILS][0][intr_helper.CUSTOM_INTERRUPTION_PARAMS] = custom_params
    return intr_helper.perform_interrupt_recovery_operation(
        fxHermesRunSettings, fxBlockchain, None, node, scenario_details, scenario_details[intr_helper.NODE_INTERRUPTION_DETAILS][0], mode)


def make_daml_request(reraise, blockchain_id, client_host, no_of_txns=1, wait_time=0.3):
    # Default port can be 6865 or 80 (helper.FORWARDED_DAML_LEDGER_API_ENDPOINT_PORT)
    # But after powering off and on the host, it listens on 6865 only
    # So using 6865 here
    log.info("\n*** Submit daml transaction(s) ***")
    url = get_daml_url(client_host)
    username, password = helper.getNodeCredentials(blockchain_id, client_host)
    if not helper.check_docker_health(client_host, username, password,
                                      helper.TYPE_DAML_PARTICIPANT, max_timeout=5, verbose=False):
        log.warning("\n*** Unexpected crash ***")
        return False

    def make_daml_request(url, no_of_txns, wait_time):
        simple_request(url, no_of_txns, wait_time)
    log.info("\nStarting call to make_daml_request for url {}".format(url))
    p_daml_txn = multiprocessing.Process(target=make_daml_request,
                                         args=(url, no_of_txns, wait_time))
    p_daml_txn.start()
    count, retries, wait = 0, 10, 60
    while count < retries:
        if p_daml_txn.is_alive():
            log.info("\nIs process alive for trial no {}? {}".format(count+1, p_daml_txn.is_alive()))
            time.sleep(wait)
            count+=1
        else:
            break
    if count == retries:
        reraise()
        p_daml_txn.terminate()
        return False
    else:
        return True


def perform_sanity_check(reraise, fixture_tuple, fxHermesRunSettings, fxBlockchain):
    '''
    Function to perform sanity check after executing every test.
    It powers on the node and start containers, if brought down or stopped during any test
    Performs daml transaction as last check. 
    Args:
        fixture_tuple: Local setup fixture tuple.
        fxHermesRunSettings: Hermes command line arguments.
    Returns:
        None
    '''
    log.info("\n**** Performing sanity check after every test ****")
    try:
        log.debug(
            "\nCheck if all the participant nodes are up, if not bring them up")
        for client_host in fixture_tuple.client_hosts:
            # Check if client host is up
            assert interrupt_node(fxHermesRunSettings, fxBlockchain, client_host,
                                  helper.TYPE_DAML_PARTICIPANT,
                                  intr_helper.NODE_INTERRUPT_VM_STOP_START,
                                  intr_helper.NODE_RECOVER), \
                "Failed to power on participant {}".format(client_host)

        log.debug("\nCheck if all the committer nodes are up, if not bring them up")
        for concord_host in fixture_tuple.concord_hosts:
            # Check if concord host is up
            assert interrupt_node(fxHermesRunSettings, fxBlockchain, concord_host,
                                  helper.TYPE_DAML_COMMITTER,
                                  intr_helper.NODE_INTERRUPT_VM_STOP_START,
                                  intr_helper.NODE_RECOVER), \
                "Failed to power on committer {}".format(concord_host)

        cmd = "docker start daml_ledger_api; docker inspect --format {{.State.Status}} daml_ledger_api"
        helper.ssh_parallel(fxBlockchain.blockchainId,
                            fixture_tuple.client_hosts, cmd, verbose=False)

        cmd = "docker start daml_index_db; docker inspect --format {{.State.Status}} daml_index_db"
        helper.ssh_parallel(fxBlockchain.blockchainId,
                            fixture_tuple.client_hosts, cmd, verbose=False)

        cmd = "docker start concord; docker inspect --format {{.State.Status}} concord"
        helper.ssh_parallel(fxBlockchain.blockchainId,
                            fixture_tuple.concord_hosts, cmd, verbose=False)

        log.debug("\nPerform daml transaction as final check")
        for client_host in fixture_tuple.client_hosts:
            assert make_daml_request(
                reraise, fxBlockchain.blockchainId, client_host), PARTICIPANT_GENERIC_ERROR_MSG + "as part of sanity check"

        log.info("\n*** Sanity check successfully done for given test ***\n")
    except Exception as excp:
        assert False, excp


def power_off_committers(fxHermesRunSettings, fxBlockchain, concord_hosts, f_count=None):
    '''
    Function to power off all or f committer nodes
    If f_count is passed to this method, then it powers off only f nodes.
    Otherwise all nodes are powered off.
    Args:
        fxHermesRunSettings: Hermes command line arguments
        concord_hosts: List of Committer Node IPs
        f_count: If passed, then the number of f nodes is passed
    Returns:
        None
    '''
    for count, concord_host in enumerate(concord_hosts):
        if f_count and f_count == count + 1:
            break
        assert interrupt_node(fxHermesRunSettings, fxBlockchain, concord_host,
                              helper.TYPE_DAML_COMMITTER,
                              intr_helper.NODE_INTERRUPT_VM_STOP_START,
                              intr_helper.NODE_INTERRUPT), \
            COMMITTER_POWER_OFF_ERROR_MSG + "[{}]".format(
                concord_host)
    log.info("\n*** Powered off all the committers ***")


def power_on_all_participants(fxHermesRunSettings, fxBlockchain, client_hosts):
    '''
    Function to switch on all participant nodes
    Args:
        fxHermesRunSettings: Hermes command line arguments
        client_hosts: List containing Participant IPs
    Returns:
        None
    '''
    for count, client_host in enumerate(client_hosts):
        assert interrupt_node(fxHermesRunSettings, fxBlockchain, client_host,
                              helper.TYPE_DAML_PARTICIPANT,
                              intr_helper.NODE_INTERRUPT_VM_STOP_START,
                              intr_helper.NODE_RECOVER), \
            PARTICIPANT_POWER_ON_ERROR_MSG + "[{}]".format(client_host)
        log.debug("Participant no {} : {} is on".format(
            count + 1, client_host))
    log.info("\n*** Powered on all the participants ***")


def staggered_start_committers(fxHermesRunSettings, fxBlockchain, concord_hosts):
    '''
    Function for staggered startup of committer nodes
    Args:
        fxHermesRunSettings: Hermes command line arguments
        concord_hosts: List of Committer Node IPs
    Returns:
        None
    '''
    list_of_committer_indexes = list(range(len(concord_hosts)))
    while list_of_committer_indexes:
        concord_host_index = random.choice(list_of_committer_indexes)
        concord_host = concord_hosts[concord_host_index]
        assert interrupt_node(fxHermesRunSettings, fxBlockchain, concord_host,
                              helper.TYPE_DAML_COMMITTER,
                              intr_helper.NODE_INTERRUPT_VM_STOP_START,
                              intr_helper.NODE_RECOVER), \
            COMMITTER_POWER_ON_ERROR_MSG + "[{}]".format(
                concord_host)
        log.debug("Committer on index {}: {} is on".format(
            concord_host_index, concord_host))
        list_of_committer_indexes.remove(concord_host_index)
        sleep_time = random.randrange(5, 10)
        log.debug("Sleeping for time {}".format(sleep_time))
        time.sleep(sleep_time)
    log.info("\n*** Started all the committers in staggered manner ***")


def start_for_replica_list(blockchain_id, replica_list, container_name, count):
    '''
    Function to start committer nodes from list of replicas
    for maximum count provided
    Args:
        blockchain_id: Blockchain Id
        replica_list: List of committer IPs
        container_name: Name of the container
        count: Number of committer nodes to be started
    Returns:
        None
    '''
    for i in range(count):
        concord_host = replica_list[i]
        assert intr_helper.start_container(
            blockchain_id, concord_host, container_name), "Failed to start committer node [{}]".format(concord_host)
    log.info("\n*** Started {} replicas ***".format(count))


def stop_for_replica_list(blockchain_id, replica_list, container_name, count):
    '''
    Function to stop committer nodes from list of replicas
    for maximum count provided
    Args:
        blockchain_id: Blockchain Id
        replica_list: List of committer IPs
        container_name: Name of the container
        count: Number of committer nodes to be stopped
    Returns:
        None
    '''
    for i in range(count):
        concord_host = replica_list[i]
        assert intr_helper.stop_container(
            blockchain_id, concord_host, container_name), "Failed to stop committer node [{}]".format(concord_host)
    log.info("\n*** Stopped {} replicas ***".format(count))


def verify_view_change(reraise, fxBlockchain, init_primary_rip, init_primary_index, interrupted_nodes=[]):
    '''
    Function to verify view change happened successfully
    Args:
        fxBlockchain: Blockchain tuple of (blockchainId, consortiumId, replicas, clientNodes).
        init_primary_rip: Initial primary IP
        init_primary_index: Initial primary index
        interrupted_nodes: Nodes which are stopped        
    Returns:
        bool: True if view change happened successfully, False otherwise.
    '''
    try:
        log.debug("Finding new primary replica ip")
        committers_mapping = blockchain_ops.map_committers_info(
            fxBlockchain, interrupted_nodes)
        new_primary_rip = committers_mapping["primary_ip"]
        new_primary_index = committers_mapping["primary_index"]
        assert new_primary_rip or new_primary_index, "Primary replica IP & index not found after view change"

        if init_primary_index != new_primary_index or init_primary_rip != new_primary_rip:
            log.debug("View change successful")
            return True
        else:
            reraise()
            return False
    except Exception as excp:
        assert False, excp


def trigger_checkpoint(bc_id, client_host):
    '''
    Function to trigger multiple checkpoints by sending daml requests
    Args:
        bc_id: ID of Blockchain
        client_host: Client host IP
    Returns:
        boolean: True or False if checkpoint was triggered
    '''
    no_of_txns_for_checkpoint, wait_time, duration = 170, 0, 200
    checkpoint_before_txns = get_last_checkpoint(bc_id)
    log.debug("Checkpoint before transactions: {}".format(
        checkpoint_before_txns))
    p_tc_daml_txn = multiprocessing.Process(target=continuous_daml_request_submission,
                                         args=(client_host, no_of_txns_for_checkpoint, wait_time, duration))
    p_tc_daml_txn.start()
    p_tc_daml_txn.join()
    p_tc_daml_txn.terminate()
    checkpoint_after_txns = get_last_checkpoint(bc_id)
    log.debug("Checkpoint after transactions: {}".format(checkpoint_after_txns))
    if checkpoint_after_txns > checkpoint_before_txns:
        return True
    else:
        return False


def get_last_checkpoint(bc_id):
    '''
    Function to get last generated checkpoint from wavefront API metrics
    Args:
        bc_id: ID of Blockchain
    Returns:
        int: Last checkpoint
    '''
    start_epoch = (datetime.now()).strftime('%s')
    end_epoch = (datetime.now() + timedelta(seconds=20)).strftime('%s')
    metric_name = "vmware.blockchain.concord.concordbft.last.stored.checkpoint.gauge"
    metric_query = "ts({}".format(metric_name)
    metric_query = metric_query + ",blockchain={})".format(bc_id)
    log.debug("Metric Query to Wavefront API: {}".format(metric_query))
    log.debug("Start epoch time: {} ; End epoch time: {}".format(
        start_epoch, end_epoch))
    time.sleep(60)
    str_output = wavefront.call_wavefront_chart_api(
        metric_query, start_epoch, end_epoch, granularity="s")
    output = json.loads(str_output)
    timeseries_length = len(output["timeseries"])
    last_checkpoint = -1
    # Find last checkpoint in reverse loop
    for i in range(timeseries_length-1, -1, -1):
        if output["timeseries"][i]["data"]:
            last_checkpoint = output["timeseries"][i]["data"][-1][1]
            break
        else:
            continue
    log.info("\nLast checkpoint is {}".format(last_checkpoint))
    return last_checkpoint
