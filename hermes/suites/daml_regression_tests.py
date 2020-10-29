#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
from datetime import datetime, timedelta
from suites.case import describe
from collections import namedtuple
import pytest
import time
import random
from util import helper, hermes_logging, blockchain_ops
from threading import Thread
import util.daml.daml_helper as daml_helper
from util.daml.daml_requests import simple_request, continuous_daml_request_submission
import util.node_interruption_helper as intr_helper
from asyncio import set_event_loop, new_event_loop
from fixtures.common_fixtures import fxBlockchain, fxNodeInterruption, fxProduct
import json
from pathlib import Path
import os
from subprocess import check_output, CalledProcessError
from itertools import zip_longest

log = hermes_logging.getMainLogger()

LocalSetupFixture = namedtuple(
    "LocalSetupFixture", "client_hosts, concord_hosts, f_count")

PARTICIPANT_GENERIC_ERROR_MSG = "DAML request submission/verification failed "
COMMITTER_POWER_ON_ERROR_MSG = "Failed to power on committer node "
COMMITTER_POWER_OFF_ERROR_MSG = "Failed to power off committer node "
PARTICIPANT_POWER_ON_ERROR_MSG = "Failed to power on participant node "
PARTICIPANT_POWER_OFF_ERROR_MSG = "Failed to power off participant node "
DAML_LEDGER_API_PORT = '6865'


@pytest.fixture(scope="function")
@describe("fixture; local setup for given test suite")
def fxLocalSetup(request, reraise, fxHermesRunSettings, fxNodeInterruption, fxBlockchain, fxProduct):
    '''
    Function scoped fixture to enable all the test cases utilize common func.
    Args:
        fxHermesRunSettings: Hermes command line arguments (part of conftest.py so no explicit import).
        fxNodeInterruption: Node Interruption fixture for related functions
        fxBlockchain: Blockchain fixture required to get f_count
        fxProduct: Product fixture
    Returns:
        client_hosts: Participant hosts
        concord_hosts: Concord (committer) replicas
        f_count: Maximum faulty replicas allowed.
    '''
    log.info("\n\nBlockchain fixture is {}".format(fxBlockchain))
    f_count = intr_helper.get_f_count(fxBlockchain)
    client_hosts, concord_hosts = format_hosts_structure(fxBlockchain.replicas)
    log.info("\nIn fxLocalSetup fixture, Participants are {}\nCommitters are {}\n".format(
        client_hosts, concord_hosts))

    local_tuple = LocalSetupFixture(
        client_hosts=client_hosts, concord_hosts=concord_hosts, f_count=f_count)

    def fin():
        perform_sanity_check(reraise, local_tuple, fxHermesRunSettings)

    request.addfinalizer(fin)

    return local_tuple


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


def install_sdk_deploy_daml(client_host):
    '''
    Function to install DAML SDK and deploy the dar file
    on the client_host where Ledger API is running.
    Args:
        client_host: Host where ledger API is running
    Returns:
        None
    '''
    home = str(Path.home())
    daml_sdk_path = os.path.join(home, ".daml", "bin", "daml")
    cmd = [daml_sdk_path, "deploy", "--host",
           client_host, "--port", get_port(client_host)]
    party_project_dir = "util/daml/request_tool"
    success, output = helper.execute_ext_command(
        cmd, timeout=180, working_dir=party_project_dir, verbose=True)
    if "Party already exists" in output:
        assert False, "DAML Error: Party already exists."
    assert success, "DAML Error: Unable to deploy DAML app for one/more parties"


def staggered_start_committers(fxHermesRunSettings, concord_hosts):
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
        assert interrupt_node(fxHermesRunSettings, concord_host,
                              helper.TYPE_DAML_COMMITTER,
                              intr_helper.NODE_INTERRUPT_VM_STOP_START,
                              intr_helper.NODE_RECOVER), \
            COMMITTER_POWER_ON_ERROR_MSG + "[{}]".format(
                concord_host)
        log.info("Committer on index {}: {} is on".format(
            concord_host_index, concord_host))
        list_of_committer_indexes.remove(concord_host_index)
        sleep_time = random.randrange(5, 10)
        log.info("Sleeping for time {}".format(sleep_time))
        time.sleep(sleep_time)


def power_on_all_participants(fxHermesRunSettings, client_hosts):
    '''
    Function to switch on all participant nodes
    Args:
        fxHermesRunSettings: Hermes command line arguments
        client_hosts: List containing Participant IPs
    Returns:
        None
    '''
    for count, client_host in enumerate(client_hosts):
        assert interrupt_node(fxHermesRunSettings, client_host,
                              helper.TYPE_DAML_PARTICIPANT,
                              intr_helper.NODE_INTERRUPT_VM_STOP_START,
                              intr_helper.NODE_RECOVER), \
            PARTICIPANT_POWER_ON_ERROR_MSG + "[{}]".format(client_host)
        log.info("Participant no {} : {} is on".format(count + 1, client_host))


def power_off_committers(fxHermesRunSettings, concord_hosts, f_count=None):
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
        assert interrupt_node(fxHermesRunSettings, concord_host,
                              helper.TYPE_DAML_COMMITTER,
                              intr_helper.NODE_INTERRUPT_VM_STOP_START,
                              intr_helper.NODE_INTERRUPT), \
            COMMITTER_POWER_OFF_ERROR_MSG + "[{}]".format(
                concord_host)


def interrupt_node(fxHermesRunSettings, node, node_type, interruption_type,
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
    node_interruption_details = {
        intr_helper.NODE_INTERRUPTION_TYPE: interruption_type,
        intr_helper.NODE_TYPE_TO_INTERRUPT: node_type,
        intr_helper.SKIP_MASTER_REPLICA: False
    }
    if custom_params is None:
        node_interruption_details[intr_helper.CUSTOM_INTERRUPTION_PARAMS] = {}
    else:
        node_interruption_details[intr_helper.CUSTOM_INTERRUPTION_PARAMS] = \
            custom_params
    return intr_helper.perform_interrupt_recovery_operation(
        fxHermesRunSettings, fxBlockchain, None, node, node_interruption_details, mode)


def make_daml_request_in_thread(reraise, client_host, no_of_txns=1, wait_time=1):
    # Default port can be 6865 or 80 (helper.FORWARDED_DAML_LEDGER_API_ENDPOINT_PORT)
    # But after powering off and on the host, it listens on 6865 only
    # So using 6865 here
    log.info("\nStarting thread for daml transaction")
    url = 'http://{}:{}'.format(client_host, get_port(client_host))

    def make_daml_request(url, no_of_txns, wait_time):
        set_event_loop(new_event_loop())
        simple_request(url, no_of_txns, wait_time)

    thread_daml_txn = Thread(target=make_daml_request,
                             args=(url, no_of_txns, wait_time))
    thread_daml_txn.start()
    time.sleep(90)
    log.info("\nIs thread alive? {}".format(thread_daml_txn.isAlive()))
    if(thread_daml_txn.isAlive()):
        reraise()
        thread_daml_txn.join(10)
        return False
    else:
        return True


def get_port(client_host):
    client_port = '6861' if client_host == 'localhost' else DAML_LEDGER_API_PORT
    return client_port


def perform_sanity_check(reraise, fixture_tuple, fxHermesRunSettings):
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
        log.info("\nCheck if all the participant nodes are up")
        for client_host in fixture_tuple.client_hosts:
            # Check if client host is up
            assert interrupt_node(fxHermesRunSettings, client_host,
                                  helper.TYPE_DAML_PARTICIPANT,
                                  intr_helper.NODE_INTERRUPT_VM_STOP_START,
                                  intr_helper.NODE_RECOVER), \
                "Failed to power on participant {}".format(client_host)

        log.info("\nCheck if all the committer nodes are up")
        for concord_host in fixture_tuple.concord_hosts:
            # Check if concord host is up
            assert interrupt_node(fxHermesRunSettings, concord_host,
                                  helper.TYPE_DAML_COMMITTER,
                                  intr_helper.NODE_INTERRUPT_VM_STOP_START,
                                  intr_helper.NODE_RECOVER), \
                "Failed to power on committer {}".format(concord_host)

        cmd = "docker start daml_ledger_api; docker inspect --format {{.State.Status}} daml_ledger_api"
        helper.ssh_parallel(fixture_tuple.client_hosts, cmd, verbose=False)

        cmd = "docker start daml_index_db; docker inspect --format {{.State.Status}} daml_index_db"
        helper.ssh_parallel(fixture_tuple.client_hosts, cmd, verbose=False)

        cmd = "docker start concord; docker inspect --format {{.State.Status}} concord"
        helper.ssh_parallel(fixture_tuple.concord_hosts, cmd, verbose=False)

        log.info("\nPerform daml transaction as final check")
        for client_host in fixture_tuple.client_hosts:
            assert make_daml_request_in_thread(reraise, client_host), \
                PARTICIPANT_GENERIC_ERROR_MSG + "as part of sanity check"

        log.info("\n*** Sanity check successfully done for given test ***\n")
    except Exception as excp:
        assert False, excp


def stop_for_replica_list(replica_list, container_name, count):
    '''
    Function to stop committer nodes from list of replicas
    for maximum count provided
    Args:
        replica_list: List of committer IPs
        container_name: Name of the container
        count: Number of committer nodes to be stopped
    Returns:
        None
    '''
    log.info("Stopping process")
    for i in range(count):
        concord_host = replica_list[i]
        assert intr_helper.stop_container(
            concord_host, container_name), "Failed to stop committer node [{}]".format(concord_host)
    log.info("\nStopped {} replicas".format(count))


def start_for_replica_list(replica_list, container_name, count):
    '''
    Function to start committer nodes from list of replicas
    for maximum count provided
    Args:
        replica_list: List of committer IPs
        container_name: Name of the container
        count: Number of committer nodes to be started
    Returns:
        None
    '''
    for i in range(count):
        concord_host = replica_list[i]
        assert intr_helper.start_container(
            concord_host, container_name), "Failed to start committer node [{}]".format(concord_host)
    log.info("\nStarted {} replicas".format(count))


@pytest.mark.skip
@describe("daml test for single transaction without any interruption")
def test_daml_single_transaction(reraise, fxLocalSetup):
    '''
    Verify case by submitting sequential client requests using DAML tool.
    - Connect to a blockchain network.
    - Submit valid requests to it.
    - Verify that requests are processed correctly.
    Args:
        fxLocalSetup: Local fixture
    '''
    for client_host in fxLocalSetup.client_hosts:
        try:
            install_sdk_deploy_daml(client_host)

            # Create & verify transactions
            assert make_daml_request_in_thread(
                reraise, client_host), PARTICIPANT_GENERIC_ERROR_MSG

        except Exception as excp:
            assert False, excp


@pytest.mark.skip
@describe("fault tolerance - f replicas are stopped/started, powered off/on")
def test_daml_stop_start_replicas(reraise, fxLocalSetup, fxHermesRunSettings):
    '''
    Verify below using DAML tool.
    - Connect to a blockchain network.
    - Submit valid requests to it.
    - Verify that requests are processed correctly.
    - Crash f concord containers (It will recover automatically).
    - Continuously submit and verify requests.
    - Stop the f replicas.
    - Continuously submit and verify requests.
    - Restart the stopped replicas.
    - Verify that new requests are processed correctly.
    - Submit and verify requests for power off/on of committer nodes as well.
    Args:
        fxLocalSetup: Local fixture
        fxHermesRunSettings: Hermes command line arguments
    '''
    for client_host in fxLocalSetup.client_hosts:
        try:
            install_sdk_deploy_daml(client_host)

            # Create & verify transactions
            assert make_daml_request_in_thread(
                reraise, client_host), PARTICIPANT_GENERIC_ERROR_MSG

            # Crash/Recover concord containers
            for i in range(fxLocalSetup.f_count):
                concord_host = fxLocalSetup.concord_hosts[i]
                custom_params = {
                    intr_helper.CONTAINERS_TO_CRASH: ["concord"]
                }
                assert interrupt_node(fxHermesRunSettings, concord_host,
                                      helper.TYPE_DAML_COMMITTER,
                                      intr_helper.NODE_INTERRUPT_CONTAINER_CRASH,
                                      intr_helper.NODE_INTERRUPT,
                                      custom_params),\
                    "Failed to crash container [{}]".format(concord_host)

            # Create & verify transactions after f crash/recovery of concord
            assert make_daml_request_in_thread(reraise, client_host), \
                PARTICIPANT_GENERIC_ERROR_MSG + "after restarting f concord containers"

            # Power off f committer nodes
            for i in range(fxLocalSetup.f_count):
                concord_host = fxLocalSetup.concord_hosts[i]
                assert interrupt_node(fxHermesRunSettings, concord_host,
                                      helper.TYPE_DAML_COMMITTER,
                                      intr_helper.NODE_INTERRUPT_VM_STOP_START,
                                      intr_helper.NODE_INTERRUPT), \
                    COMMITTER_POWER_OFF_ERROR_MSG + "[{}]".format(
                        concord_host)

            # Create & verify transactions after powering off f committer nodes
            assert make_daml_request_in_thread(reraise, client_host), \
                PARTICIPANT_GENERIC_ERROR_MSG + "after powering off f committer nodes"

            # Power on f committer nodes
            for i in range(fxLocalSetup.f_count):
                concord_host = fxLocalSetup.concord_hosts[i]
                assert interrupt_node(fxHermesRunSettings, concord_host,
                                      helper.TYPE_DAML_COMMITTER,
                                      intr_helper.NODE_INTERRUPT_VM_STOP_START,
                                      intr_helper.NODE_RECOVER), \
                    COMMITTER_POWER_ON_ERROR_MSG + "[{}]".format(
                        concord_host)

            assert make_daml_request_in_thread(reraise, client_host), \
                PARTICIPANT_GENERIC_ERROR_MSG + "after powering on f committer nodes"

        except Exception as excp:
            assert False, excp


@pytest.mark.skip
@describe("fault tolerance - participant ledger api restarted, node is powered off/on")
def test_participant_ledgerapi_restart(reraise, fxLocalSetup,
                                       fxHermesRunSettings):
    '''
    Verify fault tolerance - stop/start participant node and submit
    sequential client requests using DAML tool.
    - Connect to a blockchain network.
    - Submit valid requests to it.
    - Verify that requests are processed correctly.
    - Stop and start participant's ledger api container.
    - Submit valid requests to it.
    - Verify that requests are processed correctly.
    - Submit and verify requests for power off/on of given participant node as well.
    Args:
        fxLocalSetup: Local fixture
        fxHermesRunSettings: Hermes command line arguments
    '''
    for client_host in fxLocalSetup.client_hosts:
        try:
            install_sdk_deploy_daml(client_host)

            # Create & verify transactions
            assert make_daml_request_in_thread(
                reraise, client_host), PARTICIPANT_GENERIC_ERROR_MSG

            custom_params = {
                intr_helper.CONTAINERS_TO_CRASH: ["daml_ledger_api"]
            }
            assert interrupt_node(fxHermesRunSettings, client_host,
                                  helper.TYPE_DAML_PARTICIPANT,
                                  intr_helper.NODE_INTERRUPT_CONTAINER_CRASH,
                                  intr_helper.NODE_INTERRUPT, custom_params), \
                "Failed to crash container [{}]".format(client_host)

            # Create & verify transactions
            assert make_daml_request_in_thread(reraise, client_host), \
                PARTICIPANT_GENERIC_ERROR_MSG + "after restarting ledger api container"

            # Power off Participant node
            assert interrupt_node(fxHermesRunSettings, client_host,
                                  helper.TYPE_DAML_PARTICIPANT,
                                  intr_helper.NODE_INTERRUPT_VM_STOP_START,
                                  intr_helper.NODE_INTERRUPT), \
                PARTICIPANT_POWER_OFF_ERROR_MSG + "[{}]".format(client_host)

            # Power on Participant node
            assert interrupt_node(fxHermesRunSettings, client_host,
                                  helper.TYPE_DAML_PARTICIPANT,
                                  intr_helper.NODE_INTERRUPT_VM_STOP_START,
                                  intr_helper.NODE_RECOVER), \
                PARTICIPANT_POWER_ON_ERROR_MSG + "[{}]".format(client_host)

            assert make_daml_request_in_thread(reraise, client_host), \
                PARTICIPANT_GENERIC_ERROR_MSG + "after powering on participant node"

        except Exception as excp:
            assert False, excp


@pytest.mark.skip
@describe("fault tolerance - participant node powered off/on, ledger api, index db restarted")
def test_participant_ledgerapi_indexdb_restart(reraise, fxLocalSetup, fxHermesRunSettings):
    '''
    Verify below using DAML tool.
    - Connect to a blockchain network.
    - Submit a small number of sequential client requests.
    - Restart the participant node to which the client is connected.
    - Verify that the client application can reconnect to the participant node.
    - Continuously submit and verify requests.
    - Repeat it for Ledger API and Index DB as well.
    Args:
        fxLocalSetup: Local fixture
        fxHermesRunSettings: Hermes command line arguments
    '''
    for client_host in fxLocalSetup.client_hosts:
        try:
            install_sdk_deploy_daml(client_host)

            # Create & verify transactions
            assert make_daml_request_in_thread(reraise, client_host), \
                PARTICIPANT_GENERIC_ERROR_MSG

            # Power off Participant node
            assert interrupt_node(fxHermesRunSettings, client_host,
                                  helper.TYPE_DAML_PARTICIPANT,
                                  intr_helper.NODE_INTERRUPT_VM_STOP_START,
                                  intr_helper.NODE_INTERRUPT), \
                PARTICIPANT_POWER_OFF_ERROR_MSG + "[{}]".format(client_host)

            # Power on Participant node
            assert interrupt_node(fxHermesRunSettings, client_host,
                                  helper.TYPE_DAML_PARTICIPANT,
                                  intr_helper.NODE_INTERRUPT_VM_STOP_START,
                                  intr_helper.NODE_RECOVER), \
                PARTICIPANT_POWER_ON_ERROR_MSG + "[{}]".format(client_host)

            # Create & verify transactions
            assert make_daml_request_in_thread(reraise, client_host), \
                PARTICIPANT_GENERIC_ERROR_MSG + "after powering on participant node"

            # Crash and recover daml ledger api & index db containers
            custom_params = {
                intr_helper.CONTAINERS_TO_CRASH: [
                    "daml_ledger_api", "daml_index_db"]
            }
            assert interrupt_node(fxHermesRunSettings, client_host,
                                  helper.TYPE_DAML_PARTICIPANT,
                                  intr_helper.NODE_INTERRUPT_CONTAINER_CRASH,
                                  intr_helper.NODE_INTERRUPT, custom_params), \
                "Failed to crash container(s) [{}]".format(client_host)

            assert make_daml_request_in_thread(reraise, client_host), \
                PARTICIPANT_GENERIC_ERROR_MSG + "after restarting client containers"

        except Exception as excp:
            assert False, excp


@pytest.mark.skip
@describe("fault tolerance - recovery after checkpoints (network failure), f nodes powered off/on")
def test_daml_network_failure(reraise, fxLocalSetup, fxHermesRunSettings):
    '''
    Verify below using DAML tool.
    - Connect to a blockchain network.
    - Submit valid requests enough to trigger multiple checkpoints. ** Will be done later **
    - Disable the network interface on all replicas.
    - Enable the network interface on all replicas.
    - Submit a small number of sequential client requests
    - Verify that new requests are processed correctly.
    - Submit and verify requests for power off/on of f committer nodes as well.    
    Args:
        fxLocalSetup: Local fixture
        fxHermesRunSettings: Hermes command line arguments
    '''
    for client_host in fxLocalSetup.client_hosts:
        try:
            install_sdk_deploy_daml(client_host)

            # Create & verify transactions
            assert make_daml_request_in_thread(
                reraise, client_host), PARTICIPANT_GENERIC_ERROR_MSG

            # Disconnect network of all committer nodes
            custom_params = {
                intr_helper.NETWORK_DISCONNECT_LEVEL: intr_helper.NETWORK_DISCONNECT_VM_LEVEL
            }

            for index in range(len(fxLocalSetup.concord_hosts)):
                concord_host = fxLocalSetup.concord_hosts[index]
                assert interrupt_node(fxHermesRunSettings, concord_host,
                                      helper.TYPE_DAML_COMMITTER,
                                      intr_helper.NODE_INTERRUPT_NETWORK_DISCONNECT,
                                      intr_helper.NODE_INTERRUPT,
                                      custom_params), \
                    "Failed to disconnect committer node [{}]".format(
                        concord_host)

            # Reconnect network of all committer nodes
            for index in range(len(fxLocalSetup.concord_hosts)):
                concord_host = fxLocalSetup.concord_hosts[index]
                assert interrupt_node(fxHermesRunSettings, concord_host,
                                      helper.TYPE_DAML_COMMITTER,
                                      intr_helper.NODE_INTERRUPT_NETWORK_DISCONNECT,
                                      intr_helper.NODE_RECOVER,
                                      custom_params), \
                    "Failed to reconnect committer node [{}]".format(
                        concord_host)

            # Create & verify transactions after disconnect/reconnect of all committer nodes
            assert make_daml_request_in_thread(reraise, client_host), \
                PARTICIPANT_GENERIC_ERROR_MSG + "after disconnect/reconnect of all committer nodes"

            log.info(
                "\nTransaction successful after reconnecting all committer nodes")

            # Power off f committer nodes
            for index in range(fxLocalSetup.f_count):
                concord_host = fxLocalSetup.concord_hosts[index]
                assert interrupt_node(fxHermesRunSettings, concord_host,
                                      helper.TYPE_DAML_COMMITTER,
                                      intr_helper.NODE_INTERRUPT_VM_STOP_START,
                                      intr_helper.NODE_INTERRUPT), \
                    COMMITTER_POWER_OFF_ERROR_MSG + "[{}]".format(
                        concord_host)

            # Create & verify transactions after powering off f committer nodes
            time.sleep(20)
            assert make_daml_request_in_thread(reraise, client_host), \
                PARTICIPANT_GENERIC_ERROR_MSG + "after powering off f committer nodes"

            log.info(
                "\nTransaction successful after powering off f committer nodes")

            # Power on f committer nodes
            for index in range(fxLocalSetup.f_count):
                concord_host = fxLocalSetup.concord_hosts[index]
                assert interrupt_node(fxHermesRunSettings, concord_host,
                                      helper.TYPE_DAML_COMMITTER,
                                      intr_helper.NODE_INTERRUPT_VM_STOP_START,
                                      intr_helper.NODE_RECOVER), \
                    COMMITTER_POWER_ON_ERROR_MSG + "[{}]".format(
                        concord_host)

            assert make_daml_request_in_thread(reraise, client_host), \
                PARTICIPANT_GENERIC_ERROR_MSG + "after powering on f committer nodes"

        except Exception as excp:
            assert False, excp


@pytest.mark.skip
@describe("fault tolerance - requests not to be processed without quorum")
@pytest.mark.parametrize("step", [0, 1])
def test_requests_processed_only_with_quorum(reraise, fxLocalSetup, fxHermesRunSettings, fxBlockchain, step):
    '''
    Verify below using DAML tool.
    - Connect to a blockchain network.
    - Stop f + 1 non-primary replicas.
    - Submit and verify requests, it is expected to fail.
    - Restart the stopped replicas.
    - Verify that new requests are processed correctly.
    - Stop f non-primary replicas and primary replica.
    - Submit and verify requests, it is expected to fail.
    - Restart the stopped replicas.
    - Verify that new requests are processed correctly.
    Args:
        fxLocalSetup: Local fixture
        fxHermesRunSettings: Hermes command line arguments
    '''
    for client_host in fxLocalSetup.client_hosts:
        try:
            # Install sdk only for one iteration
            if step == 0:
                install_sdk_deploy_daml(client_host)

            # Find primary replica and primary replica id
            replicas_mapping = blockchain_ops.map_committers_info(fxBlockchain)
            primary_rip = replicas_mapping["primary_ip"]
            log.info("Primary Replica IP: {}".format(primary_rip))

            all_replicas = fxLocalSetup.concord_hosts[:]
            container_name = 'concord'

            non_primary_replicas = [
                n for n in all_replicas if n != primary_rip]
            primary_and_non_primary_replicas = [
                primary_rip] + [n for n in non_primary_replicas]

            # Step 0 - f+1 non primary replicas to be stopped
            # Step 1 - f non primary and 1 primary replicas to be stopped
            if step == 0:
                stop_for_replica_list(
                    non_primary_replicas, container_name, fxLocalSetup.f_count + 1)
            else:
                stop_for_replica_list(
                    primary_and_non_primary_replicas, container_name, fxLocalSetup.f_count + 1)

            # Transactions should fail at this moment
            success = make_daml_request_in_thread(reraise, client_host, 1)
            log.info("daml txn after f+1 down is {}".format(success))
            if success:
                log.info(
                    "Daml transaction was successful even with f+1 replicas down")
                assert False, "Expected daml transaction to fail after f+1 replicas are down"

            # Step 0 - f+1 non primary replicas to be started
            # Step 1 - f non primary and 1 primary replicas to be started
            if step == 0:
                start_for_replica_list(
                    non_primary_replicas, container_name, fxLocalSetup.f_count + 1)
            else:
                start_for_replica_list(
                    primary_and_non_primary_replicas, container_name, fxLocalSetup.f_count + 1)

            # Create & verify transactions after powering on stopped replicas
            assert make_daml_request_in_thread(reraise, client_host), \
                PARTICIPANT_GENERIC_ERROR_MSG

        except Exception as excp:
            assert False, excp


@pytest.mark.skip
@describe("fault tolerance - nodes started with staggered startup")
@pytest.mark.parametrize("participant_first", [True, False])
def test_system_after_staggered_startup(reraise, fxLocalSetup, fxHermesRunSettings, participant_first):
    '''
    Verify below using DAML tool.
    - Connect to a blockchain network.
    - Submit valid requests to it.
    - Verify that requests are processed correctly.
    - Stop all nodes
    - Start the participant Nodes
    - Perform the staggered startup on replica nodes
    - Continuously submit and verify requests.
    - Verify that new requests are processed correctly.
    Args:
        fxLocalSetup: Local fixture
        fxHermesRunSettings: Hermes command line arguments
    '''
    for count, client_host in enumerate(fxLocalSetup.client_hosts):
        # Install sdk only for one iteration
        if participant_first:
            install_sdk_deploy_daml(client_host)

        # Create & verify transactions
        assert make_daml_request_in_thread(
            reraise, client_host), PARTICIPANT_GENERIC_ERROR_MSG

        log.info("Participant off {}: {}".format(
            count + 1, client_host))

        # Power off participant node
        assert interrupt_node(fxHermesRunSettings, client_host,
                              helper.TYPE_DAML_PARTICIPANT,
                              intr_helper.NODE_INTERRUPT_VM_STOP_START,
                              intr_helper.NODE_INTERRUPT), \
            "Failed to power off participant node [{}]".format(client_host)

    # Power off all committer Nodes
    power_off_committers(fxHermesRunSettings, fxLocalSetup.concord_hosts)

    if participant_first:
        # Power on all participant nodes
        power_on_all_participants(
            fxHermesRunSettings, fxLocalSetup.client_hosts)

        # Power on all committer nodes
        staggered_start_committers(
            fxHermesRunSettings, fxLocalSetup.concord_hosts)
    else:
        # Power on all committer nodes
        staggered_start_committers(
            fxHermesRunSettings, fxLocalSetup.concord_hosts)

        # Power on all participant nodes
        power_on_all_participants(
            fxHermesRunSettings, fxLocalSetup.client_hosts)

    # Create & verify transactions after powering on all nodes
    for client_host in fxLocalSetup.client_hosts:
        assert make_daml_request_in_thread(reraise, client_host), \
            PARTICIPANT_GENERIC_ERROR_MSG + "after staggered startup"


@describe("fault tolerance - view change")
def test_fault_tolerance_view_change(reraise, fxLocalSetup, fxHermesRunSettings, fxBlockchain):
    '''
    Verify fault tolerance using below steps:
    - Connect to a blockchain network.
    - Stop f-1 non primary replica.
    - Submit client requests continuously.
    - Stop and start current primary (same) node continuously.
    - Submit client requests.
    - Verify that requests are processed correctly.
    Args:
        fxLocalSetup: Local fixture
        fxHermesRunSettings: Hermes command line arguments
        fxBlockchain: blockchain
    '''
    for client_host in fxLocalSetup.client_hosts:
        try:
            install_sdk_deploy_daml(client_host)

            # Submit Daml requests before finding primary replica
            assert make_daml_request_in_thread(
                reraise, client_host), PARTICIPANT_GENERIC_ERROR_MSG

            committers_mapping = blockchain_ops.map_committers_info(
                fxBlockchain)

            # Find primary replica ip
            init_primary_rip = committers_mapping["primary_ip"]
            log.info("Initial Primary Replica IP is : {}".format(
                init_primary_rip))

            interrupted_nodes = []
            container_name = 'concord'

            non_primary_replicas = fxLocalSetup.concord_hosts[:]
            non_primary_replicas.remove(init_primary_rip)

            # Stop f-1 non-primary replica
            for index in range(fxLocalSetup.f_count - 1):
                concord_host = non_primary_replicas[index]
                log.info(
                    "Stop concord in non-primary replica: {}".format(concord_host))
                intr_helper.stop_container(concord_host, container_name)
                interrupted_nodes.append(concord_host)

            log.info("Stopped f-1 non-primary replica concord containers")
            log.info("\nInterrupted nodes are : {}".format(interrupted_nodes))

            # Start new thread for daml request submissions and stop & start current primary replica
            thread_daml_txn = Thread(target=continuous_daml_request_submission,
                                     args=(client_host, get_port(client_host), 2, 0.2, 180))
            thread_stop_start_primary = Thread(target=intr_helper.continuous_stop_start_container,
                                     args=(init_primary_rip, container_name, 180))
            interrupted_nodes.append(init_primary_rip)

            log.info("\nAfter adding primary replica to interrupted nodes are : {}".format(
                interrupted_nodes))
            threads_list = []
            log.info("\nStarting Daml transaction thread")
            thread_daml_txn.start()
            log.info("\nStarting Stop start primary thread")
            thread_stop_start_primary.start()
            threads_list.append(thread_daml_txn)
            threads_list.append(thread_stop_start_primary)
            for thread in threads_list:
                thread.join(200)

            for thread in threads_list:
                log.info("\nIs thread alive? {}".format(thread.isAlive()))
                if(thread.isAlive()):
                    log.info("\nKilling this thread in 10 seconds")
                    thread.join(10)
                    assert False, "Failure during multiple view changes & daml request submission"

            # Find new primary replica ip
            committers_mapping = blockchain_ops.map_committers_info(
                fxBlockchain, interrupted_nodes)
            new_primary_rip = committers_mapping["primary_ip"]

            log.info("New Primary Replica IP is : {}".format(new_primary_rip))

            assert init_primary_rip != new_primary_rip, "View Change did not happen successfully"

            # Submit Daml requests after view change
            assert make_daml_request_in_thread(reraise, client_host), \
                PARTICIPANT_GENERIC_ERROR_MSG + "after view change"

        except Exception as excp:
            assert False, excp


@describe("fault tolerance - multiple view changes")
def test_fault_tolerance_after_multiple_view_changes(reraise, fxLocalSetup, fxHermesRunSettings, fxBlockchain):
    '''
    Verify fault tolerance view changes after multiple using below steps:
    - Connect to a blockchain network.
    - Stop f-1 non primary replica.
    - Submit client requests continuously.
    - Stop and start current primary node multiple times.
    - Submit client requests.
    - Verify that requests are processed correctly.
    Args:
        fxLocalSetup: Local fixture
        fxHermesRunSettings: Hermes command line arguments
        fxBlockchain: blockchain
    '''
    for client_host in fxLocalSetup.client_hosts:
        try:
            install_sdk_deploy_daml(client_host)

            # Submit Daml requests before finding primary replica
            assert make_daml_request_in_thread(
                reraise, client_host), PARTICIPANT_GENERIC_ERROR_MSG

            # Find primary replica ip
            committers_mapping = blockchain_ops.map_committers_info(
                fxBlockchain)
            init_primary_rip = committers_mapping["primary_ip"]
            log.info("Primary Replica IP is : {}".format(init_primary_rip))

            interrupted_nodes = []
            container_name = 'concord'

            # List of f non-primary replicas
            non_primary_replicas = fxLocalSetup.concord_hosts[:]
            non_primary_replicas.remove(init_primary_rip)

            # Stop f-1 non-primary replica
            for i in range(fxLocalSetup.f_count - 1):
                concord_host = non_primary_replicas[i]
                intr_helper.stop_container(concord_host, container_name)
                interrupted_nodes.append(concord_host)

            log.info("Stopped f-1 non-primary replica concord containers")
            log.info("\nInterrupted nodes are : {}".format(interrupted_nodes))

            # Start new thread for daml request submissions
            thread_daml_txn = Thread(target=continuous_daml_request_submission,
                                     args=(client_host, get_port(client_host), 1, 0.2, 900))
            thread_daml_txn.start()

            # Stop and Restart the current primary replicas multiple times
            for _ in range(0, 7):
                committers_mapping = blockchain_ops.map_committers_info(
                    fxBlockchain, interrupted_nodes)
                primary_rip = committers_mapping["primary_ip"]
                intr_helper.stop_container(primary_rip, container_name, 30)
                intr_helper.start_container(primary_rip, container_name, 90)

            thread_daml_txn.join()
            time.sleep(60)
            log.info("\nIs thread alive? {}".format(thread_daml_txn.isAlive()))
            if(thread_daml_txn.isAlive()):
                log.info("\nKilling this thread in 10 seconds")
                thread_daml_txn.join(10)
                assert False, "during multiple view changes"

            # Submit Daml requests after multiple view changes
            assert make_daml_request_in_thread(reraise, client_host), \
                PARTICIPANT_GENERIC_ERROR_MSG + "after multiple view changes"

        except Exception as excp:
            assert False, excp
