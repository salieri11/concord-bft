#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
from datetime import datetime, timedelta
from suites.case import describe
from collections import namedtuple
import pytest
import time
import random
from util import helper, hermes_logging, blockchain_ops, wavefront
from threading import Thread
import util.daml.daml_helper as daml_helper
from util.daml.daml_requests import simple_request, continuous_daml_request_submission
import util.node_interruption_helper as intr_helper
import asyncio
from fixtures.common_fixtures import fxNodeInterruption, fxBlockchain, fxProduct
import json
from subprocess import check_output, CalledProcessError

log = hermes_logging.getMainLogger()

LocalSetupFixture = namedtuple(
    "LocalSetupFixture", "client_hosts, concord_hosts, f_count")


@pytest.fixture(scope="function")
@describe("fixture; local setup for given test suite")
def fxLocalSetup(fxHermesRunSettings, fxNodeInterruption, fxBlockchain, fxProduct):
    '''
    Module scoped fixture to enable all the test cases utilize common func.
    Functionalities:
        - Installation of DAML SDK (if not already there)
        - DAML deployment
    Args:
        fxHermesRunSettings: Hermes command line arguments.
        fxBlockchain: Blockchain fixture required to get f_count
        fxNodeInterruption: Node Interruption fixture for related functions
        fxProduct: Product fixture
    Returns:
        client_hosts: Participant hosts (and ports) with running ledger API
        concord_hosts: Concord (committer) replicas
        f_count: Maximum faulty replicas allowed.
    '''
    ledger_api_hosts, concord_hosts = None, None

    # Get Client IP from Replicas config file
    args = fxHermesRunSettings["hermesCmdlineArgs"]
    if args.replicasConfig:
        all_replicas = helper.parseReplicasConfig(args.replicasConfig)
        ledger_api_hosts = all_replicas["daml_participant"]
        concord_hosts = all_replicas["daml_committer"]
    else:
        ledger_api_hosts = args.damlParticipantIP.split(",")

    f_count = intr_helper.get_f_count(fxBlockchain)
    client_hosts = {}
    for ledger_api_host in ledger_api_hosts:
        if ledger_api_host == 'localhost':
            upload_port = '6861'
        else:
            upload_port = '6865'
        client_hosts[ledger_api_host] = upload_port
    return LocalSetupFixture(client_hosts=client_hosts,
                             concord_hosts=concord_hosts, f_count=f_count)


def install_sdk_deploy_daml(client_host, client_port):
    '''
    Function to install DAML SDK and deploy the dar file
    on the client_host where Ledger API is running.
    Args:
        client_host: Host where ledger API is running
        client_port: Port on which ledger API is running
    Returns:
        None
    '''
    daml_sdk_version = daml_helper.get_ledger_api_version(client_host)
    daml_sdk_path = daml_helper.install_daml_sdk(daml_sdk_version)
    cmd = [daml_sdk_path, "deploy", "--host",
           client_host, "--port", client_port]
    party_project_dir = "util/daml/request_tool"
    success, output = helper.execute_ext_command(
        cmd, timeout=150, working_dir=party_project_dir, verbose=True)

    assert success or "Party already exists" in output, \
        "Unable to deploy DAML app for one/more parties"


def staggered_startup_of_committers(fxHermesRunSettings, concord_hosts):
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
        log.info("Committer on {}: {}".format(
            concord_host_index, concord_host))
        assert interrupt_node(fxHermesRunSettings, concord_host,
                              helper.TYPE_DAML_COMMITTER,
                              intr_helper.NODE_INTERRUPT_VM_STOP_START,
                              intr_helper.NODE_RECOVER), \
            "Failed to power on committer node [{}]".format(
                concord_host)
        list_of_committer_indexes.remove(concord_host_index)
        sleep_time = random.randrange(5, 15)
        log.info("Sleeping for time {}".format(sleep_time))
        time.sleep(sleep_time)


def power_on_all_participants(fxHermesRunSettings, client_host_items):
    '''
    Function to switch on all participant nodes
    Args:
        fxHermesRunSettings: Hermes command line arguments
        client_host_items: List containing pairs of participant IPs and their corresponding port
    Returns:
        None
    '''
    client_on_count = 1
    for client_host in client_host_items:
        log.info("Participant on {}: {}".format(
            client_on_count, client_host[0]))
        assert interrupt_node(fxHermesRunSettings, client_host[0],
                              helper.TYPE_DAML_PARTICIPANT,
                              intr_helper.NODE_INTERRUPT_VM_STOP_START,
                              intr_helper.NODE_RECOVER), \
            "Failed to power on participant node [{}]".format(client_host)
        client_on_count = client_on_count + 1


# If f_count is passed to this method, then it powers off only f nodes. Otherwise all nodes are powered off.
def power_off_committers(fxHermesRunSettings, concord_hosts, f_count=None):
    '''
    Function to power off all or f committer nodes
    Args:
        fxHermesRunSettings: Hermes command line arguments
        concord_hosts: List of Committer Node IPs
        f_count: If passed, then the number of f nodes is passed
    Returns:
        None
    '''
    nodes_switched_off = 0
    for concord_host in concord_hosts:
        if f_count and f_count == nodes_switched_off:
            break
        assert interrupt_node(fxHermesRunSettings, concord_host,
                              helper.TYPE_DAML_COMMITTER,
                              intr_helper.NODE_INTERRUPT_VM_STOP_START,
                              intr_helper.NODE_INTERRUPT), \
            "Failed to power off committer node [{}]".format(
                concord_host)
        nodes_switched_off = nodes_switched_off + 1


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


@describe("daml test for single transaction")
def test_daml_single_transaction(fxLocalSetup):
    '''
    Verify case by submitting sequential client requests using DAML tool.
    - Connect to a blockchain network.
    - Submit valid requests to it.
    - Verify that requests are processed correctly.
    Args:
        fxLocalSetup: Local fixture
    '''
    for client_host, client_port in fxLocalSetup.client_hosts.items():
        try:
            install_sdk_deploy_daml(client_host, client_port)

            no_of_txns, wait_time = 2, 1
            url = 'http://{}:{}'.format(client_host, client_port)
            # Create & verify transactions of count no_of_txns
            assert simple_request(url, no_of_txns, wait_time), \
                "DAML request submission/verification failed"
        except Exception as excp:
            assert False, excp


@describe("fault tolerance when f replicas are stopped/started")
def test_daml_stop_start_replicas(fxLocalSetup, fxHermesRunSettings):
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
    Args:
        fxLocalSetup: Local fixture
        fxHermesRunSettings: Hermes command line arguments
    '''
    for client_host, client_port in fxLocalSetup.client_hosts.items():
        try:
            install_sdk_deploy_daml(client_host, client_port)

            no_of_txns, wait_time = 2, 1
            url = 'http://{}:{}'.format(client_host, client_port)
            assert simple_request(url, no_of_txns), \
                "DAML request submission/verification failed for participant \
                [{}]".format(client_host)

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
            assert simple_request(url, no_of_txns), \
                "DAML request submission/verification failed after \
                crash/recovery of {} concord containers"\
                .format(fxLocalSetup.f_count)

            # Power off f committer nodes
            for i in range(fxLocalSetup.f_count):
                concord_host = fxLocalSetup.concord_hosts[i]
                assert interrupt_node(fxHermesRunSettings, concord_host,
                                      helper.TYPE_DAML_COMMITTER,
                                      intr_helper.NODE_INTERRUPT_VM_STOP_START,
                                      intr_helper.NODE_INTERRUPT), \
                    "Failed to power off committer node [{}]".format(
                        concord_host)

            # Create & verify transactions after powering off f committer nodes
            assert simple_request(url, no_of_txns), \
                "DAML request submission/verification failed after \
                powering off {} committers".format(fxLocalSetup.f_count)

            # Power on f committer nodes
            for i in range(fxLocalSetup.f_count):
                concord_host = fxLocalSetup.concord_hosts[i]
                assert interrupt_node(fxHermesRunSettings, concord_host,
                                      helper.TYPE_DAML_COMMITTER,
                                      intr_helper.NODE_INTERRUPT_VM_STOP_START,
                                      intr_helper.NODE_RECOVER), \
                    "Failed to power on committer node [{}]".format(
                        concord_host)

            # Create & verify transactions after powering on f committer nodes
            assert simple_request(url, no_of_txns), \
                "DAML request submission/verification failed after powering \
                on {} committer nodes".format(fxLocalSetup.f_count)

        except Exception as excp:
            assert False, excp


@describe("verify fault tolerance - participant restart")
def test_fault_tolerance_participant_restart(fxLocalSetup,
                                             fxHermesRunSettings):
    '''
    Verify fault tolerance - stop/start participant node and submit
    sequential client requests using DAML tool.
    - Connect to a blockchain network.
    - Submit valid requests to it.
    - Verify that requests are processed correctly.
    - Stop and start participant node.
    - Submit valid requests to it.
    - Verify that requests are processed correctly.
    Args:
        fxLocalSetup: Local fixture
        fxHermesRunSettings: Hermes command line arguments
    '''
    for client_host, client_port in fxLocalSetup.client_hosts.items():
        try:
            install_sdk_deploy_daml(client_host, client_port)

            # Create & verify transactions
            no_of_txns, wait_time = 2, 1
            url = 'http://{}:{}'.format(client_host, client_port)

            assert simple_request(url, no_of_txns, wait_time), \
                "DAML request submission/verification failed for participant \
                [{}]".format(client_host)

            custom_params = {
                intr_helper.CONTAINERS_TO_CRASH: ["daml_ledger_api"]
            }
            assert interrupt_node(fxHermesRunSettings, client_host,
                                  helper.TYPE_DAML_PARTICIPANT,
                                  intr_helper.NODE_INTERRUPT_CONTAINER_CRASH,
                                  intr_helper.NODE_INTERRUPT, custom_params), \
                "Failed to crash container [{}]".format(client_host)

            # Create & verify transactions
            assert simple_request(url, no_of_txns, wait_time), \
                "DAML request submission/verification failed for participant \
                [{}]".format(client_host)

            # Power off Participant node
            assert interrupt_node(fxHermesRunSettings, client_host,
                                  helper.TYPE_DAML_PARTICIPANT,
                                  intr_helper.NODE_INTERRUPT_VM_STOP_START,
                                  intr_helper.NODE_INTERRUPT), \
                "Failed to power off participant node [{}]".format(client_host)

            # Power on Participant node
            assert interrupt_node(fxHermesRunSettings, client_host,
                                  helper.TYPE_DAML_PARTICIPANT,
                                  intr_helper.NODE_INTERRUPT_VM_STOP_START,
                                  intr_helper.NODE_RECOVER), \
                "Failed to power on participant node [{}]".format(client_host)

            # Create & verify transactions
            assert simple_request(url, no_of_txns, wait_time), \
                "DAML request submission/verification failed for participant \
                [{}]".format(client_host)

        except Exception as excp:
            assert False, excp


@describe("fault tolerance on restart of participant,ledger api, index db")
def test_participant_ledgerapi_indexdb_restart(fxLocalSetup, fxHermesRunSettings):
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
    for client_host, client_port in fxLocalSetup.client_hosts.items():
        try:
            install_sdk_deploy_daml(client_host, client_port)

            no_of_txns, wait_time = 2, 1
            url = 'http://{}:{}'.format(client_host, client_port)

            assert simple_request(url, no_of_txns), \
                "DAML request submission/verification failed for participant \
                [{}]".format(client_host)

            # Power off Participant node
            assert interrupt_node(fxHermesRunSettings, client_host,
                                  helper.TYPE_DAML_PARTICIPANT,
                                  intr_helper.NODE_INTERRUPT_VM_STOP_START,
                                  intr_helper.NODE_INTERRUPT), \
                "Failed to power off participant node [{}]".format(client_host)

            # Power on Participant node
            assert interrupt_node(fxHermesRunSettings, client_host,
                                  helper.TYPE_DAML_PARTICIPANT,
                                  intr_helper.NODE_INTERRUPT_VM_STOP_START,
                                  intr_helper.NODE_RECOVER), \
                "Failed to power on participant node [{}]".format(client_host)

            # Create & verify transactions
            assert simple_request(url, no_of_txns, wait_time), \
                "DAML request submission/verification after restart of participant \
                [{}]".format(client_host)

            # Crash and recover daml ledger api container
            custom_params = {
                intr_helper.CONTAINERS_TO_CRASH: ["daml_ledger_api"]
            }
            assert interrupt_node(fxHermesRunSettings, client_host,
                                  helper.TYPE_DAML_PARTICIPANT,
                                  intr_helper.NODE_INTERRUPT_CONTAINER_CRASH,
                                  intr_helper.NODE_INTERRUPT, custom_params), \
                "Failed to crash container [{}]".format(client_host)

            # Create & verify transactions
            assert simple_request(url, no_of_txns, wait_time), \
                "DAML request submission/verification failed after daml_ledger_api container crash for \
                [{}]".format(client_host)

            # Crash and recover daml index db container
            custom_params = {
                intr_helper.CONTAINERS_TO_CRASH: ["daml_index_db"]
            }
            assert interrupt_node(fxHermesRunSettings, client_host,
                                  helper.TYPE_DAML_PARTICIPANT,
                                  intr_helper.NODE_INTERRUPT_CONTAINER_CRASH,
                                  intr_helper.NODE_INTERRUPT, custom_params), \
                "Failed to crash container [{}]".format(client_host)

            # Create & verify transactions
            assert simple_request(url, no_of_txns, wait_time), \
                "DAML request submission/verification failed after daml_index_db container crash for \
                [{}]".format(client_host)
        except Exception as excp:
            assert False, excp


@describe("recovery test after checkpoints (network failure)")
def test_daml_network_failure(fxLocalSetup, fxHermesRunSettings):
    '''
    Verify below using DAML tool.
    - Connect to a blockchain network.
    - Submit valid requests enough to trigger multiple checkpoints. ** Will be done later **
    - Disable the network interface on all replicas.
    - Enable the network interface on all replicas.
    - Submit a small number of sequential client requests
    - Verify that new requests are processed correctly.
    Args:
        fxLocalSetup: Local fixture
        fxHermesRunSettings: Hermes command line arguments
    '''
    for client_host, client_port in fxLocalSetup.client_hosts.items():
        try:
            install_sdk_deploy_daml(client_host, client_port)

            no_of_txns, wait_time = 2, 1
            url = 'http://{}:{}'.format(client_host, client_port)

            assert simple_request(url, no_of_txns), \
                "DAML request submission/verification failed for participant \
                [{}]".format(client_host)

            # Disconnect network of all committer nodes
            custom_params = {
                intr_helper.NETWORK_DISCONNECT_LEVEL: intr_helper.NETWORK_DISCONNECT_VM_LEVEL
            }

            for i in range(len(fxLocalSetup.concord_hosts)):
                concord_host = fxLocalSetup.concord_hosts[i]
                assert interrupt_node(fxHermesRunSettings, concord_host,
                                      helper.TYPE_DAML_COMMITTER,
                                      intr_helper.NODE_INTERRUPT_NETWORK_DISCONNECT,
                                      intr_helper.NODE_INTERRUPT,
                                      custom_params), \
                    "Failed to disconnect committer node [{}]".format(
                        concord_host)

            # Reconnect network of all committer nodes
            for i in range(len(fxLocalSetup.concord_hosts)):
                concord_host = fxLocalSetup.concord_hosts[i]
                assert interrupt_node(fxHermesRunSettings, concord_host,
                                      helper.TYPE_DAML_COMMITTER,
                                      intr_helper.NODE_INTERRUPT_NETWORK_DISCONNECT,
                                      intr_helper.NODE_RECOVER,
                                      custom_params), \
                    "Failed to reconnect committer node [{}]".format(
                        concord_host)

            # Create & verify transactions after disconnect/reconnect of all committer nodes
            assert simple_request(url, no_of_txns), \
                "DAML request submission/verification failed after network \
                    disconnect and reconnect for all committer nodes"

            # Power off f committer nodes
            for i in range(fxLocalSetup.f_count):
                concord_host = fxLocalSetup.concord_hosts[i]
                assert interrupt_node(fxHermesRunSettings, concord_host,
                                      helper.TYPE_DAML_COMMITTER,
                                      intr_helper.NODE_INTERRUPT_VM_STOP_START,
                                      intr_helper.NODE_INTERRUPT), \
                    "Failed to power off committer node [{}]".format(
                        concord_host)

            # Create & verify transactions after powering off f committer nodes
            assert simple_request(url, no_of_txns), \
                "DAML request submission/verification failed after \
                powering off {} committers".format(fxLocalSetup.f_count)

            # Power on f committer nodes
            for i in range(fxLocalSetup.f_count):
                concord_host = fxLocalSetup.concord_hosts[i]
                assert interrupt_node(fxHermesRunSettings, concord_host,
                                      helper.TYPE_DAML_COMMITTER,
                                      intr_helper.NODE_INTERRUPT_VM_STOP_START,
                                      intr_helper.NODE_RECOVER), \
                    "Failed to power on committer node [{}]".format(
                        concord_host)

            # Create & verify transactions after powering on f committer nodes
            assert simple_request(url, no_of_txns), \
                "DAML request submission/verification failed after powering \
                on {} committer nodes".format(fxLocalSetup.f_count)
        except Exception as excp:
            assert False, excp


@describe("verify fault tolerance - view change")
def test_fault_tolerance_view_change(fxLocalSetup, fxHermesRunSettings, fxBlockchain):
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
    for client_host, client_port in fxLocalSetup.client_hosts.items():
        try:
            install_sdk_deploy_daml(client_host, client_port)

            no_of_txns, wait_time = 2, 0.2
            url = 'http://{}:{}'.format(client_host, client_port)

            # Submit Daml requests before finding primary replica
            assert simple_request(url, no_of_txns, wait_time), \
                "DAML request submission/verification failed for participant \
            [{}]".format(client_host)

            committers_mapping = blockchain_ops.map_committers_info(
                fxBlockchain)

            # Find primary replica ip
            initial_primary_replica_ip = committers_mapping["primary_ip"]
            log.info("initial_primary_replica_info: {}".format(
                initial_primary_replica_ip))

            interrupted_nodes = []
            container_name = 'concord'

            # Stop f-1 non-primary replica
            for i in range(fxLocalSetup.f_count - 1):
                concord_host = fxLocalSetup.concord_hosts[i]
                if concord_host != initial_primary_replica_ip:
                    log.info(
                        "Stop concord in non-primary replica: {}".format(concord_host))
                    intr_helper.stop_container(concord_host, container_name)
                    interrupted_nodes.append(concord_host)

            log.info("Completed stopping f-1 non-primary replica concord container")

            # Start new thread for daml request submissions and stop & start current primary replica
            thread_daml_txn = Thread(target=continuous_daml_request_submission,
                                     args=(client_host, client_port, no_of_txns, wait_time, 180))
            thread_start_stop_primary = Thread(
                target=intr_helper.continuous_stop_start_container,
                args=(initial_primary_replica_ip, container_name, 180))
            interrupted_nodes.append(initial_primary_replica_ip)
            threads_list = []
            thread_daml_txn.start()
            thread_start_stop_primary.start()
            threads_list.append(thread_daml_txn)
            threads_list.append(thread_start_stop_primary)
            for threads in threads_list:
                threads.join()

            # Find new primary replica ip
            committers_mapping = blockchain_ops.map_committers_info(
                fxBlockchain, interrupted_nodes)
            new_primary_replica_ip = committers_mapping["primary_ip"]

            log.info("\n\nInitial primary replica: {}".format(
                initial_primary_replica_ip))

            log.info("\n\nNew primary replica: {}".format(
                new_primary_replica_ip))
            assert initial_primary_replica_ip != new_primary_replica_ip, "View Change did not happen successfully"

            # Submit Daml requests after view change
            assert simple_request(url, no_of_txns, wait_time), \
                "DAML request submission/verification failed for participant \
            [{}]".format(client_host)

        except Exception as excp:
            assert False, excp


@describe("Verify fault tolerance when nodes started with staggered startup ")
@pytest.mark.parametrize("participant_first", [True, False])
def test_system_after_staggered_startup(fxLocalSetup, fxHermesRunSettings, participant_first):
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
    for client_host, client_port in fxLocalSetup.client_hosts.items():
        # Install sdk only for one iteration
        if participant_first:
            install_sdk_deploy_daml(client_host, client_port)

        no_of_txns, wait_time = 1, 1
        client_off_count = 0
        url = 'http://{}:{}'.format(client_host, client_port)

        assert simple_request(url, no_of_txns), \
            "DAML request submission/verification failed for participant \
            [{}]".format(client_host)
        log.info("Participant off {}: {}".format(
            client_off_count, client_host))

        # Power off participant node
        assert interrupt_node(fxHermesRunSettings, client_host,
                              helper.TYPE_DAML_PARTICIPANT,
                              intr_helper.NODE_INTERRUPT_VM_STOP_START,
                              intr_helper.NODE_INTERRUPT), \
            "Failed to power off participant node [{}]".format(client_host)
        client_off_count = client_off_count + 1

    # Power off all committer Nodes
    power_off_committers(fxHermesRunSettings, fxLocalSetup.concord_hosts)

    if participant_first:
        # Power on all participant nodes
        power_on_all_participants(
            fxHermesRunSettings, fxLocalSetup.client_hosts.items())

        # Power on all committer nodes
        staggered_startup_of_committers(
            fxHermesRunSettings, fxLocalSetup.concord_hosts)

    else:
        # Power on all committer nodes
        staggered_startup_of_committers(
            fxHermesRunSettings, fxLocalSetup.concord_hosts)

        # Power on all participant nodes
        power_on_all_participants(
            fxHermesRunSettings, fxLocalSetup.client_hosts.items())

    # Wait for sometime before creating transactions
    time.sleep(60)

    # Create & verify transactions after powering on all nodes
    for client_host, client_port in fxLocalSetup.client_hosts.items():
        no_of_txns, wait_time = 1, 1
        url = 'http://{}:{}'.format(client_host, client_port)
        assert simple_request(url, no_of_txns), \
            "DAML request submission/verification failed for participant \
            [{}]".format(client_host)


@describe("verify fault tolerance - after multiple view changes")
def test_fault_tolerance_after_multiple_view_changes(fxLocalSetup, fxHermesRunSettings, fxBlockchain):
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
    for client_host, client_port in fxLocalSetup.client_hosts.items():
        try:
            install_sdk_deploy_daml(client_host, client_port)

            no_of_txns, wait_time = 1, 0.2
            url = 'http://{}:{}'.format(client_host, client_port)

            # Submit Daml requests before finding primary replica
            assert simple_request(url, no_of_txns, wait_time), \
                "DAML request submission/verification failed for participant \
        [{}]".format(client_host)

            # Find primary replica ip
            committers_mapping = blockchain_ops.map_committers_info(
                fxBlockchain)
            initial_primary_replica_ip = committers_mapping["primary_ip"]
            log.info("initial_primary_replica_info: {}".format(
                initial_primary_replica_ip))

            interrupted_nodes = []
            container_name = 'concord'

            # List of f non-primary replicas
            non_primary_replicas = fxLocalSetup.concord_hosts[:]
            non_primary_replicas.remove(initial_primary_replica_ip)

            # Stop f-1 non-primary replica
            for i in range(fxLocalSetup.f_count - 1):
                concord_host = non_primary_replicas[i]
                intr_helper.stop_container(concord_host, container_name)
                interrupted_nodes.append(concord_host)

            log.info("Stopped f-1 non-primary replica concord container")

            # Start new thread for daml request submissions
            thread_daml_txn = Thread(target=continuous_daml_request_submission,
                                     args=(client_host, client_port, no_of_txns, wait_time, 900))
            thread_daml_txn.start()

            # Stop and Restart the current primary replicas multiple times
            for _ in range(0, 7):
                committers_mapping = blockchain_ops.map_committers_info(
                    fxBlockchain, interrupted_nodes)
                primary_replica_ip = committers_mapping["primary_ip"]
                intr_helper.stop_container(
                    primary_replica_ip, container_name, 30)
                intr_helper.start_container(
                    primary_replica_ip, container_name, 90)

            thread_daml_txn.join()

            # Submit Daml requests after multiple view changes
            assert simple_request(url, no_of_txns, wait_time), \
                "DAML request submission/verification failed for participant \
        [{}]".format(client_host)

        except Exception as excp:
            assert False, excp


@describe("verify that blockchain is connected to Wavefront")
def test_wavefront_smoke(fxBlockchain):
    '''
    Verify that Blockchain is connected to Wavefront
    - Call Wavefront API which returns success for valid blockchain.
    Args:
        fxBlockchain: blockchain fixture to get blockchain Id
    API Reference:
        https://vmware.wavefront.com/api-docs/ui
    '''
    blockchain_id = fxBlockchain.blockchainId
    assert blockchain_id, "Blockchain Id not found, can't proceed with this test"

    wf_url = 'https://vmware.wavefront.com/api/v2/source/{}'.format(
        blockchain_id)

    wavefront_cmd = ["curl", "-X", "GET",
                     "-H", "Accept: application/json",
                     "-H", "Content-Type: application/json",
                     "-H", "Authorization: Bearer {}".format(wavefront.wf_api_token()),
                     "--connect-timeout", "5",  # Connection timeout for each retry
                     "--max-time", "10",  # Wait time for each retry
                     "--retry", "5",  # Maximum retries
                     "--retry-delay", "0",  # Delay in next retry
                     "--retry-max-time", "60",  # Total time before this call is considered a failure
                     wf_url]

    str_output = check_output(wavefront_cmd).decode('utf8')
    try:
        json_output = json.loads(str_output)
    except (ValueError, json.decoder.JSONDecodeError, CalledProcessError) as e:
        log.error("Error is : [{}]".format(e))
        assert False, str_output

    assert "error" not in json_output.keys(), json_output["message"]
    assert json_output["status"]["code"] == 200 \
        or json_output["status"]["result"] == "OK", json_output["status"]["message"]


@describe("verify that blockchain is generating Wavefront metrics")
@pytest.mark.parametrize(
    ("counter", "metric_name", "operation", "filter"), [
        (1, "vmware.blockchain.concord.concordbft.receivedClientRequestMsgs.counter",
         None, "blockchain"),
        (2, "vmware.blockchain.concord.command.handler.operation.counters.total.counter",
         "daml_writes", "host")
    ])
def test_wavefront_metrics(fxLocalSetup, fxBlockchain, counter, metric_name, operation, filter):
    '''
    Verify that Blockchain is generating Wavefront metrics
    - Call Wavefront API which returns Client requests per second details.
    - Output contains data blocks.
    - Make a DAML client request
    - Call the API again to check if a data block was added.
    Args:
        fxLocalSetup: Local fixture for client hosts details.
        fxBlockchain: blockchain fixture to get blockchain Id
        counter: To ensure daml sdk install function is called only once.
        metric_name: Metric name to be tested
        operation: Metric operation
        filter: Different metrics uses different filter names, like host or blockchain for same field.
    API Reference:
        https://vmware.wavefront.com/api-docs/ui
    Note:
        This test should run standalone and not in parallel with any other test.
        A warning message has been added for display to notify the same.
    '''
    warncolor = "\033[1;33m"
    reset = "\033[0m"
    log.warning("\n\n{}".format(warncolor))
    log.warning(
        "{}".format("*"*85))
    log.warning(
        "** This test assumes that no other test is running in parallel for given blockchain")
    log.warning(
        "** There could be wrong impact on test outcome otherwise")
    log.warning(
        "{}{}\n\n".format("*"*85, reset))
    log.info("Running this test for \nMetric name [{}] and Operation [{}]".format(
        metric_name, operation))

    blockchain_id = fxBlockchain.blockchainId
    assert blockchain_id, "Blockchain Id not found, can't proceed with this test"

    metric_query = "ts({}".format(metric_name)

    if operation is not None:
        metric_query = metric_query + ",operation={}".format(operation)

    metric_query = metric_query + ",{}={})".format(filter, blockchain_id)

    # Get start and end datetime in epoch
    # Time range is a crucial parameter, do not increase/decrease
    # without analyzing the API calls properly.
    start_epoch = (datetime.now() - timedelta(seconds=60)).strftime('%s')
    end_epoch = (datetime.now() + timedelta(seconds=60)).strftime('%s')
    log.info("Start time is {} and end time is {}".format(
        start_epoch, end_epoch))

    # Check Wavefront before Client request
    str_output = wavefront.call_wavefront_chart_api(
        blockchain_id, metric_query, start_epoch, end_epoch)
    output = None

    try:
        output = json.loads(str_output)
        assert "warnings" not in output.keys(), json_output["warnings"]
        assert "errorType" not in output.keys(
        ), "{} - {}".format(output["errorType"], output["errorMessage"])
    except (ValueError, json.decoder.JSONDecodeError, CalledProcessError) as e:
        log.error("Error is : [{}]".format(e))
        assert False, str_output

    before_data = output["timeseries"][0]["data"]
    log.info("\nMetric {} data before daml transaction is {} \n\n".format(
        metric_name, before_data))

    # Adding 10 seconds sleep so that generated transaction's epoch
    # does not overlap with above timeseries data epoch.
    time.sleep(10)

    # Make a client request
    for client_host, client_port in fxLocalSetup.client_hosts.items():
        try:
            # Call function to install sdk only first time.
            if counter == 1:
                install_sdk_deploy_daml(client_host, client_port)
            no_of_txns, wait_time = 1, 20
            url = 'http://{}:{}'.format(client_host, client_port)
            # Create & verify transactions of count no_of_txns
            assert simple_request(url, no_of_txns, wait_time), \
                "DAML request submission/verification failed"
        except Exception as excp:
            assert False, excp

    # Check Wavefront after Client request
    str_output = wavefront.call_wavefront_chart_api(
        blockchain_id, metric_query, start_epoch, end_epoch)
    try:
        output = json.loads(str_output)
        assert "warnings" not in output.keys(), output["warnings"]
        assert "errorType" not in output.keys(
        ), "{} - {}".format(output["errorType"], output["errorMessage"])
    except (ValueError, json.decoder.JSONDecodeError, CalledProcessError) as e:
        log.error("Error is : [{}]".format(e))
        assert False, str_output

    after_data = output["timeseries"][0]["data"]
    log.info(
        "\nMetric {} data after daml transaction is {} \n\n".format(metric_name, after_data))

    assert len(after_data) and len(after_data) > len(
        before_data), "Blockchain didn't generate metric for given transaction on Wavefront"
