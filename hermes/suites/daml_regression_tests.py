#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
from datetime import datetime, timedelta
from suites.case import describe
from collections import namedtuple
import pytest
from util import helper, hermes_logging, blockchain_ops
from threading import Thread
import util.daml.daml_helper as daml_helper
from util.daml.daml_requests import simple_request, continuous_daml_request_submission
import util.node_interruption_helper as intr_helper
import asyncio
from fixtures.common_fixtures import fxNodeInterruption, fxBlockchain, fxProduct

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

