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
import util.helper as helper
import util.daml.daml_helper as daml_helper
import util.node_interruption_helper as intr_helper
from util.daml.daml_requests import simple_request
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
        cmd, timeout=120, working_dir=party_project_dir, verbose=True)

    assert success or "Party already exists" in output, \
        "Unable to deploy DAML app for one/more parties"


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
        intr_helper.SKIP_MASTER_REPLICA: True
    }
    if custom_params is None:
        node_interruption_details[intr_helper.CUSTOM_INTERRUPTION_PARAMS] = {}
    else:
        node_interruption_details[intr_helper.CUSTOM_INTERRUPTION_PARAMS] = \
            custom_params
    return intr_helper.perform_interrupt_recovery_operation(
        fxHermesRunSettings, node, node_interruption_details, mode)


def find_primary_replica_id(master):
    username, password = helper.getNodeCredentials()
    cmd = "docker logs concord | cut -d '|' -f 11 | grep 'primary' | cut -d ':' -f 2| cut -d ',' -f 1|tail -1"
    primary_id = helper.ssh_connect(master, username, password, cmd)
    return primary_id


def stop_start_primary_replica(master, stop_wait_time=60, start_wait_time=30):
    username, password = helper.getNodeCredentials()
    cmd_to_get_status = "docker inspect concord --format {{.State.Status}}"
    status = helper.ssh_connect(master, username, password, cmd_to_get_status)
    if "running" in status:
        cmd_to_stop_primary = "docker stop concord"
        helper.ssh_connect(master, username, password, cmd_to_stop_primary)
        time.sleep(stop_wait_time)
        status = helper.ssh_connect(
            master, username, password, cmd_to_get_status)
        if "exited" in status:
            log.info("Primary replica {} stopped".format(master))
            return True
        else:
            log.info("Primary replica {} failed to stop".format(master))
            return False
    elif "exited" in status:
        cmd_to_start_primary = "docker start concord"
        helper.ssh_connect(master, username, password, cmd_to_start_primary)
        time.sleep(start_wait_time)
        status = helper.ssh_connect(
            master, username, password, cmd_to_get_status)
        if "running" in status:
            log.info("Primary replica {} restarted".format(master))
            return True
        else:
            log.info("Primary replica {} failed to restart".format(master))
            return False


def crash_recovery_master(master):
    try:
        status = True
        start_time = datetime.now()
        end_time = start_time + timedelta(minutes=5)
        while status and start_time <= end_time:
            status = stop_start_primary_replica(master)
            start_time = datetime.now()
    except Exception as excp:
        log.debug("Failed to crash and recover Primary replica:{}".format(master))
        assert False, excp


def send_request(client_host, client_port, no_of_txns, wait_time):
    try:
        status = True
        url = 'http://{}:{}'.format(client_host, client_port)
        start_time = datetime.now()
        end_time = start_time + timedelta(minutes=5)
        asyncio.set_event_loop(asyncio.new_event_loop())
        while status and start_time <= end_time:
            status = simple_request(url, no_of_txns, wait_time)
            start_time = datetime.now()
    except Exception as excp:
        log.debug(
            "Failed to submit Daml transactions on participant: {}".format(client_host))
        assert False, excp


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

            no_of_txns, wait_time = 5, 0.2
            url = 'http://{}:{}'.format(client_host, client_port)

            # Submit Daml requests before finding primary replica id
            assert simple_request(url, no_of_txns, wait_time), \
                "DAML request submission/verification failed for participant \
                [{}]".format(client_host)

            # Find primary replica and primary replica id
            initial_master_replica = blockchain_ops.fetch_master_replica(
                fxBlockchain)
            initial_primary_replica_id = find_primary_replica_id(
                initial_master_replica)
            log.info("Master Replica: {} and Replica ID: {}".format(
                initial_master_replica, initial_primary_replica_id))

            # Stop f-1 non-primary replica
            for i in range(fxLocalSetup.f_count - 1):
                custom_params = {
                    intr_helper.CONTAINERS_TO_CRASH: ["concord"]
                }
                concord_host = fxLocalSetup.concord_hosts[i]
                if concord_host != initial_master_replica:
                    log.info(
                        "Stop concord in non-primary replica: {}".format(concord_host))
                    assert interrupt_node(fxHermesRunSettings, concord_host, helper.TYPE_DAML_COMMITTER,
                                          intr_helper.NODE_INTERRUPT_CONTAINER_CRASH,
                                          intr_helper.NODE_INTERRUPT,
                                          custom_params), "Failed to crash container [{}]".format(
                        concord_host)
            log.info("Completed stopping f-1 non-primary replica concord container")

            # Start new thread for daml request submissions and stop & start primary replica
            thread_daml_txn = Thread(target=send_request,
                                     args=(client_host, client_port, no_of_txns, wait_time))
            thread_stop_start_primary = Thread(
                target=crash_recovery_master, args=(initial_master_replica,))
            t = []
            thread_daml_txn.start()
            thread_stop_start_primary.start()
            t.append(thread_daml_txn)
            t.append(thread_stop_start_primary)
            thread_daml_txn.join()
            thread_stop_start_primary.join()

            # Find primary replica
            committer_no = random.randint(1, 3 * fxLocalSetup.f_count)
            log.info(
                "\n\n Find new primary using random committer - {}".format(committer_no))
            new_primary_replica_id = find_primary_replica_id(
                fxLocalSetup.concord_hosts[committer_no])
            log.info("New primary replica ID: {}".format(
                new_primary_replica_id))
            assert initial_primary_replica_id != new_primary_replica_id, "View Change did not happen successfully"

        except Exception as excp:
            assert False, excp
