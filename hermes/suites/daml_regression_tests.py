#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
from datetime import datetime, timedelta
from threading import Thread
from suites.case import describe
from collections import namedtuple
import pytest
import time
import random
from util import helper, hermes_logging, blockchain_ops
from util import daml_regression_helper as dr_helper
from util import rocksdb_helper
import multiprocessing
from util.daml.daml_requests import simple_request, \
    continuous_daml_request_submission, get_daml_url
import util.node_interruption_helper as intr_helper
from fixtures.common_fixtures import fxBlockchain, \
    fxNodeInterruption, fxProduct
from subprocess import check_output, CalledProcessError
import queue
import util.daml.daml_helper as daml_helper

log = hermes_logging.getMainLogger()

LocalSetupFixture = namedtuple(
    "LocalSetupFixture", "client_hosts, concord_hosts, f_count")
installed = False


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
    global installed
    log.info("\n*** Blockchain fixture is {} ***".format(fxBlockchain))
    f_count = blockchain_ops.get_f_count(fxBlockchain.replicas)
    client_hosts, concord_hosts = \
        helper.format_hosts_structure(fxBlockchain.replicas)

    local_tuple = LocalSetupFixture(
        client_hosts=client_hosts, concord_hosts=concord_hosts, f_count=f_count)

    # SDK Installation & Daml Deployment should happen once per run
    if not installed:
        for client_host in client_hosts:
            dr_helper.install_sdk_deploy_daml(client_host)
        installed = True

    def fin():
        dr_helper.perform_sanity_check(
            reraise, local_tuple, fxHermesRunSettings, fxBlockchain)

    request.addfinalizer(fin)

    return local_tuple


@pytest.mark.smoke
@pytest.mark.basic
@describe("daml test for single transaction without any interruption")
def test_daml_single_transaction(reraise, fxLocalSetup, fxBlockchain):
    '''
    Verify case by submitting sequential client requests using DAML tool.
    - Connect to a blockchain network.
    - Submit valid requests to it.
    - Verify that requests are processed correctly.
    Args:
        fxLocalSetup: Local fixture
        fxBlockchain: Blockchain fixture
    '''
    for client_host in fxLocalSetup.client_hosts:
        try:
            # Create & verify transactions
            assert dr_helper.make_daml_request(
                reraise, fxBlockchain.blockchainId, client_host), dr_helper.PARTICIPANT_GENERIC_ERROR_MSG

        except Exception as excp:
            assert False, excp


@pytest.mark.basic
@describe("fault tolerance - f replicas are stopped/started, powered off/on")
def test_daml_stop_start_replicas(reraise, fxLocalSetup, fxHermesRunSettings, fxBlockchain):
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
        fxBlockchain: Blockchain fixture
    '''
    for client_host in fxLocalSetup.client_hosts:
        try:
            # Create & verify transactions
            assert dr_helper.make_daml_request(
                reraise, fxBlockchain.blockchainId, client_host), dr_helper.PARTICIPANT_GENERIC_ERROR_MSG

            # Crash/Recover concord containers
            for i in range(fxLocalSetup.f_count):
                concord_host = fxLocalSetup.concord_hosts[i]
                custom_params = {
                    intr_helper.CONTAINERS_TO_CRASH: ["concord"]
                }
                assert dr_helper.interrupt_node(fxHermesRunSettings, fxBlockchain, concord_host,
                                                helper.TYPE_DAML_COMMITTER,
                                                intr_helper.NODE_INTERRUPT_CONTAINER_CRASH,
                                                intr_helper.NODE_INTERRUPT,
                                                custom_params),\
                    "Failed to crash container [{}]".format(concord_host)

            # Create & verify transactions after f crash/recovery of concord
            assert dr_helper.make_daml_request(reraise, fxBlockchain.blockchainId, client_host), \
                dr_helper.PARTICIPANT_GENERIC_ERROR_MSG + \
                "after restarting f concord containers"

            # Power off f committer nodes
            for i in range(fxLocalSetup.f_count):
                concord_host = fxLocalSetup.concord_hosts[i]
                assert dr_helper.interrupt_node(fxHermesRunSettings, fxBlockchain, concord_host,
                                                helper.TYPE_DAML_COMMITTER,
                                                intr_helper.NODE_INTERRUPT_VM_STOP_START,
                                                intr_helper.NODE_INTERRUPT), \
                    dr_helper.COMMITTER_POWER_OFF_ERROR_MSG + "[{}]".format(
                        concord_host)

            # Create & verify transactions after powering off f committer nodes
            assert dr_helper.make_daml_request(reraise, fxBlockchain.blockchainId, client_host), \
                dr_helper.PARTICIPANT_GENERIC_ERROR_MSG + \
                "after powering off f committer nodes"

            # Power on f committer nodes
            for i in range(fxLocalSetup.f_count):
                concord_host = fxLocalSetup.concord_hosts[i]
                assert dr_helper.interrupt_node(fxHermesRunSettings, fxBlockchain, concord_host,
                                                helper.TYPE_DAML_COMMITTER,
                                                intr_helper.NODE_INTERRUPT_VM_STOP_START,
                                                intr_helper.NODE_RECOVER), \
                    dr_helper.COMMITTER_POWER_ON_ERROR_MSG + "[{}]".format(
                        concord_host)

            assert dr_helper.make_daml_request(reraise, fxBlockchain.blockchainId, client_host), \
                dr_helper.PARTICIPANT_GENERIC_ERROR_MSG + "after powering on f committer nodes"

        except Exception as excp:
            assert False, excp


@pytest.mark.basic
@describe("fault tolerance - participant ledger api restarted, node is powered off/on")
def test_participant_ledgerapi_restart(reraise, fxLocalSetup,
                                       fxHermesRunSettings, fxBlockchain):
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
        fxBlockchain: Blockchain fixture
    '''
    for client_host in fxLocalSetup.client_hosts:
        try:
            # Create & verify transactions
            assert dr_helper.make_daml_request(
                reraise, fxBlockchain.blockchainId, client_host), dr_helper.PARTICIPANT_GENERIC_ERROR_MSG

            custom_params = {
                intr_helper.CONTAINERS_TO_CRASH: ["daml_ledger_api"]
            }
            assert dr_helper.interrupt_node(fxHermesRunSettings, fxBlockchain, client_host,
                                            helper.TYPE_DAML_PARTICIPANT,
                                            intr_helper.NODE_INTERRUPT_CONTAINER_CRASH,
                                            intr_helper.NODE_INTERRUPT, custom_params), \
                "Failed to crash container [{}]".format(client_host)

            # Create & verify transactions
            assert dr_helper.make_daml_request(reraise, fxBlockchain.blockchainId, client_host), \
                dr_helper.PARTICIPANT_GENERIC_ERROR_MSG + \
                "after restarting ledger api container"

            # Power off Participant node
            assert dr_helper.interrupt_node(fxHermesRunSettings, fxBlockchain, client_host,
                                            helper.TYPE_DAML_PARTICIPANT,
                                            intr_helper.NODE_INTERRUPT_VM_STOP_START,
                                            intr_helper.NODE_INTERRUPT), \
                dr_helper.PARTICIPANT_POWER_OFF_ERROR_MSG + \
                "[{}]".format(client_host)

            # Power on Participant node
            assert dr_helper.interrupt_node(fxHermesRunSettings, fxBlockchain, client_host,
                                            helper.TYPE_DAML_PARTICIPANT,
                                            intr_helper.NODE_INTERRUPT_VM_STOP_START,
                                            intr_helper.NODE_RECOVER), \
                dr_helper.PARTICIPANT_POWER_ON_ERROR_MSG + \
                "[{}]".format(client_host)

            assert dr_helper.make_daml_request(reraise, fxBlockchain.blockchainId, client_host), \
                dr_helper.PARTICIPANT_GENERIC_ERROR_MSG + "after powering on participant node"

        except Exception as excp:
            assert False, excp


@pytest.mark.basic
@describe("fault tolerance - participant node powered off/on, ledger api, index db restarted")
def test_participant_ledgerapi_indexdb_restart(reraise, fxLocalSetup, fxHermesRunSettings, fxBlockchain):
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
        fxBlockchain: Blockchain fixture
    '''
    for client_host in fxLocalSetup.client_hosts:
        try:
            # Create & verify transactions
            assert dr_helper.make_daml_request(reraise, fxBlockchain.blockchainId, client_host), \
                dr_helper.PARTICIPANT_GENERIC_ERROR_MSG

            # Power off Participant node
            assert dr_helper.interrupt_node(fxHermesRunSettings, fxBlockchain, client_host,
                                            helper.TYPE_DAML_PARTICIPANT,
                                            intr_helper.NODE_INTERRUPT_VM_STOP_START,
                                            intr_helper.NODE_INTERRUPT), \
                dr_helper.PARTICIPANT_POWER_OFF_ERROR_MSG + \
                "[{}]".format(client_host)

            # Power on Participant node
            assert dr_helper.interrupt_node(fxHermesRunSettings, fxBlockchain, client_host,
                                            helper.TYPE_DAML_PARTICIPANT,
                                            intr_helper.NODE_INTERRUPT_VM_STOP_START,
                                            intr_helper.NODE_RECOVER), \
                dr_helper.PARTICIPANT_POWER_ON_ERROR_MSG + \
                "[{}]".format(client_host)

            # Create & verify transactions
            assert dr_helper.make_daml_request(reraise, fxBlockchain.blockchainId, client_host), \
                dr_helper.PARTICIPANT_GENERIC_ERROR_MSG + "after powering on participant node"

            # Crash and recover daml ledger api & index db containers
            custom_params = {
                intr_helper.CONTAINERS_TO_CRASH: [
                    "daml_ledger_api", "daml_index_db"]
            }
            assert dr_helper.interrupt_node(fxHermesRunSettings, fxBlockchain, client_host,
                                            helper.TYPE_DAML_PARTICIPANT,
                                            intr_helper.NODE_INTERRUPT_CONTAINER_CRASH,
                                            intr_helper.NODE_INTERRUPT, custom_params), \
                "Failed to crash container(s) [{}]".format(client_host)

            assert dr_helper.make_daml_request(reraise, fxBlockchain.blockchainId, client_host), \
                dr_helper.PARTICIPANT_GENERIC_ERROR_MSG + "after restarting client containers"

        except Exception as excp:
            assert False, excp


@pytest.mark.network_failure
@pytest.mark.basic
@describe("fault tolerance - recovery after checkpoints (network failure), f nodes powered off/on")
def test_daml_network_failure(reraise, fxLocalSetup, fxHermesRunSettings, fxBlockchain):
    '''
    Verify below using DAML tool.
    - Connect to a blockchain network.
    - Submit valid requests enough to trigger multiple checkpoints.
    - Disable the network interface on all replicas.
    - Enable the network interface on all replicas.
    - Submit a small number of sequential client requests
    - Verify that new requests are processed correctly.
    - Submit and verify requests for power off/on of f committer nodes as well.
    Args:
        fxLocalSetup: Local fixture
        fxHermesRunSettings: Hermes command line arguments
        fxBlockchain: Blockchain fixture
    '''
    for client_host in fxLocalSetup.client_hosts:
        try:
            url = get_daml_url(client_host)
            # Start the daml transactions and trigger checkpoint
            assert dr_helper.trigger_checkpoint(fxBlockchain.blockchainId,
                                                client_host), dr_helper.CHECKPOINT_ERROR_MESSAGE

            # Disconnect network of all committer nodes
            custom_params = {
                intr_helper.NETWORK_DISCONNECT_LEVEL: intr_helper.NETWORK_DISCONNECT_VM_LEVEL
            }

            for index in range(len(fxLocalSetup.concord_hosts)):
                concord_host = fxLocalSetup.concord_hosts[index]
                assert dr_helper.interrupt_node(fxHermesRunSettings, fxBlockchain, concord_host,
                                                helper.TYPE_DAML_COMMITTER,
                                                intr_helper.NODE_INTERRUPT_NETWORK_DISCONNECT,
                                                intr_helper.NODE_INTERRUPT,
                                                custom_params), \
                    "Failed to disconnect committer node [{}]".format(
                        concord_host)

            # Reconnect network of all committer nodes
            for index in range(len(fxLocalSetup.concord_hosts)):
                concord_host = fxLocalSetup.concord_hosts[index]
                assert dr_helper.interrupt_node(fxHermesRunSettings, fxBlockchain, concord_host,
                                                helper.TYPE_DAML_COMMITTER,
                                                intr_helper.NODE_INTERRUPT_NETWORK_DISCONNECT,
                                                intr_helper.NODE_RECOVER,
                                                custom_params), \
                    "Failed to reconnect committer node [{}]".format(
                        concord_host)

            # Create & verify transactions after disconnect/reconnect of all committer nodes
            assert simple_request(url, 1, 0), dr_helper.PARTICIPANT_GENERIC_ERROR_MSG + \
                "after disconnect/reconnect of all committer nodes"

            log.info(
                "\nTransaction successful after reconnecting all committer nodes")

            # Power off f committer nodes
            for index in range(fxLocalSetup.f_count):
                concord_host = fxLocalSetup.concord_hosts[index]
                assert dr_helper.interrupt_node(fxHermesRunSettings, fxBlockchain, concord_host,
                                                helper.TYPE_DAML_COMMITTER,
                                                intr_helper.NODE_INTERRUPT_VM_STOP_START,
                                                intr_helper.NODE_INTERRUPT), \
                    dr_helper.COMMITTER_POWER_OFF_ERROR_MSG + "[{}]".format(
                        concord_host)

            # Create & verify transactions after powering off f committer nodes
            assert simple_request(
                url, 1, 0), dr_helper.PARTICIPANT_GENERIC_ERROR_MSG + "after powering off f committer nodes"

            log.info(
                "\nTransaction successful after powering off f committer nodes")

            # Power on f committer nodes
            for index in range(fxLocalSetup.f_count):
                concord_host = fxLocalSetup.concord_hosts[index]
                assert dr_helper.interrupt_node(fxHermesRunSettings, fxBlockchain, concord_host,
                                                helper.TYPE_DAML_COMMITTER,
                                                intr_helper.NODE_INTERRUPT_VM_STOP_START,
                                                intr_helper.NODE_RECOVER), \
                    dr_helper.COMMITTER_POWER_ON_ERROR_MSG + "[{}]".format(
                        concord_host)

            assert simple_request(
                url, 1, 0), dr_helper.PARTICIPANT_GENERIC_ERROR_MSG + "after powering on f committer nodes"

        except Exception as excp:
            assert False, excp


@pytest.mark.processed_with_quorum
@pytest.mark.basic
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
        fxBlockchain: Blockchain fixture
    '''
    for client_host in fxLocalSetup.client_hosts:
        try:
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
                dr_helper.stop_for_replica_list(
                    fxBlockchain.blockchainId, non_primary_replicas, container_name, fxLocalSetup.f_count + 1)
            else:
                dr_helper.stop_for_replica_list(
                    fxBlockchain.blockchainId, primary_and_non_primary_replicas, container_name, fxLocalSetup.f_count + 1)

            # Transactions should fail at this moment
            success = dr_helper.make_daml_request(
                reraise, fxBlockchain.blockchainId, client_host, 1)
            log.info(
                "Expected result of daml txn after f+1 down is False, actual result is {}".format(success))
            if success:
                log.info(
                    "Daml transaction was successful even with f+1 replicas down")
                assert False, "Expected daml transaction to fail after f+1 replicas are down"

            # Step 0 - f+1 non primary replicas to be started
            # Step 1 - f non primary and 1 primary replicas to be started
            if step == 0:
                dr_helper.start_all_replicas(
                    fxBlockchain.blockchainId, non_primary_replicas, container_name, fxLocalSetup.f_count + 1)
            else:
                dr_helper.start_all_replicas(
                    fxBlockchain.blockchainId, primary_and_non_primary_replicas, container_name, fxLocalSetup.f_count + 1)

            # Create & verify transactions after powering on stopped replicas
            assert dr_helper.make_daml_request(reraise, fxBlockchain.blockchainId, client_host), \
                dr_helper.PARTICIPANT_GENERIC_ERROR_MSG

        except Exception as excp:
            assert False, excp


@pytest.mark.staggered_startups
@describe("fault tolerance - nodes started with staggered startup")
@pytest.mark.parametrize("participant_first", [True, False])
def test_system_after_staggered_startup(reraise, fxLocalSetup, fxHermesRunSettings, fxBlockchain, participant_first):
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
        fxBlockchain: Blockchain fixture
    '''
    for count, client_host in enumerate(fxLocalSetup.client_hosts):
        # Create & verify transactions
        assert dr_helper.make_daml_request(
            reraise, fxBlockchain.blockchainId, client_host), dr_helper.PARTICIPANT_GENERIC_ERROR_MSG

        log.info("Participant off {}: {}".format(
            count + 1, client_host))

        # Power off participant node
        assert dr_helper.interrupt_node(fxHermesRunSettings, fxBlockchain, client_host,
                                        helper.TYPE_DAML_PARTICIPANT,
                                        intr_helper.NODE_INTERRUPT_VM_STOP_START,
                                        intr_helper.NODE_INTERRUPT), \
            "Failed to power off participant node [{}]".format(client_host)

    # Power off all committer Nodes
    dr_helper.power_off_committers(
        fxHermesRunSettings, fxBlockchain, fxLocalSetup.concord_hosts)

    if participant_first:
        # Power on all participant nodes
        dr_helper.power_on_all_participants(
            fxHermesRunSettings, fxBlockchain, fxLocalSetup.client_hosts)

        # Power on all committer nodes
        dr_helper.staggered_start_committers(
            fxHermesRunSettings, fxBlockchain, fxLocalSetup.concord_hosts)
    else:
        # Power on all committer nodes
        dr_helper.staggered_start_committers(
            fxHermesRunSettings, fxBlockchain, fxLocalSetup.concord_hosts)

        # Power on all participant nodes
        dr_helper.power_on_all_participants(
            fxHermesRunSettings, fxBlockchain, fxLocalSetup.client_hosts)

    # Create & verify transactions after powering on all nodes
    for client_host in fxLocalSetup.client_hosts:
        assert dr_helper.make_daml_request(reraise, fxBlockchain.blockchainId, client_host), \
            dr_helper.PARTICIPANT_GENERIC_ERROR_MSG + "after staggered startup"


@pytest.mark.view_changes
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
        fxBlockchain: Blockchain fixture
    '''
    for client_host in fxLocalSetup.client_hosts:
        try:
            # Submit Daml requests before finding primary replica
            assert dr_helper.make_daml_request(
                reraise, fxBlockchain.blockchainId, client_host), dr_helper.PARTICIPANT_GENERIC_ERROR_MSG

            # Find primary replica ip and id
            init_mapping = blockchain_ops.map_committers_info(fxBlockchain)
            init_primary_rip = init_mapping["primary_ip"]
            init_primary_index = init_mapping["primary_index"]

            assert init_primary_rip, "Primary Replica not found"
            log.info("Primary Replica IP and index: {} and {}".format(
                init_primary_rip, init_primary_index))

            interrupted_nodes = []
            container_name = 'concord'

            non_primary_replicas = fxLocalSetup.concord_hosts[:]
            non_primary_replicas.remove(init_primary_rip)

            # Stop f-1 non-primary replica
            for index in range(fxLocalSetup.f_count - 1):
                concord_host = non_primary_replicas[index]
                log.info(
                    "\nStop concord in non-primary replica: {}".format(concord_host))
                intr_helper.stop_container(
                    fxBlockchain.blockchainId, concord_host, container_name)
                interrupted_nodes.append(concord_host)

            log.info("\nStopped f-1 non-primary replica concord containers")
            log.info("\nInterrupted nodes are : {}".format(interrupted_nodes))

            # Start new process for daml request submissions and stop & start current primary replica
            p_daml_txn = multiprocessing.Process(target=continuous_daml_request_submission,
                                                 args=(client_host, 1, 0.2, 480))

            p_stop_start_primary = multiprocessing.Process(target=intr_helper.continuous_stop_start_container,
                                                           args=(fxBlockchain.blockchainId, init_primary_rip, container_name, 300))

            interrupted_nodes.append(init_primary_rip)

            log.info("\nAfter adding primary replica to interrupted nodes are : {}".format(
                interrupted_nodes))

            log.info("\nStarting daml transaction process")
            p_daml_txn.start()
            log.info("\nStarting stop start primary process")
            p_stop_start_primary.start()
            time.sleep(300)

            p_stop_start_primary.join()
            if p_stop_start_primary.is_alive():
                reraise()
                p_stop_start_primary.terminate()
                log.info("\nStop & start primary process completed")

            assert dr_helper.verify_view_change(reraise, fxBlockchain, init_primary_rip, init_primary_index,
                                                [interrupted_nodes]), \
                "View Change did not happen successfully"

            p_daml_txn.join(5)
            if p_daml_txn.is_alive():
                reraise()
                p_daml_txn.terminate()
                log.info("\nDaml transaction process completed")

        except Exception as excp:
            assert False, excp


@pytest.mark.view_changes
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
        fxBlockchain: Blockchain fixture
    '''
    for client_host in fxLocalSetup.client_hosts:
        try:
            # Submit Daml requests before finding primary replica
            assert dr_helper.make_daml_request(
                reraise, fxBlockchain.blockchainId, client_host), dr_helper.PARTICIPANT_GENERIC_ERROR_MSG

            # Find primary replica ip and id
            init_mapping = blockchain_ops.map_committers_info(
                fxBlockchain)
            init_primary_rip = init_mapping["primary_ip"]
            init_primary_index = init_mapping["primary_index"]

            assert init_primary_rip, "Primary Replica not found"
            log.info("Primary Replica IP and index: {} and {}".format(
                init_primary_rip, init_primary_index))

            interrupted_nodes = []
            container_name = 'concord'

            # List of f non-primary replicas
            non_primary_replicas = fxLocalSetup.concord_hosts[:]
            non_primary_replicas.remove(init_primary_rip)

            # Stop f-1 non-primary replica
            for i in range(fxLocalSetup.f_count - 1):
                concord_host = non_primary_replicas[i]
                intr_helper.stop_container(
                    fxBlockchain.blockchainId, concord_host, container_name)
                interrupted_nodes.append(concord_host)

            log.info("Stopped f-1 non-primary replica concord containers")
            log.info("\nInterrupted nodes are : {}".format(interrupted_nodes))

            # Start new process for daml request submissions
            daml_duration = len(fxLocalSetup.concord_hosts) * 150
            p_daml_txn = multiprocessing.Process(target=continuous_daml_request_submission,
                                                 args=(client_host, 1, 0.2, daml_duration))

            log.info("\nStarting daml transaction process")
            p_daml_txn.start()

            # Stop and Restart the current primary replicas multiple times
            for _ in range(0, len(fxLocalSetup.concord_hosts)):
                new_mapping = blockchain_ops.map_committers_info(
                    fxBlockchain, interrupted_nodes)
                new_primary_rip = new_mapping["primary_ip"]
                new_primary_index = new_mapping["primary_index"]

                assert new_primary_rip, "New Primary Replica not found"
                log.info("Primary Replica IP and index: {} and {}".format(
                    new_primary_rip, new_primary_index))
                intr_helper.stop_container(
                    fxBlockchain.blockchainId, new_primary_rip, container_name, 30)
                intr_helper.start_container(
                    fxBlockchain.blockchainId, new_primary_rip, container_name, 120)

            p_daml_txn.join(5)
            if p_daml_txn.is_alive():
                reraise()
                p_daml_txn.terminate()
                log.info("\nDaml transaction process completed")

        except Exception as excp:
            assert False, excp


@pytest.mark.lack_of_quorum
@describe("fault tolerance - temporary lack of quorum after view change")
@pytest.mark.parametrize("step", [0, 1])
def test_temporary_lack_of_quorum_after_view_change(reraise, fxLocalSetup, fxHermesRunSettings, fxBlockchain, step):
    '''
    Verify below using DAML tool
    - Connect to a blockchain network.
    - Stop f non-primary replicas.
    - Submit requests continuously.
    - Start and restart current primary replica or non primary replica.
    - Verify that new requests are processed correctly.
    Args:
        fxLocalSetup: Local fixture
        fxHermesRunSettings: Hermes command line arguments
        fxBlockchain: Blockchain fixture
    '''
    for client_host in fxLocalSetup.client_hosts:
        try:
            interrupted_nodes = []

            # Find primary replica and primary replica id
            init_mapping = blockchain_ops.map_committers_info(fxBlockchain)
            init_primary_rip = init_mapping["primary_ip"]
            init_primary_index = init_mapping["primary_index"]
            log.info("Primary Replica IP and index: {} and {}".format(
                init_primary_rip, init_primary_index))

            all_replicas = fxLocalSetup.concord_hosts[:]
            container_name = 'concord'

            non_primary_replicas = [
                n for n in all_replicas if n != init_primary_rip]

            # Stop f non-primary replica
            for i in range(fxLocalSetup.f_count):
                concord_host = non_primary_replicas[i]
                intr_helper.stop_container(
                    fxBlockchain.blockchainId, concord_host, container_name)
                interrupted_nodes.append(concord_host)
            log.info("\n\nStopped f non primary replica")

            # Start new process for daml request submissions
            p_daml_txn = multiprocessing.Process(target=continuous_daml_request_submission,
                                                 args=(client_host, 1, 1, 240))

            p_daml_txn.start()

            # Step 0 - current primary replica to be stopped and restarted
            # Step 1 - non primary replica to be stopped and restarted
            if step == 0:
                log.info(
                    "\n\nDoing Step 0 - current primary replica to be stopped and restarted")
                intr_helper.stop_container(
                    fxBlockchain.blockchainId, init_primary_rip, container_name, 30)
                intr_helper.start_container(
                    fxBlockchain.blockchainId, init_primary_rip, container_name, 90)
                log.info("\n\nStopped and restarted current primary replica: {}".format(
                    init_primary_rip))
            if step == 1:
                log.info(
                    "\n\nDoing Step 1 - non primary replica to be stopped and restarted")
                uninterrupted_non_primary_replicas = \
                    [i for i in non_primary_replicas if i not in interrupted_nodes]

                log.info("\n\nUninterrupted non primary replicas: {}".
                         format(uninterrupted_non_primary_replicas))

                non_primary_replica_to_be_interrupted = random.choice(
                    uninterrupted_non_primary_replicas)

                intr_helper.stop_container(
                    fxBlockchain.blockchainId, non_primary_replica_to_be_interrupted, container_name, 30)
                intr_helper.start_container(
                    fxBlockchain.blockchainId, non_primary_replica_to_be_interrupted, container_name, 120)
                log.info("\n\nStopped and restarted non primary replica: {}".
                         format(non_primary_replica_to_be_interrupted))

            p_daml_txn.join(5)
            if p_daml_txn.is_alive():
                p_daml_txn.terminate()

            # Find current primary replica
            new_mapping = blockchain_ops.map_committers_info(
                fxBlockchain, interrupted_nodes)
            new_primary_rip = new_mapping["primary_ip"]
            new_primary_index = new_mapping["primary_index"]

            log.info("New Primary Replica IP and index: {} and {}".
                     format(new_primary_rip, new_primary_index))

            assert init_primary_index != new_primary_index or init_primary_rip != new_primary_rip, \
                "View Change did not happen successfully"

            # Submit daml requests after view change
            assert dr_helper.make_daml_request(reraise, fxBlockchain.blockchainId, client_host), \
                dr_helper.PARTICIPANT_GENERIC_ERROR_MSG + "after view change"

        except Exception as excp:
            assert False, excp


@pytest.mark.st_coincide_vc
@describe("fault tolerance - state transfer coinciding with view change (f>=2)")
def test_st_coinciding_vc(reraise, fxLocalSetup, fxHermesRunSettings, fxBlockchain):
    '''
    Verify state transfer completes even if it coincides with view change using DAML tool.
    - Connect to a blockchain network.
    - Stop one non-primary replica.
    - Submit client requests until multiple checkpoints are triggered.
    - Restart stopped replica.
    - Stop primary replica.
    - Start new thread to verify State transfer before view change happens.
    - After State transfer completes, stop f-1 non-primary replicas.
    - Verify that client requests are processed correctly.
    - Restart stopped replicas.
    Args:
        fxLocalSetup: Local fixture
        fxHermesRunSettings: Hermes command line arguments
        fxBlockchain: Blockchain fixture
    '''

    if fxLocalSetup.f_count < 2:
        log.info("Test is expected to check state transfer for f>=2, but found f={}".format(
            fxLocalSetup.f_count))
        pytest.skip("Test is expected to check state transfer for f>=2, but found f={}".format(
            fxLocalSetup.f_count))

    for client_host in fxLocalSetup.client_hosts:
        try:
            # Step 1 : Find primary replica
            replicas_mapping = blockchain_ops.map_committers_info(fxBlockchain)
            init_primary_rip = replicas_mapping["primary_ip"]
            init_primary_index = replicas_mapping["primary_index"]
            assert init_primary_rip, "Primary Replica IP not found"

            log.info("Primary Replica: {}".format(init_primary_rip))

            non_primary_replicas = fxLocalSetup.concord_hosts[:]
            non_primary_replicas.remove(init_primary_rip)
            container_name = 'concord'

            # Step 2 : Stop one non-primary replica
            assert intr_helper.stop_container(
                fxBlockchain.blockchainId, non_primary_replicas[0], container_name), "Failed to stop committer node [{}]".format(
                non_primary_replicas[0])

            # Step 3 : Find block id of stopped replica
            init_block_id = rocksdb_helper.get_block_id(
                fxBlockchain.blockchainId, non_primary_replicas[0],
                skip_start=True, skip_stop=True)

            # Step 4 : Start the daml transactions and trigger checkpoint
            assert dr_helper.trigger_checkpoint(
                fxBlockchain.blockchainId, client_host), dr_helper.CHECKPOINT_ERROR_MESSAGE

            # Step 5 : Restart the stopped replica
            assert intr_helper.start_container(
                fxBlockchain.blockchainId, non_primary_replicas[0], container_name), "Failed to start committer node [{}]".format(
                non_primary_replicas[0])

            # Step 6 : Stopping primary to get block ID
            assert intr_helper.stop_container(
                fxBlockchain.blockchainId, init_primary_rip, container_name, 30), "Failed to stop primary [{}]".format(init_primary_rip)

            # Step 7 : Find block id of stopped primary replica
            p_block_id = rocksdb_helper.get_block_id(
                fxBlockchain.blockchainId, init_primary_rip,
                skip_start=True, skip_stop=True)

            # Step 8 : Start new thread for State transfer check
            que_st_check = queue.Queue()
            thread_st_check = Thread(target=lambda q, arg1, arg2, arg3, arg4, arg5, arg6, arg7: q.put(
                intr_helper.sleep_and_check(arg1, arg2, arg3, arg4, arg5, arg6, arg7)),
                args=(que_st_check, fxBlockchain.blockchainId, 0, 30, 420, init_block_id, p_block_id, non_primary_replicas))
            thread_st_check.start()

            # Step 9 : Verify view change is successful
            assert dr_helper.verify_view_change(reraise, fxBlockchain, init_primary_rip, init_primary_index, [init_primary_rip]), \
                "View Change did not happen successfully"

            log.info("\nInside main test - view change done")
            
            # Step 10 : Checking whether view change happened before state transfer
            if not thread_st_check.isAlive():
                st_check_result = que_st_check.get()
                if st_check_result != True:
                    log.info("Exception : ")
                    log.info(st_check_result)
                    assert False, "State transfer failed due to Exception"
                else:
                    assert False, "View change was expected to complete before state transfer"

            # Step 11 : Block main thread execution until state transfer is completed and verifying it
            thread_st_check.join()
            assert que_st_check.get(), "State transfer failed"

            log.info("State transfer check completed")

            # Step 12 :  Stop f-1 non-primary replicas
            non_primary_replicas.remove(non_primary_replicas[0])
            dr_helper.stop_for_replica_list(
                fxBlockchain.blockchainId, non_primary_replicas, container_name, fxLocalSetup.f_count - 1), "Error while stopping replica"

            # Step 13 :  Submit & verify Daml requests after state transfer
            url = get_daml_url(client_host)
            assert simple_request(
                url, 1, 0), dr_helper.PARTICIPANT_GENERIC_ERROR_MSG

            log.info("*** Test is successful")
        except Exception as excp:
            assert False, excp
