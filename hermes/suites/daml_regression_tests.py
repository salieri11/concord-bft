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
import queue

log = hermes_logging.getMainLogger()

LocalSetupFixture = namedtuple(
    "LocalSetupFixture", "client_hosts, concord_hosts, f_count, fx_blockchain")

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
    log.info("\n\nIn fxLocalSetup, Blockchain fixture is {}".format(fxBlockchain))
    f_count = intr_helper.get_f_count(fxBlockchain)
    client_hosts, concord_hosts = format_hosts_structure(fxBlockchain.replicas)
    log.info("\nIn fxLocalSetup, Participants are {}\nCommitters are {}\n".format(
        client_hosts, concord_hosts))

    local_tuple = LocalSetupFixture(
        client_hosts=client_hosts, concord_hosts=concord_hosts, f_count=f_count, fx_blockchain=fxBlockchain)

    # def fin():
    #     perform_sanity_check(reraise, local_tuple, fxHermesRunSettings)

    # request.addfinalizer(fin)

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
    time_to_sleep = 60 * no_of_txns
    time.sleep(time_to_sleep)
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


@describe("fault tolerance - requests not to be processed without quorum")
def test_requests_processed_only_with_quorum(reraise, fxLocalSetup, fxHermesRunSettings):
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
        step: Parametrization argument
    '''
    log.info("\nInside test_requests_processed_only_with_quorum function, localsetup is as below\n")
    log.info(fxLocalSetup)
    log.info("Blockchain fixture is : {}".format(fxLocalSetup.fx_blockchain))
    try:
        # Find primary replica and primary replica id
        replicas_mapping = blockchain_ops.map_committers_info(
            fxLocalSetup.fx_blockchain)
        primary_rip = replicas_mapping["primary_ip"]
        log.info("Primary Replica IP: {}".format(primary_rip))

    except Exception as excp:
        assert False, excp
