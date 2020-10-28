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
