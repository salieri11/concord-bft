#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
from suites.case import describe
from collections import namedtuple
import pytest
import time
from util import helper, hermes_logging, backup_restore_helper
from fixtures.common_fixtures import fxBlockchain, fxProduct
from itertools import zip_longest

log = hermes_logging.getMainLogger()

LocalSetupFixture = namedtuple(
    "LocalSetupFixture", "client_node_db, f_count")

@pytest.fixture(scope="function")
@describe("fixture; local setup for given test suite")
def fxLocalSetup(fxHermesRunSettings, fxBlockchain, fxProduct):
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
    log.info("\n\nBlockchain product is {}".format(fxProduct))
    version = 0  # get version
    if version < 1.0:
        client_node_db = "/config/daml-index-db/db/"
        replica_node_db = "/config/concord/rocksdbdata/"
    else:
        client_node_db = "/mnt/data/db/"
        replica_node_db = "/mnt/data/rocksdbdata/"

    client_hosts, concord_hosts = format_hosts_structure(fxBlockchain.replicas)
    log.info("\nIn fxLocalSetup fixture, Participants are {}\nCommitters are {}\n".format(
        client_hosts, concord_hosts))
    local_tuple = LocalSetupFixture(client_node_db="concord_hosts", f_count='f_count')

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


def test_participant_post_backup(fxLocalSetup):
    log.info("Random test")
    # log.info(fxLocalSetup.client_hosts)
    return


def test_replica_post_backup():
    log.info("Random test")
    return