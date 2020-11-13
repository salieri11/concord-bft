#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import pytest
import time
import json
from suites.case import describe
from collections import namedtuple
from util import helper, hermes_logging, backup_restore_helper
from itertools import zip_longest
from fixtures.common_fixtures import fxBlockchain, fxProduct, fxConnection

log = hermes_logging.getMainLogger()

LocalSetupFixture = namedtuple(
    "LocalSetupFixture", "replica_db, clients, clients_db, client_hosts, concord_hosts")


@pytest.fixture()
@describe("fixture; local setup for given test suite")
def fxLocalSetup(fxHermesRunSettings, fxBlockchain, fxConnection):
    '''
    Function scoped fixture to enable all the test cases utilize common func.
    Args:
        fxHermesRunSettings: Hermes command line arguments (part of conftest.py so no explicit import).
        fxBlockchain: Blockchain fixture.
        fxConnection: Connection to helen.
    Returns:
        replica_db: Path of DB of replica nodes.
        clients: Detailed list of clients.
        clients_db: Path of DB of client nodes.
        client_hosts: Participant hosts.
        concord_hosts: Concord (committer) replicas.
    '''
    log.info("\n\nBlockchain fixture is {}".format(fxBlockchain))
    clients = fxConnection.request.get_participant_details(blockchain_id=fxBlockchain.blockchainId)
    log.info("Clients {}".format(clients))

    client_hosts, concord_hosts = format_hosts_structure(fxBlockchain.replicas)
    log.info("\nIn fxLocalSetup fixture, Participants are {}\nCommitters are {}\n".format(
        client_hosts, concord_hosts))

    version = backup_restore_helper.get_product_version(concord_hosts[0]).split(".")
    if int(version[0]) == 0:
        if int(version[1]) == 9:
            client_node_db = "/config/daml-index-db/db/"
            replica_node_db = "/config/concord/rocksdbdata/"
        else:
            client_node_db = "/mnt/data/db/"
            replica_node_db = "/mnt/data/rocksdbdata/"
    else:
        client_node_db = "/mnt/data/db/"
        replica_node_db = "/mnt/data/rocksdbdata/"

    local_tuple = LocalSetupFixture(replica_db=replica_node_db, clients=clients, clients_db=client_node_db,
                                    client_hosts=client_hosts, concord_hosts=concord_hosts)
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


def check_replica_block(replica_nodes):
    '''
    Verify all the replicas have the same blocks after backup or restore
    - get last block id of one replica.
    - get raw block detail of the last block.
    - compare this raw block detail with all other replicas of the network.
    Args:
        replica_nodes: List of replica node details of network.
    '''
    length = len(replica_nodes)
    block_id = backup_restore_helper.get_block_id(replica_nodes[length - 1])
    raw_block_detail = backup_restore_helper.get_raw_block(replica_nodes[length-1], block_id)
    while length > 1:
        length = length - 1
        node_raw_block_detail = backup_restore_helper.get_raw_block(replica_nodes[length-1], block_id)
        assert node_raw_block_detail == raw_block_detail, \
            'Contents of last block do not match for the nodes {} & {}'.format(replica_nodes[length-1],
                                                                               replica_nodes[-1])


@describe("test participants post backup")
def test_participants_post_backup(fxLocalSetup, fxHermesRunSettings):
    '''
    Verify participant node post backup.
    - Connect to a participant node of a group.
    - Take backup including config.
    - Run DAML test.
    - Repeat it for one Participant of a group.
    Args:
        fxLocalSetup: Local fixture
        fxHermesRunSettings: Hermes command line arguments
    '''
    log.info("Test participant post backup")
    output_dir = fxHermesRunSettings["hermesTestLogDir"]
    client_nodes = fxLocalSetup.clients
    client_group = None
    for client_node in client_nodes:
        if client_group != client_node['group_name']:
            client_group = client_node['group_name']
            assert backup_restore_helper.node_backup(client_node['private_ip'], fxLocalSetup.clients_db,
                                                     backup_restore_helper.CLIENT), \
                'Backup failed on {}'.format(client_node)
            assert helper.run_daml_sanity(fxLocalSetup.client_hosts, output_dir), 'DAML test failed'


@describe("test replicas post backup")
def test_replicas_post_backup(fxLocalSetup, fxHermesRunSettings):
    '''
    Verify replica node post backup.
    - Connect to a replica node of network.
    - Take backup of the node.
    - Repeat it for each replica of network..
    - Run DAML test.
    - Compare serialized raw block of each replicas.
    Args:
        fxLocalSetup: Local fixture
        fxHermesRunSettings: Hermes command line arguments
    '''
    log.info("Test replica post backup")
    output_dir = fxHermesRunSettings["hermesTestLogDir"]
    replica_nodes = fxLocalSetup.concord_hosts
    for replica_node in replica_nodes:
        assert backup_restore_helper.node_backup(replica_node, fxLocalSetup.replica_db,
                                                 backup_restore_helper.REPLICA),\
            'Backup failed on {}'.format(replica_node)
    assert helper.run_daml_sanity(fxLocalSetup.client_hosts, output_dir), 'DAML test failed'
    check_replica_block(fxLocalSetup.concord_hosts)


@describe("test all replica post restore")
def test_all_replicas_post_restore(fxLocalSetup, fxHermesRunSettings):
    '''
    Verify all replica post restore.
    - Take backup on all client and replica nodes.
    - Perform some transaction.
    - Stop client node.
    - Restore backups of replica nodes.
    - Take the latest backup from the node, restore it in all other nodes.
    - Start all replicas with incremental sequential waits between replicas.
    - Restore backup on client nodes.
    - Start client nodes.
    - Run DAML test.
    - Compare serialized raw block of each replicas.
    Args:
        fxLocalSetup: Local fixture
        fxHermesRunSettings: Hermes command line arguments
    '''
    log.info('Test all replicas post restore')
    output_dir = fxHermesRunSettings["hermesTestLogDir"]
    client_nodes = fxLocalSetup.client_hosts
    replica_nodes = fxLocalSetup.concord_hosts

    for client_node in client_nodes:
        backup_restore_helper.node_backup(client_node, fxLocalSetup.clients_db, backup_restore_helper.CLIENT)
    for replica_node in replica_nodes:
        backup_restore_helper.node_backup(replica_node, fxLocalSetup.replica_db, backup_restore_helper.REPLICA)

    assert helper.run_daml_sanity(fxLocalSetup.client_hosts, output_dir), 'DAML test failed'

    for client_node in client_nodes:
        backup_restore_helper.node_start_stop(client_node, backup_restore_helper.STOP_NODE)

    # stopping one and restoring the backup.
    for replica_node in replica_nodes:
        assert backup_restore_helper.node_restore(replica_node, fxLocalSetup.replica_db,
                                                  backup_restore_helper.REPLICA, skip_start=True), \
            'Restoration failed on {}'.format(replica_node)
    # getting the latest backup from a node.
    replicas_with_block = {}
    for replica_node in replica_nodes:
        replicas_with_block[replica_node] = \
            int(backup_restore_helper.get_block_id(replica_node, skip_start=True, skip_stop=True))

    replica_with_highest_block = max(replicas_with_block.items(), key=lambda x: x[1])
    log.info('Max block id: {}'.format(replica_with_highest_block[1]))
    log.info('Node With Max block id: {}'.format(replica_with_highest_block[0]))

    # restore latest block on all other nodes
    for replica_node in replica_nodes:
        if replica_node != replica_with_highest_block[0]:
            assert backup_restore_helper.cross_node_restore(replica_with_highest_block[0],
                                                            replica_node, backup_restore_helper.REPLICA,
                                                            fxLocalSetup.replica_db, skip_start=True), \
                'Cross restore failed'

    factor = 0
    sec = 60
    log.info("Starting all the Nodes")
    for replica_node in replica_nodes:
        time.sleep(factor * factor * sec + 60)
        backup_restore_helper.node_start_stop(replica_node, backup_restore_helper.START_NODE)
        factor = factor + 1
    log.info("Nodes started successfully")

    for client_node in client_nodes:
        backup_restore_helper.node_restore(client_node, fxLocalSetup.clients_db, backup_restore_helper.CLIENT)

    assert helper.run_daml_sanity(fxLocalSetup.client_hosts, output_dir), 'DAML test failed'
    check_replica_block(fxLocalSetup.concord_hosts)


@describe("test participants post restore")
def test_participants_post_restore(fxLocalSetup, fxHermesRunSettings):
    '''
    Verify participant nodes post restore
    - Connect to a participant node of network.
    - Take backup on participant of a group.
    - Perform some transaction.
    - Restore the backup on all participants where backup taken/available.
    - Run DAML test.
    Args:
        fxLocalSetup: Local fixture
        fxHermesRunSettings: Hermes command line arguments
    '''
    log.info('Test participant post restore')
    output_dir = fxHermesRunSettings["hermesTestLogDir"]
    client_nodes = fxLocalSetup.clients
    client_group = None
    nodes_for_restore = []

    for client_node in client_nodes:
        if client_group != client_node['group_name']:
            client_group = client_node['group_name']
            nodes_for_restore.append(client_node)
            backup_restore_helper.node_backup(client_node['private_ip'], fxLocalSetup.clients_db,
                                              backup_restore_helper.CLIENT)
    assert helper.run_daml_sanity(fxLocalSetup.client_hosts, output_dir), 'DAML test failed'
    for client_node in nodes_for_restore:
        assert backup_restore_helper.node_restore(client_node['private_ip'], fxLocalSetup.clients_db,
                                                  backup_restore_helper.CLIENT), \
            'Restore failed on {}'.format(client_node['private_ip'])
        assert helper.run_daml_sanity(fxLocalSetup.client_hosts, output_dir), 'DAML test failed'
