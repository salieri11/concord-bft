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
    "LocalSetupFixture", "replicas, replica_db, clients, clients_db, client_hosts, concord_hosts")


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
        replicas: Detailed list of replicas.
        replica_db: Path of DB of replica nodes.
        clients: Detailed list of clients.
        clients_db: Path of DB of client nodes.
        client_hosts: Participant hosts.
        concord_hosts: Concord (committer) replicas.
    '''
    log.info("\n\nBlockchain fixture is {}".format(fxBlockchain))
    blockchain_details = fxConnection.request.getBlockchainDetails(blockchainId=fxBlockchain.blockchainId)
    log.info("blockchain details {}".format(blockchain_details))
    version = (blockchain_details['version'].split(" ")[2][:-1]).split(".")
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

    replicas = fxConnection.request.getReplicas(blockchainId=fxBlockchain.blockchainId)
    log.info("Replica details {}".format(replicas))
    clients = fxConnection.request.get_participant_details(blockchain_id=fxBlockchain.blockchainId)
    log.info("Clients {}".format(clients))

    client_hosts, concord_hosts = format_hosts_structure(fxBlockchain.replicas)
    log.info("\nIn fxLocalSetup fixture, Participants are {}\nCommitters are {}\n".format(
        client_hosts, concord_hosts))
    local_tuple = LocalSetupFixture(replicas=replicas, replica_db=replica_node_db,
                                    clients=clients, clients_db=client_node_db, client_hosts=client_hosts,
                                    concord_hosts=concord_hosts)

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


def test_participant_post_backup(fxLocalSetup, fxHermesRunSettings):
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
            backup_restore_helper.node_backup(client_node['private_ip'], fxLocalSetup.clients_db)
            assert helper.run_daml_sanity(fxLocalSetup.client_hosts, output_dir), 'DAML test failed'


def test_replica_post_backup(fxLocalSetup, fxHermesRunSettings):
    '''
    Verify replica node post backup.
    - Connect to a replica node of network.
    - Take backup of the node.
    - Repeat it for each replica of network.
    - Wait for 5 min, as all nodes sync up.
    - Run DAML test.
    - Compare serialized raw block of each replicas.
    Args:
        fxLocalSetup: Local fixture
        fxHermesRunSettings: Hermes command line arguments
    '''
    log.info("Test replica post backup")
    output_dir = fxHermesRunSettings["hermesTestLogDir"]
    replica_nodes = fxLocalSetup.replicas
    for replica_node in replica_nodes:
        backup_restore_helper.node_backup(replica_node['private_ip'], fxLocalSetup.replica_db)
    time.sleep(300)
    assert helper.run_daml_sanity(fxLocalSetup.client_hosts, output_dir), 'DAML test failed'
    replica_block(fxLocalSetup, fxLocalSetup.concord_hosts)


def test_replica_post_restore(fxLocalSetup, fxHermesRunSettings):
    '''
    Verify replicas post restore.
    - Connect to a replica node of network.
    - Take backup if it is not available for all the replica nodes.
    - Restore the backup on replica.
    - Repeat it for each replica of network.
    - Wait for 5 min, as all nodes sync up.
    - Run DAML test.
    - Compare serialized raw block of each replicas.
    Args:
        fxLocalSetup: Local fixture
        fxHermesRunSettings: Hermes command line arguments
    '''

    log.info("Test replica post restore")
    output_dir = fxHermesRunSettings["hermesTestLogDir"]
    for replica_node in fxLocalSetup.replicas:
        if not backup_restore_helper.check_backup(replica_node['private_ip']):
            backup_restore_helper.node_backup(replica_node['private_ip'], fxLocalSetup.replica_db)
            time.sleep(300)
    assert helper.run_daml_sanity(fxLocalSetup.client_hosts, output_dir), 'DAML test failed'

    for replica_node in fxLocalSetup.replicas:
        backup_restore_helper.node_restore(replica_node['private_ip'], fxLocalSetup.replica_db,
                                           backup_restore_helper.REPLICA)
        time.sleep(600)
        assert helper.run_daml_sanity(fxLocalSetup.client_hosts, output_dir), 'DAML test failed'

    replica_block(fxLocalSetup, fxLocalSetup.concord_hosts)


def test_all_replicas_post_restore(fxLocalSetup, fxHermesRunSettings):
    '''
    Verify all replica post restore.
    - Connect to a replica node of network.
    - Take backup if it is not available for all the replica nodes.
    - Restore the backup on all replica and do not start any replica.
    - Start all replicas with incremental sequential waits between replicas.
    - Wait for 5 min, as all nodes sync up.
    - Run DAML test.
    - Compare serialized raw block of each replicas.
    Args:
        fxLocalSetup: Local fixture
        fxHermesRunSettings: Hermes command line arguments
    '''
    log.info('Test all replicas post restore')
    output_dir = fxHermesRunSettings["hermesTestLogDir"]
    # log.info(fxLocalSetup.client_hosts)
    client_nodes = fxLocalSetup.clients
    for client_node in client_nodes:
        if not backup_restore_helper.node_backup(client_node['private_ip'], fxLocalSetup.clients_db, skip_start=True):
            raise Exception("Client backup failed")
    replica_nodes = fxLocalSetup.replicas
    for replica_node in replica_nodes:
        backup_restore_helper.node_backup(replica_node['private_ip'], fxLocalSetup.replica_db)
    time.sleep(300)
    for replica_node in replica_nodes:
        backup_restore_helper.node_restore(replica_node['private_ip'], fxLocalSetup.replica_db,
                                           backup_restore_helper.REPLICA, skip_start=True)
    replicas_with_block = {}
    for replica_node in replica_nodes:
        replicas_with_block[replica_node['private_ip']] = \
            int(backup_restore_helper.get_block_id(replica_node['private_ip'], skip_start=True, skip_stop=True))

    replica_with_highest_block = max(replicas_with_block.items(), key=lambda x: x[1])
    log.info('Max block id: {}'.format(replica_with_highest_block[1]))
    log.info('Node With Max block id: {}'.format(replica_with_highest_block[0]))

    for replica_node in replica_nodes:
        if replica_node['private_ip'] != replica_with_highest_block[0]:
            backup_restore_helper.cross_node_restore(replica_with_highest_block[0],
                                                     replica_node['private_ip'], backup_restore_helper.REPLICA,
                                                     fxLocalSetup.replica_db, skip_start=True)

    factor = 0
    sec = 60
    log.info("Starting all the Nodes")
    for replica_node in replica_nodes:
        time.sleep(factor * factor * sec + 60)
        backup_restore_helper.node_start_stop(replica_node['private_ip'], 'start')
        factor = factor + 1
    log.info("Node started successfully")
    time.sleep(300)
    for client_node in client_nodes:
        backup_restore_helper.node_restore(client_node['private_ip'], fxLocalSetup.clients_db,
                                           backup_restore_helper.CLIENT)
    assert helper.run_daml_sanity(fxLocalSetup.client_hosts, output_dir), 'DAML test failed'
    replica_block(fxLocalSetup, fxLocalSetup.concord_hosts)


def test_participant_post_restore(fxLocalSetup, fxHermesRunSettings):
    '''
    Verify participant nodes post restore
    - Connect to a participant node of network.
    - Take backup if it is not available on on participant of a group.
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
            backup_restore_helper.node_backup(client_node['private_ip'], fxLocalSetup.clients_db)

    for client_node in nodes_for_restore:
        backup_restore_helper.node_restore(client_node['private_ip'], fxLocalSetup.clients_db,
                                           backup_restore_helper.CLIENT)
    assert helper.run_daml_sanity(fxLocalSetup.client_hosts, output_dir), 'DAML test failed'


def replica_block(replica_nodes):
    '''
    Verify all the replicas have the same blocks after backup or restore
    - get last block id of one replica.
    - get raw block detail of the last block.
    - compare this raw block detail with all other replicas of the network.
    Args:
        replica_nodes: List of replica node details of network.
    '''
    i = len(replica_nodes)
    block_id = backup_restore_helper.get_block_id(replica_nodes[i - 1])
    raw_block_detail = backup_restore_helper.get_raw_block(replica_nodes[i-1], block_id)
    while i > 0:
        time.sleep(120)
        i = i - 1
        node_raw_block_detail = backup_restore_helper.get_raw_block(replica_nodes[i-1], block_id)
        assert node_raw_block_detail == raw_block_detail, 'Failed to assert block'
