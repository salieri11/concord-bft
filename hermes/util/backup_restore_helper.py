#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
from datetime import datetime
import time
import json
from util import helper, hermes_logging

log = hermes_logging.getMainLogger()
username, password = helper.getNodeCredentials()
BASE_BACKUP_PATH = '/backup/'
REMOTE_PATH = '/tmp/'
START_NODE = 'start'
STOP_NODE = 'stop'
REPLICA = 'replica'
CLIENT = 'client'


def get_product_version(node):
    '''
        Function to get product version on node.
        Args:
            node: Node to check backup.
        Returns:
            Version in '0.0.0.0000' format.
            False when unable to get version.
       '''
    cmd = 'docker inspect concord | grep "com.vmware.blockchain.version"'
    status_post_action = helper.ssh_connect(node, username, password, cmd)
    if status_post_action is None or status_post_action is '':
        log.error('Unable get the product version')
        return False
    return status_post_action.split(": ")[1].rstrip("\n").rstrip("\r").replace('"', '')


def check_backup(node):
    '''
    Function to check if a backup is available on a node.
    Args:
        node: Node to check backup.
    Returns:
        True when the backup is available.
        False when there are no backup.
   '''
    backup_path = BASE_BACKUP_PATH
    cmd = "ls -td -- {}/*/*/ | head -n 1".format(backup_path + node)
    latest_backup_path = helper.ssh_connect(node, username=username, password=password,
                                            command=cmd).rstrip("\n").rstrip("\r")
    if 'No such file or directory' in latest_backup_path:
        return False
    else:
        return True


def get_backup_command(node, path):
    '''
    Function to get the backup command.
    Args:
        node: Node to take backup.
        path: Path of folder to take backup.
    Returns:
        tar cvzf when the files to backup is less than 64GB.
        rsync -avh when the files to backup is more than 64GB.
    '''
    cmd = 'du -sh {}'.format(path)
    status_post_action = helper.ssh_connect(node, username, password, cmd)
    size = status_post_action.split('\t')[0]
    log.debug('{} with path {} has size {}'.format(node, path, size))
    if size[-1] is 'G' or size[-1] is 'T':
        if float(size[:-1]) > 64:
            print("grater tha 64GB")
            return 'rsync -avh'
        else:
            return 'tar cvzf'
    else:
        return 'tar cvzf'


def get_block_id(node, skip_start=False, skip_stop=False):
    '''
    Function to get last block id on a node.
    Args:
        node: Node to get last block id.
        skip_start: skips starting the node.
        skip_stop: skips stopping the node.
    Returns:
        LastBlockID when available.
        False when it fails to get block id.
   '''
    log.info('Getting block id for {}'.format(node))
    if not skip_stop:
        assert node_start_stop(node, STOP_NODE), 'Failed to Stop Node {}'.format(node)

    version = get_product_version(node)
    if not version:
        log.error('Unable to get product version')
        return False
    
    cmd = 'image=$(docker images --format "{{.Repository}}" | grep "concord-core");'
    docker_cmd = 'docker run -it --entrypoint="" --mount type=bind,source=/mnt/data/rocksdbdata,' \
                 'target=/concord/rocksdbdata $image:{} /concord/sparse_merkle_db_editor ' \
                 '/concord/rocksdbdata getLastBlockID'.format(version)
    status_post_action = helper.ssh_connect(node, username=username, password=password,
                                            command=(cmd + docker_cmd))
    if 'Failed to execute command' in status_post_action:
        log.error('Unable to get LastBlockID from {}'.format(node))
        return False
    log.debug("{} IP with block Id {}".format(node, status_post_action))
    block_id = json.loads(status_post_action)
    if not skip_start:
        assert node_start_stop(node, START_NODE), 'Failed to Start Node {}'.format(node)

    return block_id['lastBlockID']


def get_raw_block(node, block_id):
    '''
    Function to get raw block for a given block id on a node.
    Args:
        node: Node to check backup.
        block_id: block id for which raw block detail is taken.
    Returns:
        True when the backup is available.
        False when there are no backup.
   '''
    log.info('Getting raw block for {}'.format(node))
    assert node_start_stop(node, STOP_NODE), 'Failed to Stop Node {}'.format(node)

    version = get_product_version(node)
    if not version:
        log.error('Unable to get product version')
        return False

    cmd = 'image=$(docker images --format "{{.Repository}}" | grep "concord-core");'
    docker_cmd = 'docker run -it --entrypoint="" --mount type=bind,source=/mnt/data/rocksdbdata,' \
                 'target=/concord/rocksdbdata $image:{} /concord/sparse_merkle_db_editor ' \
                 '/concord/rocksdbdata getRawBlock {}'.format(version, block_id)
    status_post_action = helper.ssh_connect(node, username=username, password=password,
                                            command=(cmd + docker_cmd))
    log.debug("{} IP with raw block {}".format(node, status_post_action))
    if 'NotFoundException' in status_post_action:
        return False
    assert node_start_stop(node, START_NODE), 'Failed to Start Node {}'.format(node)
    return status_post_action


def node_start_stop(node, action):
    '''
    Function to start/stop all components except node-agent of participant or replica.
    Args:
        node: Node to start/stop.
        action: Either to start or stop node.
    Returns:
        True when the desired action is completed.
        False when fails perform the desired action.
   '''
    log.debug("{} containers on {}".format(action.upper(), node))

    cmd = "curl -X POST 127.0.0.1:8546/api/node/management?action={}".format(action)
    status_post_action = helper.ssh_connect(node, username=username, password=password, command=cmd)

    if status_post_action is None or 'Failed' in status_post_action or 'Connection refused' in status_post_action:
        log.error('Unable to {} containers.\nResponse {}'.format(action, status_post_action))
        return False

    log.debug("{} on {} completed".format(action.upper(), node))
    return True


def node_backup(node, remote_file_path, node_type, skip_start=False, skip_stop=False):
    '''
    Function to backup from node.
    Args:
        node: Node to take backup.
        remote_file_path: Location of DB to take backup.
        node_type: Type of the node.
        skip_start: skips starting the node.
        skip_stop: skips stopping the node.
    Returns:
        True when the backup is completed.
        False when backup fails.
   '''
    log.info("Starting backup process on {}".format(node))
    if not skip_stop:
        assert node_start_stop(node, STOP_NODE), 'Failed to Stop Node {}'.format(node)
    backup_path = BASE_BACKUP_PATH

    # to avoid over writing of backup, new folder is created with date and current time during execution.
    time_date = str(datetime.fromtimestamp(time.time()))
    backup_path = backup_path + node + "/" + time_date.replace(" ", "/")[:-7]
    log.debug("Remote file path: {}\nBackup file path: {}".format(remote_file_path, backup_path))

    cmd = "mkdir -p {}; {} {} {}".format(backup_path, get_backup_command(node, remote_file_path),
                                         (backup_path + "/db-backup.tar.gz"), remote_file_path)
    if node_type == CLIENT:
        config_backup_cmd = "tar cvzf {} /config/daml-ledger-api/environment-vars".\
            format((backup_path + "/env-backup.tar.gz"))
        cmd = cmd + ";" + config_backup_cmd
    status_post_action = helper.ssh_connect(node, username=username, password=password, command=cmd)
    if 'No such file or directory' in status_post_action or 'errors' in status_post_action:
        log.error('Failed to take backup due to error {}'.format(status_post_action))
        return False

    if not skip_start:
        assert node_start_stop(node, START_NODE), 'Failed to Start Node {}'.format(node)
    log.info("Node backup completed")
    return True


def node_restore(node, remote_file_path, node_type, skip_start=False, skip_stop=False, clean_metadata=True):
    '''
    Function to restore a backup of a node.
    Args:
        node: Node to take backup.
        remote_file_path: Location of DB to take backup.
        node_type: Type of the node.
        skip_start: skips starting the node.
        skip_stop: skips stopping the node.
        clean_metadata: cleans metadata of node.
    Returns:
        True when the restore is completed.
        False when restore fails.
   '''
    log.info("Starting restore process on {}".format(node))
    backup_path = BASE_BACKUP_PATH
    if not skip_stop:
        assert node_start_stop(node, STOP_NODE), 'Failed to Stop Node {}'.format(node)

    # getting the path of latest backup from the folder for restoration.
    cmd = "ls -td -- {}/*/*/ | head -n 1".format(backup_path + node)
    latest_backup_path = helper.ssh_connect(node, username=username, password=password,
                                            command=cmd).rstrip("\n").rstrip("\r")
    if 'No such file or directory' in latest_backup_path:
        log.error('Unable to get the backup path to restore\n{}'.format(latest_backup_path))
        return False

    cmd = "rm -rf {}; tar xzf {}db-backup.tar.gz --directory /".format(remote_file_path, latest_backup_path)
    status_post_action = helper.ssh_connect(node, username=username, password=password, command=cmd)
    if 'No such file or directory' in status_post_action:
        log.error('Unable to extract the backup file on {}\n{}'.format(node, status_post_action))
        return False
    if node_type == REPLICA and clean_metadata:
        version = get_product_version(node)
        if not version:
            log.error('Unable to get product version')
            return False
        image_cmd = 'image=$(docker images --format "{{.Repository}}" | grep "concord-core")'

        docker_cmd = 'docker run -it --entrypoint="" --mount type=bind,source={},target=/concord/rocksdbdata ' \
                     '$image:{} /concord/sparse_merkle_db_editor /concord/rocksdbdata removeMetadata; ' \
                     'rm -rf /config/concord/config-generated/genSec_*'.format(remote_file_path, version)
        cmd = image_cmd + ';' + docker_cmd
    else:
        cmd = 'rm -rf /config/daml-ledger-api/environment-vars; ' \
              'tar xzf {}env-backup.tar.gz --directory /'.format(latest_backup_path)

    status_post_action = helper.ssh_connect(node, username=username, password=password, command=cmd)
    if 'error' in status_post_action or 'failure' in status_post_action:
        log.error('Unable to restore {}'.format(status_post_action))
        return False
    if not skip_start:
        assert node_start_stop(node, START_NODE), 'Failed to Stop Node {}'.format(node)
    log.info("Restore completed")
    return True


def cross_node_restore(node, restore_node, node_type, db_path, skip_start=False, skip_stop=False, clean_metadata=True):
    '''
    Function to restore a backup of one node in another node.
    Args:
        node: Node where backup is present.
        restore_node: Node where restore has to be performed.
        node_type: Type of the node. Either replica or client.
        db_path: DB path of the node where restore is performed.
        skip_start: skips starting the node.
        skip_stop: skips stopping the node.
        clean_metadata: cleans metadata of node.
    Returns:
        True when the restore is completed.
        False when restore fails.
   '''
    log.info("Cross restore started from {} to {}".format(node, restore_node))
    backup_path = BASE_BACKUP_PATH
    remote_path = REMOTE_PATH
    cmd = "ls -td -- {}/*/*/ | head -n 1".format(backup_path + node)
    latest_backup_path = helper.ssh_connect(node, username=username, password=password, command=cmd).rstrip("\n"). \
        rstrip("\r")
    if 'cannot access' in latest_backup_path:
        log.error('Unable get the latest backup path\n{}'.format(latest_backup_path))
        return False

    cmd = "ssh-keyscan {} >> $HOME/.ssh/known_hosts;"\
          "sshpass -p {} rsync -a {}db-backup.tar.gz root@{}:{}".format(restore_node, password,
                                                                        latest_backup_path,
                                                                        restore_node,
                                                                        remote_path)
    status_post_action = helper.ssh_connect(node, username=username, password=password, command=cmd)
    if 'failed' in status_post_action or 'error' in status_post_action:
        log.error('Unable to send backup from {} to {}'.format(node, restore_node))
        return False
    if not skip_stop:
        assert node_start_stop(restore_node, STOP_NODE), 'Failed to Stop Node {}'.format(node)

    cmd = "rm -rf {}; tar xzf {}db-backup.tar.gz --directory /".format(db_path, remote_path)
    status_post_action = helper.ssh_connect(restore_node, username=username, password=password, command=cmd)
    if 'No such file or directory' in status_post_action:
        log.error('Unable to extract the backup file on {}\n{}'.format(restore_node, status_post_action))
        return False

    if node_type == REPLICA and clean_metadata:
        version = get_product_version(node)
        if not version:
            log.error('Unable to get product version')
            return False
        cmd = 'image=$(docker images --format "{{.Repository}}" | grep "concord-core");'

        docker_cmd = 'docker run -it --entrypoint="" --mount type=bind,source={},target=/concord/rocksdbdata ' \
                     '$image:{} /concord/sparse_merkle_db_editor /concord/rocksdbdata removeMetadata; ' \
                     'rm -rf /config/concord/config-generated/genSec_*'.format(db_path, version)

        status_post_action = helper.ssh_connect(restore_node, username=username, password=password,
                                                command=(cmd + docker_cmd))
        if 'Failed to execute command' in status_post_action or 'Error' in status_post_action:
            log.error('Unable to clean metadata on {}\n{}'.format(node, status_post_action))
            return False
    if not skip_start:
        assert node_start_stop(restore_node, START_NODE), 'Failed to Start Node {}'.format(node)
    log.info("Cross restore completed")
    return True
