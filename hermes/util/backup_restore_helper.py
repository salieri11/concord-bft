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
START_NODE = 'start'
STOP_NODE = 'stop'
REPLICA = 'replica'
CLIENT = 'client'


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
    latest_backup_path = helper.ssh_connect(node, username=username, password=password, command=cmd).rstrip("\n").rstrip(
        "\r")
    if 'No such file or directory' in latest_backup_path:
        return False
    else:
        return True


def get_block_id(node, skip_start=False, skip_stop=False):
    '''
    Function to get last block id on a node.
    Args:
        node: Node to get last block id.
    Returns:
        True when the backup is available.
        False when there are no backup.
   '''
    log.info('Getting block id for {}'.format(node))
    if not skip_stop:
        node_start_stop(node, STOP_NODE)
    cmd = 'image=$(docker images --format "{{.Repository}}:{{.Tag}}" | grep "concord-core");'
    docker_cmd = 'docker run -it --entrypoint="" --mount type=bind,source=/mnt/data/rocksdbdata,' \
                 'target=/concord/rocksdbdata $image /concord/sparse_merkle_db_editor ' \
                 '/concord/rocksdbdata getLastBlockID'
    status_post_action = helper.ssh_connect(node, username=username, password=password,
                                            command=(cmd + docker_cmd))
    log.info("{} IP with block Id {}".format(node, status_post_action))

    block_id = json.loads(status_post_action)
    if not skip_start:
        node_start_stop(node, START_NODE)
    # docker_cmd = 'docker run -it --entrypoint="" --mount type=bind,source=/mnt/data/rocksdbdata,' \
    #              'target=/concord/rocksdbdata $image /concord/sparse_merkle_db_editor ' \
    #              '/concord/rocksdbdata getLastReachableBlockID'
    # status_post_action = helper.ssh_connect(ip, username=username, password=password,
    #                                         command=(cmd + docker_cmd))
    # 'getLastReachableBlockID'
    # logger.info("{} IP with reachable block Id {}".format(ip, status_post_action))

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
    node_start_stop(node, STOP_NODE)
    cmd = 'image=$(docker images --format "{{.Repository}}:{{.Tag}}" | grep "concord-core");'
    docker_cmd = 'docker run -it --entrypoint="" --mount type=bind,source=/mnt/data/rocksdbdata,' \
                 'target=/concord/rocksdbdata $image /concord/sparse_merkle_db_editor ' \
                 '/concord/rocksdbdata getRawBlock {}'.format(block_id)
    status_post_action = helper.ssh_connect(node, username=username, password=password,
                                            command=(cmd + docker_cmd))
    log.info("{} IP with raw block {}".format(node, status_post_action))
    if 'NotFoundException' in status_post_action:
        return False
    node_start_stop(node, START_NODE)
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
    # cmd = "docker ps -aq | grep -v $(docker ps -aq --filter='name=^/agent') | xargs docker {}".format(action)
    status_post_action = helper.ssh_connect(node, username=username, password=password, command=cmd)

    if status_post_action is None or '"docker {}" requires at least 1 argument'.format(action) in status_post_action:
        log.info('Unable to {} containers with following error{}'.format(action, status_post_action))
        return False

    log.debug("Log from remote VM \n{}".format(status_post_action))
    log.debug("{} on {} completed".format(action.upper(), node))
    return True


def node_backup(node, remote_file_path, skip_start=False, skip_stop=False):
    '''
    Function to backup from node.
    Args:
        node: Node to take backup.
        remote_file_path: Location of DB to take backup.
        skip_start: skips starting the node.
        skip_stop: skips stopping the node.
    Returns:
        True when the backup is completed.
        False when backup fails.
   '''
    log.info("Starting backup process on {}".format(node))
    if not skip_stop:
        if not node_start_stop(node, STOP_NODE):
            return False
    backup_path = BASE_BACKUP_PATH

    # to avoid over writing of backup, new folder is created with date and current time during execution.
    time_date = str(datetime.fromtimestamp(time.time()))
    backup_path = backup_path + node + "/" + time_date.replace(" ", "/")[:-7]
    log.info("Remote file path: {}\nBackup file path: {}".format(remote_file_path, backup_path))

    cmd = "mkdir -p {}".format(backup_path)
    status_post_action = helper.ssh_connect(node, username=username, password=password, command=cmd)
    log.debug("MSG from VM {}".format(status_post_action))
    cmd = "tar czf {} {}".format(backup_path + "/db-backup.tar.gz", remote_file_path)

    # if False:
    #     config_backup_cmd = "tar cvzf {} /config/daml-ledger-api/environmentvars".\
    #         format(backup_path + "/")
    #     cmd = cmd + ";" + config_backup_cmd

    status_post_action = helper.ssh_connect(node, username=username, password=password, command=cmd)
    log.info("MSG from VM {}".format(status_post_action))
    if 'No such file or directory' in status_post_action or 'errors' in status_post_action:
        log.info('Failed to take backup due to error {}'.format(status_post_action))
        return False
    if not skip_start:
        if not node_start_stop(node, START_NODE):
            return False
    log.info("Node backup completed")
    return True


def node_restore(node, remote_file_path, node_type, skip_start=False, skip_stop=False):
    '''
    Function to restore a backup of a node.
    Args:
        node: Node to take backup.
        remote_file_path: Location of DB to take backup.
        skip_start: skips starting the node.
        skip_stop: skips stopping the node.
    Returns:
        True when the restore is completed.
        False when restore fails.
   '''
    log.info("Starting restore process on {}".format(node))
    backup_path = BASE_BACKUP_PATH
    if not skip_stop:
        if not node_start_stop(node, STOP_NODE):
            return False

    # getting the path of latest backup from the folder for restoration.
    cmd = "ls -td -- {}/*/*/ | head -n 1".format(backup_path + node)
    latest_backup_path = helper.ssh_connect(node, username=username, password=password, command=cmd).rstrip("\n").rstrip(
        "\r")
    if 'No such file or directory' in latest_backup_path:
        log.debug('Unable to get the backup for restore')
        return False

    cmd = "rm -rf {}".format(remote_file_path)
    status_post_action = helper.ssh_connect(node, username=username, password=password, command=cmd)
    if status_post_action is None:
        log.debug('Unable clear the existing DB before restore {}'. format(status_post_action))
        return False

    cmd = "tar xzf {}db-backup.tar.gz --directory /".format(latest_backup_path)
    status_post_action = helper.ssh_connect(node, username=username, password=password, command=cmd)
    log.info("Restore Process {}".format(status_post_action))
    if node_type == "replica":
        image_cmd = 'image=$(docker images --format "{{.Repository}}:{{.Tag}}" | grep "concord-core")'

        docker_cmd = 'docker run -it --entrypoint="" --mount type=bind,source={},target=/concord/rocksdbdata ' \
                     '$image /concord/sparse_merkle_db_editor /concord/rocksdbdata removeMetadata; ' \
                     'rm /config/concord/config-generated/genSec_*'.format(remote_file_path)

        status_post_action = helper.ssh_connect(node, username=username, password=password, command=image_cmd + ';' + docker_cmd)
        log.info("Sanitizing {}".format(status_post_action))

    #remove
    if 'error' in status_post_action or 'failure' in status_post_action:
        log.debug('Unable to restore {}'.format(status_post_action))
        return False

    if not skip_start:
        time.sleep(300)
        if not node_start_stop(node, START_NODE):
            return False
    log.info("Restore completed")
    return True


def cross_node_restore(node, restore_node, node_type, db_path, skip_start=False, skip_stop=False):
    '''
    Function to restore a backup of one node in another node.
    Args:
        node: Node where backup is present.
        restore_node: Node where restore has to be performed.
        node_type: Type of the node. Either replica or client.
        db_path: DB path of the node where restore is performed.
        skip_start: skips starting the node.
        skip_stop: skips stopping the node.
    Returns:
        True when the restore is completed.
        False when restore fails.
   '''
    log.info("Cross restore started from {} to {}".format(node, restore_node))
    backup_path = BASE_BACKUP_PATH
    remote_path = "/tmp/"
    cmd = "ls -td -- {}/*/*/ | head -n 1".format(backup_path + node)
    latest_backup_path = helper.ssh_connect(node, username=username, password=password, command=cmd).rstrip("\n"). \
        rstrip("\r")

    cmd = "ssh-keyscan {} >> $HOME/.ssh/known_hosts;"\
          "sshpass -p {} rsync -a {}db-backup.tar.gz root@{}:{}".format(restore_node, password,
                                                                        latest_backup_path,
                                                                        restore_node,
                                                                        remote_path)
    status_post_action = helper.ssh_connect(node, username=username, password=password, command=cmd)

    if not skip_stop:
        if not node_start_stop(restore_node, STOP_NODE):
            return False

    cmd = "rm -rf {}".format(db_path)
    status_post_action = helper.ssh_connect(restore_node, username=username, password=password, command=cmd)

    cmd = "tar xzf {}db-backup.tar.gz --directory /".format(remote_path)
    status_post_action = helper.ssh_connect(restore_node, username=username, password=password, command=cmd)

    if node_type == "replica":
        cmd = 'image=$(docker images --format "{{.Repository}}:{{.Tag}}" | grep "concord-core");'

        docker_cmd = 'docker run -it --entrypoint="" --mount type=bind,source={},target=/concord/rocksdbdata ' \
                     '$image /concord/sparse_merkle_db_editor /concord/rocksdbdata removeMetadata; ' \
                     'rm /config/concord/config-generated/genSec_*'.format(db_path)

        status_post_action = helper.ssh_connect(restore_node, username=username, password=password,
                                                command=(cmd + docker_cmd))
    if not skip_start:
        if not node_start_stop(restore_node, START_NODE):
            return False
    log.info("Cross restore completed")
    return True


