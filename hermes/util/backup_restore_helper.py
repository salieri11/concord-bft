#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
from datetime import datetime
import time
from util import helper, hermes_logging, rocksdb_helper

log = hermes_logging.getMainLogger()
BASE_BACKUP_PATH = '/backup/'
REMOTE_PATH = '/tmp/'
REPLICA = 'replica'
CLIENT = 'client'


def check_backup(blockchain_id, node):
    '''
    Function to check if a backup is available on a node.
    Args:
        blockchain_id: Blockchain id
        node: Node to check backup.
    Returns:
        True when the backup is available.
        False when there are no backup.
    '''
    backup_status = False
    backup_path = BASE_BACKUP_PATH
    cmd = "ls -td -- {}/*/*/ | head -n 1".format(backup_path + node)

    latest_backup_path = helper.\
        exec_cmd_on_node(node,
                         cmd,
                         blockchain_id=blockchain_id).rstrip("\n").rstrip("\r")
    if 'No such file or directory' not in latest_backup_path:
        backup_status = True

    return backup_status


def get_backup_command(blockchain_id, node, path):
    '''
    Function to get the backup command.
    Args:
        blockchain_id: Blockchain id
        node: Node to take backup.
        path: Path of folder to take backup.
    Returns:
        tar cvzf when the files to backup is less than 64GB.
        rsync -avh when the files to backup is more than 64GB.
    '''
    cmd = 'du -sh {}'.format(path)
    status_post_action = helper.\
        exec_cmd_on_node(node,
                         cmd,
                         blockchain_id=blockchain_id)
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


def node_backup(blockchain_id, node, remote_file_path, node_type, skip_start=False, skip_stop=False):
    '''
    Function to backup from node.
    Args:
        blockchain_id: Blockchain id
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
        assert helper.node_start_stop(blockchain_id, node, helper.STOP_NODE), 'Failed to Stop Node {}'.format(node)
    backup_path = BASE_BACKUP_PATH

    # to avoid over writing of backup, new folder is created with date and current time during execution.
    time_date = str(datetime.fromtimestamp(time.time()))
    backup_path = backup_path + node + "/" + time_date.replace(" ", "/")[:-7]
    log.debug("Remote file path: {}\nBackup file path: {}".format(remote_file_path, backup_path))

    cmd = "mkdir -p {}; {} {} {}".format(backup_path, get_backup_command(blockchain_id, node, remote_file_path),
                                         (backup_path + "/db-backup.tar.gz"), remote_file_path)
    if node_type == CLIENT:
        config_backup_cmd = "tar cvzf {} /config/daml-ledger-api/environment-vars".\
            format((backup_path + "/env-backup.tar.gz"))
        cmd = cmd + ";" + config_backup_cmd
    status_post_action = helper.exec_cmd_on_node(node, cmd, blockchain_id=blockchain_id)
    if 'No such file or directory' in status_post_action or 'errors' in status_post_action:
        log.error('Failed to take backup due to error {}'.format(status_post_action))
        return False

    if not skip_start:
        assert helper.node_start_stop(blockchain_id, node, helper.START_NODE), 'Failed to Start Node {}'.format(node)
    log.info("Node backup completed")
    return True


def node_restore(blockchain_id, node, remote_file_path, node_type, skip_start=False, skip_stop=False, clean_metadata=True):
    '''
    Function to restore a backup of a node.
    Args:
        blockchain_id: Blockchain id
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
        assert helper.node_start_stop(blockchain_id, node, helper.STOP_NODE), 'Failed to Stop Node {}'.format(node)

    # getting the path of latest backup from the folder for restoration.
    cmd = "ls -td -- {}/*/*/ | head -n 1".format(backup_path + node)
    latest_backup_path = helper.\
        exec_cmd_on_node(node, cmd,
                         blockchain_id=blockchain_id).rstrip("\n").rstrip("\r")
    if 'No such file or directory' in latest_backup_path:
        log.error('Unable to get the backup path to restore\n{}'.format(latest_backup_path))
        return False

    cmd = "rm -rf {}; tar xzf {}db-backup.tar.gz --directory /".format(remote_file_path, latest_backup_path)
    status_post_action = helper.\
        exec_cmd_on_node(node, cmd,
                         blockchain_id=blockchain_id)
    if 'No such file or directory' in status_post_action:
        log.error('Unable to extract the backup file on {}\n{}'.format(node, status_post_action))
        return False
    if node_type == REPLICA and clean_metadata:
        status_post_action = rocksdb_helper.\
            rocksdbdata_node_restore(node, blockchain_id, remote_file_path)
    elif node_type == CLIENT:
        cmd = 'rm -rf /config/daml-ledger-api/environment-vars; ' \
              'tar xzf {}env-backup.tar.gz --directory /'.format(latest_backup_path)
        status_post_action = helper.exec_cmd_on_node(node, cmd, blockchain_id=blockchain_id )

    if not status_post_action or 'error' in status_post_action or 'failure' in status_post_action:
        log.error('Unable to restore {}'.format(status_post_action))
        return False
    if not skip_start:
        assert helper.node_start_stop(blockchain_id, node, helper.START_NODE), 'Failed to Stop Node {}'.format(node)
    log.info("Restore completed")
    return True


def cross_node_restore(blockchain_id, node, restore_node,
                       node_type, db_path, skip_start=False,
                       skip_stop=False, clean_metadata=True):
    '''
    Function to restore a backup of one node in another node.
    Args:
        blockchain_id: Blockchain id
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
    username, password = helper.getNodeCredentials(blockchain_id, node)
    backup_path = BASE_BACKUP_PATH
    remote_path = REMOTE_PATH
    cmd = "ls -td -- {}/*/*/ | head -n 1".format(backup_path + node)
    latest_backup_path = helper.\
        exec_cmd_on_node(node, cmd,
                         blockchain_id=blockchain_id).rstrip("\n").rstrip("\r")
    if 'cannot access' in latest_backup_path:
        log.error('Unable get the latest backup path\n{}'.format(latest_backup_path))
        return False

    cmd = "ssh-keyscan {} >> $HOME/.ssh/known_hosts;"\
          "sshpass -p {} rsync -a {}db-backup.tar.gz root@{}:{}".format(restore_node, password,
                                                                        latest_backup_path,
                                                                        restore_node,
                                                                        remote_path)
    status_post_action = helper.\
        exec_cmd_on_node(node, cmd, blockchain_id=blockchain_id)
    if 'failed' in status_post_action or 'error' in status_post_action:
        log.error('Unable to send backup from {} to {}'.format(node, restore_node))
        return False
    if not skip_stop:
        assert helper.node_start_stop(blockchain_id, restore_node, helper.STOP_NODE), 'Failed to Stop Node {}'.format(node)

    cmd = "rm -rf {}; tar xzf {}db-backup.tar.gz --directory /".format(db_path, remote_path)
    status_post_action = helper.\
        exec_cmd_on_node(restore_node, cmd, blockchain_id=blockchain_id)

    if 'No such file or directory' in status_post_action:
        log.error('Unable to extract the backup file on {}\n{}'.format(restore_node, status_post_action))
        return False

    if node_type == REPLICA and clean_metadata:
        status_post_action = rocksdb_helper.\
            rocksdbdata_node_restore(node, blockchain_id, db_path)
        if not status_post_action or 'Error' in status_post_action:
            log.error('Unable to clean metadata on {}\n{}'.format(node, status_post_action))
            return False

    if not skip_start:
        assert helper.node_start_stop(blockchain_id, restore_node, helper.START_NODE), 'Failed to Start Node {}'.format(node)
    log.info("Cross restore completed")
    return True
