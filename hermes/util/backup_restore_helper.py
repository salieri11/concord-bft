#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Script performing stop, start , backup and restore of client or replica nodes
#########################################################################


#########################################################################
# Example executions
# ./node_backup_restore.py client /temp/test/
#########################################################################

import subprocess
import logging
import sys
from datetime import datetime
import time
import argparse

from util import helper


version = 1.0
global node_type, action, local
backup_path = '/backup/'
node_dict = {"10.72.228.56": "replica"}

logger = logging.getLogger()
logging.basicConfig(level=logging.DEBUG)


def action_on_node(ip, activity):
    '''
   Method to start/ stop docker container on remote machine
   :param ip: ip of the remote machine
   :param activity:
   :return:
   '''
    logger.info("Performing action on docker containers")
    if version < 1.0:
        cmd = "docker ps -aq | grep -v $(docker ps -aq --filter='name=^/agent') | xargs docker {}".format(activity)
    elif version == 1.0:
        cmd = "curl -X POST 127.0.0.1:8546/api/node/management?action={}".format(activity)
    else:
        raise Exception("Invalid version")

    username, password = helper.getNodeCredentials()
    status_post_action = helper.ssh_connect(ip, username=username, password=password, command=cmd)

    if status_post_action is None:
        raise Exception("Stopping execution")
    logger.info("Log from remote VM \n{}".format(status_post_action))


def client_node_backup(ip):
    '''
   Method to take backup from remote machine
   :param ip: ip of the remote machine
   :return:
   '''
    logger.info("Starting backup process")
    if node == "client":
        remote_file_path = client_node_db
    else:
        remote_file_path = replica_node_db
    global backup_path
    time_date = str(datetime.fromtimestamp(time.time()))
    backup_path = backup_path + ip + "/" + time_date.replace(" ", "/")[:-7]
    logger.info("Remote file path: {}\nBackup file path: {}".format(remote_file_path, backup_path))

    cmd = "mkdir -p {}".format(backup_path)
    status_post_action = helper.ssh_connect(ip, username=username, password=password, command=cmd)
    logger.info("MSG from VM {}".format(status_post_action))
    cmd = "tar cvzf {} {}".format(backup_path + "/db-backup.tar.gz", remote_file_path)

    # if False:
    #     config_backup_cmd = "tar cvzf {} /config/daml-ledger-api/environmentvars".\
    #         format(backup_path + "/")
    #     cmd = cmd + ";" + config_backup_cmd

    status_post_action = helper.ssh_connect(ip, username=username, password=password, command=cmd)
    logger.info("MSG from VM {}".format(status_post_action))


def client_node_restore(ip):
    '''
   Method to takes backup from remote machine
   :param ip: ip of the remote machine
   :return:
   '''

    cmd = "ls -td -- {}/*/*/ | head -n 1".format(backup_path + ip)
    print(cmd)
    latest_backup_path = helper.ssh_connect(ip, username=username, password=password, command=cmd).rstrip("\n").rstrip(
        "\r")

    logger.info("Starting restoration process")

    if node_type == "client":
        remote_file_path = client_node_db
    else:
        remote_file_path = replica_node_db

    logger.info("Clearing remote file path before restoring")
    cmd = "rm -rf {}".format(remote_file_path)
    status_post_action = helper.ssh_connect(ip, username=username, password=password, command=cmd)

    if status_post_action is None:
        raise Exception("Stopping execution")
    logger.info("Log from remote VM \n{}".format(status_post_action))

    cmd = "tar xvzf {}db-backup.tar.gz --directory /".format(latest_backup_path)
    print(cmd)
    status_post_action = helper.ssh_connect(ip, username=username, password=password, command=cmd)
    logger.info("Log from remote VM \n{}".format(status_post_action))


def cross_node_restore(ip, remote_ip):
    remote_path = "/tmp/"
    cmd = "ls -td -- {}/*/*/ | head -n 1".format(backup_path + ip)
    latest_backup_path = helper.ssh_connect(ip, username=username, password=password, command=cmd).rstrip("\n"). \
        rstrip("\r")

    cmd = "ssh-keyscan {} >> $HOME/.ssh/known_hosts;"\
          "sshpass -p Bl0ckch@!n rsync -a {}db-backup.tar.gz root@{}:{}".format(remote_ip,
                                                                                latest_backup_path,
                                                                                remote_ip,
                                                                                remote_path)
    status_post_action = helper.ssh_connect(ip, username=username, password=password, command=cmd)

    action_on_node(remote_ip, "stop")
    if node_type == "replica":
        if version < 1.0:
            cmd = "rm -rf /config/concord/rocksdbdata/"
        else:
            cmd = "rm -rf /mnt/data/rocksdbdata/"
        status_post_action = helper.ssh_connect(remote_ip, username=username, password=password, command=cmd)

    cmd = "tar xvzf {}db-backup.tar.gz --directory /".format(remote_path)
    status_post_action = helper.ssh_connect(remote_ip, username=username, password=password, command=cmd)

    if node_type == "replica":
        cmd = 'image=$(docker images --format "{{.Repository}}:{{.Tag}}" | grep "concord-core");'
        if version >= 1.0:
            docker_cmd = 'docker run -it --entrypoint="" --mount type=bind,source=/mnt/data/rocksdbdata,target=' \
                         '/concord/rocksdbdata $image /concord/sparse_merkle_db_editor ' \
                         '/concord/rocksdbdata removeMetadata'
        else:
            docker_cmd = 'docker run -it --entrypoint="" --mount type=bind,source=/config/concord/rocksdbdata,' \
                         'target=/concord/rocksdbdata $image /concord/sparse_merkle_db_editor /concord/' \
                         'rocksdbdata removeMetadata'

        status_post_action = helper.ssh_connect(remote_ip, username=username, password=password,
                                                command=(cmd + docker_cmd))
    action_on_node(remote_ip, "start")
    return
