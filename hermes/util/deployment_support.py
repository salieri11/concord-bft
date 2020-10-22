#########################################################################
# Copyright 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

# Helper file that has to be copied onto the concord node and be executed to
# create deployment support bundle

import argparse
import glob
import json
import logging
import os
import shutil
import subprocess
import tarfile
import tempfile
import time

DEFAULT_SUPPORT_LOG_PREFIX = "support"
files_folders_to_backup = [
   "/config",
   "/mnt/data/cores",
   "/var/log/healthd.log",
   "/var/log/cloud-init.log",
   "/var/log/cloud-init-output.log",
   "/var/lib/cloud/instance/user-data.txt",
]

path_to_exclude_backup = [
   "/config/concord/rocksdbdata"
]

support_commands_to_execute = [
   "/usr/sbin/ifconfig",
   "/usr/sbin/ip route",
]

def gather_product_logs(docker_containers):
   '''
   Method to gather product logs from concord node/replica
   :param docker_containers: List of expected docker containers to be running on the node
   '''
   for container in docker_containers:
      log.debug("Gathering '{}' product logs...".format(container))
      container_inspect_json = os.path.join(deployment_support_bundle_dir_path,
                                   "{}-inspect.json").format(container)
      command = ["docker", "inspect", "{}".format(container)]
      if execute_ext_command(command, container_inspect_json):
         with open(container_inspect_json, "r") as fp:
            data = json.load(fp)
            if data and "LogPath" in data[0]:
               log_path = data[0]["LogPath"]
               log.debug("LogPath: {}".format(log_path))
               for log_file in glob.glob("{}*".format(log_path)):
                  files_folders_to_backup.append({log_file: container})


def gather_deployment_docker_logs(docker_containers, deployment_support_bundle_dir_path):
   '''
   Method to gather docker logs from concord node/replica
   :param docker_containers: List of expected docker containers to be running on the node
   :param deployment_support_bundle_dir_path: Support bundle destination dir
   '''
   for container in docker_containers:
      log.debug("Gathering docker container '{}' logs...".format(container))
      container_log = os.path.join(deployment_support_bundle_dir_path,
                                   "{}.log").format(container)
      command = ["docker", "logs", "{}".format(container)]
      execute_ext_command(command, container_log)


def backup_logfiles_folders(deployment_support_bundle_dir_path):
   '''
   Method to take a backup of files and folders that are to be part of the support bundle
   :param deployment_support_bundle_dir_path: Support bundle destination dir
   '''
   for file_folder in files_folders_to_backup:
      sub_folder = ""
      if isinstance(file_folder, dict):
         for filepath in file_folder:
            sub_folder = file_folder[filepath]
            file_folder = filepath

      log.debug("Back up log file/folder '{}'...".format(file_folder))
      dir, file = os.path.split(file_folder)
      dest_dir = os.path.join(deployment_support_bundle_dir_path, sub_folder)
      os.makedirs(dest_dir, exist_ok = True)
      dest = os.path.join(dest_dir, file)
      copy_util(file_folder, dest)


def execute_support_commands(deployment_support_bundle_dir_path):
   '''
   Method to execute and capture the output of commands required for debugging
   :param deployment_support_bundle_dir_path: Support bundle destination dir
   '''
   for command in support_commands_to_execute:
      log.debug("Executing command: {}...".format(command))
      dir, file = os.path.split(command)
      output_filename = "{}.log".format(file.replace(' ', '-'))
      command_output_logfile_path = os.path.join(
         deployment_support_bundle_dir_path, output_filename)
      execute_ext_command(command.split(), command_output_logfile_path)

def get_ignore_backup_paths(path, filenames):
   '''
   Return ignore excluded directories
   '''
   ret = []
   for filename in filenames:
      if os.path.join(path, filename) in path_to_exclude_backup:
         log.info("Excluding {} from support logs".format(filename))
         ret.append(filename)
   return ret

def copy_util(src, dest):
   '''
   Utility method to copy files & folders to a destination folder
   :param src: Source file or folder
   :param dest: Destination path
   '''
   try:
      if os.path.isfile(src):
         shutil.copy(src, dest)
      elif os.path.isdir(src):
         shutil.copytree(src, dest, ignore=get_ignore_backup_paths)
   except Exception as e:
      log.error("Failed to copy: {}".format(e))


def execute_ext_command(command, logfile=None):
   '''
   Helper method to execute an external command
   :param command: command to be executed
   :param logfile: Optional file path to capture command output
   :return: True if command exit status is 0, else False
   '''
   log.debug("Executing external command: {}".format(command))

   if logfile:
      with open(logfile, "w") as output_logfile:
         completedProcess = subprocess.run(command, stdout=output_logfile,
                                           stderr=output_logfile,
                                           universal_newlines=True)
   else:
      completedProcess = subprocess.run(command, stdout=subprocess.PIPE,
                                        stderr=subprocess.STDOUT,
                                        universal_newlines=True)
   try:
      completedProcess.check_returncode()
      if completedProcess.stdout:
         log.info("stdout: {}".format(
            completedProcess.stdout))
      if completedProcess.stderr:
         log.error("stderr: {}".format(completedProcess.stderr))
   except subprocess.CalledProcessError as e:
      log.error(
         "Command '{}' failed to execute: {}".format(command, e.returncode))
      log.error("stdout: '{}', stderr: '{}'".format(completedProcess.stdout,
                                                    completedProcess.stderr))
      return False

   return True

def bundle_support_logs(src_dir, dest_file):
   '''
   Method to bundle support logs as tar.gz file
   :param src_dir: Source directory
   :param dest_file: Destination path of tar.gz file
   '''
   log.info("Creating '{}'...".format(dest_file))
   with tarfile.open(dest_file, "w:gz") as tar:
      tar.add(src_dir, arcname=os.path.basename(src_dir))
   

if __name__ == '__main__':
   parser = argparse.ArgumentParser()
   parser.add_argument("--supportBundleBaseDir", required=True,
                       help="Support bundle base path on blockchain nodes/replicas")
   parser.add_argument("--concordIP", required=True,
                       help="concord node/replica IP")
   parser.add_argument("--nodeType", default=DEFAULT_SUPPORT_LOG_PREFIX,
                       help="node type(committer/participant)")
   parser.add_argument("--dockerContainers", nargs='+', required=True,
                       help="list of expected docker containers to be running on concord node")

   args = parser.parse_args()

   logging.basicConfig(level=logging.INFO,
                       format='%(asctime)s %(levelname)s %(message)s',
                       datefmt='%Y-%m-%d %H:%M:%S')
   log = logging.getLogger(__name__)

   time_stamp=time.strftime("%Y-%m-%d-%H-%M-%S", time.gmtime())
   deployment_support_bundle_base_dir = args.supportBundleBaseDir
   if not os.path.exists(deployment_support_bundle_base_dir):
      os.makedirs(deployment_support_bundle_base_dir)
   deployment_support_bundle_name = "{}-logs_{}_{}".format(
      args.nodeType, args.concordIP, time_stamp)
   deployment_support_bundle_dir_path = os.path.join(
      deployment_support_bundle_base_dir, deployment_support_bundle_name)
   deployment_support_bundle_path = os.path.join(
      deployment_support_bundle_base_dir,
      "{}.tar.gz".format(deployment_support_bundle_name))
   os.makedirs(deployment_support_bundle_dir_path, exist_ok=True)

   gather_product_logs(args.dockerContainers)
   gather_deployment_docker_logs(args.dockerContainers,
                                 deployment_support_bundle_dir_path)
   backup_logfiles_folders(deployment_support_bundle_dir_path)
   execute_support_commands(deployment_support_bundle_dir_path)
   bundle_support_logs(deployment_support_bundle_dir_path,
                       deployment_support_bundle_path)

   if os.path.isfile(deployment_support_bundle_path):
      log.info("Support bundle created successfully: {}".format(
         deployment_support_bundle_path))
      exit(0)
   else:
      log.error("Failed to create deployment support bundle: {}".format(
         deployment_support_bundle_path))
      exit(1)

