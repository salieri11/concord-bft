#########################################################################
# Copyright 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

# Helper file that has to be copied onto the concord node and be executed to
# create deployment support bundle

import argparse
import logging
import tarfile
import os
import shutil
import subprocess
import tempfile
import time

files_folders_to_backup = [
   "/config",
   "/var/log/healthd.log",
   "/var/log/cloud-init.log",
   "/var/log/cloud-init-output.log",
   "/var/lib/cloud/instance/user-data.txt",
]

support_commands_to_execute = [
   "/usr/sbin/ifconfig",
   "/usr/sbin/ip route",
]

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
      log.debug("Back up log file/folder '{}'...".format(file_folder))
      dir, file = os.path.split(file_folder)
      dest = os.path.join(deployment_support_bundle_dir_path, file)
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

def copy_util(src, dest):
   '''
   Utility method to copy files & folders to a destination folder
   :param src: Source file or folder
   :param dest: Destination path
   '''
   try:
      if os.path.isfile(src):
         shutil.copy(src, dest)
      else:
         shutil.copytree(src, dest)
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
                                           stderr=output_logfile)
   else:
      completedProcess = subprocess.run(command, stdout=subprocess.PIPE,
                                        stderr=subprocess.STDOUT)
   try:
      completedProcess.check_returncode()
      if completedProcess.stdout:
         log.info("stdout: {}".format(
            completedProcess.stdout.decode().replace(os.linesep, "")))
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
   parser.add_argument("--concordIP", required=True,
                       help="concord node/replica IP")
   parser.add_argument("--dockerContainers", nargs='+', required=True,
                       help="list of expected docker containers to be running on concord node")

   args = parser.parse_args()

   logging.basicConfig(level=logging.INFO,
                       format='%(asctime)s %(levelname)s %(message)s',
                       datefmt='%Y-%m-%d %H:%M:%S')
   import util.hermes_logging
log = util.hermes_logging.getMainLogger()

   time_stamp=time.strftime("%Y-%m-%d-%H-%M-%S", time.gmtime())
   deployment_support_bundle_base_dir = tempfile.gettempdir()
   deployment_support_bundle_name = "deployment-support-bundle_{}_{}".format(
      args.concordIP, time_stamp)
   deployment_support_bundle_dir_path = os.path.join(
      deployment_support_bundle_base_dir, deployment_support_bundle_name)
   deployment_support_bundle_path = os.path.join(
      deployment_support_bundle_base_dir,
      "{}.tar.gz".format(deployment_support_bundle_name))
   os.makedirs(deployment_support_bundle_dir_path, exist_ok=True)

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

