#!/usr/bin/env python3
############################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential #
############################################################################
#
# CLI version (standalone) utility file to run DLR on supplied
# daml participant node IPs

from os import path
from pathlib import Path
import util.helper as helper
from util.daml import daml_helper
import util.hermes_logging as hermes_logging

log = hermes_logging.getMainLogger()

DLR_TEST_LAUNCHER = "util/dlr/dlr_test_launcher.sh"
DEFAULT_NO_OF_AGREEMENT = 100
DEFAULT_NO_OF_VUSER = 10
DEFAULT_LOAD_BATCH_SIZE = 100
DEFAULT_TEST_TIMEOUT = "600" # 30 mins in seconds
DLR_PATH = str(Path.home())
LOCALHOST = "localhost"

def install_daml():
   '''
      Installs Latest Daml version
   '''
   daml_sdk = daml_helper.get_ledger_api_version(LOCALHOST)
   if daml_sdk:
      daml_helper.install_daml_sdk(daml_sdk)
   else:
      daml_helper.install_daml_sdk()

def check_k6_tool():
   '''
      Open source load testing tool
      :return: k6 tool status
   '''
   k6_path = path.join("/usr", "local", "bin", "k6")
   log.debug("Checking if k6 is present at: {}".format(k6_path))
   if path.exists(k6_path):
      log.debug("K6 tool is available")
      k6_status = True
   else:
      log.error("k6 tool is not available")
      k6_status = False
   return k6_status

def check_nodejs():
   '''
      Node js version 10 or higher is required for DLR test
      :return: node version status
   '''
   log.debug("Checking nodeJS status")
   cmd_node_version = ["node", "--version"]
   success_node_version, output_node_version = helper.execute_ext_command(cmd_node_version)
   if success_node_version:
      node_version = output_node_version.split('.')
      if int(node_version[0].strip('v')) < 10:
         node_version_status = False
         log.warn("NodeJS version should be 10 or higher, current version: {}".format(output_node_version))
      else:
         log.debug("Node is available with version: {}".format(output_node_version))
         node_version_status = True
   else:
      log.error("NodeJS is not present")
      node_version_status = False
   return node_version_status

def check_broadridge_repo():
   '''
      Workload for DLR tool currently available at
      vmware@10.40.205.205:~/bk_Broadridge
      :return broadridge repository availability status
   '''
   node_path = path.join(DLR_PATH, "bk_Broadridge", "vmbc-br", "rep_app", "orchestration")
   cmd_npm = [ "npm", "install", f"{node_path}" ]
   if path.exists(node_path):
      log.debug("Broadridge repository exists")
      if path.exists(node_path + "/node_modules"):
         log.debug("Node modules are already installed")
         repository_status = True
      else:
         log.debug("Installing node modules at: {}".format(node_path))
         success_npm, output_npm = helper.execute_ext_command(cmd_npm)
         if success_npm:
            repository_status = True
         else:
            repository_status = False
            log.error("Unable to install node modules. Details: {}".format(output_npm))
   else:
      repository_status = False
      log.error("Broadridge repository is not available at: {}".format(DLR_PATH))
   return repository_status

def run_dlr(args, ledger_api_host):
   '''
      Run DLR on a participant IP
      :param args: Command line arguments
      :param ledger_api_host: Participant node IP
      :return: DLR run status
   '''
   cmd = [
      "timeout", DEFAULT_TEST_TIMEOUT,
      "bash", DLR_TEST_LAUNCHER,
      "--ledgerHost", ledger_api_host,
      "--noOfAgreements", args.dlrNoOfAgreements,
      "--noOfVuser", args.dlrNoOfVuser,
      "--loadBatchSize", args.dlrLoadBatchSize,
      "--resultsDir", args.resultsDir,
      "--dlrLocation", DLR_PATH,
      "--logLevel", str(args.logLevel)
   ]
   log.debug("Executing command: {}".format(cmd))
   status, output = helper.execute_ext_command(cmd)
   return status