#########################################################################
# Copyright 2019-2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

# Helper file with common utility methods
import base64
import collections
import os
import yaml
import json
import shutil
import logging
import paramiko
import pprint
import warnings
import cryptography
import re
import traceback
import socket
import subprocess
import sys
import tempfile
import time
import threading
import hashlib
import uuid
import socket
import struct
import statistics
from . import numbers_strings
from urllib.parse import urlparse, urlunparse
if 'hermes_util' in sys.modules.keys():
   import hermes_util.auth as auth
   import hermes_util.daml.daml_helper as daml_helper
   import hermes_util.json_helper as json_helper_util
   import hermes_util.hermes_logging as hermes_logging_util
else:
   import util.auth as auth
   import util.daml.daml_helper as daml_helper
   import util.json_helper as json_helper_util
   import util.hermes_logging as hermes_logging_util
   import rest


log = hermes_logging_util.getMainLogger()
docker_env_file = ".env"

# Command line args in dictionary format, set by `main.py` after argparse
CMDLINE_ARGS = {}

# The config file contains information about how to run things, as opposed to
# command line parameters, which are about running tests.
CONFIG_USER_FILE = "resources/user_config.json"
CONFIG_ZONE_FILE = "resources/zone_config.json"

# list of all agent-pulled components (in-node containers)
AGENT_PULLED_COMPONENTS_FILE = '../vars/agent_pulled_components.json'

# CONFIG_CACHED["data"] is populated by `loadConfigFile`
# (containing data from user_config.json OR command line argument overrides)
CONFIG_CACHED = {}

# These are command line options for --blockchainLocation.  Various parts
# of the code need to know them.
LOCATION_LOCAL = "local"
LOCATION_SDDC = "sddc"
LOCATION_ONPREM = "onprem"

# When creating a zone, it can be one of these.
ZONE_TYPE_ON_PREM = "ON_PREM"
ZONE_TYPE_SDDC = "VMC_AWS"

# Map --blockchainLocation to values for the Helen zone "type" parameter:
LOCATION_TO_ZONE_TYPES = {
   LOCATION_ONPREM: ZONE_TYPE_ON_PREM,
   LOCATION_SDDC: ZONE_TYPE_SDDC
}

# These are command line options for --blockchainType.
# These need to match Helen.  See helen/src/main/resources/api-doc/api.yaml.
TYPE_CHESSPLUS = "chessplus"
TYPE_ETHEREUM = "ethereum"
TYPE_DAML = "daml"
TYPE_DAML_COMMITTER = "daml_committer"
TYPE_DAML_PARTICIPANT = "daml_participant"
TYPE_HLF = "hlf"
TYPE_TEE = "tee"
TYPE_NO_VERIFY = "no verification required for this blockchain"

# Port forwarding for testing under VMware firewall
FORWARDED_DAML_LEDGER_API_ENDPOINT_PORT = 80

# These are command line options for --keepBlockchains.
KEEP_BLOCKCHAINS_ALWAYS = "always"
KEEP_BLOCKCHAINS_ON_FAILURE = "on-failure"
KEEP_BLOCKCHAINS_NEVER = "never"

# Credential type
CREDENTIAL_BEARER = "BEARER"
CREDENTIAL_PASSWORD = "PASSWORD"

# When creating a logging destination, its type can
# be one of these:
LOG_DESTINATION_LOG_INTELLIGENCE = "LOG_INTELLIGENCE"
LOG_DESTINATION_LOG_INSIGHT = "LOG_INSIGHT"

# Support bundle base path on blockchain nodes/replicas
DEPLOYMENT_SUPPORT_BUNDLE_BASE_DIR = "/var/log/deployment_support_logs"

# Health Reporting Daemon path
HEALTHD_CRASH_FILE = "/var/log/replica_crashed"
HEALTHD_LOG_PATH = "/var/log/healthd.log"
HEALTHD_RECENT_REPORT_PATH = "/var/log/healthd_recent_report.json"
HEALTHD_SLACK_NOTIFICATION_INTERVAL = 3600 # 1 hour

# Migration related constants
MIGRATION_FILE = "../docker/config-helen/app/db/migration/R__zone_entities.sql"
MIGRATION_BASE_PATH = "../docker/config-helen/app/db/migration"
MIGRATION_USER_ID = "51123b25-d017-4afa-8a1c-4e99badb24c6"
MIGRATION_USER_NAME = "svc.blockchain_1@vmware.com"

# Application properties related constants
PROPERTIES_TEST_FILE = "../docker/config-helen/app/profiles/application-test.properties"
PROPERTIES_VMBC_ENABLED_VMC_ZONES = "vmbc.enabled.vmc.zones"

# Persephone gRPC bindings paths
PERSEPHONE_GRPC_BINDINGS_SRC_PATH = "../persephone/api/src/protobuf"
PERSEPHONE_GRPC_BINDINGS_DEST_PATH = "lib/persephone"

# Where Racetrack setId (run identifier) is stored in Jenkins runs
# default: ${WORKSPACE}/blockchain/vars/racetrack_set_id.json
RACETRACK_SET_ID_FILE = "racetrack_set_id.json"
RACETRACK_SET_ID_PATH = "/blockchain/vars/" + RACETRACK_SET_ID_FILE

# Replicas Information
REPLICAS_JSON_FILE = "replicas.json"
REPLICAS_JSON_PATH = os.path.join("/tmp", REPLICAS_JSON_FILE)

# Long running test related
LONG_RUN_TEST_FILE = "resources/long_running_tests.json"

# Jenkins namespaces; used by `getJenkinsBuildTraceId` for Jenkins trace context.
# This future-proofs possible multi-Jenkins contexts with partners like DA/ASX/HK
# All metrics endpoints (Racetrack/Wavefront) will have distinguishable dataset to work with
JENKINS_NAMESPACE_MAIN = "JENKINS_VMWARE_BC_MAIN" # DO NOT CHANGE; front part of hash for traceId

# CI/CD Major Run Types
JENKINS_RUN_MAIN_MR = { "type": "MAIN_MR", "exactly": "Main Blockchain Run on GitLab" }
JENKINS_RUN_MASTER = { "type": "MASTER", "exactly": "Master Branch Blockchain Run on GitLab/master" }
JENKINS_RUN_RELEASE_BRANCH = { "type": "RELEASE", "contains": ["Branch Blockchain Run on GitLab/releases"],
  # For example, "0.5 Branch Blockchain Run on GitLab/releases/0.5" is a release branch job name
  # Extract meaningful variable (.e.g "0.5") as releases progress to different versions
  "variables":[{"name": "releaseVersion", "after":"/releases/"}],
  "format": "<RELEASE_VERSION> Branch Blockchain Run on GitLab/releases/<RELEASE_VERSION>" # full job name
}
JENKINS_MAJOR_RUN_TYPES = [ JENKINS_RUN_MAIN_MR, JENKINS_RUN_MASTER, JENKINS_RUN_RELEASE_BRANCH ]

# Used by invoke.py while setting Jenkins build description
GITLAB_BASE_URL = "https://gitlab.eng.vmware.com/blockchain/vmwathena_blockchain"
JIRA_PREFIX = "BC"
JIRA_BASE_URL = "https://jira.eng.vmware.com"

# Current suite name and log path set by `main.py`
CURRENT_SUITE_NAME = ""
CURRENT_SUITE_LOG_FILE = ""
CURRENT_SUITE_PRODUCT_ATTEMPT_NUMBER = 0 # set by product.py when retrying launchProduct

# Hermes-specific traces for NON-CRITICAL exceptions (avoiding log file contamination)
# e.g. exceptions that are good to know but does not belong in product & test logs
# e.g. Racetrack/wavefront report failure: doesn't affect test, but should be logged somewhere else
NON_CRITICAL_HERMES_EXCEPTIONS = []
NON_CRITICAL_HERMES_OUTPUT_FILE = "non_critical.log"
NON_CRITICAL_HERMES_OUTPUT_PATH = "/summary/" + NON_CRITICAL_HERMES_OUTPUT_FILE

# All free async threads that needs to be joined before script exits
# (populated by racetrack.py `reportAsync`)
FREE_ASYNC_THREADS_INDEX = 0
FREE_ASYNC_THREADS = {}

# Super user privilege; with all Jenkins injected credentials available in user_config
WITH_JENKINS_INJECTED_CREDENTIALS = False

# Time formats
TIME_FMT = "%Y-%m-%d %H:%M:%S"
TIME_FMT_TIMEZONE = "%Y-%m-%d %H:%M:%S %Z%z"


def copy_docker_env_file(docker_env_file=docker_env_file):
   '''
   This file contains variables fed to docker-compose.yml. It is picked up from
   the location of the process which invokes docker compose
   :param docker_env_file: docker .env file
   '''
   if not os.path.isfile(docker_env_file):
      log.debug("Copying {} file from docker/".format(docker_env_file))
      shutil.copyfile(os.path.join("../docker/", docker_env_file), docker_env_file)


def get_docker_env(key=None):
   '''
   Helper method to read docker .env file and return the value for a
   key being passed
   :param env_key: Key from the .env file
   :return: value
   '''
   copy_docker_env_file()

   env = {}
   with open(docker_env_file) as file:
      for line in file:
         env_key, env_val = line.partition("=")[::2]
         env[env_key.strip()] = env_val.strip()

   if key:
      if key in env.keys():
         return env[key]
      else:
         log.error("No entry found for key {}".format(key))
         return None
   else:
      return env


def service_defined(docker_compose_files, service_name):
   '''
   Returns whether the service service_name is defined in docker_compose_files.
   '''
   for docker_compose_file in docker_compose_files:
      log.debug("Parsing docker-compose file: {}".format(docker_compose_file))

      with open(docker_compose_file, "r") as yaml_file:
         compose_data = yaml.load(yaml_file, Loader=yaml.FullLoader)

      services = list(compose_data["services"])

      if service_name in services:
         return True

   return False


def get_docker_compose_value(docker_compose_files, service_name, key):
   '''
   Helper method to get docker compose value for a given key & service name
   from docker-compose-*.yml passed as command line argument
   :param docker_compose_files: cmdline arg dockerComposeFile
   :param service_name: microservice name
   :param key: key in a service defined in the prodvides docker-compose file(s)
   :return: value for the key passed for the service name
   '''
   service_name_found = False
   value_found = False
   for docker_compose_file in docker_compose_files:
      log.debug("Parsing docker-compose file: {}".format(docker_compose_file))
      with open(docker_compose_file, "r") as yaml_file:
         compose_data = yaml.load(yaml_file, Loader=yaml.FullLoader)

      services = list(compose_data["services"])
      if '/' in service_name:
         tmp_service_name = service_name.split('/')[1]
      else:
         tmp_service_name = service_name

      if tmp_service_name in services:
         service_name_found = True
         service_keys = compose_data['services'][tmp_service_name]
         if key in service_keys:
            value = service_keys[key]
            value_found = True

   if value_found:
      return value

   if not service_name_found:
      raise Exception(
         "Service name '{}' not found in docker file(s): {}".format(
            service_name, docker_compose_files))
   else:
      raise Exception("Key '{}' not found in docker file(s): {}".format(key,
         docker_compose_files))


def get_deployment_service_config_file(docker_compose_files, service_name):
   '''
   Helper method to get config file for a given "service_name"
   :param docker_compose_files: cmdline arg dockerComposeFile
   :param service_name: service name (provisioning)
   :return: config file
   '''
   config_folder = get_docker_compose_value(
      docker_compose_files, service_name, "volumes")
   try:
      config_folder = config_folder[0].split(':')[0]
      config_file = "{}/app/profiles/application-test.properties".format(
         config_folder)
   except Exception as e:
      raise
   return config_file


def set_props_file_value(filename, key, value):
   '''
   Method to replace value in a properties file or add a new one if not present.
   :param filename: properties file name
   :param key: key to look for
   :param value: new value for the key
   '''
   try:
      with open(filename, 'r') as f_in, \
         tempfile.NamedTemporaryFile('w', dir=os.path.dirname(filename),
                                     delete=False) as f_out:
         found = False

         for line in f_in.readlines():
            if line.startswith("{}=".format(key)):
               line = '='.join((line.split('=')[0], '{}\n'.format(value)))
               found = True
            f_out.write(line)

         if not found:
            f_out.write("\n{}={}\n".format(key, value))

      os.replace(f_out.name, filename)

      # Make read/write for other users so subsequent Hermes runs which
      # do not use sudo (e.g. UI tests) won't fail.
      os.chmod(filename, 0o664)
   except Exception as e:
      log.error("Unable to update properties file: {}".format(filename))
      raise


def read_key(key, properties_file=PROPERTIES_TEST_FILE):
   """
   Method to read value for a given key in a properties file
   :param key: Key that needs to be read
   :param properties_file: Properties file from which key needs to be read
   :return: Value corresponding to the key. None if key is not present
   """
   try:
      with open(properties_file, 'r') as fp:
         value = None
         for line in fp.read().splitlines():
            if line.startswith("{}=".format(key)):
               value = line.split('=')[1]
               break
         if not value:
            log.warn("Key {} not found in file {}".format(key, properties_file))
         return value
   except:
      log.error("Unable to access/read properties file: {}".format(properties_file))
      raise


def ssh_connect(host, username, password, command, log_mode=None, verbose=True):
   '''
   Helper method to execute a command on a host via SSH
   :param host: IP of the destination host
   :param username: username for SSH connection
   :param password: password for username
   :param command: command to be executed on the remote host
   :param log_mode: Override to log connectivity issue as a warning
   :return: Output of the command
   '''
   warnings.simplefilter("ignore", cryptography.utils.CryptographyDeprecationWarning)
   logging.getLogger("paramiko").setLevel(logging.WARNING)

   resp = None
   ssh = None
   try:
      ssh = paramiko.SSHClient()
      ssh.set_missing_host_key_policy(paramiko.AutoAddPolicy())
      ssh.connect(host, username=username, password=password, timeout=60)
      ssh_stdin, ssh_stdout, ssh_stderr = ssh.exec_command(command,
                                                           get_pty=True)
      outlines = ssh_stdout.readlines()
      resp = ''.join(outlines)
      log.debug(resp)
   except paramiko.AuthenticationException as e:
      log.error("Authentication failed when connecting to {}".format(host))
   except EOFError as e:
      if "Error reading SSH protocol banner" in str(e):
         log.error("SSH failure, most likely due to network congestion.")
      raise
   except Exception as e:
      if verbose:
         if log_mode == "WARNING":
            log.warning("Could not connect to {}".format(host))
         else:
            log.error("Could not connect to {}: {}".format(host, e))
   finally:
      if ssh:
         ssh.close()
   return resp


def ssh_parallel(ips, cmd, condition=None, max_try=3, verbose=True):
  '''
    Parallel SSH (running identical commands)
    Default pass condition lambda is `lambda output: output is not None`
    returns list of [{ ip: target_ip, output: ssh_output }]
  '''
  log.debug("{} being sent command: {}".format(ips, cmd))
  user, pw = getNodeCredentials()
  threads = []; results = []
  default_cond = lambda output: output is not None
  if not condition: condition = default_cond
  def sshExec(ip):
    try_count = 0
    log.debug("{} being sent command: {}".format(ip, cmd))
    while try_count < max_try:
      try_count += 1
      output = ssh_connect(ip, user, pw, cmd, verbose=verbose)
      if condition(output): break # condition met; got what we wanted
      log.debug("{} did not return results matching criteria retrying...".format(ip))
    results.append({"ip":ip, "output":output})
  for ip in ips:
    thr = threading.Thread(target = lambda ip: sshExec(ip), args = (ip, ))
    threads.append(thr); thr.start()
  for thd in threads: thd.join()
  if len(results) != len(ips):
    log.error("some ips did not return results after retries".format(ip))
    return None
  return results


def sftp_client(host, username, password, src, dest, action="download", log_mode=None):
   '''
   Helper method to execute a command on a host via SSH
   :param host: IP of remote host
   :param username: username for FTP connection
   :param password: password for username
   :param src: Source file to FTP
   :param dest: Destination file to FTP
   :param action: download/upload
   :param log_mode: Override to log connectivity issue as a warning
   :return: FTP Status (True/False)
   '''
   warnings.simplefilter("ignore", cryptography.utils.CryptographyDeprecationWarning)
   logging.getLogger("paramiko").setLevel(logging.WARNING)

   result = False
   sftp = None
   transport = None
   try:
      transport = paramiko.Transport((host, 22))
      transport.connect(None, username, password)

      sftp = paramiko.SFTPClient.from_transport(transport)

      if action.lower() == "download":
         sftp.get(src, dest)
         cmd_verify_ftp = ["ls", dest]
         if execute_ext_command(cmd_verify_ftp):
            log.debug("File downloaded from {} successfully: {}".format(host, dest))
            result = True
      else:
         sftp.put(src, dest)
         cmd_verify_ftp = "ls {}".format(dest)
         ssh_output = ssh_connect(host, username,password, cmd_verify_ftp)
         log.debug(ssh_output)
         if ssh_output:
            if ssh_output.rstrip() == dest:
               log.debug("File uploaded to {} successfully: {}".format(host, dest))
               result = True
   except paramiko.AuthenticationException as e:
      log.error("Authentication failed when connecting to {}".format(host))
   except Exception as e:
      if log_mode == "WARNING":
         log.warning("On host {}: {}".format(host, e))
      else:
         log.error("On host {}: {}".format(host, e))

   if sftp:
      sftp.close()
   if transport:
      transport.close()

   return result


def execute_ext_command(command, verbose=True, timeout=None):
   '''
   Helper method to execute an external command
   :param command: command to be executed
   :param verbose: Whether to display output on stdout.
   :return: True if command exit status is 0, else False
   '''
   log.debug("Executing external command: {}".format(command))

   try:
      completedProcess = subprocess.run(command, stdout=subprocess.PIPE,
                                     stderr=subprocess.STDOUT,
                                     universal_newlines=True,
                                     timeout=timeout)

      completedProcess.check_returncode()

      if verbose:
         log.debug("stdout: {}".format(
            completedProcess.stdout))
         if completedProcess.stderr:
            log.info("stderr: {}".format(completedProcess.stderr))
   except subprocess.TimeoutExpired as e:
      log.error("Command timed out after {} seconds with exception: {}".format(timeout, e))
      log.error(traceback.format_exc())
      return False, completedProcess.stdout
   except subprocess.CalledProcessError as e:
      if verbose:
         log.error("Subprocess stdout: '{}', Subprocess stderr: '{}'"
                   .format(completedProcess.stdout, completedProcess.stderr))
         log.error("Subprocess failed with exception: {}".format(e))
         log.error(traceback.format_exc())
      return False, completedProcess.stdout

   return True, completedProcess.stdout


def protobuf_message_to_json(message_obj):
   '''
   Helper method to convert a protobuf message to json
   :param message_obj: protobuf message
   :return: json
   '''
   from google.protobuf.json_format import MessageToJson

   if message_obj:
      if isinstance(message_obj, (list,)):
         list_of_json_objects = []
         for message in message_obj:
            json_object = json.loads(MessageToJson(message))
            list_of_json_objects.append(json_object)
         return list_of_json_objects
      else:
         json_object = json.loads(MessageToJson(message_obj))
         return json_object
   else:
      log.error("protobuf_message_to_json received nothing to convert.")
      return None


def json_to_protobuf_message(j):
   from vmware.blockchain.deployment.v1.provisioning_service_pb2 import \
      DeploymentSessionIdentifier
   from google.protobuf.json_format import Parse
   return Parse(j, DeploymentSessionIdentifier())


def requireFields(ob, fieldList):
   '''
   Verifies that the pased in ob contains the fields fieldList.
   Returns whether everything is present, and if not, the first
   missing field.
   '''
   for f in fieldList:
      if not f in ob:
         return (False, f)
   return (True, None)


def setHelenProperty(key, val):
   '''
   Sets a property in Helen's application-test.properties.
   Only applicable when running from docker.
   '''
   testProperties = {}

   with open("../docker/config-helen/app/profiles/application-test.properties") as f:
      for line in f:
         if line.strip():
            k, v = line.split("=", 1)
            testProperties[k] = v.strip()

   testProperties[key] = val

   with open("../docker/config-helen/app/profiles/application-test.properties", "w") as f:
      for prop in testProperties:
         f.write(prop + "=" + testProperties[prop] + "\n")


# TODO: refactor this method to make it generic
def add_ethrpc_port_forwarding(host, username, password, src_port=443, dest_port=8545, verbose=True):
   '''
   Enable port forwarding on concord node to facilitate hitting ethrpc endpoint
   on port 443, which redirects to 8545. This is a workaround to support hitting
   ethrpc end points from within vmware network, as non 443/80 ports are blocked.
   Bug/Story: VB-1170
   :param host: concord host IP
   :param username: concord node login - username
   :param password: concord node login - password
   :param src_port: The port to be hit from outside VMware network
   :param dest_port: The port that src_port maps to on the host
   :return: Port forward status (True/False)
   '''
   try:
      if verbose: log.info(
         "Adding port forwarding to enable ethrpc listen on {}:{}".format(host,
                                                                          src_port))
      cmd_get_docker_ethrpc_ip = "iptables -t nat -vnL | grep {} | grep DNAT | cut -d':' -f3".format(
         dest_port)
      docker_ethrpc_ip = ssh_connect(host, username, password,
                                     cmd_get_docker_ethrpc_ip)

      if verbose: log.debug("Extracted IP to port forward: {}".format(docker_ethrpc_ip))

      if docker_ethrpc_ip:
         docker_ethrpc_ip = docker_ethrpc_ip.strip()
         docker_ethrpc_ip = docker_ethrpc_ip.replace("\r", "")

         # Upon network reset there may be old forwarding rules. In this case,
         # command to get container ip will return multiple rows. When new containers
         # come up, there is no guaranty that it will use the same IP. Thus one needs
         # to remove the old forwarding rule and instill a new one.
         container_ips = docker_ethrpc_ip.split("\n")
         if len(container_ips) >= 2 and container_ips[0] != container_ips[-1]: # old/new rule mismatch
            for ip in container_ips:
                ip = ip.replace('"', '')
                previous_forward_ip = ip
                remove_commands = [
                  "iptables -t nat -D PREROUTING -p tcp --dport {} -j DNAT --to-destination {}:{}".format(
                    src_port, previous_forward_ip, dest_port),
                  "iptables -t nat -D PREROUTING -p tcp --dport {} -j DNAT --to-destination {}".format(
                    src_port, previous_forward_ip),
                ]
                remove_previous_forward = "; ".join(remove_commands)
                check_port_forward_output = ssh_connect(host, username, password, remove_previous_forward)
            docker_ethrpc_ip = container_ips[-1] # last shall be new target

         cmd_check_port_forward = "iptables -t nat -C PREROUTING -p tcp --dport {} -j DNAT --to-destination {}:{}".format(
            src_port, docker_ethrpc_ip, dest_port)
         if verbose: log.info("Check if port forwarding rule already exists...")
         check_port_forward_output = ssh_connect(host, username, password, cmd_check_port_forward)
         if verbose: log.debug("Is port already forwarded command output: {}".format(check_port_forward_output))

         if "iptables: No chain/target/match by that name" in check_port_forward_output:
            if verbose: log.info("Port forwarding rule does not exist. Adding...")
            cmd_port_forward = "iptables -t nat -A PREROUTING -p tcp --dport {} -j DNAT --to-destination {}:{}".format(
               src_port, docker_ethrpc_ip, dest_port)
            if verbose: log.debug("Port forwarding command: {}".format(cmd_port_forward))
            output = ssh_connect(host, username, password, cmd_port_forward)
            if verbose: log.debug("Port forwarded command output: {}".format(output))

            cmd_check_port_forward = "iptables -t nat -vnL | grep {}".format(src_port)
            if verbose: log.debug("Port forwarding check command: {}".format(cmd_check_port_forward))
            port_forward_output = ssh_connect(host, username, password,
                                              cmd_check_port_forward)
            if verbose: log.debug("Port forwarding check output: {}".format(port_forward_output))

            check_str_port_forward = "dpt:{} to:{}:{}".format(src_port,
                                                              docker_ethrpc_ip,
                                                              dest_port)
            if check_str_port_forward in port_forward_output:
               if verbose: log.debug("Port forwarded successfully")
               return True
         else:
            if verbose: log.info("Port forwading rule already exists")
            return True
   except Exception as e:
      log.debug(str(e))

   if verbose: log.debug("Port forwarding failed")
   return False


def verify_chessplus_test_ready(max_tries=10):
   '''
   Checks the connectivity of endpoints for chessplus
   '''
   # TODO: PLACE HOLDER for local deployment
   return True


def no_blockchain_readiness_verification_required(max_tries=1):
   '''
   Return default True, letting the testsuite to validate pre-deployed blockchain
   '''
   return True


def verify_daml_test_ready(endpoint_hosts, endpoint_port, max_tries=10):
   '''
   Checks the connectivity of endpoints
   :param endpoint_hosts: list of endpoints (hosts/ips)
   :param endpoint_port: endpoint port
   :param max_tries: max tries to check the connectivity
   :return: True if connectivity works, else False
   '''
   for ip in endpoint_hosts:
      if not verify_connectivity(ip, endpoint_port, max_tries=max_tries):
         return False

   return True


def verify_connectivity(ip, port, bytes_to_send=[], success_bytes=[], min_bytes=1, max_tries=15):
   '''
   Helper method to validate connectivity.
   :param ip: IP address
   :param port: Port
   :bytes_to_send: Bytearray to send to the remote system. Optional.
   :success_bytes: Do not return True until this array of bytes is returned. Expressed as
     a list of integers, like [1,2,3]. Optional.
   :min_bytes: Do not return True until this many bytes are returned.
     Should be <= the receive_buffer_size variable.  Optional.
   :return: Verification status (True/False).

   If you just pass in the IP address and port, it will work, but that isn't
   very meaningful.

   success_bytes and min_bytes are mutually exclusive.
   '''
   log.info("Verifying connectivity ({}:{})".format(ip, port))
   attempt = 0
   first_sleep_time = 5
   sleep_time = 10
   socket_timeout = 5
   receive_buffer_size = 256
   bytes_to_search = []
   success_bytes_str = str(success_bytes)[1:-1]

   if success_bytes and min_bytes:
      raise Exception("Only pass success_bytes or min_bytes to verify_connectivity()")

   time.sleep(first_sleep_time)
   while attempt < max_tries:
      with socket.socket() as s:
         s.settimeout(socket_timeout)
         attempt += 1
         log.info("Verifying connectivity (attempt: {}/{})...".format(
                           attempt, max_tries))
         try:
            s.connect((ip, port))

            if success_bytes or min_bytes:
               if bytes_to_send:
                  log.debug("Sending {}".format(bytes_to_send))
                  s.sendall(bytes_to_send)

               data = s.recv(receive_buffer_size)

               if success_bytes:
                  while data:
                     # Keep appending in case the sequence being searched for spans a recv() call.
                     bytes_to_search += data
                     log.debug("Searching for [{}] in {}".format(success_bytes_str, bytes_to_search))

                     if success_bytes_str in str(bytes_to_search):
                        return True
                     else:
                        data = s.recv(receive_buffer_size)
                  raise Exception("Expected bytes not found.")
               else:
                  if len(data) >= min_bytes:
                     return True
                  else:
                     raise Exception("Only received {} bytes, waiting for {}".format(len(data), min_bytes))
            else:
                # The caller just wants to check a socket.  That isn't very meaningful, but ok.
                return True
         except Exception as e:
            if attempt == max_tries:
               log.debug(e)
            else:
               log.info("Retry after {} seconds...".format(sleep_time))
               time.sleep(sleep_time)
         bytes_to_search = []

   return False


def check_replica_health(all_replicas_and_type, username, password):
   '''
   Helper util method to check the health of replicas supplied. This method checks the
   crash file created by the health daemon incase of a failure (like container not up)
   :param all_replicas_and_type: List of replicas and it's blockchain type to be monitored
   :param username: replica login username
   :param password: replica login password
   :return: False if a replica is crashed, else True
   '''
   node_crashed = False
   crashed_nodes = {}
   log.info("************************************************************")
   for blockchain_type, replica_ips in all_replicas_and_type.items():
      log.info("Verifying health on {} ({})".format(replica_ips, blockchain_type))
      crashed_ips = []
      for ip in replica_ips:
         log.info("{}...".format(ip))
         cmd = "stat {}".format(HEALTHD_CRASH_FILE)
         ssh_output = ssh_connect(ip, username, password, cmd)
         log.debug("cmd '{}' output: {}".format(cmd, ssh_output))
         if ssh_output:
            for line in ssh_output.split('\n'):
               if "File: {}".format(HEALTHD_CRASH_FILE) in line:
                  log.warning("**** CRASHED!")
                  crashed_ips.append(ip)
                  node_crashed = True
         else:
            log.warning("Unable to connect to host: {}".format(ip))
            crashed_ips.append(ip)
            node_crashed = True

      crashed_nodes[blockchain_type] = crashed_ips


   if node_crashed:
      return False, crashed_nodes
   else:
      log.info("**** All replicas are healthy")
      log.info("")
      return True, crashed_nodes


def collect_support_logs_for_long_running_tests(all_replicas_and_type,
                                                save_support_logs_to,
                                                verbose=True):
   '''
   Collect support logs for long running tests
   :param all_replicas_and_type: dict of replica type & replica ips
   :param save_support_logs_to: location to save logs
   :param verbose: log info () if set to true
   '''
   log.info("Saving support logs to: {} ...".format(save_support_logs_to))
   for blockchain_type, replica_ips in all_replicas_and_type.items():
      log.debug("Collect support bundle from all replica IPs: {}...".format(
         replica_ips))
      create_concord_support_bundle(replica_ips, blockchain_type,
                                    save_support_logs_to, verbose=verbose)


def monitor_replicas(replica_config, run_duration, load_interval, log_dir,
                     test_list_json_file, testset, notify_target=None, notify_job=None):
   '''
   Helper util method to monitor the health of the replicas, and do a blockchain
   test (send/get transactions) and collect support logs incase of a replica
   failed status (crash or failed  txn test)
   :param replica_config: replica config file
   :param run_duration: No. of hrs to monitor the replicas
   :param load_interval: Interval in moins between every monitoring call
   :param log_dir: logs archiving location
   :param test_list_json_file: test list json file
   :param testset: Set of test sets to be picked up for this run
   :param notify_target: string value of channel name or email; if none, skip.
   :param notify_job: string value of shortened job name running this monitoring
   :return:False if replicas reported a failure during the run_duration time, else True
   '''
   all_replicas_and_type = parseReplicasConfig(replica_config)

   from . import slack
   configObject = getUserConfig()
   credentials = configObject["persephoneTests"]["provisioningService"][
      "concordNode"]
   username = credentials["username"]
   password = credentials["password"]

   start_time = time.time()
   end_time = start_time + run_duration * 3600
   slack_last_reported = start_time
   dashboardLink = longRunningTestDashboardLink(replica_config)
   consoleURL = os.getenv("BUILD_URL") + "console" if os.getenv("BUILD_URL") else None

   remaining_time = str(int((end_time - time.time()) / 3600))
   slackThread = None
   # shared snippet of replicas.json
   slack.reportMonitoringIfTarget(
     target=notify_target, msgType="kickOff",
     replicasPath=replica_config, jobNameShort=notify_job)
   initialStats = get_replicas_stats(all_replicas_and_type)
   # first message that will be the main thread
   if notify_job:
     firstMessageText = "<RUN> has {} hour remaining.\n\nConsole: {}\n\nStatus:\n{}".format(
       remaining_time, consoleURL,
       "\n".join(initialStats["message_format"]))
   else:
     firstMessageText = "<RUN> has {} hour remaining. Status:\n{}".format(
       remaining_time, "\n".join(initialStats["message_format"]))
   firstMessage = slack.reportMonitoringIfTarget(
     target=notify_target,
     message=firstMessageText,
     jobNameShort=notify_job
   )
   slackThread = firstMessage['ts'] if firstMessage else None
   slack.reportMonitoringIfTarget( # output link to Wavefront dashboard
     target=notify_target,
     message = dashboardLink,
     ts = slackThread,
     jobNameShort=notify_job
   )

   tests = get_long_running_tests(all_replicas_and_type, test_list_json_file, testset)
   overall_result = []
   run_count = 0
   no_of_times_all_tests_failed = 0
   overall_run_status = None
   replica_status = None
   while ((time.time() - start_time)/3600 < run_duration) and replica_status is not False:
      status, crashed_nodes = \
         check_replica_health(all_replicas_and_type, username, password)
      if not status:
         f = int((len(all_replicas_and_type[TYPE_DAML_COMMITTER])-1)/3)
         no_of_committer_crash = 0
         no_of_other_node_crash = 0
         for node_type, crashed_ips in crashed_nodes.items():
            if node_type == TYPE_DAML_COMMITTER:
               no_of_committer_crash = len(crashed_ips)
            else:
               no_of_other_node_crash += len(crashed_ips)

         crash_logs_dir = os.path.join(log_dir, "replica_crash_{}".format(
            time.strftime("%Y%m%d_%H%M%S", time.localtime())))
         collect_support_logs_for_long_running_tests(all_replicas_and_type,
                                                     crash_logs_dir,
                                                     verbose=False)
         overall_run_status = False
         if len(crashed_nodes[TYPE_DAML_COMMITTER]) > f or no_of_other_node_crash > 0:
            log.error("**** replica status is unhealthy")
            log.error("**** Crashed nodes: {}".format(
               json.dumps(crashed_nodes, indent=True)))
            replica_status = False
         else:
            log.warning(
               "**** As no. of crashed replica(s) {} ({}) is less than/equal to f ({}), continue the run...".format(
                  crashed_nodes[TYPE_DAML_COMMITTER], no_of_committer_crash, f))
            replica_status = True

      if status or replica_status:
         replica_status = True
         for blockchain_type, replica_ips in all_replicas_and_type.items():
            if blockchain_type == TYPE_DAML_PARTICIPANT or blockchain_type == TYPE_ETHEREUM:
               testset_result_dict = run_long_running_tests(tests, replica_config, log_dir)

               result, all_tests_failed = parse_long_running_test_result(testset_result_dict)
               result_details = {}
               run_count += 1
               result_details[run_count] = testset_result_dict
               overall_result.append(result_details)

               if result:
                  log.info("**** All tests passed in this iteration")
                  if overall_run_status is None:
                     overall_run_status = True
               else:
                  log.warning(
                     "**** Testsuites have failed, but continuing to monitor...")
                  overall_run_status = False

                  if all_tests_failed:
                     no_of_times_all_tests_failed += 1
                  else:
                     no_of_times_all_tests_failed = 0

               if no_of_times_all_tests_failed == 3:
                  log.error("All testsuites have failed consecutively for the 3rd time; aborting the entire run")
                  replica_status = False

               log.info("**** Testrun status for this iteration: {}".format(result))

      # report to Slack in predefined interval
      if time.time() - slack_last_reported > HEALTHD_SLACK_NOTIFICATION_INTERVAL:
        stats = get_replicas_stats(all_replicas_and_type, concise=True)
        remaining_time = str(int((end_time - time.time()) / 3600))
        duration = str(int((time.time() - start_time) / 3600))
        midRunMessage =  "<RUN> has {} hour remaining ({}h passed). Status:\n{}".format(
                            remaining_time, duration, "\n".join(stats["message_format"]))
        slack.reportMonitoringIfTarget(
          target=notify_target,
          message=midRunMessage,
          ts=slackThread,
          jobNameShort=notify_job
        ) # Add to reply thread instead of channel
        slack_last_reported = time.time()

      if not replica_status:
         duration = str(int((time.time() - start_time) / 3600))
         remaining_time = str(int((end_time - time.time()) / 3600))
         endingMessage = "<RUN> has failed after {} hours ({} had remained)\n\nConsole: {}\n\nWavefront: {}".format(
             duration, remaining_time, consoleURL, dashboardLink
         )
         slack.reportMonitoringIfTarget(
           target=notify_target,
           message=endingMessage,
           jobNameShort=notify_job)
         return False

      log.info("")
      log.info("sleep for {} min(s) and continue monitoring...".format(load_interval))
      time.sleep(load_interval*60)

   log.info("Overall run summary: ")
   for res in overall_result:
      log.info(json.dumps(res))

   if not overall_run_status:
      duration = str(int((time.time() - start_time) / 3600))
      remaining_time = str(int((end_time - time.time()) / 3600))
      slack.reportMonitoringIfTarget(
        target=notify_target,
        message="<RUN> has failed after {} hours ({} had remained)".format(duration, remaining_time),
        jobNameShort=notify_job
      )
      return False

   successEndingMessage = "<RUN> successfully passed. (total {} hours)\n\nConsole: {}\n\nWavefront: {}".format(
     run_duration, consoleURL, dashboardLink
   )
   slack.reportMonitoringIfTarget(target=notify_target, message=successEndingMessage, jobNameShort=notify_job)
   return True


def parse_long_running_test_result(testset_result_dict):
   '''
   Parse log running test results
   :param testset_result_dict: run results from  various tests
   :return: test result, and True if all tests failed, else False
   '''
   all_tests_failed = True
   test_result = True
   for res in testset_result_dict["results"]:
      if False in res.values():
         test_result = False
      if True in res.values():
         all_tests_failed = False

   if not testset_result_dict:
      test_result = False
   return test_result, all_tests_failed


def get_long_running_tests(all_replicas_and_type, test_list_json_file, testset):
   '''
   Get list of tests from testlist json
   :param all_replicas_and_type: dict of replica type & replica ips
   :param test_list_json_file: test list json file
   :param testset: Set of test sets to be picked up for this run
   :return: set of tests to be run
   '''
   tests = []
   general_blockchain_type = None
   for blockchain_type, replica_ips in all_replicas_and_type.items():
      if blockchain_type == TYPE_DAML_PARTICIPANT:
         general_blockchain_type = TYPE_DAML
      elif blockchain_type == TYPE_ETHEREUM:
         general_blockchain_type = TYPE_ETHEREUM

   with open(test_list_json_file, "r", encoding="utf-8") as fp:
      data = json.load(fp)

      for test_set in testset.split(','):
         try:
            tests += data[general_blockchain_type][test_set]
         except KeyError as e:
            log.error("Error finding testset '{}' in {}".format(test_set, test_list_json_file))
            raise

   log.info("Tests for this run: {}".format(json.dumps(tests, indent=True)))
   return tests


def run_long_running_tests(tests, replica_config, log_dir):
   '''
   Make call to execute tests
   :param tests: list of tests
   :param replica_config: replicas config json
   :param log_dir: directory to save logs
   :return: test set result
   '''
   log.info("")
   log.info("**** Running test suites...")
   test_result = False
   testset_result_dict = {}
   testset_result = []

   python_bin = os.environ.get("python_bin")
   if python_bin:
      python = "{}/python".format(python_bin)
   else:
      python = "python3"

   for test_count, test_set in enumerate(tests):
      log.debug("Test set: {}".format(test_set))
      test_results = {}
      try:
         results_dir = os.path.join(log_dir, "{}_{}".format(test_count + 1,
                                                            test_set["testname"].replace(' ', '_')))
         if not os.path.exists(results_dir):
            os.makedirs(results_dir)

         testsuite_cmd = python + " " + test_set["test"] + " --replicasConfig " + replica_config + " --logLevel DEBUG"
         test_cmd = testsuite_cmd.split(' ') + ["--resultsDir", results_dir]

         log.info("{}. {}...".format(test_count+1, test_set["testname"]))
         status, msg = execute_ext_command(test_cmd, verbose=False)
         log.debug("Run output: {}".format(msg))
         test_result = status
      except Exception as e:
         log.error("Error running testsuite: {}".format(e))
         test_result = False

      log.info("   ** {}".format("PASS" if test_result else "FAIL"))
      test_results[test_count+1] = test_result
      testset_result.append(test_results)

   testset_result_dict["results"] = testset_result
   result_data = json.dumps(testset_result_dict, indent=True, sort_keys=True)
   log.debug(result_data)

   return testset_result_dict


def get_replicas_stats(all_replicas_and_type, concise=False):
  '''
    Given replicas with health daemon installed, get the latest
    stats report from each replica with sftp_client function
    returns json and Slack message_format of the stats
  '''
  credentials = getUserConfig()["persephoneTests"]["provisioningService"]["concordNode"]
  username = credentials["username"]
  password = credentials["password"]
  temp_json_path = "/tmp/healthd_recent.json"
  all_reports = { "json": {}, "message_format": [] }
  all_committers_mem = []

  for blockchain_type, replica_ips in all_replicas_and_type.items():
    typeName = "Committer" if not concise else "c"
    if blockchain_type == TYPE_DAML_PARTICIPANT:
      typeName = "Participant" if not concise else "p"

    log.info("Retrieving stats for replicas '{}', file '{}', to local file '{}'".format(replica_ips, HEALTHD_RECENT_REPORT_PATH, temp_json_path))
    for i, replica_ip in enumerate(replica_ips):
      try:
        if sftp_client(replica_ip, username, password, HEALTHD_RECENT_REPORT_PATH,
                      temp_json_path, action="download"):
          with open(temp_json_path, "r", encoding="utf-8") as f:
            stat = json.load(f)
            stat["type"] = blockchain_type
            all_reports["json"][replica_ip] = stat

            if blockchain_type == TYPE_DAML_COMMITTER:
              all_committers_mem.append(stat["mem"])
            status_emoji = ":red_circle:" if stat["status"] == "bad" else ":green_circle:"
            if not concise:
              all_reports["message_format"].append("{} [{}-{}] ({}) cpu: {}%, mem: {}%, disk: {}%".format(
                status_emoji, typeName, i+1, replica_ip, stat["cpu"]["avg"], stat["mem"], stat["disk"]
              ))
            else:
              all_reports["message_format"].append("{} {}{} | {}% | {}% | {}%".format(
                status_emoji, typeName, i+1, stat["cpu"]["avg"], stat["mem"], stat["disk"]
              ))
        else:
           raise Exception("Failed retrieving stats for replica '{}', file '{}', to "
                           "local file '{}'".format(replica_ip, HEALTHD_RECENT_REPORT_PATH,
                                                    temp_json_path))
      except Exception as e:
        log.error(str(e))
        hermesNonCriticalTrace(e)


  standardDeviation = statistics.stdev(all_committers_mem)
  if standardDeviation > 5.0: # STDEV usually is < 5, high deviation is indication of malfunction
    all_reports["message_format"].append(
      ":warning: Committers memory standard deviation is high: {}".format(
        format(standardDeviation, ".4g")
      )
    )
  return all_reports

def create_concord_support_bundle(replicas, concord_type, test_log_dir,
                                 verbose=True):
   '''
   Helper method to create concord support bundle and upload to result dir
   :param replicas: List of IP addresses of concord nodes
   :param concord_type: Concord node type ("daml", "ethereum", etc)
   :param test_log_dir: Support bundle to be uploaded to
   '''
   support_bundle_binary_name = "deployment_support.py"
   src_support_bundle_binary_path = os.path.join('util',
                                                 support_bundle_binary_name)
   remote_support_bundle_binary_path = os.path.join("/var/tmp", # linux tmp folder
                                                  support_bundle_binary_name)
   user_config = json_helper_util.readJsonFile(CONFIG_USER_FILE)
   concord_memeber_credentials = \
      user_config["persephoneTests"]["provisioningService"]["concordNode"]
   concord_username = concord_memeber_credentials["username"]
   concord_password = concord_memeber_credentials["password"]

   expected_docker_containers = list(
      user_config["persephoneTests"]["modelService"]["defaults"][
         "deployment_components"][concord_type.lower()].values())

   if verbose: log.info("")
   if verbose: log.info("**** Collecting Support bundle ****")
   try:
      def collect_on_ip(concord_ip):
         if verbose: log.info("Concord IP: {}".format(concord_ip))
         if verbose: log.info(
            "  Upload support-bundle generation script onto concord node '{}'...".format(
               concord_ip))

         if sftp_client(concord_ip, concord_username, concord_password,
                        src_support_bundle_binary_path,
                        remote_support_bundle_binary_path, action="upload"):
            if verbose: log.debug("  Saved at '{}:{}'".format(concord_ip,
                                                 remote_support_bundle_binary_path))

            cmd_execute_collect_support_bundle = "python3 {} --supportBundleBaseDir {} --concordIP {} " \
                                                 "--dockerContainers {}".format(
               remote_support_bundle_binary_path, DEPLOYMENT_SUPPORT_BUNDLE_BASE_DIR, concord_ip,
               ' '.join(expected_docker_containers))

            if verbose: log.info("  Gathering deployment support logs...")
            ssh_output = ssh_connect(concord_ip, concord_username,
                                     concord_password,
                                     cmd_execute_collect_support_bundle)
            if verbose: log.debug("Output from script '{}': {}".format(
                                             remote_support_bundle_binary_path,
                                             ssh_output))
            supput_bundle_created = False
            if ssh_output:
               for line in ssh_output.split('\n'):
                  if "Support bundle created successfully:" in line:
                     support_bundle_to_upload = line.split(':')[-1].strip()
                     if verbose: log.info(
                        "  Support bundle created successfully on concord node {}:{}".format(
                           concord_ip, support_bundle_to_upload))
                     supput_bundle_created = True

                     if verbose: log.info("  Exporting support bundle...")
                     if not os.path.exists(test_log_dir):
                        os.makedirs(test_log_dir)
                     dest_support_bundle = os.path.join(test_log_dir,
                                                        os.path.split(
                                                           support_bundle_to_upload)[
                                                           1])
                     if sftp_client(concord_ip, concord_username,
                                    concord_password,
                                    support_bundle_to_upload,
                                    dest_support_bundle,
                                    action="download"):
                        if verbose: log.info("  {}".format(dest_support_bundle))
                     else:
                        log.error(
                           "Failed to copy support bundle from concord '{}'".format(
                              concord_ip))
                     break

            if not supput_bundle_created:
               log.error(
                  "Failed to create support bundle for concord {}".format(
                     concord_ip))
         else:
            log.error(
               "Failed to copy support bundle generation script to concord '{}'".format(
                  concord_ip))

         if verbose: log.info("  Deleting support bundle base directory {}:{}...".format(
            concord_ip, DEPLOYMENT_SUPPORT_BUNDLE_BASE_DIR))
         cmd_remove_support_bundle_base_dir = "rm -rf {}".format(
            DEPLOYMENT_SUPPORT_BUNDLE_BASE_DIR)
         ssh_output = ssh_connect(concord_ip, concord_username,
                                  concord_password,
                                  cmd_remove_support_bundle_base_dir)
         if verbose: log.debug("Output: {}".format(ssh_output))
         if verbose: log.info("")

      threads = []
      for concord_ip in replicas:
          thr = threading.Thread(target = lambda concord_ip: collect_on_ip(concord_ip),
                                  args = (concord_ip, ))
          threads.append(thr); thr.start()
      for thd in threads: thd.join()
   except Exception as e:
      log.error(e)


def waitForTask(request, taskId, expectSuccess=True, timeout=600):
   '''
   request: A Rest request object which uses Helen.
   taskId: ID of the task to wait for, returned by Helen.
   expectSuccess: Whether we expect the task to be successful.
   timeout: Seconds to wait before timing out.

   Returns a tuple of:
      1. Boolean for whether the task completed with the expected success status,
         or None if timed out.
      2. The structure returned by the final call to the api.
   '''
   sleepTime = 10
   elapsedTime = 0
   success = False
   finished = False
   response = None
   expectedFinishState = "SUCCEEDED" if expectSuccess else "FAILED"

   while not finished:
      response = request.getTaskStatus(taskId)

      log.info("Response for task {}: {}".format(taskId, response))

      if response["state"] == "RUNNING":
         if elapsedTime >= timeout:
            log.info("Task '{}' did not finish in {} seconds".format(taskId, timeout))
            finished = True
         else:
            log.info("State is still {}.  Waiting for task to finish. " \
                     "Elapsed time: {}".format(response["state"], elapsedTime))
            time.sleep(sleepTime)
            elapsedTime += sleepTime
      else:
         finished = True
         success = response["state"] == expectedFinishState
         log.info("Task has finished.")

         if not success:
            log.info("Task did not finish as expected.  Details: {}".format(response))

   return (success, response)


def loadConfigFile(args=None, filepath=None):
   '''
   Given the cmdline args, loads the main Hermes config file and returns
   the config object.
   '''
   configObject = None
   jenkinsWorkspace = os.getenv("WORKSPACE")

   # explicitly supplied path
   configFilePath = args.config if args and args.config else filepath

   if not configFilePath:
      # default user_config.json (relative path)
      if os.path.exists(CONFIG_USER_FILE):
         configFilePath = CONFIG_USER_FILE
      # otherwise try user_config.json (absolute path) based on workspace
      elif jenkinsWorkspace:
         defaultConfigAbsolutePath = jenkinsWorkspace + '/blockchain/hermes/' + CONFIG_USER_FILE
         if os.path.exists(defaultConfigAbsolutePath):
            configFilePath = defaultConfigAbsolutePath

   if configFilePath:
      configObject = json_helper_util.readJsonFile(configFilePath)
   else:
      log.error("Cannot find user config source in any of the locations.")
      return None

   # resilience valgrind user_config does not have some items present in default user_config.
   if "user_config_resilience.json" in configFilePath or "user_config_valgrind.json" in configFilePath:
      defaultConfigFilePath = configFilePath.replace("user_config_resilience.json", "user_config.json")
      defaultConfigFilePath = configFilePath.replace("user_config_valgrind.json", "user_config.json")
      defaultUserConfig = json_helper_util.readJsonFile(defaultConfigFilePath)
      for key in defaultUserConfig: # import metainf, jenkins, communication, dashboard, etc.
        if key not in configObject:
          configObject[key] = defaultUserConfig[key]

   # --su flag is set (local dev env, non-Jenkins run);
   # Replace all bracket "<SOME_NAMED_CREDENTIAL>" to credentials from last good master
   if WITH_JENKINS_INJECTED_CREDENTIALS and not jenkinsWorkspace:
      try:
        log.info("Privilege invoked from --su flag; will run Hermes with Jenkins injected credentials...")
        from . import jenkins
        configs = jenkins.getConfigFromLatestGoodMaster()
        if configs:
          CONFIG_CACHED["zoneConfigOverrideFromSU"] = configs["zoneConfig"]
          configObject = jenkins.overrideOnlyDefaultConfig(configObject, configs["userConfig"])
          configObject["metainf"]["env"]["name"] = "LOCAL"
          configObject["metainf"]["env"]["jobName"] = "None"
          configObject["metainf"]["env"]["buildNumber"] = "None"
          configObject["metainf"]["env"]["dockerTag"] = ""
          configObject["metainf"]["env"]["workspace"] = ""
          configObject["jenkins"]["username"] = jenkins.JENKINS_USER_OVERRIDE
          configObject["jenkins"]["token"] = jenkins.JENKINS_TOKEN_OVERRIDE
          log.info("Successfully pulled Jenkins injected credentials from latest good master.")
        else: sys.exit(1)
      except Exception as e:
        traceback.print_exc()

   if "ethereum" in configObject and "testRoot" in configObject["ethereum"]:
      configObject["ethereum"]["testRoot"] = os.path.expanduser(configObject["ethereum"]["testRoot"])

   # Jenkins JOB_NAME & WORKSPACE containing slashes were replaced with ___; bring slashes back.
   #    More info: see MR !1324
   if "metainf" in configObject:
      envObject = configObject["metainf"]["env"]
      if "jobName" in envObject: envObject["jobName"] = envObject["jobName"].replace("___", "/")
      if "workspace" in envObject: envObject["workspace"] = envObject["workspace"].replace("___", "/")

   CONFIG_CACHED['data'] = configObject

   return configObject


def loadZoneConfig(args=None, filepath=None):
   """
   Given cmdline args or filepath, load the zones from zones config file
   :param args: cmdline arguments
   :param filepath: file to load zones from
   :return: zone config object
   """
   zone_config_object = None

   if args and args.zoneConfig:
      zone_config_object = json_helper_util.readJsonFile(args.zoneConfig)
   elif filepath:
      zone_config_object = json_helper_util.readJsonFile(filepath)
   elif os.path.exists(CONFIG_ZONE_FILE):
      zone_config_object = json_helper_util.readJsonFile(CONFIG_ZONE_FILE)
   else:
      log.warning("Cannot find zone config source in either cmdline args, filepath, or default file {}"
                  .format(CONFIG_ZONE_FILE))
      return None

   if "zoneConfigOverrideFromSU" in CONFIG_CACHED:
     from . import jenkins
     zone_config_object = jenkins.overrideOnlyDefaultConfig(
       zone_config_object, CONFIG_CACHED["zoneConfigOverrideFromSU"])

   CONFIG_CACHED['zones'] = zone_config_object
   return zone_config_object


def getUserConfig():
  configObject = CONFIG_CACHED['data'] if 'data' in CONFIG_CACHED else loadConfigFile()
  return configObject


def getZoneConfig(args=None, filepath=None):
   """
   Get zone config object from cache if available, else loads from default file
   :return: Zone config object
   """
   zone_config_object = CONFIG_CACHED["zones"] if "zones" in CONFIG_CACHED else loadZoneConfig(args, filepath)
   return zone_config_object


def getNodeCredentials():
  '''
    Returns tuple (username, password) used for ssh_connect to concord node
    e.g. username, password = getNodeCredentials()
  '''
  configObject = getUserConfig()
  credentials = configObject["persephoneTests"]["provisioningService"]["concordNode"]
  return (credentials["username"], credentials["password"])


def checkRpcTestHelperImport():
   try:
      sys.path.append("lib/persephone")
      from persephone import rpc_test_helper
   except (ImportError, ModuleNotFoundError) as e:
      log.error("Python bindings not generated. Execute the following from the top " \
                "level of the blockchain source directory: \n" \
                "python3 ./hermes/util/generate_grpc_bindings.py " \
                "--source-path=persephone/api/src/protobuf " \
                "--target-path=hermes/lib/persephone")

      raise Exception("gRPC Python bindings not generated")


def distributeItemsRoundRobin(numSlots, availableItems):
   '''
   Given some number of slots (e.g. 4 or 7 desired blockchain nodes) and a list of availableItems
   (e.g. a list of SDDC sites), create a list of numSlots availableItems, evenly distributing them
   in round robin fashion.
   Returns the list.
   '''
   returnList = []
   itemIndex = 0

   if availableItems:
      while len(returnList) < numSlots:
         returnList.append(availableItems[itemIndex])

         if itemIndex == len(availableItems) - 1:
            itemIndex = 0
         else:
            itemIndex += 1
   else:
      log.error("No availableItems passed to distributeItemsRoundRobin.")

   return returnList


def helenIsRemote(args):
   return "localhost" not in args.reverseProxyApiBaseUrl


def blockchainIsRemote(args):
   return args.blockchainLocation != LOCATION_LOCAL


def needToCollectDeploymentEvents(cmdlineArgs):
   return blockchainIsRemote(cmdlineArgs) and \
      "persephone".lower() not in cmdlineArgs.suite and \
      cmdlineArgs.keepBlockchains in [KEEP_BLOCKCHAINS_NEVER, KEEP_BLOCKCHAINS_ON_FAILURE]


def replaceUrlParts(url, newPort=None, newScheme=None):
   '''
   Replace url parts.  Expand as needed.
   '''
   urlObj = urlparse(url)
   scheme = newScheme if newScheme else urlObj.scheme
   port = newPort if newPort else urlObj.port
   newParts = (scheme,
               urlObj.hostname + ":" + str(port),
               urlObj.path,
               urlObj.params,
               urlObj.query,
               urlObj.fragment)
   return urlunparse(newParts)


def mergeDictionaries(orig, new):
   '''Python's update() simply replaces keys at the top level.'''
   for newK, newV in new.items():
      if newK in orig:
         if isinstance(newV, collections.Mapping):
            mergeDictionaries(orig[newK], newV)
         elif isinstance(newV, list):
            orig[newK] = orig[newK] + newV
         else:
            orig[newK] = newV
      else:
         orig[newK] = newV



def getReplicaContainers(replicaType):
   '''
   Given the name of a replica, as defined in persephoneTests["modelService"]["defaults"]["deployment_components"],
   return a list of the names of the docker containers which are expected to be in that replica.
   '''
   user_config = json_helper_util.readJsonFile(CONFIG_USER_FILE)

   if replicaType in user_config["persephoneTests"]["modelService"]["defaults"]["deployment_components"]:
      return list(user_config["persephoneTests"]["modelService"]["defaults"]["deployment_components"][replicaType].values())
   else:
      log.error("Invalid replica name in getReplicaContainers(): '{}'".format(replicaType))
      return None


def waitForDockerContainers(host, username, password, replicaType, timeout=600):
   '''
   Wait for a list of docker containers to come up, given a replica type as defined in
   persephoneTests["modelService"]["defaults"]["deployment_components"].
   '''
   for name in getReplicaContainers(replicaType):
      cmd = "docker ps | grep '{}' | grep ' Up [0-9]* '".format(name)
      elapsed = 0
      sleep_time = 5
      up = False

      while elapsed <= timeout and not up:
         ssh_output = ssh_connect(host, username, password, cmd)

         if ssh_output:
            log.info("Container '{}' on '{}' is up.".format(name, host))
            up = True
         else:
            log.info("Waiting for container '{}' on '{}' to come up.".format(name, host))
            time.sleep(sleep_time)
            elapsed += sleep_time

      if not up:
         raise Exception("Container '{}' on '{}' failed to come up.".format(name, host))

def check_docker_health(node, username, password, replica_type,
                        max_timeout=600, verbose=True):
   '''
   Verify docker health status
   :param node: Node to be verified
   :param username: node login username
   :param password: node login password
   :param replica_type: blockchain node type
   :param max_timeout: max timeout for verification
   :param verbose: flag to turn verbose on/off
   :return: True if node is healthy, else false
   '''
   count = 0
   start_time = time.time()
   docker_images_found = False
   command_to_run = "docker ps --format '{{.Names}}'"
   expected_docker_containers = getReplicaContainers(replica_type)
   if verbose: log.info(
      "Waiting for all docker containers to be up on '{}' within {} mins".format(
         node, max_timeout / 60))
   while not docker_images_found:
      count += 1
      log.debug(
         "Verifying docker containers (attempt: {})...".format(
            count))

      # Don't fail just because of network congestion, etc...
      attempt = 3
      while True:
         try:
            ssh_output = ssh_connect(node, username, password, command_to_run)
            break
         except Exception as e:
            attempt -= 1

            if attempt == 0:
               raise
            else:
               log.info("SSH error retrieving docker info from {}. Retrying in a few seconds".format(node))
               time.sleep(5)

      log.debug("SSH output: {}".format(ssh_output))
      for container_name in expected_docker_containers:
         if container_name not in ssh_output:
            docker_images_found = False
            if (time.time() - start_time) > max_timeout:
               if verbose: log.info("SSH output:\n{}".format(ssh_output))
               log.error(
                  "Container '{}' not up and running on node '{}'".format(
                     container_name, node))
               return False
            else:
               if verbose: log.warning(
                  "Container '{}' not up and running on node '{}'".format(
                     container_name, node))
               time.sleep(10)
               break
         else:
            docker_images_found = True
            log.debug(
               "Container {} found in node '{}'".format(
                  container_name, node))
   if verbose: log.info("Docker containers verified on {}".format(node))
   return True


def getJenkinsBuildId(jobName=None, buildNumber=None):
  try:
    configObject = getUserConfig()
    jobName = jobName if jobName else configObject["metainf"]["env"]["jobName"]
    buildNumber = str(buildNumber) if buildNumber else configObject["metainf"]["env"]["buildNumber"]
    if jobName.startswith("<"): jobName = "None"
    if buildNumber.startswith("<"): buildNumber = "None"
    return jobName + '/' + buildNumber
  except Exception as e:
    log.info(e); traceback.print_exc()
    return "None/None"


def getJenkinsBuildTraceId(jobName=None, buildNumber=None):
  '''
    Get trace_id (UUID) from HASH(jenkinsSource + jobName + buildNumber)
    User for metrics reporting for CI/CD Dashboard. Hash-based
    uuid generation guarantees, regardless of where in groovy
    this function was invoked from, the same traceId within the run.
  '''
  ingested = JENKINS_NAMESPACE_MAIN + '/' + getJenkinsBuildId(jobName, buildNumber)
  hash32Bytes = hashlib.sha256(ingested.encode()).hexdigest()
  hash16Bytes = hash32Bytes[:32]
  traceId = str(uuid.UUID(hash16Bytes))
  return traceId


def getJenkinsBuildSpanId(traceId, setName, caseName):
  '''
    Get spanId (UUID) from HASH(traceId + stageId + eventId). This uses same parameters
    as the function in `event_recorder.py`, `record_event(stage_name, event_name)`
    and will be used to publish events/metrics to CI/CD dashboard
    e.g.
      saveTimeEvent("Gather artifacts", "Start")
      saveTimeEvent("Gather artifacts", "End")
      `setName` is stage or test suite name; "Gather artifacts"
      `caseName` is usually the event name or test case name "Start" | "End" | "TestCase1"...
  '''
  ingested = traceId + '/' + setName + '/' + caseName
  hash32Bytes = hashlib.sha256(ingested.encode()).hexdigest()
  hash16Bytes = hash32Bytes[:32]
  spandId = str(uuid.UUID(hash16Bytes))
  return spandId


def getJenkinsJobNameAndBuildNumber():
  buildId = getJenkinsBuildId()
  split = buildId.split("/")
  return { "jobName": '/'.join(split[:-1]), "buildNumber": split[-1]}


def getJenkinsRunTypeInfo(jobName=None):
  '''
    If this Hermes process is triggered by Jenkins,
    Get the type of Jenkins run given the job name
  '''
  try:
    if jobName is None:
      jobName = getUserConfig()["metainf"]["env"]["jobName"]
    for runTypeInfo in JENKINS_MAJOR_RUN_TYPES:
      runTypeInfo = json.loads(json.dumps(runTypeInfo)) # make copy for manipulation
      # Extract variables if specified
      # e.g. get "version": "0.5" from "0.5 Branch Blockchain Run on GitLab/releases/0.5"
      if "variables" in runTypeInfo:
        for extractInfo in runTypeInfo["variables"]:
          varName = extractInfo["name"]
          afterString = jobName
          if "after" in extractInfo and jobName.find(extractInfo["after"]) >= 0:
            afterString = jobName.split(extractInfo["after"])[1] # after pattern x
          if "before" not in extractInfo: extractInfo["before"] = '/'
          varValue = afterString.split(extractInfo["before"])[0] # but before pattern y
          runTypeInfo[varName] = varValue # expose the extracted value as varName
      # Exact Match
      if "exactly" in runTypeInfo and runTypeInfo["exactly"] == jobName:
        return runTypeInfo
      # Contains all specified in `contains` list?
      if "contains" in runTypeInfo:
        containsAsSpecified = True
        for shouldContain in runTypeInfo["contains"]:
          if shouldContain not in jobName: containsAsSpecified = False; break
        if containsAsSpecified: return runTypeInfo # contains all specified, return runTypeInfo
    return None # Nothing matches, must be some other non-major run type
  except Exception as e:
    log.info(e); traceback.print_exc()
    return None


def jenkinsRunTypeIs(runTypeInfoA):
  '''
  If Jenkins triggered this Hermes process, check it against a known major run type.
  Usage: helper.jenkinsRunTypeIs(helper.JENKINS_RUN_MAIN_MR) => True/False
  '''
  runTypeInfoB = getJenkinsRunTypeInfo()
  if not runTypeInfoA or not runTypeInfoB: return False
  return runTypeInfoA["type"] == runTypeInfoB["type"]


def getJenkinsWorkspace():
  try:
    if os.getenv("WORKSPACE"): return os.getenv("WORKSPACE")
    else:
      workspaceFromUserConfig = getUserConfig()["metainf"]["env"]["workspace"]
      if not workspaceFromUserConfig.startswith("<"):
        return workspaceFromUserConfig
    return ""
  except Exception as e:
    return ""


def thisHermesIsFromJenkins():
  return True if getUserConfig()["metainf"]["env"]["name"] == "JENKINS" else False


def hermesNonCriticalTrace(e, message=None):
  NON_CRITICAL_HERMES_EXCEPTIONS.append({
    "error": e, "message": message, "argv":sys.argv,
    "suite": CURRENT_SUITE_NAME,
  })


def hermesNonCriticalTraceFinalize():
  try:
    ws = getJenkinsWorkspace()
    if getJenkinsWorkspace():
      nonCriticalLogPath = getJenkinsWorkspace() + NON_CRITICAL_HERMES_OUTPUT_PATH
    else:
      nonCriticalLogPath = NON_CRITICAL_HERMES_OUTPUT_FILE
    allLines = []
    with open(nonCriticalLogPath, 'a+') as f:
      for item in NON_CRITICAL_HERMES_EXCEPTIONS:
        e = item["error"]
        divider = "=" * 128
        exceptionString = divider + "\n"
        if item["suite"]: exceptionString += "Test Suite: " + item["suite"] + "\n\n"
        if item["message"]: exceptionString += item["message"] + "\n"
        exceptionString += "\n".join(traceback.format_exception(Exception, e, e.__traceback__))
        exceptionString += "\nCommandline Arguments: " + json.dumps(item["argv"], indent=4, default=str) + "\n"
        exceptionString += divider + "\n\n\n\n"
        allLines.append(exceptionString)
      f.write("\n".join(allLines))
  except Exception as e:
    pass


def hermesGetFreeAsyncThreadIndex():
  global FREE_ASYNC_THREADS_INDEX
  asyncIndex = 'THREAD-' + str(FREE_ASYNC_THREADS_INDEX)
  FREE_ASYNC_THREADS_INDEX += 1
  return asyncIndex


def hermesJoinAllFreeAsyncThreads():
  '''Joins all non-essential async worker threads to join before process exits'''
  for threadKey in FREE_ASYNC_THREADS:
    try:
      thd = FREE_ASYNC_THREADS[threadKey]
      thd.join(timeout=30)
    except Exception as e:
      hermesNonCriticalTrace(e)


def hermesPreexitWrapUp():
  hermesJoinAllFreeAsyncThreads()
  hermesNonCriticalTraceFinalize()
  from . import infra
  if infra.DEPLOYED_REPLICAS:
    deployed_list_file = getJenkinsWorkspace() + '/summary/deployed_blockchains.json'
    deployed_list_file_exists = os.path.exists(deployed_list_file)
    deployed_blockchains_list = []
    with open(deployed_list_file , 'w+') as file:
      if deployed_list_file_exists:
        list_content = file.read()
        if list_content and list_content.startswith("["):
          deployed_blockchains_list = json.loads(list_content)
      else: deployed_blockchains_list = []
      for newly_deployed in infra.DEPLOYED_REPLICAS:
        deployed_blockchains_list.append(newly_deployed)
      json.dump(deployed_blockchains_list, file)


def parseReplicasConfig(replicas):
  '''
    replicas argument can be path to replicas.json or
    fxBlockchain dict object directly. This function
    is used to standardize replicas parsing across codebase
  '''
  if not replicas: replicas = REPLICAS_JSON_PATH
  if isinstance(replicas, str): # path supplied
    with open(replicas, 'r') as f:
      replicasObject = json.loads(f.read())
  else: # fxBlockchain dict or replicas dict directly.
    if hasattr(replicas, "replicas"):
      replicasObject = replicas
      return getattr(replicasObject, "replicas")
    else: replicasObject = replicas
  nodeTypes = [
    TYPE_ETHEREUM, TYPE_DAML, TYPE_DAML_COMMITTER,
    TYPE_DAML_PARTICIPANT, TYPE_HLF, TYPE_TEE,
  ]
  result = {} # clean up and enforce replicas structure
  for nodeType in replicasObject:
    nodeWithThisType = replicasObject[nodeType]
    if nodeType == "others":
      result[nodeType] = replicasObject[nodeType]
      continue
    if nodeType not in nodeTypes: continue
    if nodeType not in result: result[nodeType] = []
    for i, nodeInfo in enumerate(nodeWithThisType):
      if isinstance(nodeInfo, str):
        result[nodeType].append(nodeInfo)
      elif "ip" in nodeInfo:
        result[nodeType].append(nodeInfo["ip"])
      elif "public_ip" in nodeInfo and nodeInfo["public_ip"] is not None:
        result[nodeType].append(nodeInfo["public_ip"])
      elif "private_ip" in nodeInfo and nodeInfo["private_ip"] is not None:
        result[nodeType].append(nodeInfo["private_ip"])

  return result



def longRunningTestDashboardLink(replicas=None):
  '''
    Returns URL for long-running test dashboard matching
    the [commiter + participant] configuration
  '''
  config = parseReplicasConfig(replicas)
  dashBaseUrl = "https://vmware.wavefront.com/dashboards/"
  params = []
  committers = config[TYPE_DAML_COMMITTER]
  participants = config[TYPE_DAML_PARTICIPANT]
  for i, ip in enumerate(committers):
    params.append("committer{}_ip:(l:'Committer-{}%20IP',v:'{}')".format(i+1, i+1, ip))
  for i, ip in enumerate(participants):
    params.append("participant{}_ip:(l:'Participant-{}%20IP',v:'{}')".format(i+1, i+1, ip))
  dashName = None
  if len(committers) == 4 and len(participants) == 1: dashName = "Blockchain-LRT-c4-p1"
  elif len(committers) == 7 and len(participants) == 1: dashName = "Blockchain-LRT-c7-p1"
  elif len(committers) == 7 and len(participants) == 3: dashName = "Blockchain-LRT-c7-p3"
  return (
    "{}{}#".format(dashBaseUrl, dashName) + # base url
    "_v01(" +
      "g:(d:1800,ls:!t,w:'30m')," + # last 30 minutes, live
      "p:({})".format(",".join(params)) + # params comma-separated
    ")"
  )


def installHealthDaemon(replicasInfoObject):
  '''
      Installs local, small-footprint health daemon on the nodes:
      replicasInfoObject = {
        "node_type1": [ip1, ip2, ip3, ..., ip_n],
        "node_type2": [ip1, ip2, ip3, ..., ip_n],
      }
      This will collect useful information leading up to possible crash,
      and will create crash file when supplied list of containers
      are missing from the node.
  '''
  try:
    # Config object injected by the Jenkins run, which is calling this script
    configObject = getUserConfig()

    # credential for SSH and SFTP
    credentials = configObject["persephoneTests"]["provisioningService"]["concordNode"]
    username = credentials["username"]; password = credentials["password"]
    wavefrontUrl = configObject["dashboard"]["devops"]["wavefront"]["url"]
    wavefrontToken = configObject["dashboard"]["devops"]["wavefront"]["token"]

    # Daemon behavior config
    reportingInterval = 20 # seconds
    announceInterval = 21600 # 6-hours, relatively static info (e.g. CPU spec, total RAM, total disk size)

    # install function to be called so installation is in parallel
    results = []
    def installOn(ip, deployedType):
      # Identifying information
      jenkinsBuildId = getJenkinsBuildId() # if called by Jenkins, user_config will have the id, otherwise ""
      blockchainType = deployedType if deployedType.find("_") == -1 else deployedType.split("_")[0]
      nodeType = deployedType if deployedType.find("_") == -1 else deployedType.split("_")[1]
      # Components (container) to watch
      componentsWatchList = getReplicaContainers(deployedType)
      if componentsWatchList: componentsWatchList = ','.join(componentsWatchList)
      # Load config file for healthd
      command= ('mkdir -p health-daemon\n' +
                'cd health-daemon\n' +
                'echo "' +
                  'ip={}\n'.format(ip) +
                  'blockchainId=test-blockchain\n' # need to be resolved as well by looking up VM properties
                  'blockchainType={}\n'.format(blockchainType) +
                  'nodeType={}\n'.format(nodeType) +
                  'buildId={}\n'.format(jenkinsBuildId) +
                  'logFile={}\n'.format(HEALTHD_LOG_PATH) +
                  'crashReportFile={}\n'.format(HEALTHD_CRASH_FILE) +
                  'reportingInterval={}\n'.format(reportingInterval) +
                  'recentReportFile={}\n'.format(HEALTHD_RECENT_REPORT_PATH) +
                  'announceInterval={}\n'.format(announceInterval) +
                  'componentsWatchList={}\n'.format(componentsWatchList) +
                  'wavefrontUrl={}\n'.format(wavefrontUrl) +
                  'wavefrontToken={}\n'.format(wavefrontToken) +
                '" > healthd.conf\n'
                )
      daemonConfInstallOutput = ssh_connect(ip, username, password, command)
      log.debug("Replica: {}\nCommand: {}\nOutput: {}\n\n\n\n".format(ip, command, daemonConfInstallOutput))
      # Load deamon source code and shell script to run it in the background
      destBasePath = '/root/health-daemon/'
      sftp_client(ip, username, password, os.path.join('util/healthd', "healthd.py"), destBasePath + 'healthd.py', action="upload")
      sftp_client(ip, username, password, os.path.join('util/healthd', "healthd.sh"), destBasePath + 'healthd.sh', action="upload")
      # Actually run the daemon
      command= ('cd health-daemon\n' +
                'nohup bash healthd.sh > /dev/null\n')
      daemonStartOutput = ssh_connect(ip, username, password, command)
      log.debug("Replica: {}\nCommand: {}\nOutput: {}\n\n\n\n".format(ip, command, daemonStartOutput))
      log.info("healthd reporting daemon started on {}".format(ip))

    threads = []
    for deployedType in replicasInfoObject:
      ipListOfThatType = replicasInfoObject[deployedType]
      for ip in ipListOfThatType:
        log.info("Installing health reporting daemon on {} as {}...".format(ip, deployedType))
        thr = threading.Thread(
          target = lambda publicIP, deployedType: installOn(publicIP, deployedType),
          args = (ip, deployedType, )
        )
        threads.append(thr)
        thr.start()

    for thd in threads: thd.join() # wait for all installations to return
    for result in results: log.info(result)
    log.info("Sleeping for 30 second for health daemon to initialize...")
    time.sleep(30)
    return True

  except Exception as e:
    log.debug(str(e))
    return False


def ip2long(ip):
   """
   Covert string form of an IP address to long
   :param ip: String form of an IP address
   :return: Long form of an IP address
   """
   packed_ip = socket.inet_aton(ip)
   return struct.unpack("!L", packed_ip)[0]


def long2ip(iplong):
   """
   Convert long form of an IP address to string
   :param iplong: Long form of an IP address
   :return: String form of an IP address
   """
   packed_ip = struct.pack('!L', iplong)
   return socket.inet_ntoa(packed_ip)


def fetch_default_zone_ids(properties_file=PROPERTIES_TEST_FILE):
   """
   Fetches 'vmbc.enabled.vmc.zones` from properties file
   :return:
   """
   zone_ids = read_key(key=PROPERTIES_VMBC_ENABLED_VMC_ZONES, properties_file=properties_file)
   return zone_ids.split(',')


def agentPulledTagsAreUniform():
  agent_pulled_tags_are_uniform = True
  with open(AGENT_PULLED_COMPONENTS_FILE) as file:
    components = json.load(file)
    all_tags = {}
    for component in components:
      component_tag  = get_docker_env(component['name'] + "_tag")
      all_tags[component_tag] = True
    if len(all_tags) > 1: agent_pulled_tags_are_uniform = False
  return agent_pulled_tags_are_uniform


def getDefaultDeploymentComponents():
   '''
   Make a list of all deployment components from user_config.json and fetch
   the repo:tag for each from the various places we define them.
   '''
   user_config = json_helper_util.readJsonFile(CONFIG_USER_FILE)
   cfg_model_service = user_config["persephoneTests"]["modelService"]
   cfg_components_to_find = cfg_model_service["deployment_component_ids"].values()
   cfg_deployment_components = cfg_model_service["defaults"]["deployment_components"]
   ret = ""

   with open(docker_env_file, "r") as f:
      for component_to_find in cfg_components_to_find:
         # Will contain vmwblockchain/agent:12345,vmwblockchain/concord-core:12345,...
         high_level_repo = ""
         repo_and_tag = ""

         # First, get the high level repo, like "vmwblockchain".
         found_high_level_repo = False
         for dep_type in cfg_deployment_components:
            for k in cfg_deployment_components[dep_type]:
               if component_to_find in k:
                  high_level_repo = k.split("/")[0]
                  found_high_level_repo = True
                  break
            if found_high_level_repo:
               break

         # Look for the docker tag in the .env file.
         f.seek(0)
         for line in f:
            if "/{}".format(component_to_find) in line:
               repo_key = line.split("=")[0]
               tag_key = repo_key.replace("_repo", "") + "_tag"
               tag = get_docker_env(tag_key)
               repo_and_tag = "{}/{}:{}".format(high_level_repo, component_to_find, tag)
               break

         # If not found, it's probably something like wavefront-proxy,
         # which is defined in user_config.
         if not repo_and_tag:
            for deployment_type in cfg_deployment_components:
               for k in cfg_deployment_components[deployment_type]:
                  if cfg_deployment_components[deployment_type][k] == component_to_find:
                     repo_and_tag = k
                     found = True
                     break
               if repo_and_tag:
                  break

         if repo_and_tag:
            if ret:
               ret += ","

            ret += repo_and_tag
         else:
            raise Exception("Could not find deployment component {}".format(component_to_find))

   return ret


def create_work_subdir(results_dir):
   subdir_nums = []
   work_subdir_prefix = "run_"
   new_dir_created = False
   new_subdir = None

   lock = threading.Lock()
   lock.acquire()

   while not new_dir_created:
       dir_entries = os.scandir(results_dir)

       for dir_entry in dir_entries:
           if dir_entry.is_dir():
               dir_only = os.path.basename(dir_entry.path)

               if dir_only.startswith(work_subdir_prefix):
                   dir_num = dir_only.split(work_subdir_prefix)[-1]

                   if dir_num:
                       try:
                           subdir_nums.append(int(dir_num))
                       except ValueError:
                           pass

       if subdir_nums:
          subdir_nums.sort()
          new_subdir = os.path.join(results_dir, work_subdir_prefix + str(subdir_nums[-1] + 1))
       else:
          new_subdir = os.path.join(results_dir, work_subdir_prefix + "0")

       try:
           log.info("Creating subdir {}".format(new_subdir))
           os.makedirs(new_subdir, exist_ok = False)
           new_dir_created = True
       except FileExistsError:
           # We are protected for multiple threads, but maybe another process created it.  Try again.
           log.debug("Tried to create {}, but something else created it. Trying a new dir.".format(new_subdir))

   lock.release()
   return new_subdir


def create_results_sub_dir(results_dir, host):
   results_sub_dir = os.path.join(results_dir, "{}_{}".format(
      time.strftime("%Y%m%d_%H%M%S", time.localtime()), host))
   if not os.path.exists(results_sub_dir):
      os.makedirs(results_sub_dir)
   return results_sub_dir


def disable_duplicate_logs():
  while log.handlers: log.handlers.pop()


def run_daml_sanity(ledger_api_hosts, results_dir, run_all_tests=True, verbose=True):
   '''
   Run daml test to validate blockchain
   :param fxHermesRunSettings: hermes run settings (fixture)
   :param ledger_api_hosts: list of ledger api hosts (participant nodes)
   :return: Test run status
   '''
   for ledger_api_host in ledger_api_hosts:
      log.info("ledger_api_host: {}".format(ledger_api_host))

      forwarding_src_port = FORWARDED_DAML_LEDGER_API_ENDPOINT_PORT
      upload_port = test_port = str(forwarding_src_port)

      try:
         daml_helper.upload_test_tool_dars(host=ledger_api_host, port=upload_port, verbose=verbose)
         daml_helper.verify_ledger_api_test_tool(host=ledger_api_host,
                                                 port=test_port,
                                                 run_all_tests=run_all_tests,
                                                 results_dir=results_dir,
                                                 verbose=verbose)
      except Exception as e:
         log.error("  DAML tests failed")
         return False

   log.info("  DAML tests passed")
   return True


def get_zones(org_name, service):
   '''
   Utility method to get zones.
   '''

   token_descriptor = {
     "org": org_name,
     "user": "vmbc_test_con_admin",
     "role": auth.ROLE_CON_ADMIN
   }

   log.info("1")
   req = rest.request.Request("get_zones",
                              "Get zones",
                              service,
                              None,
                              tokenDescriptor=token_descriptor,
                              service=service)
   log.info("2")
   resp = req.getZones()
   zone_ids = []

   for zone in resp:
      zone_ids.append(zone["id"])

   log.info("Zones:")
   for zone_id in zone_ids:
      resp = req.getZone(zone_id)
      log.info("  {}:".format(resp["name"]))
      for k in resp:
         log.info("    {}: {}".format(k, resp[k]))
