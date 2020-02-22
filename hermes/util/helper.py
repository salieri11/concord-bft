#########################################################################
# Copyright 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

# Helper file with common utility methods
import collections
import os
import yaml
import json
import shutil
import logging
import paramiko
import warnings
import cryptography
import socket
import subprocess
import sys
import tempfile
import time
import util.json_helper
from . import numbers_strings
from . import vsphere
from urllib.parse import urlparse, urlunparse

log = logging.getLogger(__name__)
docker_env_file = ".env"

# The config file contains information aobut how to run things, as opposed to
# command line parameters, which are about running tests.
CONFIG_JSON = "resources/user_config.json"

# CONFIG_CACHED["data"] is populated by `loadConfigFile`
# (containing data from user_config.json OR command line argument overrides)
CONFIG_CACHED = {}

# These are command line options for --blockchainLocation.  Various parts
# of the code need to know them.
LOCATION_LOCAL = "local"
LOCATION_SDDC = "sddc"
LOCATION_ONPREM = "onprem"

# These are command line options for --blockchainType.
# These need to match Helen.  See helen/src/main/resources/api-doc/api.yaml.
TYPE_ETHEREUM = "ethereum"
TYPE_DAML = "daml"
TYPE_DAML_COMMITTER = "daml_committer"
TYPE_HLF = "hlf"

# These are command line options for --keepBlockchains.
KEEP_BLOCKCHAINS_ALWAYS = "always"
KEEP_BLOCKCHAINS_ON_FAILURE = "on-failure"
KEEP_BLOCKCHAINS_NEVER = "never"

# When creating a zone, it can be one of these.
ZONE_TYPE_ON_PREM = "ON_PREM"
ZONE_TYPE_SDDC = "VMC_AWS"

# When creating a loging destination, its type can
# be one of these:
LOG_DESTINATION_LOG_INTELLIGENCE = "LOG_INTELLIGENCE"
LOG_DESTINATION_LOG_INSIGHT = "LOG_INSIGHT"

# all open infra sessions for accessing VMs & Folders on the datacenter connection
# e.g. vm = INFRA["SDDC3"].getByIP("10.69.100.46")
#     populated by `sddcGetConnection` (ConnectionToSDDC objects defined in vsphere.py)
INFRA = {}

# list of all deployed replicas by Hermes from this particular build
#     auto populated by `sddcGiveDeploymentContextToVM` with replicaInfo
DEPLOYED_REPLICAS = []



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
      config_file = "{}/config.json".format(config_folder)
   except Exception as e:
      raise
   return config_file


def ssh_connect(host, username, password, command, log_mode=None):
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
   try:
      ssh = paramiko.SSHClient()
      ssh.set_missing_host_key_policy(paramiko.AutoAddPolicy())
      ssh.connect(host, username=username, password=password)
      ssh_stdin, ssh_stdout, ssh_stderr = ssh.exec_command(command,
                                                           get_pty=True)
      outlines = ssh_stdout.readlines()
      resp = ''.join(outlines)
      log.debug(resp)
   except paramiko.AuthenticationException as e:
      log.error("Authentication failed when connecting to {}".format(host))
   except Exception as e:
      if log_mode == "WARNING":
         log.warning("Could not connect to {}".format(host))
      else:
         log.error("Could not connect to {}: {}".format(host, e))

   return resp


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
            log.debug("File downloaded successfully: {}".format(dest))
            result = True
      else:
         sftp.put(src, dest)
         cmd_verify_ftp = "ls {}".format(dest)
         ssh_output = ssh_connect(host, username,password, cmd_verify_ftp)
         log.debug(ssh_output)
         if ssh_output:
            if ssh_output.rstrip() == dest:
               log.debug("File downloaded successfully: {}".format(dest))
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


def execute_ext_command(command):
   '''
   Helper method to execute an external command
   :param command: command to be executed
   :return: True if command exit status is 0, else False
   '''
   log.debug("Executing external command: {}".format(command))

   completedProcess = subprocess.run(command, stdout=subprocess.PIPE,
                                     stderr=subprocess.STDOUT)
   try:
      completedProcess.check_returncode()
      log.debug("stdout: {}".format(
         completedProcess.stdout.decode().replace(os.linesep, "")))
      if completedProcess.stderr:
         log.info("stderr: {}".format(completedProcess.stderr))
   except subprocess.CalledProcessError as e:
      log.error(
         "Command '{}' failed to execute: {}".format(command, e.returncode))
      log.error("stdout: '{}', stderr: '{}'".format(completedProcess.stdout,
                                                    completedProcess.stderr))
      return False

   return True


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


def add_ethrpc_port_forwarding(host, username, password):
   '''
   Enable port forwarding on concord node to facilititate hitting ethrpc endpoint
   on port 443, which redirects to 8545. This is a workaround to support hitting
   ethrpc end points from within vmware network, as non 443/80 ports are blocked.
   Bug/Story: VB-1170
   :param host: concord host IP
   :param username: concord node login - username
   :param password: concord node login - password
   :return: Port forward status (True/False)
   '''
   src_port= 443
   dest_port = 8545
   try:
      log.info(
         "Adding port forwarding to enable ethrpc listen on {}:{}".format(host,
                                                                          src_port))
      cmd_get_docker_ethrpc_ip = "iptables -t nat -vnL | grep {} | grep DNAT | cut -d':' -f3".format(
         dest_port)
      docker_ethrpc_ip = ssh_connect(host, username, password,
                                     cmd_get_docker_ethrpc_ip)
      
      log.debug("Extracted IP to port forward: {}".format(docker_ethrpc_ip))

      if docker_ethrpc_ip:
         docker_ethrpc_ip = docker_ethrpc_ip.strip()
         cmd_port_forward = "iptables -t nat -A PREROUTING -p tcp --dport {} -j DNAT --to-destination {}:{}".format(
            src_port, docker_ethrpc_ip, dest_port)
         log.debug("Port forwarding command: {}".format(cmd_port_forward))
         output = ssh_connect(host, username, password, cmd_port_forward)
         log.debug("Port forwarded command output: {}".format(output))

         cmd_check_port_forward = "iptables -t nat -vnL | grep {}".format(src_port)
         log.debug("Port forwarding check command: {}".format(cmd_check_port_forward))
         port_forward_output = ssh_connect(host, username, password,
                                           cmd_check_port_forward)
         log.debug("Port forwarding check output: {}".format(port_forward_output))

         check_str_port_forward = "dpt:{} to:{}:{}".format(src_port,
                                                           docker_ethrpc_ip,
                                                           dest_port)
         if check_str_port_forward in port_forward_output:
            log.debug("Port forwarded successfully")
            return True
   except Exception as e:
      log.debug(e)

   log.debug("Port forwarding failed")
   return False


def verify_connectivity(ip, port, bytes_to_send=[], success_bytes=[], min_bytes=1):
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
   max_tries = 15
   sleep_time = 10
   socket_timeout = 5
   receive_buffer_size = 256
   bytes_to_search = []
   success_bytes_str = str(success_bytes)[1:-1]

   if success_bytes and min_bytes:
      raise Exception("Only pass success_bytes or min_bytes to verify_connectivity()")

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


def create_concord_support_bundle(replicas, concord_type, test_log_dir):
   '''
   Helper method to create concord support bundle and upload to result dir
   :param replicas: List of IP addresses of concord nodes
   :param concord_type: Concord node type ("daml", "ethereum", etc)
   :param test_log_dir: Support bundle to be uploaded to
   '''
   support_bundle_binary_name = "deployment_support.py"
   src_support_bundle_binary_path = os.path.join('util',
                                                 support_bundle_binary_name)
   remote_support_bundle_binary_path = os.path.join(tempfile.gettempdir(),
                                                  support_bundle_binary_name)
   user_config = util.json_helper.readJsonFile(CONFIG_JSON)
   concord_memeber_credentials = \
      user_config["persephoneTests"]["provisioningService"]["concordNode"]
   concord_username = concord_memeber_credentials["username"]
   concord_password = concord_memeber_credentials["password"]

   expected_docker_containers = list(
      user_config["persephoneTests"]["modelService"]["defaults"][
         "deployment_components"][concord_type.lower()].values())

   log.info("")
   log.info("**** Collecting Support bundle ****")
   try:
      for concord_ip in replicas:
         log.info("Concord IP: {}".format(concord_ip))
         log.info(
            "  Upload support-bundle generation script onto concord node '{}'...".format(
               concord_ip))

         if sftp_client(concord_ip, concord_username, concord_password,
                        src_support_bundle_binary_path,
                        remote_support_bundle_binary_path, action="upload"):
            log.debug("  Saved at '{}:{}'".format(concord_ip,
                                                 remote_support_bundle_binary_path))

            cmd_execute_collect_support_bundle = "python3 {} --concordIP {} " \
                                                 "--dockerContainers {}".format(
               remote_support_bundle_binary_path, concord_ip,
               ' '.join(expected_docker_containers))

            log.info("  Gathering deployment support logs...")
            ssh_output = ssh_connect(concord_ip, concord_username,
                                     concord_password,
                                     cmd_execute_collect_support_bundle)
            log.debug("Output from script '{}': {}".format(
                                             remote_support_bundle_binary_path,
                                             ssh_output))
            supput_bundle_created = False
            if ssh_output:
               for line in ssh_output.split('\n'):
                  if "Support bundle created successfully:" in line:
                     support_bundle_to_upload = line.split(':')[-1].strip()
                     log.info(
                        "  Support bundle created successfully on concord node {}:{}".format(
                           concord_ip, support_bundle_to_upload))
                     supput_bundle_created = True

                     log.info("  Exporting support bundle...")
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
                        log.info("  {}".format(dest_support_bundle))
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
         log.info("")

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


def loadConfigFile(args):
   '''
   Given the cmdline args, loads the main Hermes config file and returns
   the config object.
   '''
   configObject = None

   if args and args.config:
      configObject = util.json_helper.readJsonFile(args.config)
   else:
      configObject = util.json_helper.readJsonFile(CONFIG_JSON)

   if "ethereum" in configObject and \
      "testRoot" in configObject["ethereum"]:

      configObject["ethereum"]["testRoot"] = \
         os.path.expanduser(configObject["ethereum"]["testRoot"])

   # Jenkins JOB_NAME containing slashes were replaced with ___; bring slashes back.
   #    More info: see MR !1324
   if "metainf" in configObject:
      configObject["metainf"]["env"]["jobName"] = \
          configObject["metainf"]["env"]["jobName"].replace("___", "/")

   CONFIG_CACHED['data'] = configObject

   return configObject


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
   return args.blockchainLocation != util.helper.LOCATION_LOCAL


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
   user_config = util.json_helper.readJsonFile(CONFIG_JSON)

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


'''
  Helper functions relevant to Hermes infrastructure automation
'''
def sddcCredentialsAreGood(sddcName, sddcInfo):
   c = sddcInfo
   if not c["host"] or not c["username"] or not c["password"]: 
      log.debug("Target 'vSphere/{}' is not well-defined in user_config.json".format(sddcName))
      return False
   if c["username"].startswith("<") and c["username"].endswith(">"): # user_config not injected correctly with credential
      log.debug("vSphere/{}: username credential is not injected (user_config.json)".format(sddcName))
      return False
   if c["password"].startswith("<") and c["password"].endswith(">"): # user_config not injected correctly with credential
      log.debug("vSphere/{}: password credential is not injected (user_config.json)".format(sddcName))
      return False
   return True


def sddcGetConnection(sddcName):
   '''
      Initializes vSphere connection to the target SDDC.
      Host and Credentials are fed from `user_config.json`, for example: {
        ...
        "infra":{
          "SDDC3":{
            "type": "vSphere",
            "host": "vcenter.sddc-35-156-16-204.vmwarevmc.com", 
            "username": "<VMC_SDDC3_VC_CREDENTIALS_USERNAME>",
            "password": "<VMC_SDDC3_VC_CREDENTIALS_PASSWORD>" 
          }
          ...
        }
      }
      where Jenkins-kept credentials brought from `withCredentials` call,
      which get injected to `user_config.json` in `gitlabBuildSteps.groovy` 
   '''
   # get config from user_config.json
   configObject = CONFIG_CACHED['data'] if 'data' in CONFIG_CACHED else loadConfigFile(None)
   try:
      if sddcName not in INFRA:
         if sddcName not in configObject["infra"]:
           log.debug("Cannot open session to {}, no vSphere credential in config object.".format(sddcName))
           return False
         
         sddcInfo = configObject["infra"][sddcName]
         
         if not sddcCredentialsAreGood(sddcName, sddcInfo):
           log.debug("Cannot open session to {}, credentials are bad".format(sddcName))
           return False
         
         conn = vsphere.ConnectionToSDDC(
            sddcName = sddcName,
            hostname = sddcInfo["host"],
            username = sddcInfo["username"],
            password = sddcInfo["password"],
         )
         if not conn.ready:
           log.debug("Cannot open session to {}, connection is not ready".format(sddcName))
           return False
         
         INFRA[sddcName] = conn
      return INFRA[sddcName]

   except Exception as e:
     log.debug(e)
     return False


def sddcGetListFromConfig(configObject = None):
  '''
      Returns sddcNumber list from config object
      (List of all SDDCs affected by Hermes testing)
      :param configObject: (optional), if not given, auto-resolved by loadConfigFile
  '''
  if configObject is None:
    configObject = CONFIG_CACHED['data'] if 'data' in CONFIG_CACHED else loadConfigFile(None)
  sddcs = []
  # Can be vSphere SDDC or other on-prem locations
  #  e.g. Other on-prem location name with any string val
  for sddcName in configObject["infra"]:
    sddcs.append(sddcName)
  return sddcs


def sddcFindReplicaVM(replicaId, sddcs = None):
  '''
      Returns VM by the supplied replicaId
      :param sddc: (optional), if not given, all SDDCs defined in user_config.json used
  '''
  # if narrow sddcs search not given, search in all SDDCs in user_config
  sddcs = sddcs if sddcs is not None else sddcGetListFromConfig()
  for sddcName in sddcs:
    if sddcGetConnection(sddcName):
      vm = INFRA[sddcName].getByNameContaining(replicaId)
      if vm:
        return {"vm": vm, "sddc": INFRA[sddcName]}
  log.debug("Cannot find vm with its name containing '{}' in datacenters [{}]".format(replicaId, ', '.join(sddcs)))
  return None


def sddcGiveDeploymentContextToVM(blockchainDetails, otherMetadata=""):
   '''
      Add detailed deployment context to the Hermes-deployed VMs
      ```python
      blockchainDetails = {
          "id": "c035100f-22e9-4596-b9d6-5daa349db342",
          "consortium_id": "bfaa0041-8ab2-4072-9023-4cedd0e81a78",
          "blockchain_type": "ETHEREUM",
          "node_list": [ ... ], # `NOT USED`
          "replica_list": [{
            "ip": "52.63.165.178",
            "url": "https://52.63.165.178:8545", # `NOT USED`
            "cert": "", # `NOT USED`
            "zone_id": "6adaf48a-9075-4e35-9a71-4ef1fb4ac90f", # `NOT USED`
            "replica_id": "a193f7b8-6ec5-4802-8c2f-33cb33516c3c"
          }, ...]
      }
      ```
   '''
   # get config from user_config.json
   try: 
      configObject = CONFIG_CACHED['data'] if 'data' in CONFIG_CACHED else loadConfigFile(None)
      sddcs = sddcGetListFromConfig(configObject)
      jobName = configObject["metainf"]["env"]["jobName"]
      buildNumber = configObject["metainf"]["env"]["buildNumber"]
      productVersion = configObject["metainf"]["env"]["productVersion"]
      pytestContext = os.getenv("PYTEST_CURRENT_TEST")
      if pytestContext is None: pytestContext = ""
      runCommand = os.getenv("SUDO_COMMAND")
      if runCommand is None: runCommand = ""

      for replicaInfo in blockchainDetails["replica_list"]:
        alreadyRegistered = [replica for replica in DEPLOYED_REPLICAS if replica.get("replica_id") == replicaInfo["replica_id"]]
        if len(alreadyRegistered) > 0: continue # this replica is already registered to DEPLOYED_REPLICAS
        DEPLOYED_REPLICAS.append(replicaInfo)

        vmAndSourceSDDC = sddcFindReplicaVM(
          replicaId = replicaInfo["replica_id"],
          sddcs = sddcs
          # above "sddcs": Perhaps, this can be abtracted later to "datacenters" and also support AWS/Azure/GCP/etc
          # It would be interesting to test concord with various mixed cloud environment set-up.
          # They all have their own version of inventory system with: instance notes, descriptions and/or tags, etc.
        )
        if vmAndSourceSDDC is None: continue # vm with the given replicaId is not found
        vm = vmAndSourceSDDC["vm"]
        sddc = vmAndSourceSDDC["sddc"]
        sddc.vmAnnotate(vm, # edit VM Notes with detailed deployment context
            "Public IP: {}\nReplica ID: {}\nBlockchain: {}\nConsortium: {}\nType: {}\n".format(
              replicaInfo["ip"],
              replicaInfo["replica_id"],
              blockchainDetails["id"],
              blockchainDetails["consortium_id"],
              blockchainDetails["blockchain_type"],
            ) + "\nDeployed By: Hermes\nProduct Version: {}\nJob Name: {}\nBuild Number: {}\nBuild URL: {}\n".format(
              productVersion,
              jobName,
              buildNumber,
              "https://blockchain.svc.eng.vmware.com/job/{}/{}".format(jobName, buildNumber)
            ) + "\nPytest Context: {}\n\nRun Command: {}\n\nOther Metadata: {}\n\n".format(
              pytestContext,
              runCommand,
              otherMetadata
            )
        )
        # Add custom attributes
        sddc.vmSetCustomAttribute(vm, "up_since", str(int(time.time()))) # UNIX timestamp in seconds
        sddc.vmSetCustomAttribute(vm, "realm", "testing")
        sddc.vmSetCustomAttribute(vm, "product_version", productVersion)
        sddc.vmSetCustomAttribute(vm, "job_name", jobName)
        sddc.vmSetCustomAttribute(vm, "build_number", buildNumber)
        sddc.vmSetCustomAttribute(vm, "replica_id", replicaInfo["replica_id"])
        sddc.vmSetCustomAttribute(vm, "ip", replicaInfo["ip"])
        sddc.vmSetCustomAttribute(vm, "blockchain_id", blockchainDetails["id"])
        sddc.vmSetCustomAttribute(vm, "consortium_id", blockchainDetails["consortium_id"])
        sddc.vmSetCustomAttribute(vm, "blockchain_type", blockchainDetails["blockchain_type"])
        sddc.vmSetCustomAttribute(vm, "other_metadata", otherMetadata)

   except Exception as e:
      log.debug(e)

