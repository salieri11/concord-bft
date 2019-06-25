# Helper file with common utility methods
import os
import yaml
import json
import shutil
import logging
import paramiko
import subprocess
from . import numbers_strings
from . import product as p

log = logging.getLogger(__name__)
docker_env_file = ".env"

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
         compose_data = yaml.load(yaml_file)

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

def ssh_connect(host, username, password, command):
   '''
   Helper method to execute a command on a host via SSH
   :param host: IP of the destination host
   :param username: username for SSH connection
   :param password: password for username
   :param command: command to be executed on the remote host
   :return: Output of the command
   '''
   resp = None
   try:
      ssh = paramiko.SSHClient()
      ssh.set_missing_host_key_policy(paramiko.AutoAddPolicy())
      ssh.connect(host, username=username, password=password)
      ssh_stdin, ssh_stdout, ssh_stderr = ssh.exec_command(command)
      outlines = ssh_stdout.readlines()
      resp = ''.join(outlines)
      log.debug(resp)
   except paramiko.AuthenticationException as e:
      log.error("Authentication failed when connecting to {}".format(host))
      raise
   except Exception as e:
      log.error("Could not connect to {}: {}".format(host, e))
      raise

   return resp

def execute_ext_command(command):
   '''
   Helper method to execute an external command
   :param command: command to be executed
   :return: True if command exit status is 0, else False
   '''
   log.info("Executing external command: {}".format(command))

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

def undeploy_blockchain_cluster(provisioning_config_file, grpc_server, session_id_json):
   '''
   Helper method to undeploy blockchain cluster deployed by deployment service
   :param provisioning_config_file: provisioning service config file
   :param grpc_server: provisioning service:port gRPC server
   :param session_id_json: deployment session ID
   :return: True if successful, else False
   '''

   if session_id_json:
      log.info("Undeploying session ID: {}".format(session_id_json))

      undeploy_docker_container = p.Product.PERSEPHONE_PROVISIONING_CLIENT_UNDEPLOY_DOCKER_REPO
      undeploy_docker_container_tag = p.Product.PERSEPHONE_PROVISIONING_CLIENT_UNDEPLOY_DOCKER_TAG
      provisioning_config_file_abspath = os.path.abspath(provisioning_config_file)
      undeploy_command = ["docker",
                          "run",
                          "--net=host",
                          "-v"
                          "{}:{}".format(provisioning_config_file_abspath, provisioning_config_file_abspath),
                          "{}:{}".format(undeploy_docker_container, undeploy_docker_container_tag),
                          "deleteCluster"
                          ]
      undeploy_command_params = [
         str(numbers_strings.to_signed_int(int(session_id_json[0]["low"]))),
         str(numbers_strings.to_signed_int(int(session_id_json[0]["high"]))),
         provisioning_config_file_abspath, grpc_server]
      command_to_execute = undeploy_command + undeploy_command_params
      log.debug("Executing Undeploy command: {}".format(command_to_execute))

      return execute_ext_command(command_to_execute)

   return True

def protobuf_message_to_json(message_obj):
   '''
   Helper method to convert a protobuf message to json
   :param message_obj: protobuf message
   :return: json
   '''
   from google.protobuf.json_format import MessageToJson
   if isinstance(message_obj, (list,)):
      list_of_json_objects = []
      for message in message_obj:
         json_object = json.loads(MessageToJson(message))
         list_of_json_objects.append(json_object)
      return list_of_json_objects
   else:
      json_object = json.loads(MessageToJson(message_obj))
      return json_object
