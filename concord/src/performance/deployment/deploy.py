import argparse
import concurrent.futures
import json
import logging
import os
import re
import select
import shutil
import subprocess
import sys
import threading
import time
import traceback
from sys import stdout

import boto3
import botocore
import paramiko
import yaml

config_file_name = 'deployment_local.yaml'
logger = logging.getLogger("main-logger")
f = logging.Formatter("%(asctime)-15s %(levelname)-6s %(message)s")
h = logging.StreamHandler(stdout)
h.setFormatter(f)
logger.addHandler(h)
logger.setLevel(10)


class Instance:
    def __init__(self, aws_instance):
        self.instance = aws_instance
        self.public_ip = aws_instance.public_ip_address
        self.private_ip = aws_instance.private_ip_address
        self.instance_id = aws_instance.instance_id
        self.concord_id = None
        self.is_replica = False
        self.ssh_client = None
        self.ssh_channel = None
        self.remote_path = None
        self.container_id = None
        self.loader_timer = None
        self.replica_thread = None
        self.replica_custom_command_result = []
        self.replica_custom_command_interval = None
        self.replica_custom_command = None
        self.done = False

    def set_concord_meta(self, ids, first_client_id):
        self.concord_id = ids.index(self.instance_id)
        self.is_replica = (self.concord_id < first_client_id)

    def run_command(self, cmd, print_pref = True):
        try:
            timeout = 10
            stdin, stdout, stderr = self.ssh_client.exec_command(cmd)
            channel = stdout.channel
            stdout_chunks = []
            stderr_chunks = []
            l = stdout.channel.recv(len(stdout.channel.in_buffer)).decode("utf-8")
            ll = stderr.channel.recv(len(stderr.channel.in_buffer)).decode("utf-8")
            stdout_chunks.append(l)
            stderr_chunks.append(ll)
            while not channel.closed or channel.recv_ready() or channel.recv_stderr_ready():
                # stop if channel was closed prematurely, and there is no data in the buffers.
                got_chunk = False
                readq, _, _ = select.select([stdout.channel], [], [], timeout)
                for c in readq:
                    if c.recv_ready():
                        l = stdout.channel.recv(len(c.in_buffer)).decode("utf-8")
                        stdout_chunks.append(l)
                        got_chunk = True
                    if c.recv_stderr_ready():
                        ll = stderr.channel.recv_stderr(len(c.in_stderr_buffer)).decode("utf-8")
                        stderr_chunks.append(ll)
                        got_chunk = True

                if not got_chunk and stdout.channel.exit_status_ready() and not stderr.channel.recv_stderr_ready():
                    if not stdout.channel.recv_ready():
                        # indicate that we're not going to read from this channel anymore
                        stdout.channel.shutdown_read()
                        # close the channel
                        stdout.channel.close()
                        break  # exit as remote side is finished and our bufferes are empty

            # close all the pseudofiles
            stdout.close()
            stderr.close()

            pref = f"stdout from concord{self.concord_id}:{self.public_ip} ---> " if print_pref else ""
            return (pref.join(stdout_chunks),
                    f"stderr from concord{self.concord_id}:{self.public_ip} ---> ".join(stderr_chunks),
                    stdout.channel.recv_exit_status())
        except paramiko.ssh_exception.SSHException as e:
            logger.error(e)
            raise Exception("command failed")

    def upload_file(self, local_path, remote_path, file_name, clean_remote_folder, config):
        try:
            logger.info(f"uploading configuration TAR to {self.public_ip}...")
            if clean_remote_folder:
                cmd = f"cd {config.get_remote_perf_results_root()} && rm -rf *"
                res = self.run_command(cmd)
            self.run_command(f"mkdir -p {remote_path}")
            ftp_client = self.ssh_client.open_sftp()
            ftp_client.put(os.path.join(local_path, file_name), os.path.join(remote_path, file_name))
            ftp_client.close()
            return True, self.public_ip
        except Exception as e:
            logger.error(e)
            return False, self.public_ip

    def start_ssh(self, config):
        retries = 24
        wait = 5
        count = 0
        self.ssh_client = paramiko.client.SSHClient()
        self.ssh_client.set_missing_host_key_policy(paramiko.AutoAddPolicy())
        pk_path = config.get_ssh_key_path()
        ssh_key = k = paramiko.RSAKey.from_private_key_file(pk_path)
        while count < retries:
            try:
                self.ssh_client.connect(self.public_ip, port=22, username=config.get_ssh_username(), pkey=ssh_key,
                                        timeout=180)
                self.ssh_client.get_transport().set_keepalive(30)
                logger.info(f"ssh to {self.public_ip} is running")
                break
            except Exception as e:
                logger.error(f"{e}, ip: {self.public_ip}")
                time.sleep(wait)
                count += 1
        if count == retries:
            raise(Exception(f"can't connect to {self.public_ip}"))

    def prepare_remote_env(self, remote_path, tar_name):
        self.remote_path = remote_path
        try:
            logger.info(f"preparing remote env for {self.public_ip} at {remote_path}...")
            cmd = f"cd {remote_path} && tar xzf {tar_name} && ./remote_pre_deploy.sh"
            res = self.run_command(cmd)
            return True, self.public_ip
        except Exception as e:
            logger.error(e)
            return False, self.public_ip

    def pull_image(self, config):
        user = config.get_docker_username()
        repo = config.get_docker_repo()
        if self.is_replica:
            image_name = user + "/" + repo + ":" + config.get_repo_tag_replica()
        else:
            image_name = user + "/" + repo + ":" + config.get_repo_tag_loader()

        try:
            logger.info(f"pulling {image_name} at {self.public_ip} ...")
            cmd = f"sudo docker pull {image_name}"
            logger.debug(cmd)
            res = self.run_command(cmd)
            logger.debug(f"pull_image, {res[0]} {res[1]}")
            return True, self.public_ip, image_name
        except Exception as e:
            logger.error(e)
            return False, self.public_ip, image_name

    def run_post_deploy(self):
        logger.info(f"running post deploy script for concord{self.concord_id} at {self.public_ip}")
        try:
            id = " " + str(self.concord_id + 1) if self.is_replica else ""
            cmd = f"cd {self.remote_path} && ./remote_post_deploy.sh{id}"
            res = self.run_command(cmd)
            logger.debug(f"run_post_deploy {res[0]} {res[1]}")
            self.stop_containers()
            return True, self.public_ip
        except Exception as e:
            logger.error(e)
            return False, self.public_ip

    def check_container_running(self, filter):
        cmd = "sudo docker ps --format \"table {{.ID}}\\t{{.Status}}\\t{{.Names}}\" --filter \"" + filter + "\""
        logger.debug(f"check running container with command \"{cmd}\"...")
        res = self.run_command(cmd)
        if re.search("Up", res[1]) is not None:
            t = res[1].split('\n')[1]
            regex = r"(\S+)\s+\w+\s\d+\s+\w+\s+(\S+)"
            m = re.match(regex, t)
            self.container_id = m.group(1)
            self.container_name = m.group(2)
            return True
        return False

    def replica_thread_func(self):
        try:
            while not self.done:
                # logger.debug(f"replica_thread_func {self.replica_custom_command}")
                res = self.run_command(self.replica_custom_command)
                self.replica_custom_command_result.append(res[0])
                self.replica_custom_command_result.append(res[1])
                # logger.debug(f"replica_thread_func res: {res}")
                if not self.done:
                    time.sleep(self.replica_custom_command_interval)
        except Exception as e:
            logger.error(e)
            pass

    def start_replica(self, custom_command_interval=0, custom_command=None):
        logger.info(f"starting concord{self.concord_id + 1} at {self.public_ip}")
        assert ((custom_command_interval > 0 and custom_command) or (
                    custom_command_interval == 0 and not custom_command))
        try:
            if self.check_container_running(f"label=custom_name=concord_perf_replica{self.concord_id + 1}"):
                logger.debug(f"concord{self.concord_id + 1} is running on {self.public_ip}")
                return True
            cmd = f"cd {self.remote_path} && sudo docker-compose " \
                  f"-f docker-compose-perf-rep-aws.yml up -d concord{self.concord_id + 1}"
            res = self.run_command(cmd)
            logger.info(f"waiting for concord{self.concord_id + 1} to start at {self.public_ip}")
            time.sleep(5)
            count = 0
            while count < 5:
                if self.check_container_running(f"label=custom_name=concord_perf_replica{self.concord_id + 1}"):
                    logger.info(f"concord{self.concord_id + 1} is running on {self.public_ip}")
                    if custom_command_interval > 0:
                        cc = custom_command.replace("CONTAINER_ID", self.container_id)
                        cc = f"cd {self.remote_path} && ./{cc}"
                        self.replica_custom_command_interval = custom_command_interval
                        self.replica_custom_command = cc
                        self.replica_thread = threading.Thread(target=self.replica_thread_func)
                        self.replica_thread.start()
                    return True
                count += 1
                time.sleep(5)
            return False
        except Exception as e:
            logger.error(e)
            return False

    def timer(self):
        logger.info("Loader timer tick!")
        self.stop_containers()
        self.ssh_client.close()
        raise Exception(f"Loader {self.concord_id + 1} at {self.public_ip} timeout occured!!! Aborting!")

    def start_loader(self, timeout_sec=5):
        logger.info(f"running loader (concord{self.concord_id + 1}) at {self.public_ip}")
        try:
            cmd = f"cd {self.remote_path} && sudo docker-compose -f docker-compose-perf-loader-aws.yml up"
            self.loader_timer = threading.Timer(timeout_sec, self.timer)
            self.loader_timer.start()
            res = self.run_command(cmd, False)
            return True, res[0], res[1]
        except Exception as e:
            logger.error(e)
            return False, "Exception occured", e
        finally:
            if self.loader_timer:
                self.loader_timer.cancel()

    def collect_results(self, local_folder, copy_all_data, config):
        logger.info(f"collecting results from concord{self.concord_id + 1} at {self.public_ip}")
        ftp_client = None
        try:
            ftp_client = self.ssh_client.open_sftp()
            if self.replica_thread:
                self.done = True
                dest_folder = os.path.join(local_folder, "logs")
                with open(os.path.join(dest_folder,
                                       f"custom_script_output{self.concord_id + 1}.txt"), "w") as text_file:
                    for line in self.replica_custom_command_result:
                        print(line, file=text_file)

            if self.is_replica:
                cmd = f"sudo tar cfz {self.remote_path}/logs/replica_logs_{self.concord_id + 1}" \
                      f".tar.gz -C /media/concord/log ."
            else:
                cmd = f"sudo tar cfz {self.remote_path}/logs/loader_Logs_{self.concord_id + 1}" \
                      f".tar.gz -C /media/concord/log ."
            logger.debug(f"creating TAR with command {cmd}")
            res = self.run_command(cmd)
            assert (res[2] == 0)
            if self.is_replica:
                cmd = f"sudo tar cfz {self.remote_path}/rocksdb_out/rocksdb{self.concord_id + 1}.tar.gz " \
                      f"/media/concord/rocksdbdata/*"
                res = self.run_command(cmd)
                assert (res[2] == 0)
                ftp_client.get(f"{self.remote_path}/logs/replica_logs_{self.concord_id + 1}.tar.gz",
                           os.path.join(local_folder, "logs", f"replica_logs{self.concord_id + 1}.tar.gz"))
            else:
                ftp_client.get(f"{self.remote_path}/logs/loader_Logs_{self.concord_id + 1}.tar.gz",
                               os.path.join(local_folder, "logs", f"loader_logs{self.concord_id + 1}.tar.gz"))

            cmd = f"ls /media/concord/cores | wc -l"
            res = self.run_command(cmd)
            if res[1] != "0\n":
                cmd = f"sudo tar cfz {self.remote_path}/core/coredump{self.concord_id + 1}.tar.gz" \
                      f" -C /media/concord/cores *"
                res = self.run_command(cmd)
                assert (res[2] == 0)
                logger.info(
                    f"!!!!!! core dump created for {self.concord_id + 1} at {self.public_ip}:{self.remote_path}/core")

            if copy_all_data:
                res = self.run_command(cmd)
                ftp_client.get(f"{self.remote_path}/core/coredump{self.concord_id + 1}.tar.gz",
                               os.path.join(local_folder, "core", f"coredump{self.concord_id + 1}.tar.gz"))
                if self.is_replica:
                    ftp_client.get(f"{self.remote_path}/rocksdb_out/rocksdb{self.concord_id + 1}.tar.gz",
                                   os.path.join(local_folder, "rocksdb", f"rocksdb{self.concord_id + 1}.tar.gz"))
            return True, self.concord_id, self.public_ip
        except Exception as e:
            logger.error(e)
            return False, f"concord{self.concord_id + 1}", self.public_ip
        finally:
            if ftp_client:
                ftp_client.close()

    def stop_containers(self):
        logger.info(f"stopping concord{self.concord_id + 1} with container ID {self.container_id} at {self.public_ip}")
        try:
            # stop all running containers, not to miss anything
            cmd = f"sudo docker kill $(sudo docker ps -q)"
            res = self.run_command(cmd)
            return res[2] == 0, self.concord_id + 1, self.public_ip
        except Exception as e:
            logger.error(e)
            return False, self.concord_id + 1, self.public_ip


class Configuration:
    def __init__(self, yaml):
        self.yaml = yaml

    # returns triplet of (AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, AWS_SESSION_TOKEN)
    def get_credentials(self):
        return (
            self.yaml['aws']['credentials']['AWS_ACCESS_KEY_ID'],
            self.yaml['aws']['credentials']['AWS_SECRET_ACCESS_KEY'],
            self.yaml['aws']['credentials']['AWS_SESSION_TOKEN'])

    def get_replicas_ids(self):
        return self.yaml['aws']['machines']['replicas']

    def get_loaders_ids(self):
        return self.yaml['aws']['machines']['loaders']

    def get_concord_root(self):
        return self.yaml['local']['concord_root']

    def get_concord_image_name(self):
        return self.yaml['docker']['concord_image_name']

    def get_loader_image_name(self):
        return self.yaml['docker']['loader_image_name']

    def get_local_perf_results_root(self):
        return self.yaml['local']['perf_test_root']

    def get_remote_perf_results_root(self):
        return self.yaml['aws']['perf_test_root']

    def get_ssh_key_path(self):
        key_name = self.get_ssh_key_name()
        return os.path.join(self.yaml['aws']['ssh_key_path'], key_name)

    def get_ssh_key_name(self):
        return self.yaml['aws']['ssh_key_name']

    def get_ssh_username(self):
        return self.yaml['aws']['ssh_username']

    def get_docker_server(self):
        return self.yaml['docker']['server']

    def get_docker_repo(self):
        return self.yaml['docker']['repo_name']

    def get_docker_username(self):
        return self.yaml['docker']['username']

    def get_docker_access_token(self):
        return self.yaml['docker']['access_token']

    def get_repo_tag_replica(self):
        return self.yaml['docker']['repo_tag_replica']

    def get_repo_tag_loader(self):
        return self.yaml['docker']['repo_tag_loader']

    def get_loader_command(self):
        return self.yaml['docker']['loader_cli']

    def get_ami_id(self):
        return self.yaml['aws']['ami_id']

    def get_region_name(self):
        return self.yaml['aws']['region_name']

    def get_az_name(self):
        return self.yaml['aws']['az_name']

    def get_num_of_replicas(self):
        return self.yaml['general']['num_of_replicas']

    def get_loader_timeout_sec(self):
        return self.yaml['general']['loader_timeout_sec']

    def get_custom_command(self):
        return self.yaml['runtime']['custom_command']

    def get_custom_command_run_interval_sec(self):
        return self.yaml['runtime']['custom_command_run_interval_sec']

    def get_custom_script_path(self):
        return self.yaml['runtime']['custom_script_path']


def load_configuration():
    config_yaml = None
    with open(config_file_name) as yaml_file:
        config_yaml = yaml.load(yaml_file)
    return Configuration(config_yaml)


def get_aws_session(credentials):
    boto_session = boto3.Session(
        aws_access_key_id=credentials[0],
        aws_secret_access_key=credentials[1],
        aws_session_token=credentials[2])
    return boto_session


def wait_for_machine(waiter, instance_id, description):
    try:
        waiter.wait(InstanceIds=[instance_id])
        logger.info(f"instance {instance_id} is {description}")
        return True
    except botocore.exceptions.WaiterError as we:
        logger.error(we)
        return False


def start_machine(id, client, session, config):
    logger.info(f"instance {id} is starting")
    try:
        response = client.start_instances(InstanceIds=[id])
        res = wait_for_machine(client.get_waiter('instance_running'), id, "running")
        if not res:
            return res, None
        aws_instance = session.resource("ec2", region_name=config.get_region_name()).Instance(id)
        return True, Instance(aws_instance)
    except botocore.exceptions.ClientError as e:
        logger.error(e)
        return False, None


def stop_machine(id, client):
    logger.info(f"instance {id} is stopping")
    try:
        response = client.stop_instances(InstanceIds=[id])
        wait_for_machine(client.get_waiter('instance_stopped'), id, "stopped")
    except Exception as e:
        logger.error(e)


def replace_in_file(file_path, pattern, replacement):
    logger.debug(f"replace_in_file {file_path} {pattern} {replacement}")
    in_place = "-i"
    if sys.platform.startswith("darwin"):
        in_place += " ''"
    try:
        cmd = f"sed {in_place} \'s/{pattern}/{replacement}/g\' {file_path}"
        subprocess.check_call(cmd, shell=True)
    except subprocess.CalledProcessError as e:
        logger.error(e)
        raise e


def set_network(instances, conf_file_path):
    logger.info(f"updating configuration files in {conf_file_path} ...")
    try:
        for i in instances:
            replace_in_file(conf_file_path, f"concord{i.concord_id + 1}", i.private_ip)
    except Exception as e:
        logger.error(e)
        raise e


def set_static_params(conf_file_path, args):
    logger.info("setting static params...")
    try:
        if args.inMemory:
            replace_in_file(conf_file_path, "blockchain_db_impl: rocksdb", "blockchain_db_impl: memory")
        else:
            replace_in_file(conf_file_path, "blockchain_db_impl: memory", "blockchain_db_impl: rocksdb")
        if args.useBasicKVBC:
            replace_in_file(conf_file_path, "blockchain_storage_type: merkle", "blockchain_storage_type: basic")
        else:
            replace_in_file(conf_file_path, "blockchain_storage_type: basic", "blockchain_storage_type: merkle")
    except Exception as e:
        logger.error(e)
        raise e


def set_docker_params(conf_file_path, config):
    logger.info("setting docker parameters...")
    try:
        docker_username = config.get_docker_username()
        docker_token = config.get_docker_access_token()
        docker_server = config.get_docker_server()
        replace_in_file(conf_file_path, "DOCKER_USERNAME", docker_username)
        replace_in_file(conf_file_path, "DOCKER_TOKEN", docker_token)
        replace_in_file(conf_file_path, "DOCKER_SERVER", docker_server)
    except subprocess.CalledProcessError as e:
        logger.error(e)
        raise e


def set_replica_docker_compose_params(yaml_file_path, config):
    logger.info("setting replica docker compose parameters...")
    try:
        docker_username = config.get_docker_username()
        docker_repo = config.get_docker_repo()
        replica_tag = config.get_repo_tag_replica()

        replace_in_file(yaml_file_path, "DOCKER_USERNAME", docker_username)
        replace_in_file(yaml_file_path, "DOCKER_REPO", docker_repo)
        replace_in_file(yaml_file_path, "REPLICA_TAG", replica_tag)
    except subprocess.CalledProcessError as e:
        logger.error(e)
        raise e


def set_loader_docker_compose_params(yaml_file_path, config):
    logger.info("setting loader docker compose parameters...")
    try:
        docker_username = config.get_docker_username()
        docker_repo = config.get_docker_repo()
        loader_tag = config.get_repo_tag_loader()
        command = config.get_loader_command()

        replace_in_file(yaml_file_path, "DOCKER_USERNAME", docker_username)
        replace_in_file(yaml_file_path, "DOCKER_REPO", docker_repo)
        replace_in_file(yaml_file_path, "LOADER_TAG", loader_tag)
        replace_in_file(yaml_file_path, "LOADER_COMMAND", command)
    except subprocess.CalledProcessError as e:
        logger.error(e)
        raise e


def create_local_env(config, args, replica_instances, loader_instances):
    concord_root_folder = config.get_concord_root()
    run_root_folder = config.get_local_perf_results_root()
    if not os.path.exists(run_root_folder):
        os.mkdir(run_root_folder)
    old_dir = os.getcwd()
    # create folders
    time_stamp = time.strftime("%Y%m%dT%H%M%S")
    run_root_folder = os.path.join(run_root_folder, time_stamp)
    os.mkdir(run_root_folder)
    config_folder = os.path.join(run_root_folder, "config")
    os.mkdir(config_folder)
    os.mkdir(os.path.join(run_root_folder, "config-public"))
    os.mkdir(os.path.join(run_root_folder, "logs"))
    if args.copyAllData:
        os.mkdir(os.path.join(run_root_folder, "core"))
        os.mkdir(os.path.join(run_root_folder, "rocksdb"))
    perf_script_folder = os.path.join(concord_root_folder, "concord", "src", "performance", "deployment")
    # copy config files
    config_file = f"dockerConfigurationInput-performance-handler-deploy-{config.get_num_of_replicas()}.yaml"
    shutil.copy(os.path.join(concord_root_folder, "docker", "config-public", config_file), config_folder)
    remote_sh_file_path = os.path.join(perf_script_folder, "remote_pre_deploy.sh")
    shutil.copy(remote_sh_file_path, run_root_folder)
    remote_sh_file_path = os.path.join(perf_script_folder, "remote_post_deploy.sh")
    shutil.copy(remote_sh_file_path, run_root_folder)
    # modify concord config file - set network
    config_file_path_out = os.path.join(config_folder, config_file)
    set_network(replica_instances, config_file_path_out)
    # set static set_static_params
    set_static_params(config_file_path_out, args)
    shutil.copy(
        os.path.join(concord_root_folder, perf_script_folder, "log4cplus.properties"),
        os.path.join(run_root_folder, "config-public"))
    bft_client_config_file = f"bftclient-{config.get_num_of_replicas()}.config"
    shutil.copy(os.path.join(perf_script_folder, bft_client_config_file),
                os.path.join(config_folder, "bftclient.config"))
    # copy custom run script
    script_path = os.path.join(concord_root_folder, perf_script_folder, config.get_custom_script_path())
    shutil.copy(os.path.join(concord_root_folder, perf_script_folder, script_path),
                run_root_folder)
    shutil.copytree(os.path.join(perf_script_folder, "tls_certs"),
                    os.path.join(run_root_folder, "tls_certs"))
    shutil.copytree(os.path.join(concord_root_folder, "docker", "trs_trc_tls_certs"),
                    os.path.join(run_root_folder, "trs_trc_tls_certs"))
    set_network(replica_instances, os.path.join(config_folder, "bftclient.config"))
    replace_in_file(os.path.join(config_folder, "bftclient.config"), "\/concord\/tls_certs", "\/perf_loader\/tls_certs")
    # set docker parameters
    set_docker_params(os.path.join(run_root_folder, "remote_pre_deploy.sh"), config)
    replica_docker_compose_file_path = os.path.join(concord_root_folder, perf_script_folder,
                                                    "docker-compose-perf-rep-aws.yml")
    shutil.copy(replica_docker_compose_file_path, run_root_folder)
    set_replica_docker_compose_params(os.path.join(run_root_folder, "docker-compose-perf-rep-aws.yml"), config)
    loader_docker_compose_file_path = os.path.join(concord_root_folder, perf_script_folder,
                                                   "docker-compose-perf-loader-aws.yml")
    shutil.copy(loader_docker_compose_file_path, run_root_folder)
    set_loader_docker_compose_params(os.path.join(run_root_folder, "docker-compose-perf-loader-aws.yml"), config)
    tar_name = f"{time_stamp}.tar.gz"
    try:
        os.chdir(concord_root_folder)
        # create concord config
        os.chdir("docker")
        create_conc_cfg_cmd = os.path.join(".",
                                            "gen-docker-concord-config.sh") + f" config/{config_file} " \
                                                                              f"config-public {run_root_folder}"
        logger.debug(f"create concord configuration with command {create_conc_cfg_cmd}")
        subprocess.check_call(create_conc_cfg_cmd, shell=True)
        subprocess.check_call(create_conc_cfg_cmd, shell=True)
        # create tar
        out_tar_path = os.path.join(run_root_folder, tar_name)
        os.chdir(config.get_local_perf_results_root())
        logger.debug("cwd: " + os.getcwd())
        logger.info(f"create deployment tar as {out_tar_path} ...")
        subprocess.check_call(f"tar cfz {tar_name} -C {time_stamp} .", shell=True)
        subprocess.check_call(f"mv {tar_name} {time_stamp}", shell=True)
        return True, run_root_folder, tar_name, time_stamp
    except subprocess.CalledProcessError as e:
        logger.error(e)
        return False, run_root_folder, tar_name, time_stamp
    finally:
        os.chdir(old_dir)


def build_image(docker_context_folder, docker_file, image_name):
    logger.info(f"building container with {docker_file} with tag {image_name} ...")
    try:
        cmd = f'docker build -f {docker_file} -t {image_name} {docker_context_folder}'
        process = subprocess.Popen([cmd], stdout=subprocess.PIPE, shell=True, bufsize=8196)
        while process.poll() is None:
            line = process.stdout.readline().decode("ascii")
            logger.info(f"building {image_name} --->" + line)
        sys.stdout.buffer.write(process.stdout.read())
        return process.returncode == 0
    except Exception as e:
        logger.error(e)
        return False


def do_docker_login(config):
    try:
        u = config.get_docker_username()
        p = config.get_docker_access_token()
        s = config.get_docker_server()
        logger.debug(f"log in to {s} with {u}:{p}")
        cmd = f"docker login -u {u} -p {p} {s}"
        res = subprocess.check_output(cmd, shell=True)
        res_str = res.decode("utf-8")
        if res_str.find("Login Succeeded") == -1:
            return False
        return True
    except Exception as e:
        logger.error(e)
        return False


def upload_docker_images(image_name, config, tag):
    try:
        u = config.get_docker_username()
        repo = f"{u}/{config.get_docker_repo()}"
        image_tag = repo + ":" + tag
        logger.debug(f"tagging {image_name} with {image_tag}")
        cmd = f"docker tag {image_name} {image_tag}"
        subprocess.check_call(cmd, shell=True)
        cmd = f"docker push {image_tag}"
        process = subprocess.Popen([cmd], stdout=subprocess.PIPE, shell=True, bufsize=8196)
        while process.poll() is None:
            line = process.stdout.readline().decode("ascii")
            logger.info(f"pushing {image_tag} --->" + line)
        sys.stdout.buffer.write(process.stdout.read())
        return process.returncode == 0, image_tag
    except Exception as e:
        logger.error(e)
        return False, None


def raise_error(msg, pool=None):
    logger.error("ERROR!!!")
    logger.error(msg)
    if pool:
        pool._threads.clear()
        concurrent.futures.thread._threads_queues.clear()
    sys.exit(-1)


def get_instances(config, filters, client):
    res = client.describe_instances(Filters=filters)
    logger.debug(f"describe_instances output: {os.linesep} {res}")
    loaders = []
    replicas = []
    if len(res["Reservations"]) == 0:
        return replicas, loaders
    for r in res["Reservations"]:
        for instance in r["Instances"]:
            if instance["State"]["Name"] == "terminated":
                continue
            for tag in instance["Tags"]:
                if tag["Key"] == "concord_perf":
                    if tag["Value"] == "loader":
                        loaders.append(instance["InstanceId"])
                    elif tag["Value"] == "replica":
                        replicas.append(instance["InstanceId"])
    logger.debug(f"get_instances result: {replicas} , {loaders}")
    return replicas, loaders


def setup_infra(config, client):
    vpc_id = None
    subnet_id = None
    sec_group_id = None

    res = client.describe_vpcs()
    logger.debug("***** vpcs" + json.dumps(res, indent=4))
    for vpc in res["Vpcs"]:
        if vpc["IsDefault"]:
            vpc_id = vpc["VpcId"]
            break
    if not vpc_id:
        client.create_default_vpc()

    res = client.describe_subnets(Filters=[{"Name": "vpc-id", "Values": [f"{vpc_id}", ], }, ])
    logger.debug("***** subnets" + json.dumps(res, indent=4))

    res = client.describe_security_groups(Filters=[{"Name": "vpc-id", "Values": [f"{vpc_id}", ], }, ])
    logger.debug("***** security groups" + json.dumps(res, indent=4))
    for sg in res["SecurityGroups"]:
        if sg["Description"] == "default VPC security group":
            gid = sg["GroupId"]
            try:
                res1 = client.authorize_security_group_ingress(
                    GroupId=f"{gid}",
                    IpPermissions=[
                        {
                            "FromPort": -1,
                            "IpProtocol": "-1",
                            "IpRanges": [
                                {
                                    "CidrIp": "0.0.0.0/0",
                                },
                            ],
                            "ToPort": -1,
                        },
                    ], )
                logger.debug(f"security group set {json.dumps(res1, indent=4)}")
            except Exception as e:
                logger.error(e)
                # this exception may occur if the sec group has been setup, need to fix it


def setup(config, num_of_replicas, num_of_loaders, ec2):
    replicas = []
    loaders = []
    tags = [("concord_perf", "replica") for x in range(0, num_of_replicas)]
    tags.extend([("concord_perf", "loader") for x in range(0, num_of_loaders)])

    logger.debug(f"setup, {tags}")

    key_name = config.get_ssh_key_name().split(".")[0]
    ami_id = config.get_ami_id()
    availability_zone = config.get_az_name()

    for i in range(0, len(tags)):
        res = ec2.create_instances(
            BlockDeviceMappings=[
                {
                    "DeviceName": "/dev/sda1",
                    "Ebs": {
                        "Encrypted": False,
                        "DeleteOnTermination": True,
                        "SnapshotId": "snap-0077d65e4e66c8a48",
                        "VolumeSize": 500,
                        "VolumeType": "gp2"
                    },
                },
            ],
            Placement=
                {
                    "AvailabilityZone": f"{availability_zone}"
                },
            ImageId=f"{ami_id}",
            InstanceType="i3.2xlarge",
            KeyName=f"{key_name}",
            DisableApiTermination=False,
            MaxCount=1,
            MinCount=1,
            InstanceInitiatedShutdownBehavior="stop",
            TagSpecifications=[
                {
                    "ResourceType": "instance",
                    "Tags": [
                        {
                            "Key": f"{tags[i][0]}",
                            "Value": f"{tags[i][1]}"
                        },
                    ]
                },
            ])
        if i < num_of_replicas:
            replicas.append(res[0].id)
        else:
            loaders.append(res[0].id)
    return replicas, loaders


def main():
    parser = argparse.ArgumentParser(
        description="Simple perf automation script",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument("-inMemory",
                        help="Use in-memory storage instead of rocksdb(default)",
                        action="store_true")
    parser.add_argument("-useBasicKVBC",
                        help="Use v1 KVBC instead of Merkle(default)",
                        action="store_true")
    parser.add_argument("-noBuildConcord",
                        help="Don't build concord container. Will deploy existing containers",
                        action="store_true")
    parser.add_argument("-noBuildLoader",
                        help="Don't build loader container. Will deploy existing containers",
                        action="store_true")
    parser.add_argument("-noPushConcord",
                        help="Don't push concord image (default False)",
                        action="store_true")
    parser.add_argument("-noPushLoader",
                        help="Don't push loader image (default False)",
                        action="store_true")
    parser.add_argument("-cleanRemoteFolder",
                        help="Clean all previous runs from the machines (default False)",
                        action="store_true")
    parser.add_argument("-copyAllData",
                        help="Copy rocksdb files and core dumps (if created) to the local machine "
                             "(default False, MAY TAKE TIME!!!)",
                        action="store_true")
    parser.add_argument("-useExistingInstances",
                        help="Use instances IDs from the config file (default False)",
                        action="store_true")
    parser.add_argument("-setupMachinesOnly",
                        help="Only setup instances (for debug purposes)(default False)",
                        action="store_true")
    parser.add_argument("-stopMachines",
                        help="Stop machines after the run (default False)",
                        action="store_true")
    parser.add_argument("-noRunTest",
                        help="Dont run the test (For debug) (default False)",
                        action="store_true")
    parser.add_argument("-logLevel",
                        help="Dont run the test (For debug) (default False)",
                        default="INFO")
    args = parser.parse_args()
    logger.setLevel(args.logLevel)

    config = load_configuration()
    credentials = config.get_credentials()
    session = get_aws_session(credentials)
    ec2 = session.resource("ec2", region_name=config.get_region_name())
    client = ec2.meta.client

    setup_infra(config, client)

    replica_ids = []
    loader_ids = []
    num_of_replicas = config.get_num_of_replicas()
    num_of_loaders = 1
    if args.useExistingInstances:
        replica_ids = config.get_replicas_ids()
        loader_ids = config.get_loaders_ids()
    else:
        replica_ids, loader_ids = get_instances(config, [{"Name": "tag-key", "Values": ["concord_perf", ]},
                                     {"Name": "availability-zone", "Values": [f"{config.get_az_name()}", ]}, ], client)
        n_o_r = num_of_replicas - len(replica_ids)
        n_o_l = num_of_loaders - len(loader_ids)
        print(n_o_r, n_o_l)
        if n_o_r + n_o_l > 0:
            reps, loads = setup(config, n_o_r, n_o_l, ec2)
            print(reps, loads)
            replica_ids.extend(reps)
            loader_ids.extend(loads)
        else:
            logger.info("got all instances")
    assert(len(replica_ids) >= num_of_replicas)
    assert(len(loader_ids) >= num_of_loaders)
    replica_ids = replica_ids[0:num_of_replicas]
    loader_ids = loader_ids[0:num_of_loaders]

    if args.setupMachinesOnly:
        return

    concord_root = config.get_concord_root()
    concord_image_name = config.get_concord_image_name()
    loader_image_name = config.get_loader_image_name()

    local_perf_result_folder = config.get_local_perf_results_root()
    remote_perf_result_folder = config.get_remote_perf_results_root()

    replica_fut = {}
    loaders_fut = {}
    build_fut = {}
    replica_instances = []
    loader_instances = []
    local_folder = None

    try:
        with concurrent.futures.ThreadPoolExecutor(5) as pool:
            replica_fut = {pool.submit(start_machine, machine_id, client, session, config): machine_id for machine_id in
                           replica_ids}
            loaders_fut = {pool.submit(start_machine, machine_id, client, session, config): machine_id for machine_id in
                           loader_ids}
            build_conc_fut = None
            if not args.noBuildConcord:
                docker_file = os.path.join(concord_root, "concord", "Dockerfile")
                build_conc_fut = pool.submit(build_image, concord_root, docker_file, concord_image_name)
            for future in concurrent.futures.as_completed(replica_fut):
                if not future.result()[0]:
                    raise_error(f'replica {replica_fut[future]} failed to start', pool)
                inst = future.result()[1]
                inst.set_concord_meta(replica_ids, len(replica_ids))
                replica_instances.append(inst)
            ids = replica_ids.copy()
            ids.extend(loader_ids)

            for future in concurrent.futures.as_completed(loaders_fut):
                if not future.result()[0]:
                    raise_error(f'loader {loaders_fut[future]} failed to start', pool)
                inst = future.result()[1]
                inst.set_concord_meta(ids, len(replica_ids))
                loader_instances.append(inst)
            logger.info("machines started...")
            if not args.noBuildConcord and not build_conc_fut.result():
                pool.shutdown(wait=False)
                raise_error('concord build failed')
            logger.info("concord image build finished...")

            if not args.noBuildLoader:
                docker_file = os.path.join(concord_root, "concord", "src", "performance", "deployment",
                                           "LoaderDockerFile")
                res = build_image(concord_root, docker_file, loader_image_name)
                if not res:
                    raise_error('loader build failed')
            logger.info("loader image build finished...")

            res, local_folder, conf_tar_name, timestamp = \
                create_local_env(config, args, replica_instances, loader_instances)
            instances = replica_instances.copy()
            instances.extend(loader_instances)
            remote_path = os.path.join(config.get_remote_perf_results_root(), timestamp)
            logger.info("local environment created...")

            fut = []
            for r in instances:
                r.start_ssh(config)

            with open(os.path.join(local_folder, "logs", "loader_output.txt"), "w") as text_file:
                for r in instances:
                    print(
                        f"instance: {inst.instance_id}, concord_id: {r.concord_id + 1}, "
                        f"private ip: {r.private_ip}, public_ip: {r.public_ip}",
                        file=text_file)
                    fut.append(
                        pool.submit(r.upload_file, local_folder, remote_path, conf_tar_name, args.cleanRemoteFolder,
                                    config))
            for future in concurrent.futures.as_completed(fut):
                if not future.result()[0]:
                    raise_error(f"upload configuration file to {future.result()[1]} failed", pool)
            logger.info("config TAR uploaded...")

            fut = []
            for r in instances:
                fut.append(pool.submit(r.prepare_remote_env, remote_path, conf_tar_name))
            for future in concurrent.futures.as_completed(fut):
                if not future.result()[0]:
                    raise_error(f"prepare remote environment on {future.result()[1]} failed", pool)
            logger.info("remote environment prepared...")

            fut = []
            res = do_docker_login(config)
            if not res:
                raise_error("docker login failed")

            if not args.noPushConcord:
                fut.append(pool.submit(upload_docker_images, concord_image_name, config, config.get_repo_tag_replica()))
            if not args.noPushLoader:
                fut.append(pool.submit(upload_docker_images, loader_image_name, config, config.get_repo_tag_loader()))
            for future in concurrent.futures.as_completed(fut):
                if not future.result()[0]:
                    raise_error(f"uploading concord container to {future.result()[1]} failed", pool)
            logger.info("containers ready...")

            fut = []
            for r in instances:
               fut.append(pool.submit(r.pull_image, config))
            for future in concurrent.futures.as_completed(fut):
                if not future.result()[0]:
                    raise_error(f"pulling {future.result()[2]} container at {future.result()[1]} failed", pool)

            for r in instances:
                res = r.run_post_deploy()
                if not res[0]:
                    raise_error(f"running post deploy task at {res[1]} failed")

            print(replica_instances, loader_instances)
            for r in replica_instances:
                res = r.start_replica(config.get_custom_command_run_interval_sec(),
                                      config.get_custom_command())
                if not res:
                    raise_error(f"failed to start replica concord{r.concord_id + 1} at {r.public_ip}")

            if not args.noRunTest:
                for r in loader_instances:
                    res = r.start_loader(config.get_loader_timeout_sec())
                    if not res[0]:
                        raise_error(f"failed to start replica concord{r.concord_id + 1} at {r.public_ip}", None)
                with open(os.path.join(local_folder, "logs", "loader_output.txt"), "a") as text_file:
                    print("*******************************", file=text_file)
                    print(res[1], file=text_file)
                    print(res[2], file=text_file)

                logger.info(f"Loader output:\n{res[1]}\n{res[2]}")

                fut = []
                for r in instances:
                    r.done = True
                    r.stop_containers()
                    fut.append(pool.submit(r.collect_results, local_folder, args.copyAllData, config))
                for future in concurrent.futures.as_completed(fut):
                    if not future.result()[0]:
                        raise_error(f"collecting results from {future.result()[1]} at {future.result()[2]} failed",
                                    pool)
    except Exception as e:
        logger.error(e)
        traceback.print_stack()
        if instances:
            for r in instances:
                if not local_folder:
                    local_folder = config.get_local_perf_results_root()
                r.collect_results(local_folder, args.copyAllData, config)

    if args.stopMachines:
        logger.info("stopping machines...")
        for id in replica_ids + loader_ids:
            stop_machine(id, client)
    logger.info(f"Done! Results folder {local_folder}")
    pool._threads.clear()
    concurrent.futures.thread._threads_queues.clear()

if __name__ == '__main__':
    main()