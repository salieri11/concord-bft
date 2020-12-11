#!/usr/bin/env python3
############################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential #
############################################################################
#
# utility file to run blockbench on supplied participant node IP
# This file is useful for automating the launch of blockbench in LRT(Long Running Test)
"""
Steps:
 Some of these are skipped if running in Jenkins pipeline
 Find the folder where block_bench will be cloned. 
    Goal is to use a mechanism now and post Hermes repo creation.
 Clone block_bench repo if not available already
    git clone https://gitlab.eng.vmware.com/blockchain/block-bench.git
 cd to docker folder in block_bench
 Save .env file
 execute make-prebuilt-env.sh, collect output and replace .env file.
  Save .env prior to change
     ./make-prebuilt-env.sh
 execute ./influxdb-setup.s
 Docker has to be running. 
 execute docker compose - 'docker-compose up -d' if blockbench is not running
 Get the port from blockbench logs
 Url for Blockbench service  http://localhost:<port>
 return to working directory
 Start the load generation using Blockbench REST API
    Construct jsonfile
    http://localhost:8080/webjars/swagger-ui/index.html?configUrl=/v3/api-docs/swagger-config
    Check for 200 return, testid is also returned
 Setup a polling loop
    Use REST API to check progress
    If no progress - leave the loop
    else wait and continue
 Output the result
"""
import os
import sys
from subprocess import Popen, PIPE, check_output
from shutil import copyfile
import traceback
import uuid
import docker
import re
import requests
import json
from time import sleep

import util.hermes_logging
log = util.hermes_logging.getMainLogger()

class BlockbenchRestAPIs:
    """
    Wrappers for Blockbench REST API's
    """

    def __init__(self, url):
        """
        Setup the object
        :param url: REST service end point
        """
        self.url = url

    def get_progress_info(self):
        """
        This API indicates if the test is in progress
        :return: True - Test in progress, False - Not running
        """
        progress_api = '/progress/phase/current'
        url = self.url + progress_api
        log.info('Blockbench URL {}'.format(url))
        ret = requests.get(url, headers={'accept': '*/*'})
        return_status = ret.status_code
        if return_status == 200:
            response = json.loads(ret.text)
            log.info('Return text from progress API - {}'.format(json.dumps(response, indent=4)))
            if response['participants'][0]['workloadCount']:
                success_percentage = (response['participants'][0]['hops'][0]['completion'][
                                          'success'] +
                                      response['participants'][0]['hops'][0]['completion'][
                                          'failure'] +
                                      response['participants'][0]['hops'][0]['completion'][
                                          'skipped']) / response['participants'][0][
                    'workloadCount'] * 100
            else:
                success_percentage = 0.0
            return True, success_percentage
        else:
            # Done with loadgeneration
            response = ret.text
            # response = json.loads(ret.text)
            log.info('Return Status from progress API - {}\nResponse - {}'.format(return_status, response))
            return False, 0.0

    def start_loadgen(self, participant, name, desc, dapp, hopcount, upload_dar, workload_model,
                      workload_size, timeout, concurrency, connections):
        """
        Start the test
        """
        # Payload for the REST API
        # The json file is in a folder relative to this file - ../resources/
        payload_file = os.path.dirname(os.path.abspath(__file__)) + os.sep +\
            '..' + os.sep + 'resources' + os.sep + 'blockbench_start.json'
        payload_file_path = os.path.normpath(payload_file)
        log.info('Getting the payload for starting blockbench from - {}'.format(payload_file_path))
        with open(payload_file_path, 'r') as jsonfd:
            start_data = json.load(jsonfd)
        start_data['name'] = name
        start_data['description'] = desc
        start_data['dappPlan']['dapp'] = dapp
        start_data['dappPlan']['hopCount'] = hopcount
        start_data['dappPlan']['uploadDar'] = upload_dar
        start_data['workloadModel'] = workload_model
        # Count of trades
        start_data['phasePlans'][0]['workloadCount'] = workload_size
        start_data['phasePlans'][0]['workloadTimeoutSec'] = timeout
        start_data['phasePlans'][0]['loadFactor'] = concurrency
        start_data['scaleFactor'] = 0
        start_data['participants'][0]['ledger']['host'] = participant
        start_data['participants'][0]['ledger']['port'] = '6865'
        # Same as in DA Examples
        start_data['participants'][0]['party']['owner'] = 'Elis'
        start_data['participants'][0]['party']['newOwner'] = 'Bob'
        start_data['participants'][0]['connections'] = connections
        start_data['tlsId'] = 0
        start_api = '/orch/test/start'
        url = self.url + start_api
        log.info('Blockbench URL - {}'.format(url))
        log.info('Data for Blockbench start API - \n{}'.format(start_data))
        ret = requests.post(url, data=json.dumps(start_data),
                            headers={'accept': '*/*',
                                     'Content-Type': 'application/json'}
                            )
        return_status = ret.status_code
        if return_status == 200:
            response = ret.text
            log.info('Return text from Stsrt API - {}'.format(json.dumps(response, indent=4)))
            return True
        else:
            response = ret.text
            log.info('Return Status from start API - {}\nResponse - {}'.format(return_status, response))
            return False


def get_blockbench_repo_path():
    """
    Get the path to folder where blockbench repo exists or will exist.
     To be used in hermes folder or hermes repo
     Goal is that we could run this test now and when hermes repo is created.
    :return: Path to blockbench repo
    """
    current_location = os.path.normpath(os.path.abspath(__file__))
    folders_in_path = current_location.split(os.sep)
    # If there is no hermes in the path then there is some error
    # There are two possibilities.
    # (Now) hermes is under vmwathena_blockchain
    # (future) hermes is not under vmwathena_blockchain
    # Get the path to either vmwathena_blockchain repo or hermes repo
    if 'hermes' not in folders_in_path:
        raise Exception('hermes is not in the path to the file- {}'.format(__file__))
    if 'vmwathena_blockchain' in folders_in_path and \
            (folders_in_path.index('vmwathena_blockchain') < folders_in_path.index('hermes')):
        bc_index = folders_in_path.index('vmwathena_blockchain')
        blockbench_repo_path = os.sep.join(folders_in_path[:bc_index]) + os.sep
    elif 'vmwathena_blockchain' not in folders_in_path:
        hermes_index = folders_in_path.index('hermes')
        blockbench_repo_path = os.sep.join(folders_in_path[:hermes_index]) + os.sep
    else:
        raise Exception('Path to the file - {} does not seem valid'.format(__file__))
    return blockbench_repo_path


def clone_blockbench(destination):
    """
    Clone the repo if needed
    :param destination: Folder where block-bench will be cloned
    :return: Path to the repo
    """
    blockbench_repo_path = destination + os.sep + 'block-bench/'
    if os.path.isdir(blockbench_repo_path):
        log.info('block-bench repo exists in {}'.format(destination))
        return blockbench_repo_path
    currdir = os.getcwd()
    os.chdir(destination)
    # git clone https://gitlab.eng.vmware.com/blockchain/block-bench.git
    command = 'git clone https://gitlab.eng.vmware.com/blockchain/block-bench.git'
    proc = Popen(command.encode(), stdout=PIPE, stderr=PIPE, shell=True)
    std_out, std_err = proc.communicate()
    exit_code = proc.returncode
    log.info('Git clone block-bench: rc = {}, stdout = {}, stderr = {}'.format(exit_code, std_out, std_err))
    if exit_code != 0:
        raise Exception('Could not clone block-bench repo at {}, strout - {}\n stderr - {}'.
                        format(destination, std_out, std_err))
    else:
        log.info('Cloned the block-bench repo at {}'.format(destination))
    os.chdir(currdir)
    return blockbench_repo_path


def update_dot_env_file(repopath):
    """
    Update loadgen version number in .env file
    :param repopath:
    :return: Path to the docker folder
    """
    currdir = os.getcwd()
    docker_path = repopath + os.sep + 'docker/'
    os.chdir(docker_path)
    dot_env_file = docker_path + '.env'
    dot_env_file_saved = docker_path + '.env.{}'.format(str(uuid.uuid4()))
    copyfile(dot_env_file, dot_env_file_saved)
    log.info('Blockbench .env file saved in {}'.format(dot_env_file_saved))
    command = './make-prebuilt-env.sh'
    proc = Popen(command.encode(), stdout=PIPE, stderr=PIPE, shell=True)
    std_out, std_err = proc.communicate()
    exit_code = proc.returncode
    log.info('blockbench prebuild-env: rc = {}\n, stdout = \n{}\n, stderr = {}'.
             format(exit_code, std_out.decode("utf-8"), std_err.decode("utf-8")))
    if exit_code != 0:
        raise Exception('Could not get the new .env file at  {}, strout - {}\n stderr - {}'.
                        format(dot_env_file, std_out, std_err))
    else:
        # Copy stdout to .env
        with open(dot_env_file, 'wb') as envfd:
            envfd.write(std_out)
        log.info('Created new .env file for blockbench at  {}'.format(dot_env_file))
    os.chdir(currdir)
    return docker_path


def start_influxdb(repopath):
    """
    Start influxdb - Need to be done following .env update
    :param repopath:
    :return: Path to the docker folder
    """
    currdir = os.getcwd()
    docker_path = repopath + os.sep + 'docker/'
    os.chdir(docker_path)
    command = './influxdb-setup.sh'
    proc = Popen(command.encode(), stdout=PIPE, stderr=PIPE, shell=True)
    std_out, std_err = proc.communicate()
    exit_code = proc.returncode
    log.info('influxdb init: rc = {}\n, stdout = \n{}\n, stderr = {}'.format(exit_code, std_out.decode("utf-8"),
                                                              std_err.decode("utf-8")))
    if exit_code != 0:
        raise Exception('Could not start influxdb, stdout - {}\n stderr - {}'.
                        format(std_out, std_err))
    os.chdir(currdir)



def check_if_loadgen_is_running():
    """
    Start the test
    :param docker_path: Path to docker folder in block-repo
    :return: Port on which Blockbench is available
    """
    try:
        docker_ps_output = check_output('docker ps', shell=True)
    except Exception as e:
        raise Exception('Docker not running. Exception - {}'.format(e))
    # Check loadgen is already running
    log.info('docker ps output - \n{}'.format(docker_ps_output.decode('utf-8')))
    loadgen_pattern = r'\sloadgen'
    match = re.search(loadgen_pattern, docker_ps_output.decode('utf-8'),
                      flags=re.MULTILINE | re.IGNORECASE)
    if match:
        return True
    else:
        return False

def start_loadgen(docker_path):
    """
    Start the app.
    :param docker_path: Path to the docker folder in the repo
    """
    currdir = os.getcwd()
    os.chdir(docker_path)
    stdout = check_output('docker-compose up -d', shell=True)
    log.info('docker-compose up -d output - {}'.format(stdout))
    os.chdir(currdir)

def get_loadgen_port():
    """
    Get the port on which loadgen is running
    :return: port number
    """
    docker_client = docker.from_env()
    # Container gets created even if loadgen is not running
    loadgen_container = docker_client.containers.get('loadgen')
    log.info('Loadgen container - {}'.format(loadgen_container))
    container_state = loadgen_container.attrs['State']
    container_is_running = container_state['Status'] == 'running'
    if not container_is_running:
        raise Exception('Loadgen container is not running')
    log.info('loadgen is running')
    # Wait for the logs to be ready. There seem to some delay before docker-compose
    # and the availability of the logs
    retry_count = 3
    retry = True
    while retry:
        logfile = loadgen_container.logs()
        if not logfile:
            if retry_count:
                log.info('Failed to get logs for loadgen. Will retry')
                sleep(30)
                retry_count -= 1
            else:
                raise Exception('Could not get loadgen logs')
        else:
            break
    # Read the log to get the port
    log.info('loadgen log - {}'.format(logfile.decode("utf-8")))
    port_pattern = r'Netty started on port\(s\): (\d+)'
    # Default
    port = 8080
    successful_search = re.search(port_pattern, logfile.decode('utf-8'), re.MULTILINE)
    if successful_search:
        port = successful_search.group(1)
        log.info('Loadgen waiting on Port - {}'.format(port))
    return port


def get_blockbench_progress(bc_obj):
    """
    Using blockbench REST API check if the test is still running.
    Compute completion rate
    :param bc_obj:
    :return: True - in progress Else Ended
    """
    result, completion_rate = bc_obj.get_progress_info()
    return result, completion_rate


def start_blockbench(bc_obj, participant, name, desc, dapp, hopcount, upload_dar, workload_model,
                     workload_size, timeout, concurrency, connections):
    """
    Start blockbench and finish
    :param bc_obj: Blockbench REST APIs
    :param participant: IP
    :param name: Test Name
    :param desc: Test description
    :param dapp: DAML App to run
    :param hopcount: Number of hops
    :param upload_dar: Should upload DAR file?
    :param workload_model: OPEN/CLOSE
    :param workload_size: Blockbench parameter
    :return: result
    """
    try:
        return_value = bc_obj.start_loadgen(participant, name, desc, dapp, hopcount, upload_dar,
                                            workload_model, workload_size, timeout, concurrency,
                                            connections)
        result = return_value
    except Exception as e:
        log.error('Exception - {}'.format(e))
        log.error('Traceback:\n{}'.format(traceback.format_exc()))
        result = False
    return result


def run_blockbench(bc_obj, participant, name, desc, dapp, hopcount, upload_dar, workload_model,
                   workload_size, timeout, concurrency, connections):
    """
    Execute blockbench load generator
    :param bc_obj: Blockbench REST APIs
    :param participant: IP
    :param name: Test Name
    :param desc: Test description
    :param dapp: DAML App to run
    :param hopcount: Number of hops
    :param upload_dar: Should upload DAR file?
    :param workload_model: OPEN/CLOSE
    :param workload_size: Blockbench parameter
    :return: True - Successful False - unsuccessful
    """
    # in seconds
    wait_between_progress_checks = 60
    completion_percentage = 0
    result = start_blockbench(bc_obj, participant, name, desc, dapp, hopcount, upload_dar,
                              workload_model, workload_size, timeout, concurrency, connections)
    if result:
        wait_for_endoftest = True
        while wait_for_endoftest:
            (outcome, completion_percentage) = bc_obj.get_progress_info()
            if not outcome:
                wait_for_endoftest = False
            if wait_for_endoftest:
                sleep(wait_between_progress_checks)
        result = True
    return result, completion_percentage


def prepare_for_blockbench_test():
    """
    Get the port at which loadgen is running
    :return: url
    """
    res = check_if_loadgen_is_running()
    if res:
        port = get_loadgen_port()
        service_url = 'http://localhost:{}'.format(port)
        return service_url
    # Loadgen is not running
    if check_if_in_pipeline():
        log.info('Executing a pipeline in Jenkins')
        log.error('Loadgen is not running')
        raise Exception('Loadgen is not running')
    else:
        # Let us start loadgen
        log.info('Not executing in a Jenkins Pipeline')
        log.info('Not running loadgen, Starting')
        blockbench_repo_folder = get_blockbench_repo_path()
        log.info('Blockbench repo folder - {}'.format(blockbench_repo_folder))
        blockbench_repo_path = clone_blockbench(blockbench_repo_folder)
        docker_path = update_dot_env_file(blockbench_repo_path)
        start_influxdb(docker_path)
        start_loadgen(docker_path)
        port = get_loadgen_port()
        service_url = 'http://localhost:{}'.format(port)
        return service_url


def blockbench_main(args):
    """
    Main logic of loadgen
    :return:
    """
    # args = get_commandline_args()
    # Sanity checking
    if args.blockbench_operation != 'getprogress' and not args.blockbench_participant:
        log.error('Participant is not specified')
        return False
    service_url = prepare_for_blockbench_test()
    url = args.blockbench_url if args.blockbench_url else service_url
    if args.blockbench_operation == 'getprogress':
        bc_obj = BlockbenchRestAPIs(url)
        result, completion_percentage = get_blockbench_progress(bc_obj)
        if result:
            log.info('Blockbench completion percentage - {}%'.format(completion_percentage))
    else:
        participant = args.blockbench_participant
        name = args.blockbench_name
        desc = args.blockbench_desc
        dapp = args.blockbench_dapp
        hopcount = args.blockbench_hopcount
        upload_dar = args.blockbench_uploaddar
        workload_model = args.blockbench_workload_model
        workload_size = args.blockbench_workload_size
        timeout = args.blockbench_timeout
        concurrency = args.blockbench_concurrency
        connections = args.blockbench_connections
        bc_obj = BlockbenchRestAPIs(url)
        if args.blockbench_operation == 'start':
            result = start_blockbench(bc_obj, participant, name, desc, dapp, hopcount,
                                      upload_dar, workload_model, workload_size, timeout,
                                      concurrency, connections)
        else:
            # Start and finish
            result, completion_percentage = run_blockbench(bc_obj, participant, name, desc,
                                                           dapp, hopcount, upload_dar,
                                                           workload_model, workload_size,
                                                           timeout, concurrency, connections)
            log.info('Blockbench Completion Percentage - {}'.format(completion_percentage))
    return result

def check_if_in_pipeline():
    """
    Check if we are executing in a Jenkins pipeline.
    We have some actions that we don't perform if running on Jenkins
    eg. Clone a repo, docker-compose
    :return: True if under Jenkins False otherwise
    """
    # The json file is in a folder relative to this file - ../resources/
    user_config_file = os.path.dirname(os.path.abspath(__file__)) + os.sep + \
                   '..' + os.sep + 'resources' + os.sep + 'user_config.json'
    user_config_file_path = os.path.normpath(user_config_file)
    log.info('Checking if being executed in a pipeline from - {}'.format(user_config_file_path))
    with open(user_config_file_path, 'r') as jsonfd:
        start_data = json.load(jsonfd)
    name = start_data['metainf']['env']['name']
    if name.lower() == 'jenkins':
        return True
    else:
        return False
