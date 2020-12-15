#!/usr/bin/env python3
############################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential #
############################################################################
#
# Utility file to run blockbench on supplied participant node IPs
# This file is useful for automating the launch of blockbench in LRT(Long Running Test)
"""
Steps:
 Check if loadgen is running
 cd to docker folder in block_bench
 execute ./influxdb-setup.s
 Docker has to be running. 
 execute docker compose - 'docker-compose up -d' if blockbench is not running
 Url for Blockbench service  http://localhost:<port>
 return to working directory
 Start the load generation using Blockbench REST API
    http://localhost:8080/webjars/swagger-ui/index.html?configUrl=/v3/api-docs/swagger-config
    Check for 200 return, testid is also returned
 Setup a polling loop
    Use REST API to check progress
    If no progress - leave the loop
    else wait and continue
Bring down the app in case of successful execution
 Output the result
"""
import os
from subprocess import Popen, PIPE, check_output
from shutil import copyfile
import traceback
import uuid
import re
import requests
import json
from time import sleep
import copy

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
        elif return_status == 500:
            raise Exception('Progree return code - {}, Response - {}'.format(return_status, ret.text))
        else:
            # Done with loadgeneration
            response = ret.text
            # response = json.loads(ret.text)
            log.info('Return Status from progress API - {}\nResponse - {}'.format(return_status, response))
            return False, 0.0

    def submit_payload(self, start_data):
        """
        Start the test
        """
        start_api = '/orch/test/start'
        url = self.url + start_api
        log.info('Blockbench URL - {}'.format(url))
        spec_prettyprint = json.dumps(start_data, indent=2)
        log.info('Data for Blockbench start API - \n{}'.format(spec_prettyprint))
        ret = requests.post(url, data=json.dumps(start_data),
                            headers={'accept': '*/*',
                                     'Content-Type': 'application/json'}
                            )
        return_status = ret.status_code
        if return_status == 200:
            response = ret.text
            log.info('Return text from Start API - {}'.format(json.dumps(response, indent=4)))
            return True
        elif return_status == 500:
            raise Exception('Progree return code - {}, Response - {}'.format(return_status, ret.text))
        else:
            response = ret.text
            log.info('Return Status from start API - {}\nResponse - {}'.format(return_status, response))
            return False


def start_influxdb(repopath):
    """
    Start influxdb - Need to be done following .env update
    This is not called in pipeline. Useful for local testing
    :param repopath:
    :return: Path to the docker folder
    """
    currdir = os.getcwd()
    docker_path = repopath + os.sep + 'docker/'
    log.info('influxdb-setup: path - {}'.format(docker_path))
    os.chdir(docker_path)
    command = './influxdb-setup.sh'
    proc = Popen(command.encode(), stdout=PIPE, stderr=PIPE, shell=True)
    std_out, std_err = proc.communicate()
    exit_code = proc.returncode
    log.info('influxdb init: rc = {}\n, stdout = \n{}\n, stderr = {}'.
             format(exit_code, std_out.decode("utf-8"), std_err.decode("utf-8")))
    if exit_code != 0:
        raise Exception('Could not start influxdb, stdout - {}\n stderr - {}'.
                        format(std_out, std_err))
    os.chdir(currdir)


def check_if_loadgen_is_running():
    """
    Start the test
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


def start_loadgen(repopath):
    """
    Start the app.
    This is not called in pipeline. Useful for local testing
    :param repopath: Path to block-bench repo
    """
    docker_path = repopath + os.sep + 'docker/'
    currdir = os.getcwd()
    os.chdir(docker_path)
    stdout = check_output('docker-compose up -d', shell=True)
    log.info('Starting loadgen: docker-compose up -d output - {}'.format(stdout))
    os.chdir(currdir)
    # Check that loadgen is running
    # Wait for loadgen to be running. There seem to some delay before docker-compose
    # is effective
    retry_count = 3
    retry = True
    while retry:
        is_running = check_if_loadgen_is_running()
        if not is_running:
            if retry_count:
                log.info('Loadgen has not started yet. Will retry')
                sleep(5)
                retry_count -= 1
            else:
                raise Exception('Could not start loadgen')
        else:
            break
    log.info('Loadgen is running')


def stop_loadgen(repopath):
    """
    Stop the app.
    This is called at the end of suuccessful execution
    :param repopath: Path to blockbench repo
    """
    docker_path = repopath + os.sep + 'docker/'
    currdir = os.getcwd()
    os.chdir(docker_path)
    stdout = check_output('docker-compose down', shell=True)
    log.info('Stopping loadgen: docker-compose down output - {}'.format(stdout))
    os.chdir(currdir)


def get_loadgen_port():
    """
    Get the port on which loadgen is running
    :return: port number
    """
    # may have some code to get the port in the future
    return '8080'


def get_blockbench_progress(bc_obj):
    """
    Using blockbench REST API check if the test is still running.
    Compute completion rate
    :param bc_obj:
    :return: True - in progress Else Ended
    """
    result, completion_rate = bc_obj.get_progress_info()
    return result, completion_rate


def start_blockbench(bc_obj, spec):
    """
    Start blockbench and finish
    :param bc_obj: Blockbench REST APIs
    :param spec: Start API payload
    :return: result
    """
    try:
        return_value = bc_obj.submit_payload(spec)
        result = return_value
    except Exception as e:
        log.error('Exception - {}'.format(e))
        log.error('Traceback:\n{}'.format(traceback.format_exc()))
        result = False
    return result


def run_blockbench(bc_obj, spec):
    """
    Execute blockbench load generator and wait for finish
    :param bc_obj: Blockbench REST APIs
    :param spec: .json file that specifies loadgen payload
    :return: True - Successful False - unsuccessful
    """
    # in seconds
    wait_between_progress_checks = 60
    completion_percentage = 0
    result = start_blockbench(bc_obj, spec)
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


def prepare_for_blockbench_test(repo_path):
    """
    Get the port at which loadgen is running
    :return: url
    """
    res = check_if_loadgen_is_running()
    if res:
        port = get_loadgen_port()
        service_url = 'http://localhost:{}'.format(port)
        return service_url
    log.info('Loadgen Not running, Starting')
    # blockbench_repo_folder = get_blockbench_repo_path()
    log.info('Blockbench repo folder - {}'.format(repo_path))
    # Need to be done once initially, but ok to do subsequently
    # This is failing intermittently. Won't stop the test.
    try:
        start_influxdb(repo_path)
    except Exception as e:
        log.info('Influxdb did not start, Exception - {}'.format(e))
    start_loadgen(repo_path)
    port = get_loadgen_port()
    service_url = 'http://localhost:{}'.format(port)
    return service_url


def blockbench_main(args, ledger_api_hosts):
    """
    Main logic of Blockbench load generation
    :return: True if success, False otherwise
    """
    if args.blockbench_repo_path:
        spec = args.blockbench_spec
    elif args.blockbench_operation != 'getprogress':
        log.error('Blockbench spec json file  is not specified. Use --blockbenchSpec. Exiting.')
        return False
    if args.blockbench_repo_path:
        repo_path = args.blockbench_repo_path
    else:
        log.error('Blockbench repo path is not specified. Use --blockbenchRepoPath. Exiting.')
        return False
    # Payload for start rest API
    with open(spec, 'r') as payload_fd:
        start_data = json.load(payload_fd)
    # Add hosts to payload
    if ledger_api_hosts:
        # We are assuming that there is at least one sample element
        participant_element = start_data['participants'][0]
        # host's will be added to this list
        start_data['participants'] = []
        for host in ledger_api_hosts:
            log.info('Adding client IP {} to the spec'.format(host))
            new_host_element = copy.deepcopy(participant_element)
            new_host_element['ledger']['host'] = host
            start_data['participants'].append(new_host_element)
    # getprogress does not need client ip
    if args.blockbench_operation != 'getprogress' and not start_data['participants'][0]['ledger']['host']:
        log.error('Blockbench: Participant is not specified. Exiting')
        return False
    service_url = prepare_for_blockbench_test(repo_path)
    bc_obj = BlockbenchRestAPIs(service_url)
    if args.blockbench_operation == 'getprogress':
        result, completion_percentage = get_blockbench_progress(bc_obj)
        if result:
            log.info('Blockbench completion percentage - {}%'.format(completion_percentage))
    else:
        log.info('Getting the payload for starting blockbench from - {}'.format(spec))
        if args.blockbench_operation == 'start':
            result = start_blockbench(bc_obj, start_data)
        else:
            # Start and finish
            result, completion_percentage = run_blockbench(bc_obj, start_data)
            log.info('Blockbench Completion Percentage - {}'.format(completion_percentage))
    if result:
        stop_loadgen(repo_path)
    return result

