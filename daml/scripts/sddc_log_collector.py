#!/usr/bin/env nix-shell
#!nix-shell -i python3 -p python3 python3Packages.paramiko

# prerequisite: pip install paramiko
# usage: ./sddc_log_collector.py < /path/to/sddc/descriptor.json
# the JSON file describes the SDDC deployment and should look like the following
#
# [
#    {
#        "id": "<some_id>",
#        "consortium_id": "<some_id>",
#        "blockchain_type": "daml",
#        "nodes_list": [
#            {
#                "node_id": "<node_id>",
#                "private_ip": "<ip_address_reachable_from_vpn>",
#                "public_ip": "",
#                "username": "<username>",
#                "password": "<password>",
#                "node_type": "<node_type>",
#                "id": "<node_id>"
#            },
# ...
#
# instruction on how to spin up the SDDC are available here: https://digitalasset.atlassian.net/wiki/spaces/CONCORD/pages/1512145052/Testing+vDAML+in+SDDC

import json
import paramiko
import sys
import re
import traceback

def escape_ansi(line):
    ansi_escape = re.compile(r'(\x9B|\x1B\[)[0-?]*[ -\/]*[@-~]')
    return ansi_escape.sub('', line)

def collect_log_for(container_name, node_id):
    print(f'sddc_log_collector: retriving logs for container "{container_name}"')
    command = f'docker logs {container_name}'
    (stdin, stdout, stderr) = ssh_client.exec_command(command)
    print('sddc_log_collector: execution command dispatched')
    log_file_name = f'{container_name}-{node_id}.log'
    print(f'sddc_log_collector: writing logs to {log_file_name}...')
    with open(log_file_name, 'w') as log_file:
        for line in stdout.readlines():
            log_file.write(escape_ansi(line))
            print(f'sddc_log_collector: logs written to {log_file_name}')

ssh_client = paramiko.SSHClient()
ssh_client.load_system_host_keys()
ssh_client.set_missing_host_key_policy(paramiko.WarningPolicy())

sddc_descriptor = json.load(sys.stdin)

nodes = sddc_descriptor[0]['nodes_list']

docker_ps_command = 'docker ps --format "{{.Names}}"'

for node in nodes:
    node_id = node['node_id']
    private_ip = node['private_ip']
    username = node['username']
    password = node['password']
    print(f'sddc_log_collector: start processing node {node_id}')
    print(f'sddc_log_collector: {username}:{password}@{private_ip}:22 connecting...')
    try:
        ssh_client.connect(private_ip, 22, username, password, allow_agent = False, look_for_keys = False)
        print(f'sddc_log_collector: connected, listing containers...')
        (stdin, stdout, stderr) = ssh_client.exec_command(docker_ps_command)
        container_names = [raw_name.rstrip() for raw_name in stdout.readlines()]
        print(f'sddc_log_collector: currently active containers are {container_names}')
        for container_name in container_names:
            collect_log_for(container_name, node_id)
    except Exception as e:
        print(f'sddc_log_collector: caught exception: {e.__class__}: {e}')
        traceback.print_exc()
        pass
    finally:
        try:
            print(f'sddc_log_collector: {username}:{password}@{private_ip}:22 disconnecting...')
            ssh_client.close()
            print(f'sddc_log_collector: processing finished for node {node_id}')
        except:
            print(f'sddc_log_collector: fatal, cannot close client, exiting: {e.__class__}: {e}')
            sys.exit(1)
