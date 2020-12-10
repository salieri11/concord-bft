# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential

# In order to run DAML tests we need a special docker-compose and concord
# configuration file.
#
# To run the test execute the following:
# python3 -m pytest suites/pruning_tests.py --runConcordConfigurationGeneration
#   --concordConfigurationInput /concord/config/dockerConfigurationInput-pruning.yaml
#   --dockerComposeFile ../docker/docker-compose-daml-nano.yml ../docker/docker-compose-operator.yml

import logging
import os
import pytest
import subprocess
import time
import traceback
import docker
import json
import util.helper as helper
import time
import util.daml.daml_helper as daml_helper
import util.docker_utils as hermes_docker_utils

from . import test_suite
from suites.case import describe
from fixtures.common_fixtures import fxProduct
import util.network_utils as network_utils

import util.hermes_logging

log = util.hermes_logging.getMainLogger()

# Read by the fxProduct fixture.
productType = helper.TYPE_DAML
MAX_TRIES_TO_PERFORM_AN_ACTION = 10

net_utils = network_utils.NetworkUtils()
operator_docker_utils = None

@describe()
def test_init(fxProduct, fxHermesRunSettings):
    global operator_docker_utils
    operator_docker_utils = hermes_docker_utils.DockerUtils(hermes_docker_utils.operator_containers)

def _try_to_perform_an_action(action, stop_condition):
    for i in range(MAX_TRIES_TO_PERFORM_AN_ACTION):
        res = action()
        if stop_condition(res):
            return res
        if i == MAX_TRIES_TO_PERFORM_AN_ACTION - 1:
            log.error("unable to perform reconfiguration actions.")
            assert False
        time.sleep(1)


def _get_latestPrunableBlock():
    cmd = "./concop prune latestPruneableBlock"
    response = operator_docker_utils.exec_cmd(id=0, cmd=cmd)
    if response.lower() == "none":
        return None
    latest_prunable_blocks_id = {}
    res = json.loads(response)
    for k in res:
        latest_prunable_blocks_id[k] = res[k]['block_id']
    return latest_prunable_blocks_id


def _execute_prune_request():
    cmd = "./concop prune execute"
    response = operator_docker_utils.exec_cmd(id=0, cmd=cmd)
    if response.lower() == "none":
        return None, -1
    res = json.loads(response)
    if res["succ"]:
        return True, res["pruned_block"]
    else:
        return False, -1


@describe()
def test_get_latestPruneableBlock(fxProduct, fxHermesRunSettings):
    latest_pruneable_blocks_a = _try_to_perform_an_action(_get_latestPrunableBlock, lambda _: _ is not None)
    time.sleep(1)
    latest_pruneable_blocks_b = _try_to_perform_an_action(_get_latestPrunableBlock, lambda _: _ is not None)
    for k in latest_pruneable_blocks_a:
        assert latest_pruneable_blocks_a[k] < latest_pruneable_blocks_b[k]


@describe()
def test_get_latestPruneableBlock_with_lagging_replica(fxProduct, fxHermesRunSettings):
    net_utils.isolate_target(hermes_docker_utils.concord_containers[3], hermes_docker_utils.concord_containers[:3])
    time.sleep(2)
    latest_pruneable_blocks = _try_to_perform_an_action(_get_latestPrunableBlock, lambda _: _ is not None)
    lagging_replica_latestPruneableBlock = min(latest_pruneable_blocks.values())
    num_of_bigger_ids = 0
    for k in latest_pruneable_blocks:
        if latest_pruneable_blocks[k] > lagging_replica_latestPruneableBlock:
            num_of_bigger_ids += 1
    assert num_of_bigger_ids == 3
    net_utils.flush_ip_tables(hermes_docker_utils.concord_containers[3])


@describe()
def test_prune_request_and_verify_pruned_blockid(fxProduct, fxHermesRunSettings):
    latest_pruneable_blocks = _try_to_perform_an_action(_get_latestPrunableBlock, lambda _: _ is not None)
    latest_pruneable_block = min(latest_pruneable_blocks.values())
    prune_res = _try_to_perform_an_action(_execute_prune_request, lambda _: _[0] is True)
    assert prune_res[0] is True
    # We found before the minimal pruneAbleBlock id, the actual pruned block has to be bigger (if the replicas have
    # made some progress in the meanwhile) or equal
    assert prune_res[1] >= latest_pruneable_block


@describe()
def test_pruning_with_lagging_replica(fxProduct, fxHermesRunSettings):
    net_utils.isolate_target(hermes_docker_utils.concord_containers[3], hermes_docker_utils.concord_containers[:3])
    time.sleep(2)
    latest_pruneable_blocks = _try_to_perform_an_action(_get_latestPrunableBlock, lambda _: _ is not None)
    highest_prunable_block = max(latest_pruneable_blocks.values())
    min_pruneable_block = min(latest_pruneable_blocks.values())
    prune_res = _try_to_perform_an_action(_execute_prune_request, lambda _: _[0] is True)
    assert prune_res[0] is True
    # We expect the actual pruned block to be less than the highest pruneable block we found before
    assert prune_res[1] < highest_prunable_block
    assert prune_res[1] == min_pruneable_block
    net_utils.flush_ip_tables(hermes_docker_utils.concord_containers[3])
