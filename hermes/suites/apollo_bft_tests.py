# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential

# In order to run Apollo BFT tests we need a special docker-compose and concord
# configuration file.
#
# python main.py --runConcordConfigurationGeneration
#         --concordConfigurationInput /concord/config/dockerConfigurationInput-tee.yaml
#         --dockerComposeFile=../docker/docker-compose-tee.yml
#         ApolloBftTests
#
"""Test BFT protocol"""
import sys
import os
import importlib.util

# BEGIN - Fix conflicting 'util' module names between Apollo and Hermes
sys.modules['hermes_util'] = __import__('util')
apollo_util_spec = importlib.util.spec_from_file_location(
    name="util",
    location="../concord/submodules/concord-bft/tests/apollo/util/__init__.py")
apollo_util = importlib.util.module_from_spec(apollo_util_spec)
apollo_util_spec.loader.exec_module(apollo_util)
sys.modules['util'] = apollo_util
# END - Fix conflicting 'util' module names between Apollo and Hermes

sys.path.append(os.path.abspath("../concord/submodules/concord-bft/tests/apollo"))
sys.path.append(os.path.abspath("../concord/submodules/concord-bft/tests/apollo/util"))
sys.path.append(os.path.abspath("../concord/submodules/concord-bft/util/pyclient"))

import pytest

import trio
from bft import BftTestNetwork, TestConfig, with_trio
from bft_config import Replica
from test_skvbc import SkvbcTest
from test_skvbc_fast_path import SkvbcFastPathTest
from test_skvbc_slow_path import SkvbcSlowPathTest
from test_skvbc_view_change import SkvbcViewChangeTest
from test_skvbc_checkpoints import SkvbcCheckpointTest

from fixtures.common_fixtures import fxHermesRunSettings, fxProduct
from suites.case import describe
import hermes_util.helper as helper
from hermes_util.skvbc.concord_external_client import ExternalBftClient
from hermes_util.apollo_helper import with_timeout
from hermes_util.apollo_helper import start_replica_cmd
from hermes_util.apollo_helper import stop_replica_cmd
import hermes_util.hermes_logging as logging

log = logging.getMainLogger()

# Read by the fxProduct fixture.
productType = helper.TYPE_TEE


@pytest.fixture(scope="module")
@with_trio
async def bft_network():
    config = TestConfig(n=4,
                        f=1,
                        c=0,
                        num_clients=4,
                        key_file_prefix=None,
                        start_replica_cmd=start_replica_cmd,
                        stop_replica_cmd=stop_replica_cmd,
                        num_ro_replicas=0)

    replicas = [Replica(id=i, ip="127.0.0.1", port=3501 + i, metrics_port=4501 + i)
                for i in range(config.num_clients)]
    clients = config.num_clients * [ExternalBftClient()]

    bft_network = BftTestNetwork.existing(config, replicas, clients)

    return bft_network


@describe()
def test_skvbc_get_block_data(fxProduct, bft_network):
    trio.run(_test_skvbc_get_block_data, bft_network)


@with_timeout
async def _test_skvbc_get_block_data(bft_network):
    skvbc_test = SkvbcTest()
    log.info("Running SKVBC test_get_block_data...")
    await skvbc_test.test_get_block_data(
        bft_network=bft_network,
        already_in_trio=True
    )
    log.info("SKVBC test_get_block_data: OK.")


@describe()
def test_skvbc_fast_path(fxProduct, bft_network):
    trio.run(_test_skvbc_fast_path, bft_network)


@with_timeout
async def _test_skvbc_fast_path(bft_network):
    skvbc_fast_path_test = SkvbcFastPathTest()
    skvbc_fast_path_test.setUp()
    log.info("Running SKVBC (fast path only)...")
    await skvbc_fast_path_test.test_fast_path_only(
        bft_network=bft_network,
        already_in_trio=True,
        disable_linearizability_checks=True
    )
    log.info("SKVBC (fast path only): OK")


@describe()
def test_skvbc_slow_path(fxProduct, bft_network):
    trio.run(_test_skvbc_slow_path, bft_network)


@with_timeout
async def _test_skvbc_slow_path(bft_network):
    skvbc_slow_path_test = SkvbcSlowPathTest()
    skvbc_slow_path_test.setUp()
    log.info("Running SKVBC slow to fast path transition...")
    await skvbc_slow_path_test.test_slow_to_fast_path_transition(
        bft_network=bft_network,
        already_in_trio=True,
        disable_linearizability_checks=True
    )
    log.info("SKVBC slow to fast path transition: OK")


@describe()
def test_skvbc_checkpoint_creation(fxProduct, bft_network):
    trio.run(_test_skvbc_checkpoint_creation, bft_network)


@with_timeout
async def _test_skvbc_checkpoint_creation(bft_network):
    skvbc_test = SkvbcCheckpointTest()
    log.info("Running SKVBC checkpoint creation test...")
    await skvbc_test.test_checkpoint_creation(
        bft_network=bft_network,
        already_in_trio=True
    )
    log.info("SKVBC checkpoint creation: OK.")


@describe()
def test_skvbc_state_transfer(fxProduct, bft_network):
    trio.run(_test_skvbc_state_transfer, bft_network)


@with_timeout
async def _test_skvbc_state_transfer(bft_network):
    skvbc_test = SkvbcTest()
    log.info("Running SKVBC state transfer test...")
    await skvbc_test.test_state_transfer(
        bft_network=bft_network,
        already_in_trio=True
    )
    log.info("SKVBC state transfer test: OK.")


@describe()
def test_skvbc_view_change(fxProduct, bft_network):
    trio.run(_test_skvbc_view_change, bft_network)


@with_timeout
async def _test_skvbc_view_change(bft_network):
    skvbc_view_change_test = SkvbcViewChangeTest()
    log.info("Running SKVBC view change test...")
    await skvbc_view_change_test.test_single_vc_only_primary_down(
        bft_network=bft_network,
        already_in_trio=True,
        disable_linearizability_checks=True
    )
    log.info("SKVBC view change test: OK")
