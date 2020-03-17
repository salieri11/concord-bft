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
from test_skvbc_view_change import SkvbcViewChangeTest

from fixtures.common_fixtures import fxHermesRunSettings, fxProduct
import hermes_util.helper as helper
from hermes_util.skvbc.concord_skvbc_client import RotatingSkvbcClient
import hermes_util.hermes_logging as logging

log = logging.getMainLogger()

# Read by the fxProduct fixture.
productType = helper.TYPE_TEE


def start_replica_cmd(builddir, replica_id):
    log.info(f"Starting replica #{replica_id}")
    return ["docker", "start", f"docker_concord{replica_id + 1}_1"]


def stop_replica_cmd(replica_id):
    log.info(f"Stopping replica #{replica_id}")
    return ["docker", "kill", f"docker_concord{replica_id + 1}_1"]


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
    concord_api_ports = [50051 + i for i in range(config.n)]
    clients = config.num_clients * [RotatingSkvbcClient(*concord_api_ports)]

    bft_network = BftTestNetwork.existing(config, replicas, clients)

    return bft_network


def test_skvbc_get_block_data(fxProduct, bft_network):
    trio.run(_test_skvbc_get_block_data, bft_network)


async def _test_skvbc_get_block_data(bft_network):
    skvbc_test = SkvbcTest()
    log.info("Running SKVBC test_get_block_data...")
    await skvbc_test.test_get_block_data(
        bft_network=bft_network,
        already_in_trio=True
    )
    log.info("SKVBC test_get_block_data: OK.")


def test_skvbc_fast_path(fxProduct, bft_network):
    trio.run(_test_skvbc_fast_path, bft_network)


async def _test_skvbc_fast_path(bft_network):
    skvbc_fast_path_test = SkvbcFastPathTest()
    skvbc_fast_path_test.setUp()
    log.info("Running SKVBC read-your-writes (fast path)...")
    await skvbc_fast_path_test.test_fast_path_read_your_write(
        bft_network=bft_network,
        already_in_trio=True,
        disable_linearizability_checks=True
    )
    log.info("SKVBC read-your-writes (fast path): OK")


def test_skvbc_view_change(fxProduct, bft_network):
    trio.run(_test_skvbc_view_change, bft_network)


async def _test_skvbc_view_change(bft_network):
    skvbc_view_change_test = SkvbcViewChangeTest()
    log.info("Running SKVBC view change test...")
    await skvbc_view_change_test.test_single_vc_only_primary_down(
        bft_network=bft_network,
        already_in_trio=True,
        disable_linearizability_checks=True
    )
    log.info("SKVBC view change test: OK")
