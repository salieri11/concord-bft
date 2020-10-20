# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential

# In order to run Apollo BFT tests we need a special docker-compose and concord
# configuration file.
#
# python main.py --runConcordConfigurationGeneration
#         --concordConfigurationInput /concord/config/dockerConfigurationInput-tee.yaml
#         --dockerComposeFile=../docker/docker-compose-tee.yml
#         SkvbcViewchangeTests
#
"""Test BFT viewchange protocol"""
import sys
import os
import importlib.util
from fixtures.common_fixtures import fxProduct
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
from bft import with_trio
from test_skvbc import SkvbcTest
from test_skvbc_fast_path import SkvbcFastPathTest
from test_skvbc_slow_path import SkvbcSlowPathTest
from test_skvbc_view_change import SkvbcViewChangeTest
from test_skvbc_checkpoints import SkvbcCheckpointTest


from suites.case import describe
import hermes_util.helper as helper
from hermes_util.apollo_helper import with_timeout
from hermes_util.apollo_helper import start_replica_cmd
from hermes_util.apollo_helper import stop_replica_cmd
from hermes_util.apollo_helper import create_bft_network
import hermes_util.hermes_logging as logging

log = logging.getMainLogger()

# Read by the fxProduct fixture.
productType = helper.TYPE_TEE


@pytest.fixture(scope="module")
@describe("fixture; bft network")
@with_trio
async def bft_network():
    return await create_bft_network()

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
        disable_linearizability_checks=True,
        exchange_keys=False
    )
    log.info("SKVBC view change test: OK")
