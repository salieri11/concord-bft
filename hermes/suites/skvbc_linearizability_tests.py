# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential

# python main.py --runConcordConfigurationGeneration
#         --concordConfigurationInput /concord/config/dockerConfigurationInput-tee.yaml
#         --dockerComposeFile=../docker/docker-compose-tee.yml
#         SkvbcLinearizabiltyTests
#
"""Test SKVBC linearizability"""

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
from bft import with_trio

from test_skvbc_linearizability import SkvbcChaosTest

from fixtures.common_fixtures import fxProduct
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
@describe("fixture; bft_network")
@with_trio
async def bft_network():
    return await create_bft_network()

@describe()
def test_skvbc_linearizability(fxProduct, bft_network):
    trio.run(_test_skvbc_linearizability, bft_network)


@with_timeout
async def _test_skvbc_linearizability(bft_network):
    skvbc_chaos_test = SkvbcChaosTest()
    log.info("Running SKVBC linearizability test...")
    await skvbc_chaos_test.test_healthy(
        bft_network=bft_network,
        already_in_trio=True
    )
    log.info("SKVBC linearizability test: OK.")