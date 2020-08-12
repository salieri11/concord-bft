# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential

# In order to run the SKVBC pre-execution tests we need a special docker-compose and concord
# configuration file.
#
# python main.py --runConcordConfigurationGeneration
#         --concordConfigurationInput /concord/config/dockerConfigurationInput-tee.yaml
#         --dockerComposeFile=../docker/docker-compose-tee.yml
#         SkvbcPreexecutionTests
#
"""Test pre-execution with the SKVBC engine"""
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
from test_skvbc_preexecution import SkvbcPreExecutionTest

from fixtures.common_fixtures import fxHermesRunSettings, fxProduct
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
def test_skvbc_preexecution_concurrent(fxProduct, bft_network):
    trio.run(_test_skvbc_preexecution_concurrent, bft_network)


@with_timeout
async def _test_skvbc_preexecution_concurrent(bft_network):
    skvbc_preexecution_test = SkvbcPreExecutionTest()
    log.info("Running SKVBC concurrent pre-execution test...")
    await skvbc_preexecution_test.test_concurrent_pre_process_requests(
        bft_network=bft_network,
        already_in_trio=True
    )
    log.info("SKVBC concurrent pre-execution: OK.")

@describe()
def test_skvbc_preexecution_sequential(fxProduct, bft_network):
    trio.run(_test_skvbc_preexecution_sequential, bft_network)


@with_timeout
async def _test_skvbc_preexecution_sequential(bft_network):
    skvbc_test = SkvbcPreExecutionTest()
    log.info("Running SKVBC sequential pre-execution test...")
    await skvbc_test.test_sequential_pre_process_requests(
        bft_network=bft_network,
        already_in_trio=True,
        disable_linearizability_checks=True
    )
    log.info("SKVBC sequential pre-execution: OK.")


@describe()
def test_skvbc_preexecution_conflicting_requests(fxProduct, bft_network):
    trio.run(_test_skvbc_preexecution_conflicting_requests, bft_network)


@with_timeout
async def _test_skvbc_preexecution_conflicting_requests(bft_network):
    skvbc_preexecution_test = SkvbcPreExecutionTest()
    log.info("Running SKVBC pre-execution conflicts test...")
    await skvbc_preexecution_test.test_conflicting_requests(
        bft_network=bft_network,
        already_in_trio=True,
        disable_linearizability_checks=True
    )
    log.info("SKVBC pre-execution conflicts test: OK.")


@describe()
def test_skvbc_parallel_tx_after_f_nonprimary_crash(fxProduct, bft_network):
    trio.run(_test_skvbc_parallel_tx_after_f_nonprimary_crash, bft_network)


@with_timeout
async def _test_skvbc_parallel_tx_after_f_nonprimary_crash(bft_network):
    skvbc_preexecution_test = SkvbcPreExecutionTest()
    log.info("Running SKVBC parallel pre-executions test (f crashed replicas)...")
    await skvbc_preexecution_test.test_parallel_tx_after_f_nonprimary_crash(
        bft_network=bft_network,
        already_in_trio=True,
        disable_linearizability_checks=True
    )
    log.info("SKVBC parallel pre-executions test (f crashed replicas).")


@describe()
def test_skvbc_preexecution_view_change(fxProduct, bft_network):
    trio.run(_test_skvbc_preexecution_view_change, bft_network)


@with_timeout
async def _test_skvbc_preexecution_view_change(bft_network):
    skvbc_preexecution_test = SkvbcPreExecutionTest()
    log.info("Running SKVBC pre-execution with view change...")
    await skvbc_preexecution_test.test_view_change(
        bft_network=bft_network,
        already_in_trio=True,
        disable_linearizability_checks=True
    )
    log.info("SKVBC pre-execution with view change: OK.")
