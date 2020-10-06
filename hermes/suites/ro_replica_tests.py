# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential

"""Test RO replica"""
import sys
import os
import subprocess
import importlib.util
import time
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


from suites.case import describe
import hermes_util.helper as helper
from hermes_util.apollo_helper import with_timeout
from hermes_util.apollo_helper import start_replica_cmd
from hermes_util.apollo_helper import stop_replica_cmd
from hermes_util.apollo_helper import create_bft_network
import hermes_util.hermes_logging as logging

from util import skvbc as kvbc

log = logging.getMainLogger()

# Read by the fxProduct fixture.
productType = helper.TYPE_TEE

@pytest.fixture(scope="module")
@describe("fixture; bft network")
@with_trio
async def bft_network():
    return await create_bft_network(num_ro_replicas=1)


class MinioContainer:
    """
    This is an abstraction for the S3 service. Uses docker commands
    to provide convenient methods for starting and stopping minio
    """
    def start(self):
        subprocess.run(
            ["docker", "start", "docker_minio_1"],
            check=True
        )

    def stop(self):
        subprocess.run(
            ["docker", "kill", "docker_minio_1"],
            check=True
        )

    def remove_bucket(self):
        subprocess.run(
            ["docker", "exec", "docker_minio_1", "rm", "-rf", "/data/blockchain"],
            check=True
        )

    def create_bucket(self):
        subprocess.run(
            ["docker", "exec", "docker_minio_1", "mkdir", "-p", "/data/blockchain"],
            check=True
        )

    def cleanup(self):
        pass

async def _wait_for_st(bft_network, ro_replica_id, lastExecutedSeqNumThreshold):
    """
    ro_replica_id - id of the replica to be queried.
    lastExecutedSeqNumThreshold - threshold value for the lastExecutedSeqNum.

    Queries the specified replica about its lastExecutedSeqNum metric and checks if
    it is equal or bigger to the treshold specified. 
    Silently returns on success.
    On failure error is cause by trio.fail_after, which causes the unit test to fail.
    """
    with trio.fail_after(seconds=70):
        while True:
            with trio.move_on_after(seconds=.5):
                try:
                    key = ['replica', 'Gauges', 'lastExecutedSeqNum']
                    lastExecutedSeqNum = await bft_network.metrics.get(ro_replica_id, *key)
                except KeyError:
                    continue
                else:
                    # success!
                    if lastExecutedSeqNum >= lastExecutedSeqNumThreshold:
                        print("Replica" + str(ro_replica_id) + " : lastExecutedSeqNum:" + str(lastExecutedSeqNum))
                        break

@describe()
def test_ro_replica_happy_case(fxProduct, bft_network):
    trio.run(_test_ro_replica_happy_case, bft_network)

@with_timeout
async def _test_ro_replica_happy_case(bft_network):
    """
    This test represents the happy case for the RO replica. 
    A checkpoint is generated and then the test waits for the
    RO replica to sync it.
    """
    skvbc = kvbc.SimpleKVBCProtocol(bft_network)
    ro_replica_id = bft_network.config.n

    print("Generating checkpoint")
    await skvbc.fill_and_wait_for_checkpoint(
        initial_nodes=bft_network.all_replicas(),
        num_of_checkpoints_to_add=1,
        verify_checkpoint_persistency=False,
        assert_state_transfer_not_started=False
    )

    print(f"Verify state transfer for replica {ro_replica_id}")
    await _wait_for_st(bft_network, ro_replica_id, 150)

@pytest.mark.skip(reason="currently this won't pass")
@describe()
def test_ro_replica_no_obj_store(fxProduct, bft_network):
   trio.run(_test_ro_replica_no_obj_store, bft_network)

@with_timeout
async def _test_ro_replica_no_obj_store(bft_network):
    """
    Tests the behaviour of the RO replica when the S3 service is
    not available.
    Initially RO and S3 are stopped. Then a checkpoint is generated 
    and after some time - the S3 service.
    The purpose of the test is to verify that the RO replica constantly
    tries sto establish connection to the S3 service.
    """
    skvbc = kvbc.SimpleKVBCProtocol(bft_network)
    ro_replica_id = bft_network.config.n

    s3 = MinioContainer()

    # curr_checkpoint = await bft_network.wait_for_checkpoint(1)
    # ro_checkpoint = await bft_network.wait_for_checkpoint(ro_replica_id)

    # assert curr_checkpoint == ro_checkpoint

    print("Stop RO and S3")
    bft_network.stop_replica(ro_replica_id)
    s3.stop()

    print("Generating checkpoint")
    await skvbc.fill_and_wait_for_checkpoint(
        initial_nodes=bft_network.all_replicas(),
        num_of_checkpoints_to_add=1,
        verify_checkpoint_persistency=False,
        assert_state_transfer_not_started=False
    )

    key = ['replica', 'Gauges', 'lastStableSeqNum']
    target_seq_num = await bft_network.metrics.get(1, *key)
    print(f"New lastStableSeqNum is {target_seq_num}")

    print("Start RO replica")
    bft_network.start_replica(ro_replica_id)
    time.sleep(3)

    print("Start S3")
    s3.start()

    print("Verify state transfer")
    await _wait_for_st(bft_network, ro_replica_id, target_seq_num)

@pytest.mark.skip(reason="currently this won't pass")
@describe()
def test_ro_replica_flaky_obj_store(fxProduct, bft_network):
   trio.run(_test_ro_replica_flaky_obj_store, bft_network)

@with_timeout
async def _test_ro_replica_flaky_obj_store(bft_network):
    """
    The test simulates flaky S3 service.
    RO replica is intially stopped and checkpoint is generated.
    Then RO replica is started again and the S3 service is being
    stopped for short periods of time.
    The purpose of the test is to verify that the RO replica
    syncs state despite the unstable behaviour of S3.
    """
    skvbc = kvbc.SimpleKVBCProtocol(bft_network)
    ro_replica_id = bft_network.config.n

    s3 = MinioContainer()

    # curr_checkpoint = await bft_network.wait_for_checkpoint(1)
    # ro_checkpoint = await bft_network.wait_for_checkpoint(ro_replica_id)

    # assert curr_checkpoint == ro_checkpoint

    print("Stop RO")
    bft_network.stop_replica(ro_replica_id)

    print("Generating checkpoint")
    await skvbc.fill_and_wait_for_checkpoint(
        initial_nodes=bft_network.all_replicas(),
        num_of_checkpoints_to_add=2,
        verify_checkpoint_persistency=False,
        assert_state_transfer_not_started=False
    )

    key = ['replica', 'Gauges', 'lastStableSeqNum']
    target_seq_num = await bft_network.metrics.get(1, *key)
    print(f"New lastStableSeqNum is {target_seq_num}")

    print("Start RO replica")
    bft_network.start_replica(ro_replica_id)
    
    # simulate flaky s3 
    for _ in range(5):
        s3.stop()
        time.sleep(1)
        s3.start()
        time.sleep(1)

    print("Verify state transfer")
    await _wait_for_st(bft_network, ro_replica_id, target_seq_num)

@pytest.mark.skip(reason="currently this won't pass")
@describe()
def test_ro_replica_not_enough_replies(fxProduct, bft_network):
   trio.run(_test_ro_replica_not_enough_replies, bft_network)

@with_timeout
async def _test_ro_replica_not_enough_replies(bft_network):
    """
    The test verifies that the RO replica doesn't fetch blocks
    if there are not enough honest replicas. Check the comments
    in the test for implementation details.
    """
    skvbc = kvbc.SimpleKVBCProtocol(bft_network)
    ro_replica_id = bft_network.config.n
    
    print("Stop RO")
    bft_network.stop_replica(ro_replica_id)

    print("Generating checkpoint")
    await skvbc.fill_and_wait_for_checkpoint(
        initial_nodes=bft_network.all_replicas(),
        num_of_checkpoints_to_add=1,
        verify_checkpoint_persistency=False,
        assert_state_transfer_not_started=False
    )

    key = ['replica', 'Gauges', 'lastStableSeqNum']
    target_seq_num = await bft_network.metrics.get(1, *key)
    print(f"New lastStableSeqNum is {target_seq_num}")

    # Leave only f replicas alive
    # The purpose is to leave f alive replicas and not to kill primary
    print("Leave only f replicas alive")
    primary = await bft_network.get_current_primary()
    replicas_to_stop = bft_network.all_replicas(without={primary})[0:bft_network.config.f-1]
    bft_network.stop_replicas(replicas_to_stop)

    print("Start RO replica")
    bft_network.start_replica(ro_replica_id)
    
    time.sleep(5)

    # State is not fetched, because there are not enough replcias
    with trio.move_on_after(seconds=.5):
        try:
            key = ['replica', 'Gauges', 'lastExecutedSeqNum']
            lastExecutedSeqNum = await bft_network.metrics.get(ro_replica_id, *key)
        except KeyError:
            pass
        else:
            assert(lastExecutedSeqNum < target_seq_num) 

    print("Start all stopped replicas")
    bft_network.start_replicas(replicas_to_stop)

    print("Verify state transfer")
    await _wait_for_st(bft_network, ro_replica_id, target_seq_num)

@pytest.mark.skip(reason="currently this won't pass")
@describe
def test_ro_replica_missing_bucket(fxProduct, bft_network):
    trio.run(_test_ro_replica_missing_bucket, bft_network)

@with_timeout
async def _test_ro_replica_missing_bucket(bft_network):
    """
    This test is used to simulate a bug - the RO replica
    crashes if the S3 bucket doesn't exist.
    https://jira.eng.vmware.com/browse/BC-4616
    """
    skvbc = kvbc.SimpleKVBCProtocol(bft_network)
    s3 = MinioContainer()
    ro_replica_id = bft_network.config.n

    print("Stop RO")
    bft_network.stop_replica(ro_replica_id)

    print("Remove bucket")
    s3.remove_bucket()

    print("Start RO")
    bft_network.start_replica(ro_replica_id)

    print("Generating checkpoint")
    await skvbc.fill_and_wait_for_checkpoint(
        initial_nodes=bft_network.all_replicas(),
        num_of_checkpoints_to_add=1,
        verify_checkpoint_persistency=False,
        assert_state_transfer_not_started=False
    )

    print(f"Verify state transfer for replica {ro_replica_id}")
    await _wait_for_st(bft_network, ro_replica_id, 150)

@pytest.mark.skip(reason="currently this won't pass")
@describe()
def test_ro_replica_restart(fxProduct, bft_network):
    trio.run(_test_ro_replica_restart, bft_network)

@with_timeout
async def _test_ro_replica_restart(bft_network):
    """
    This test is used to simulate a bug - the RO replica
    crashes if it restarted after some state is fetched.
    https://jira.eng.vmware.com/browse/BC-4617
    """
    skvbc = kvbc.SimpleKVBCProtocol(bft_network)
    ro_replica_id = bft_network.config.n

    print("Generating checkpoint")
    await skvbc.fill_and_wait_for_checkpoint(
        initial_nodes=bft_network.all_replicas(),
        num_of_checkpoints_to_add=1,
        verify_checkpoint_persistency=False,
        assert_state_transfer_not_started=False
    )

    print(f"Verify state transfer for replica {ro_replica_id}")
    await _wait_for_st(bft_network, ro_replica_id, 150)

    # Restart RO replica
    print("Restart RO")
    bft_network.stop_replica(ro_replica_id)
    bft_network.start_replica(ro_replica_id)

    print("Generating checkpoint")
    await skvbc.fill_and_wait_for_checkpoint(
        initial_nodes=bft_network.all_replicas(),
        num_of_checkpoints_to_add=1,
        verify_checkpoint_persistency=False,
        assert_state_transfer_not_started=False
    )

    key = ['replica', 'Gauges', 'lastStableSeqNum']
    target_seq_num = await bft_network.metrics.get(1, *key)

    print(f"Verify state transfer for replica {ro_replica_id}")
    await _wait_for_st(bft_network, ro_replica_id, target_seq_num)