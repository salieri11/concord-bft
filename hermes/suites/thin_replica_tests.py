# Copyright 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# main.py --runConcordConfigurationGeneration
#         --concordConfigurationInput /concord/config/dockerConfigurationInput-daml-nano.yaml
#         --dockerComposeFile=../docker/docker-compose-daml-nano.yml
#         ThinReplicaTests
#
# Or manual testing via grpcurl, e.g.:
# $ grpcurl -plaintext -d @ localhost:50051 com.vmware.concord.thin_replica.ThinReplica.ReadStateHash <<EOM
# { "block_id" : 1337 }
# EOM

"""Test the Thin Replica mechanism"""

import logging
from tempfile import NamedTemporaryFile
import subprocess
import os
import pytest
import itertools

import util.daml.upload_dar as darutil
import util.helper as helper
from util.thin_replica.trutil import ThinReplica

from fixtures.common_fixtures import fxHermesRunSettings, fxProduct

LOG = logging.getLogger(__name__)

# Read by the fxProduct fixture.
productType = helper.TYPE_DAML

@pytest.fixture(scope="module")
def setup_test_suite():
    """Setup function for the whole test suite

    Upload DAR file so each test has something to query.
    """
    TEST_DAR = "Test-stable.dar"
    TEST_TOOL_CONTAINER = "docker_daml_test_tool_1"

    tmp_dar = ""
    with NamedTemporaryFile(delete=False) as tmp:
        getDar = "docker cp {}:/{} {}".format(TEST_TOOL_CONTAINER, TEST_DAR, tmp.name)
        try:
            subprocess.check_call(getDar.split())
        except subprocess.CalledProcessError as e:
            msg = "Failed to copy DAR ({}): {}".format(TEST_DAR, e)
            msg += "\n{}".format(e.output)
            LOG.error(msg)
            raise
        tmp_dar = tmp.name

    dar_uploaded = darutil.upload_dar(
        host="localhost", port="6861", darfile=tmp_dar)
    assert dar_uploaded, "Failed to upload test DAR " + tmp_dar
    os.remove(tmp_dar)
    return ThinReplica("localhost", "50051")

def test_basic_read_state(fxProduct, setup_test_suite):
    """Basic read state

    We uploaded a DAR, hence there must be at least two kv pairs on the
    blockchain. One is the package, the other the transaction itself.
    """
    tr = setup_test_suite
    data_stream = tr.read_state()
    size = 0
    for resp in data_stream:
        size += len(resp.data)

    assert size >= 2

def test_read_state_key_prefix(fxProduct, setup_test_suite):
    """Make sure we can filter by key_prefix

    We know that there is at least one transaction and one package.
    Hence, we can filter by transaction and compare against no filter.
    """
    tr = setup_test_suite
    size_no_filter, size_filtered = 0, 0

    for rsp in tr.read_state():
        size_no_filter += len(rsp.data)
    for rsp in tr.read_state(key_prefix=b"daml"):
        size_filtered += len(rsp.data)

    assert size_no_filter > size_filtered

def test_no_state_for_filter(fxProduct, setup_test_suite):
    """Make sure we don't leak data.

    Filter with a key prefix that isn't on the blockchain should return no data.
    """
    tr = setup_test_suite
    size = 0
    for rsp in tr.read_state(key_prefix=b"WRITING_TESTS_IS_FUN"):
        size += len(rsp.data)
    assert size == 0

def test_compare_all_hashes(fxProduct, setup_test_suite):
    """Compare hashes from all Concord nodes at the same block id
    """
    tr1 = setup_test_suite
    tr2 = ThinReplica("localhost", "50052")
    tr3 = ThinReplica("localhost", "50053")
    tr4 = ThinReplica("localhost", "50054")

    # Food-for-thought: Should we implement our "own" hash function so that we
    # can easily re-compute the hash in the test instead of relying on C++
    # stdlib's implementation of std::hash<string>?
    data_stream = tr1.read_state()
    bid = data_stream.next().block_id
    data_stream.done()

    hash1 = tr1.read_hash(bid).hash
    hash2 = tr2.read_hash(bid).hash
    hash3 = tr3.read_hash(bid).hash
    hash4 = tr4.read_hash(bid).hash

    assert hash1 == hash2 == hash3 == hash4

def test_zero_hash_if_no_state(fxProduct, setup_test_suite):
    """We don't compute a hash if we don't find state.

    The hash returned is 0.
    Food-for-thought: Maybe we should return an error code?
    """
    tr = setup_test_suite
    data_stream = tr.read_state()
    bid = data_stream.next().block_id
    data_stream.done()
    hash = tr.read_hash(bid, key_prefix=b"WRITING_TESTS_IS_FUN").hash
    assert hash == b"\x00\x00\x00\x00\x00\x00\x00\x00"

def test_compare_all_filtered_hashes(fxProduct, setup_test_suite):
    """Compare hashes from all Concord nodes at the same block id
    """
    tr1 = setup_test_suite
    tr2 = ThinReplica("localhost", "50052")
    tr3 = ThinReplica("localhost", "50053")
    tr4 = ThinReplica("localhost", "50054")

    data_stream = tr1.read_state()
    bid = data_stream.next().block_id
    data_stream.done()

    hash1 = tr1.read_hash(bid, b"daml").hash
    hash2 = tr2.read_hash(bid, b"daml").hash
    hash3 = tr3.read_hash(bid, b"daml").hash
    hash4 = tr4.read_hash(bid, b"daml").hash

    assert hash1 == hash2 == hash3 == hash4

def test_compare_filter_with_no_filter_hash(fxProduct, setup_test_suite):
    """Hashes should be different for a filtered read and a non-filtered read
    """
    tr = setup_test_suite
    data_stream = tr.read_state()
    bid = data_stream.next().block_id
    data_stream.done()

    assert tr.read_hash(bid).hash != tr.read_hash(bid, b"daml").hash

def test_basic_subscribe_to_updates(fxProduct, setup_test_suite):
    """We should be able to continously receive updates

    Each update contains a block number and a list of key-value pairs.
    """
    tr = setup_test_suite

    # For now, nothing is filtered and hence we assume monotonically increasing
    # block numbers by 1. Also, the starting block number is ignored.
    block_id = 0

    stream = tr.subscribe_to_updates(block_id=1, key_prefix=b"")
    for update in itertools.islice(stream, 5):
        if block_id == 0:
            block_id = update.block_id
            continue

        assert update.block_id == block_id + 1
        block_id += 1

def test_basic_subscribe_to_update_hashes(fxProduct, setup_test_suite):
    """We should be able to continously retrieve hashes for produced updates
    """
    tr1 = setup_test_suite
    tr2 = ThinReplica("localhost", "50052")
    tr3 = ThinReplica("localhost", "50053")
    tr4 = ThinReplica("localhost", "50054")

    tr1_stream = tr1.subscribe_to_update_hashes(block_id=1, key_prefix=b"")
    tr2_stream = tr2.subscribe_to_update_hashes(block_id=1, key_prefix=b"")
    tr3_stream = tr3.subscribe_to_update_hashes(block_id=1, key_prefix=b"")
    tr4_stream = tr4.subscribe_to_update_hashes(block_id=1, key_prefix=b"")

    # Note: In the current implementation, we don't respect the block number.
    # Get the first 5 updates from thin replica 4 and get the hashes for the
    # same block id's from the other thin replicas as well. As with the
    # subscribe_to_updates test, we rely on the time services to produce new blocks
    tr4_hashes = {x.block_id : x.hash for x in itertools.islice(tr4_stream, 5)}
    tr3_hashes = {x.block_id : x.hash for x in itertools.islice(tr3_stream, 10) if x.block_id in tr4_hashes}
    tr2_hashes = {x.block_id : x.hash for x in itertools.islice(tr2_stream, 10) if x.block_id in tr4_hashes}
    tr1_hashes = {x.block_id : x.hash for x in itertools.islice(tr1_stream, 10) if x.block_id in tr4_hashes}

    assert tr1_hashes == tr2_hashes == tr3_hashes == tr4_hashes
