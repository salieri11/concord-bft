# Copyright 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# main.py --runConcordConfigurationGeneration
#         --concordConfigurationInput /concord/config/dockerConfigurationInput-daml-nano.yaml
#         --dockerComposeFile=../docker/docker-compose-daml-nano.yml
#         ThinReplicaServerTests
#
# Or manual testing via grpcurl, e.g.:
# $ grpcurl -plaintext -d @ localhost:50051 com.vmware.concord.thin_replica.ThinReplica.ReadStateHash <<EOM
# { "block_id" : 1337 }
# EOM
#
# If thin replica server is not using TLS, set use_tls parameter to False,
# All tests by default would try to establish a secure channel with the thin replica server.

"""Test the Thin Replica Server API"""

import logging
from tempfile import NamedTemporaryFile
import subprocess
import os
import pytest
import itertools
from suites.case import describe

import util.daml.upload_dar as darutil
import util.daml.daml_helper as daml_helper
import util.helper as helper
from util.thin_replica.trutil import ThinReplica

from fixtures.common_fixtures import fxProduct

LOG = logging.getLogger("main")

# Read by the fxProduct fixture.
productType = helper.TYPE_DAML

@pytest.fixture(scope="module")
@describe("fixture; set up")
def setup_test_suite():
    """Setup function for the whole test suite

    Upload DAR file so each test has something to query.
    """
    # TODO why is only a single dar uploaded here, and why can the standard utility method
    # def upload_test_tool_dars(host='localhost', port='6861', verbose=True):
    # not be called to get dar names dynamically from the tool?
    TEST_DAR = "model-tests.dar"
    _, dars = daml_helper.download_ledger_api_test_tool("localhost")

    for dar in dars:
        if TEST_DAR in dar:
            dar_to_upload = dar
            break

    dar_uploaded = darutil.upload_dar(
        host="localhost", port="6861", darfile=dar_to_upload)
    assert dar_uploaded, "Failed to upload test DAR " + dar_to_upload
    return ThinReplica("localhost", "50051", "concord1", use_tls=True)

def get_newest_block_id(thin_replica_list):
    """Helper

    Return the newest Block ID that _ALL_ replicas have.

    This is super expensive because we iterate over _all_ blocks.
    Meaning, the longer the test runs the longer this function takes.
    """
    assert(type(thin_replica_list) is list)
    bids = []
    for tr in thin_replica_list:
      stream = tr.read_state()
      for update in stream:
        bid = update.block_id;
      bids.append(bid)
    return min(bids)


@describe()
def test_basic_read_state(fxProduct, setup_test_suite):
    """Basic read state
    We uploaded a DAR, hence we should be able to read the log entry
    fragment.
    """
    tr = setup_test_suite
    data_stream = tr.read_state()
    size = 0
    for resp in data_stream:
        size += len(resp.data)

    assert size >= 1


@describe()
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


@describe()
def test_no_state_for_filter(fxProduct, setup_test_suite):
    """Make sure we don't leak data.

    Filter with a key prefix that isn't on the blockchain should return no data.
    """
    tr = setup_test_suite
    size = 0
    for rsp in tr.read_state(key_prefix=b"WRITING_TESTS_IS_FUN"):
        size += len(rsp.data)
    assert size == 0


@describe()
def test_compare_all_hashes(fxProduct, setup_test_suite):
    """Compare hashes from all Concord nodes at the same block id
    """
    tr1 = setup_test_suite
    tr2 = ThinReplica("localhost", "50052", "concord2", use_tls=True)
    tr3 = ThinReplica("localhost", "50053", "concord3", use_tls=True)
    tr4 = ThinReplica("localhost", "50054", "concord4", use_tls=True)

    # Food-for-thought: Should we implement our "own" hash function so that we
    # can easily re-compute the hash in the test instead of relying on C++
    # stdlib's implementation of std::hash<string>?
    bid = get_newest_block_id([tr1, tr2, tr3, tr4])

    hash1 = tr1.read_hash(bid).hash
    hash2 = tr2.read_hash(bid).hash
    hash3 = tr3.read_hash(bid).hash
    hash4 = tr4.read_hash(bid).hash

    assert hash1 == hash2 == hash3 == hash4


@describe()
def test_hash_not_zero_if_no_state(fxProduct, setup_test_suite):
    """We don't compute a hash if we don't find state.

    The hash returned is 0.
    Food-for-thought: Let's hash the block id so it is never 0?
    """
    tr = setup_test_suite
    bid = get_newest_block_id([tr])
    hash = tr.read_hash(bid, key_prefix=b"WRITING_TESTS_IS_FUN").hash
    assert int(hash.hex(), 16) != 0


@describe()
def test_compare_all_filtered_hashes(fxProduct, setup_test_suite):
    """Compare hashes from all Concord nodes at the same block id
    """
    tr1 = setup_test_suite
    tr2 = ThinReplica("localhost", "50052", "concord2", use_tls=True)
    tr3 = ThinReplica("localhost", "50053", "concord3", use_tls=True)
    tr4 = ThinReplica("localhost", "50054", "concord4", use_tls=True)

    bid = get_newest_block_id([tr1, tr2, tr3, tr4])

    hash1 = tr1.read_hash(bid, b"daml").hash
    hash2 = tr2.read_hash(bid, b"daml").hash
    hash3 = tr3.read_hash(bid, b"daml").hash
    hash4 = tr4.read_hash(bid, b"daml").hash

    assert hash1 == hash2 == hash3 == hash4


@describe()
def test_compare_filter_with_no_filter_hash(fxProduct, setup_test_suite):
    """Hashes should be different for a filtered read and a non-filtered read
    """
    tr = setup_test_suite
    bid = get_newest_block_id([tr])

    assert tr.read_hash(bid).hash != tr.read_hash(bid, b"daml").hash


@describe()
def test_subscribe_history_to_future(fxProduct, setup_test_suite):
    """We should be able to continously receive updates

    Each update contains a block number and a list of key-value pairs.
    Internally, we are switching from reading from KVB to getting udpates from
    the commands handler directly.
    """
    tr = setup_test_suite
    block_id = 0

    stream = tr.subscribe_to_updates(block_id=1, key_prefix=b"")
    for update in itertools.islice(stream, get_newest_block_id([tr]) + 10):
        assert update.block_id == block_id + 1
        block_id += 1


@describe()
def test_basic_subscribe_to_update_hashes(fxProduct, setup_test_suite):
    """We should be able to continously retrieve hashes for produced updates
    """
    tr1 = setup_test_suite
    tr2 = ThinReplica("localhost", "50052", "concord2", use_tls=True)
    tr3 = ThinReplica("localhost", "50053", "concord3", use_tls=True)
    tr4 = ThinReplica("localhost", "50054", "concord4", use_tls=True)

    bid = get_newest_block_id([tr1, tr2, tr3, tr4])

    tr1_stream = tr1.subscribe_to_update_hashes(block_id=bid, key_prefix=b"")
    tr2_stream = tr2.subscribe_to_update_hashes(block_id=bid, key_prefix=b"")
    tr3_stream = tr3.subscribe_to_update_hashes(block_id=bid, key_prefix=b"")
    tr4_stream = tr4.subscribe_to_update_hashes(block_id=bid, key_prefix=b"")

    # Wait for 10 new blocks and then compare the hashes
    tr1_hashes = {x.block_id : x.hash for x in itertools.islice(tr1_stream, 10)}
    tr2_hashes = {x.block_id : x.hash for x in itertools.islice(tr2_stream, 10)}
    tr3_hashes = {x.block_id : x.hash for x in itertools.islice(tr3_stream, 10)}
    tr4_hashes = {x.block_id : x.hash for x in itertools.islice(tr4_stream, 10)}

    assert tr1_hashes == tr2_hashes == tr3_hashes == tr4_hashes
