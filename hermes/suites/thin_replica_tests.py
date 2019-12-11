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

import util.daml.upload_dar as darutil
import util.helper as helper
from util.thin_replica.trutil import ThinReplica

from fixtures.common_fixtures import fxHermesRunSettings, fxProduct

LOG = logging.getLogger(__name__)

# Read by the fxProduct fixture.
productType = helper.TYPE_DAML

def test_read_state(fxProduct):
    """Query ReadState and compare all Concord's ReadStateHash output
    """
    # We use the test tool to upload one test DAR file only
    TEST_DAR = "Test-stable.dar"
    TEST_TOOL_CONTAINER = "docker_daml_test_tool_1"

    LOG.info("Copy {} to hermes...".format(TEST_DAR))
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
        LOG.info("Save %s to %s", TEST_DAR, tmp.name)
        tmp_dar = tmp.name

    LOG.info("Upload {} ...".format(TEST_DAR))
    dar_uploaded = darutil.upload_dar(
        host="localhost", port="6861", darfile=tmp_dar)
    assert dar_uploaded, "Failed to upload test DAR " + tmp_dar
    os.remove(tmp_dar)

    # DAML data should be on the blockchain at this point. Let's query.
    tr1 = ThinReplica("localhost", "50051")
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

    LOG.info("hash1 " + hash1.hex())
    LOG.info("hash2 " + hash2.hex())
    LOG.info("hash3 " + hash3.hex())
    LOG.info("hash4 " + hash4.hex())

    assert hash1 == hash2 == hash3 == hash4

    LOG.info("ReadState/ReadStateHash test runner passed.")
