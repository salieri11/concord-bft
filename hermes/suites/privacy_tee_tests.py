# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
# In order to run Privacy TEE tests we need a special docker-compose and concord
# configuration file.
#
# python main.py --runConcordConfigurationGeneration
#         --concordConfigurationInput /concord/config/dockerConfigurationInput-tee.yaml
#         --dockerComposeFile=../docker/docker-compose-tee.yml
#         PrivacyTeeTests
#
# Manual testing via grpcurl, e.g.:
#
# setup blockchain 
# $ gen-docker-concord-config.sh docker/config-public/dockerConfigurationInput-tee.yaml
# $ docker-compose -f -compose-tee.yml up -d
#
# concord-tee command to write test block data  
# $ grpcurl -plaintext -d '{ "test_input" : "PrivacySanityTest" }' 127.0.0.1:50051 com.vmware.concord.tee.TeeService/RunTest
#
# create thin replica client as "client_id_*" - should only see "key_all" 
# $ grpcurl -plaintext -d "" -H "client_id:client_id_*" -format text 127.0.0.1:50051 com.vmware.concord.thin_replica.ThinReplica.ReadState
# block_id: 1
# data: <
#  key: "key-all"
#  value: "value-all"
# >
#
"""Test Thin Replica Privacy"""

import logging
import subprocess
import os
import pytest
import itertools

import util.helper as helper
from util.tee.tutil import Tee
from util.thin_replica.trutil import ThinReplica

from fixtures.common_fixtures import fxHermesRunSettings, fxProduct

LOG = logging.getLogger(__name__)

# Read by the fxProduct fixture.
productType = helper.TYPE_TEE

@pytest.fixture(scope="module")
def setup_test_suite():
    """Setup function for the whole test suite
    """
    return Tee("localhost", "50051")

def get_test_output(tee):
    return tee.run_test().test_output

def test_basic_tee_service(fxProduct, setup_test_suite):
    """Basic TEE service 
    """
    tee = setup_test_suite
    output = tee.run_test("x").test_output

    assert "Test Execution Handler received input 'x'" == output

def test_privacy_sanity(fxProduct, setup_test_suite):
    """Sending PrivacySanityTest 
    Test Execution Egine will generate mock transaction
    {
      [
        {
          trids: ["client_id_1", "client_id_2", "client_id_3"],
          k: key-123
          v: value-123
        },
        {
          trids: ["client_id_1"],
          k: key-1
          v: value-1
        },
        {
          trids: ["client_id_2"],
          k: key-2
          v: value-2
        },
        {
          trids: ["client_id_1", "client_id_2"],
          k: key-12
          v: value-12
        },
        {
          trids: [],
          k: key-qll
          v: value-all
        }
      ]
    }
    """
    tee = setup_test_suite
    tee.run_test("PrivacySanityTest").test_output

    assert client_filter("1")
    assert client_filter("2")
    assert client_filter("3")
    assert client_filter("4")
    assert client_filter("5")

def client_filter(clientnum):
    """Check Client only receives approved data
    ex. client_id_3 expected results
    {
        block_id: 1
        data {
            key: "key-all"
            value: "value-all"
        }
        data {
            key: "key-123"
            value: "value-123"
        }
    }
    """
    client_id = "client_id_" + clientnum
    tr = ThinReplica("localhost", "50051",client_id)
    stream = tr.read_state()
    for result in stream:
        print(client_id , result)
        for kvp in result.data:
            if not ((clientnum.encode() in kvp.key) or (b"all" in kvp.key)):
                return False
    
    return True
