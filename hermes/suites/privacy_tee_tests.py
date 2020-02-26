# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential

# In order to run Privacy TEE tests we need a special docker-compose and concord
# configuration file.
#
# python main.py --runConcordConfigurationGeneration
#         --concordConfigurationInput /concord/config/dockerConfigurationInput-tee.yaml
#         --dockerComposeFile=../docker/docker-compose-tee.yml
#         PrivacyTeeTests
#
# Or manual testing via grpcurl, e.g.:
#
# $ grpcurl -plaintext -d '{ "test_input" : "TestPrivacy" }' 127.0.0.1:50051 
# com.vmware.concord.tee.TeeService/RunTest
#
# $ grpcurl -plaintext -d "" -H "client_id:client_id_1" 127.0.0.1::50051 
# com.vmware.concord.thin_replica.ThinReplica.ReadState
#
"""Test Thin Replica Privacy"""

import logging
import subprocess
import os
import pytest
import itertools

import util.helper as helper
from util.tee.tutil import Tee

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
