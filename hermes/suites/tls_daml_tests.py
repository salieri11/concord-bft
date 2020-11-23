##########################################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
# These are system tests for pre-execution.
# Test plan: https://confluence.eng.vmware.com/display/BLOC/Pre-Execution+System+Test+Plan
##########################################################################################
import concurrent.futures
import json
import os
import pytest
import subprocess
import time
import util.daml.daml_requests
import util.helper as helper
import util.cert as cert
import util.wavefront
import yaml

from fixtures.common_fixtures import fxBlockchain, fxConnection, fxInitializeOrgs, fxProduct
from suites.case import describe, passed, failed

import util.daml.daml_helper
import util.hermes_logging
log = util.hermes_logging.getMainLogger()

daml_sdk_path = None

@pytest.fixture(scope="module")
def fxInstallDamlSdk(fxBlockchain):
    global daml_sdk_path
    participants, committers = util.helper.extract_ip_lists_from_fxBlockchain(fxBlockchain)
    host = participants[0]
    daml_sdk_version = util.daml.daml_helper.get_ledger_api_version(host)
    daml_sdk_path = util.daml.daml_helper.install_daml_sdk(daml_sdk_version)


@pytest.fixture
@describe("fixture; Initial Setup")
def fxLocalSetup(fxHermesRunSettings, fxBlockchain, fxConnection):
    warning = None
    blockchain_type = fxHermesRunSettings["hermesCmdlineArgs"].blockchainType.lower()
    tls_certificates_flag = fxHermesRunSettings["hermesCmdlineArgs"].tlsEnabledClient
    node_ip_list = []
    if blockchain_type == helper.TYPE_DAML and tls_certificates_flag:
        flag = True
    else:
        warning = ("blockchainType must be DAML and Tls flag must be enabled")
        flag = False
    all_participants = fxBlockchain.replicas[helper.TYPE_DAML_PARTICIPANT]
    for node in all_participants:
        ip = node['private_ip'] if node['public_ip'] is None else node['public_ip']
        node_ip_list.add(ip)


@describe()
@pytest.mark.smoke
@pytest.mark.blockchains
def test_valid_certificates(fxConnection, fxBlockchain, fxLocalSetup):
    '''
     Verify with valid certificates
     '''
    if not fxLocalSetup.flag:
        pytest.skip(fxLocalSetup.warning)

    for node_ip in fxLocalSetup.node_ip_list:
        cert_type = "client"
        log.info("Generating certificates for client with IP: ", node_ip)
        cert.tlsCertificate(cert_type)
        path = cert.getTlsPath()+"/"
        helper.add_host_in_etc_hosts_file(node_ip,cert_type+".ledgerapi.com")

        cmd = [path+"tls_daml_test_setup.sh" , node_ip, path]
        success, stdout = util.helper.execute_ext_command(cmd, timeout=3600, working_dir=path)
        assert success, ("Failed to build and install the request tool app.  Stdout: {}".format(stdout))
