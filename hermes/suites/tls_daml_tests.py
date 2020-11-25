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
import collections

from fixtures.common_fixtures import fxBlockchain, fxConnection, fxInitializeOrgs, fxProduct
from suites.case import describe, passed, failed

import util.daml.daml_helper
import util.hermes_logging
log = util.hermes_logging.getMainLogger()

daml_sdk_path = None

LocalSetupfixture = collections.namedtuple("LocalSetupfixture", ["flag", "participant_nodes", "warning"])

@pytest.fixture(scope="module")
def fxInstallDamlSdk(fxBlockchain):
    global daml_sdk_path
    participants, committers = util.helper.extract_ip_lists_from_fxBlockchain(fxBlockchain)
    log.info(participants)
    host = participants[0]
    log.info(host)
    daml_sdk_version = util.daml.daml_helper.get_ledger_api_version(host)
    daml_sdk_path = util.daml.daml_helper.install_daml_sdk(daml_sdk_version)


@pytest.fixture
@describe("fixture; Initial Setup")
def fxLocalSetup(fxHermesRunSettings, fxBlockchain, fxConnection):
    warning = None
    blockchain_type = fxHermesRunSettings["hermesCmdlineArgs"].blockchainType.lower()
    tls_certificates_flag = fxHermesRunSettings["hermesCmdlineArgs"].tlsEnabledClient
    skip_verify_test = fxHermesRunSettings["hermesCmdlineArgs"].skipDeploymentVerificationTest
    if blockchain_type == helper.TYPE_DAML and tls_certificates_flag and skip_verify_test:
        flag = True
    else:
        warning = ("blockchainType must be DAML and Tls flag must be enabled")
        flag = False

    participants, committers = util.helper.extract_ip_lists_from_fxBlockchain(fxBlockchain)
    log.info("Daml participant nodes ip{}".format(participants))
    return LocalSetupfixture(flag=flag, participant_nodes=participants,
                              warning=warning)


@describe()
@pytest.mark.smoke
def test_valid_certificates(fxConnection, fxBlockchain, fxLocalSetup, fxInstallDamlSdk):
    '''
     Verify with valid certificates
     '''
    log.info("execution started")
    if not fxLocalSetup.flag:
        log.info("test skipped")
        pytest.skip(fxLocalSetup.warning)
    log.info("get participants{}".format(fxLocalSetup.participant_nodes))
    for node_ip in fxLocalSetup.participant_nodes:
        cert_type = "client"
        log.info("Generating certificates for client with IP:{}".format(node_ip))
        cert.tlsCertificate(cert_type)
        path = cert.getTlsPath()+"/"
        log.info(path)
        helper.add_host_in_etc_hosts_file(node_ip, "server.ledgerapi.com")
        cmd = [path+"tls_daml_test_setup.sh", "server.ledgerapi.com", 6865, path]
        success, stdout = util.helper.execute_ext_command(cmd, timeout=3600, working_dir=path)
        assert success, ("Failed to build and install the request tool app.  Stdout: {}".format(stdout))
