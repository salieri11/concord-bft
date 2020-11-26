##########################################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
# These are system tests for pre-execution.
# Test plan: https://confluence.eng.vmware.com/display/BLOC/TLS+for+Ledger+API+System+Test+Plan#TLSforLedgerAPISystemTestPlan-CertCreationProcess
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
import time 

from fixtures.common_fixtures import fxBlockchain, fxConnection, fxInitializeOrgs, fxProduct
from suites.case import describe, passed, failed

import util.daml.daml_helper
import util.hermes_logging
log = util.hermes_logging.getMainLogger()

daml_sdk_path = None
SCRIPT_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                "..", "util", "daml", "request_tool")
SCRIPT_PATH  = os.path.join(SCRIPT_DIR,"daml_setup.sh")
hostname="server.ledgerapi.com"
port="6865"

LocalSetupfixture = collections.namedtuple("LocalSetupfixture", ["run_test", "participant_nodes", "warning"])

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
def fxLocalSetup(fxHermesRunSettings, fxBlockchain, fxConnection, fxInstallDamlSdk):
    warning = None
    blockchain_type = fxHermesRunSettings["hermesCmdlineArgs"].blockchainType.lower()
    tls_certificates_flag = fxHermesRunSettings["hermesCmdlineArgs"].tlsEnabledClient
    skip_verify_test = fxHermesRunSettings["hermesCmdlineArgs"].skipDeploymentVerificationTest
    os.chmod(SCRIPT_PATH, 0o777)
    if blockchain_type == helper.TYPE_DAML and tls_certificates_flag and skip_verify_test:
        run_test = True
    else:
        warning = ("blockchainType must be DAML and Tls flag must be enabled")
        run_test = False

    participants, committers = util.helper.extract_ip_lists_from_fxBlockchain(fxBlockchain)
    log.debug("Daml participant nodes ip{}".format(participants))
    time.sleep(50)
    return LocalSetupfixture(run_test=run_test, participant_nodes=participants,
                              warning=warning)


@describe()
@pytest.mark.smoke
def test_valid_certificates(fxLocalSetup):
    '''
     Verify with valid certificates
     '''
    if not fxLocalSetup.run_test:
        log.info("test skipped")
        pytest.skip(fxLocalSetup.warning)

    # Create certificate for client using same root-ca
    path = cert.tlsCreateCrt("client")
    time.sleep(50)

    for node_ip in fxLocalSetup.participant_nodes: 
        helper.add_host_in_etc_hosts_file(node_ip, hostname)
        cmd = [SCRIPT_PATH, hostname, port, path, "with_certificates"]
        success, stdout = util.helper.execute_ext_command(cmd, timeout=3600, working_dir=path)
        log.debug("\nSuccess and Output are {} and {}".format(success, stdout))
        assert success, ("DAML TLS connection failed  Stdout: {}".format(stdout))


@describe()
@pytest.mark.smoke
def test_invalid_certificates(fxLocalSetup):
    '''
     Verify with valid certificates
     '''
    if not fxLocalSetup.run_test:
        log.info("test skipped")
        pytest.skip(fxLocalSetup.warning)

    # create invalid certificate with new CA
    path = cert.tlsInvalidCertificate("client","invalid")
    time.sleep(50)

    for node_ip in fxLocalSetup.participant_nodes:
        helper.add_host_in_etc_hosts_file(node_ip, hostname)
        cmd = [SCRIPT_PATH, hostname, port, path, "with_invalid_cert"]
        success, stdout = util.helper.execute_ext_command(cmd, timeout=3600, working_dir=path)
        log.debug("\nSuccess and Output are {} and {}".format(success, stdout))
        assert "Handshake failed with fatal error" in stdout, ("DAML connection happened with invalid certificates  Stdout: {}".format(stdout))

@describe()
@pytest.mark.smoke
def test_no_certificates(fxLocalSetup):
    '''
     Verify with valid certificates
     '''
    if not fxLocalSetup.run_test:
        log.info("test skipped")
        pytest.skip(fxLocalSetup.warning)
    
    for node_ip in fxLocalSetup.participant_nodes:
        helper.add_host_in_etc_hosts_file(node_ip, hostname)
        cmd = [SCRIPT_PATH, hostname, port, SCRIPT_DIR, "without_certificates"]
        success, stdout = util.helper.execute_ext_command(cmd, timeout=3600, working_dir=SCRIPT_DIR)
        log.debug("\nSuccess and Output are {} and {}".format(success, stdout))
        assert "GRPCIOBadStatusCode" in stdout, ("DAML connection happened without certificates  Stdout: {}".format(stdout))
