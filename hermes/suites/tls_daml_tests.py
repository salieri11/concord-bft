##########################################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
# These are daml tests with tls certificates .
# Test plan: https://confluence.eng.vmware.com/display/BLOC/TLS+for+Ledger+API+System+Test+Plan#TLSforLedgerAPISystemTestPlan-CertCreationProcess
##########################################################################################
import pytest
import util.helper as helper
import util.cert as cert
import time
import atexit
import collections

from fixtures.common_fixtures import fxBlockchain, fxConnection, fxInitializeOrgs, fxProduct
from suites.case import describe, passed, failed

import util.daml.daml_helper
import util.hermes_logging

log = util.hermes_logging.getMainLogger()

daml_sdk_path = None

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
    atexit.register(util.daml.daml_helper.clean_atexit)
    warning = None
    blockchain_type = fxHermesRunSettings["hermesCmdlineArgs"].blockchainType.lower()
    tls_certificates_flag = fxHermesRunSettings["hermesCmdlineArgs"].tlsEnabledClient
    if blockchain_type == helper.TYPE_DAML and tls_certificates_flag:
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

    for node_ip in fxLocalSetup.participant_nodes: 
        response, stdout = util.daml.daml_helper.daml_tls_setup(host=node_ip,path= path, parameter = "with_certificates")
        assert response == True, ("DAML TLS connection failed  Stdout: {}".format(response))


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
    path = cert.tlsInvalidCertificate("client", util.daml.daml_helper.Invalid_CERT)

    for node_ip in fxLocalSetup.participant_nodes:
        response, stdout = util.daml.daml_helper.daml_tls_setup(host=node_ip, path=path, parameter = "with_invalid_cert")
        assert "Handshake failed with fatal error" in response, ("DAML connection happened with invalid certificates  Stdout: {}".format(response))

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
        response, stdout = util.daml.daml_helper.daml_tls_setup(host=node_ip,parameter = "without_certificates")
        assert "GRPCIOBadStatusCode" in response, ("DAML connection happened without certificates  Stdout: {}".format(response))
