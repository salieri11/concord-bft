#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
from datetime import datetime, timedelta
from suites.case import describe
from collections import namedtuple
import pytest
import time
from util import helper, hermes_logging, blockchain_ops, wavefront, daml_regression_helper as dr_helper
from fixtures.common_fixtures import fxBlockchain, fxProduct
import json
from subprocess import check_output, CalledProcessError
from util.daml.daml_requests import simple_request

log = hermes_logging.getMainLogger()

LocalSetupFixture = namedtuple(
    "LocalSetupFixture", "client_hosts, concord_hosts")

DAML_LEDGER_API_PORT = '6865'

@pytest.fixture(scope="function")
@describe("fixture; local setup for given test suite")
def fxLocalSetup(request, reraise, fxHermesRunSettings, fxBlockchain, fxProduct):
    '''
    Function scoped fixture to enable all the test cases utilize common func.
    Args:
        fxHermesRunSettings: Hermes command line arguments (part of conftest.py so no explicit import).
        fxBlockchain: Blockchain fixture
        fxProduct: Product fixture
    Returns:
        client_hosts: Participant hosts
        concord_hosts: Concord (committer) replicas
    '''
    log.info("\n\nBlockchain fixture is {}".format(fxBlockchain))
    client_hosts, concord_hosts = dr_helper.format_hosts_structure(fxBlockchain.replicas)

    local_tuple = LocalSetupFixture(
        client_hosts=client_hosts, concord_hosts=concord_hosts)

    return local_tuple


def make_client_request(client_hosts, counter):
    '''
    Function to install sdk and perform daml transaction using helper utility
    '''
    for client_host in client_hosts:
        try:
            # Call function to install sdk only first time.
            if counter == 1:
                dr_helper.install_sdk_deploy_daml(client_host)
            url = dr_helper.get_daml_url(client_host)
            assert simple_request(url, 1, 0), dr_helper.PARTICIPANT_GENERIC_ERROR_MSG
        except Exception as excp:
            assert False, excp


@describe("verify that blockchain is connected to Wavefront")
def test_wavefront_smoke(fxBlockchain):
    '''
    Verify that Blockchain is connected to Wavefront
    - Call Wavefront API which returns success for valid blockchain.
    Args:
        fxBlockchain: blockchain fixture to get blockchain Id
    API Reference:
        https://vmware.wavefront.com/api-docs/ui
    '''
    blockchain_id = fxBlockchain.blockchainId
    assert blockchain_id, "Blockchain Id not found, can't proceed with this test"

    wf_url = 'https://vmware.wavefront.com/api/v2/source/{}'.format(
        blockchain_id)

    wavefront_cmd = ["curl", "-X", "GET",
                     "-H", "Accept: application/json",
                     "-H", "Content-Type: application/json",
                     "-H", "Authorization: Bearer {}".format(wavefront.wf_api_token()),
                     "--connect-timeout", "5",  # Connection timeout for each retry
                     "--max-time", "10",  # Wait time for each retry
                     "--retry", "5",  # Maximum retries
                     "--retry-delay", "0",  # Delay in next retry
                     "--retry-max-time", "60",  # Total time before this call is considered a failure
                     wf_url]

    str_output = check_output(wavefront_cmd).decode('utf8')
    try:
        json_output = json.loads(str_output)
    except (ValueError, json.decoder.JSONDecodeError, CalledProcessError) as e:
        log.error("Error in Smoke test is : [{}]".format(e))
        assert False, str_output

    assert "error" not in json_output.keys(), json_output["message"]
    assert json_output["status"]["code"] == 200 \
        or json_output["status"]["result"] == "OK", json_output["status"]["message"]


@describe("verify that blockchain is generating Wavefront metrics")
@pytest.mark.parametrize(
    ("counter", "metric_name", "operation", "filter"), [
        (1, "vmware.blockchain.concord.concordbft.receivedClientRequestMsgs.counter",
         None, "blockchain"),
        (2, "vmware.blockchain.concord.command.handler.operation.counters.total.counter",
         "daml_writes", "host")
    ])
def test_wavefront_metrics(fxLocalSetup, fxBlockchain, counter, metric_name, operation, filter):
    '''
    Verify that Blockchain is generating Wavefront metrics
    - Call Wavefront API which returns Client requests per second details.
    - Output contains data blocks.
    - Make a DAML client request
    - Call the API again to check if a data block was added.
    Args:
        fxLocalSetup: Local fixture for client hosts details.
        fxBlockchain: blockchain fixture to get blockchain Id
        counter: To ensure daml sdk install function is called only once.
        metric_name: Metric name to be tested
        operation: Metric operation
        filter: Different metrics uses different filter names, like host or blockchain for same field.
    API Reference:
        https://vmware.wavefront.com/api-docs/ui
    Note:
        This test should run standalone and not in parallel with any other test.
        A warning message has been added for display to notify the same.
    '''
    warncolor = "\033[1;33m"
    reset = "\033[0m"
    log.warning("\n\n{}".format(warncolor))
    log.warning("{}".format("*"*85))
    log.warning("** This test assumes that no other test is running in parallel for given blockchain")
    log.warning("** There could be wrong impact on test outcome otherwise")
    log.warning("{}{}\n\n".format("*"*85, reset))
    log.info("\nMetric name [{}] and Operation [{}]".format(metric_name, operation))

    blockchain_id = fxBlockchain.blockchainId
    assert blockchain_id, "Blockchain Id not found, can't proceed with this test"

    metric_query = "ts({}".format(metric_name)

    if operation is not None:
        metric_query = metric_query + ",operation={}".format(operation)

    metric_query = metric_query + ",{}={})".format(filter, blockchain_id)

    # Get start and end datetime in epoch
    # Time range is a crucial parameter, do not increase/decrease
    # without analyzing the API calls properly.
    start_epoch = (datetime.now() - timedelta(seconds=300)).strftime('%s')
    end_epoch = (datetime.now() + timedelta(seconds=120)).strftime('%s')
    log.info("Start time is {} and end time is {}".format(
        start_epoch, end_epoch))

    # Check Wavefront before Client request
    str_output = wavefront.call_wavefront_chart_api(
        metric_query, start_epoch, end_epoch)
    output = None

    try:
        output = json.loads(str_output)
        assert "warnings" not in output.keys(), output["warnings"]
        assert "errorType" not in output.keys(
        ), "{} - {}".format(output["errorType"], output["errorMessage"])
    except (ValueError, json.decoder.JSONDecodeError, CalledProcessError) as e:
        log.error("Error in metrics test is : [{}]".format(e))
        assert False, str_output

    before_data = output["timeseries"][0]["data"]
    log.info("\nMetric [{}] data before daml transaction is {} \n\n".format(
        metric_name, before_data))

    # Adding 10 seconds sleep so that generated transaction's epoch
    # does not overlap with above timeseries data epoch.
    time.sleep(10)

    # Make a client request
    make_client_request(fxLocalSetup.client_hosts, counter)

    time.sleep(30)
    # Check Wavefront after Client request
    str_output = wavefront.call_wavefront_chart_api(
        metric_query, start_epoch, end_epoch)
    try:
        output = json.loads(str_output)
        assert "warnings" not in output.keys(), output["warnings"]
        assert "errorType" not in output.keys(
        ), "{} - {}".format(output["errorType"], output["errorMessage"])
    except (ValueError, json.decoder.JSONDecodeError, CalledProcessError) as e:
        log.error("Error is : [{}]".format(e))
        assert False, str_output

    after_data = output["timeseries"][0]["data"]
    log.info(
        "\nMetric [{}] data after daml transaction is {} \n\n".format(metric_name, after_data))

    assert len(after_data) and len(after_data) > len(
        before_data), "Blockchain didn't generate metric for given transaction on Wavefront"
