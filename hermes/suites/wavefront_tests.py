#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
from datetime import datetime, timedelta
from suites.case import describe
from collections import namedtuple
import pytest
import time
import util.daml.daml_helper as daml_helper
from util import helper, hermes_logging, blockchain_ops, wavefront
from util.daml.daml_requests import simple_request
from fixtures.common_fixtures import fxBlockchain, fxProduct
import json
from pathlib import Path
import os
from subprocess import check_output, CalledProcessError
from itertools import zip_longest

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
    client_hosts, concord_hosts = format_hosts_structure(fxBlockchain.replicas)
    log.info("\nIn fxLocalSetup fixture, Participants are {}\nCommitters are {}\n".format(
        client_hosts, concord_hosts))

    local_tuple = LocalSetupFixture(
        client_hosts=client_hosts, concord_hosts=concord_hosts)

    return local_tuple


def format_hosts_structure(all_replicas):
    '''
    Currently the structure of replicas is different when blockchain is deployed 
    and when replicasConfig argument is provided.
    Once the utility functions are corrected to allow only single format, 
    this function would be removed.
    '''
    client_hosts = all_replicas["daml_participant"]
    concord_hosts = all_replicas["daml_committer"]

    client_hosts_list, concord_hosts_list = [], []

    # Parse through the participants and committers
    for client_host, concord_host in list(zip_longest(client_hosts, concord_hosts)):
        # Participant hosts
        if client_host:
            if (isinstance(client_host, dict)):
                client_host = client_host["private_ip"] if client_host[
                    "private_ip"] is not None else client_host["public_ip"]
            client_hosts_list.append(client_host)

        # Committer hosts
        if concord_host:
            if (isinstance(concord_host, dict)):
                concord_host = concord_host["private_ip"] \
                    if concord_host["private_ip"] is not None else concord_host["public_ip"]
            concord_hosts_list.append(concord_host)

    client_hosts = client_hosts_list if len(client_hosts_list) else client_hosts
    concord_hosts = concord_hosts_list if len(concord_hosts_list) else concord_hosts

    return client_hosts, concord_hosts


def get_port(client_host):
    client_port = '6861' if client_host == 'localhost' else DAML_LEDGER_API_PORT
    return client_port


def install_sdk_deploy_daml(client_host):
    '''
    Function to install DAML SDK and deploy the dar file
    on the client_host where Ledger API is running.
    Args:
        client_host: Host where ledger API is running
    Returns:
        None
    '''
    home = str(Path.home())
    daml_sdk_path = os.path.join(home, ".daml", "bin", "daml")
    cmd = [daml_sdk_path, "deploy", "--host",
           client_host, "--port", get_port(client_host)]
    party_project_dir = "util/daml/request_tool"
    success, output = helper.execute_ext_command(
        cmd, timeout=180, working_dir=party_project_dir, verbose=True)
    if "Party already exists" in output:
        assert False, "DAML Error: Party already exists."
    assert success, "DAML Error: Unable to deploy DAML app for one/more parties"


def make_client_request(client_hosts):
    '''
    Function to perform daml transaction
    Wait time is 20 sec to provide some window/range fetching block data from Wavefront
    '''
    for client_host in client_hosts:
        try:
            # Call function to install sdk only first time.
            install_sdk_deploy_daml(client_host)
            no_of_txns, wait_time = 1, 20
            url = 'http://{}:{}'.format(client_host, get_port(client_host))
            # Create & verify transactions of count no_of_txns
            assert simple_request(url, no_of_txns, wait_time), \
                "DAML request submission/verification failed"
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
        log.error("Error is : [{}]".format(e))
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
    log.warning(
        "{}".format("*"*85))
    log.warning(
        "** This test assumes that no other test is running in parallel for given blockchain")
    log.warning(
        "** There could be wrong impact on test outcome otherwise")
    log.warning(
        "{}{}\n\n".format("*"*85, reset))
    log.info("Running this test for \nMetric name [{}] and Operation [{}]".format(
        metric_name, operation))

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
    end_epoch = (datetime.now() + timedelta(seconds=60)).strftime('%s')
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
        log.error("Error is : [{}]".format(e))
        assert False, str_output

    before_data = output["timeseries"][0]["data"]
    log.info("\nData for metric [{}] before daml transaction is {} \n\n".format(
        metric_name, before_data))

    # Adding 10 seconds sleep so that generated transaction's epoch
    # does not overlap with above timeseries data epoch.
    time.sleep(10)

    # Make a client request
    make_client_request(fxLocalSetup.client_hosts)

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
        "\nMetric {} data after daml transaction is {} \n\n".format(metric_name, after_data))

    assert len(after_data) and len(after_data) > len(
        before_data), "Blockchain didn't generate metric for given transaction on Wavefront"
