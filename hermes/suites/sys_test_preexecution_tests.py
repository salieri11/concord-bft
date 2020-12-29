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
import util.helper
import util.wavefront
import yaml

from fixtures.common_fixtures import fxBlockchain, fxConnection, fxInitializeOrgs, fxProduct
from suites.case import describe, passed, failed

import util.daml.daml_helper
import util.hermes_logging
log = util.hermes_logging.getMainLogger()

# 32 is a good starting point.
fibo_max = 30
fibo_absolute_max = 32

daml_sdk_path = None

REQUEST_TOOL_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                "..", "util", "daml", "request_tool")
REQUEST_TOOL_SETUP = os.path.join(REQUEST_TOOL_DIR, "setup.sh")
LEDGER_PORT = 6865

@pytest.fixture(scope="module")
def fxInstallDamlSdk(fxBlockchain):
    global daml_sdk_path
    participants, committers = util.helper.extract_ip_lists_from_fxBlockchain(fxBlockchain)
    host = participants[0]
    daml_sdk_version = util.daml.daml_helper.get_ledger_api_version(host)
    daml_sdk_path = util.daml.daml_helper.install_daml_sdk(daml_sdk_version)


@pytest.fixture(scope="module")
def fxAppSetup(fxBlockchain):
    participants, committers = util.helper.extract_ip_lists_from_fxBlockchain(fxBlockchain)
    host = participants[0]
    cmd = [REQUEST_TOOL_SETUP, host]
    success, stdout = util.helper.execute_ext_command(cmd, timeout=3600, working_dir=REQUEST_TOOL_DIR)

    if not success:
        log.error("Failed to build and install the request tool app.  Stdout: {}".format(stdout))


@pytest.fixture(scope="module")
def fxFiboMax(fxBlockchain):
    '''
    The maximum number to pass to the fibonacci app can vary based on the performance
    of the blockchain systems.  This function starts at 32 (determined by experience)
    and figures out what the maximum value can be without triggering a timeout
    from the leder api server.
    Sets the global max value.  Returns nothing.
    '''
    global fibo_max
    new_max = fibo_max
    success = True
    successes = 0
    failures = 0
    participants, committers = util.helper.extract_ip_lists_from_fxBlockchain(fxBlockchain)
    host = participants[0]

    while True:
        success, output = run_fibo(host, num_fib_values=new_max, test_mode=False)

        if success:
            successes += 1

            if failures > 0:
                # It failed last time, but not last time.
                # That means have been decrementing and found one that is low enough.
                log.info("Breaking, new_max is {}".format(new_max))
                break
            else:
                # It is either:
                #   1. our first try or
                #   2. we have been seeing only successes, which means
                #      we have been incrementing from the initial value.
                # Either way, keep incrementing and checking.
                if new_max < fibo_absolute_max:
                    new_max += 1
                    log.info("new_max has been incremented to {}".format(new_max))
                else:
                    break
        else:
            failures += 1
            new_max -= 1
            log.info("new_max has been decremented to {}".format(new_max))

            if new_max <= 1:
                raise Exception("The Fibonacci test app cannot run, so tests cannot proceed.")

            if successes > 0 and "outside of range" in output:
                # It worked last time, not this time. That means we went too high.
                # We have already decremented; just save that value.
                log.info("Breaking, new_max is {}".format(new_max))
                break

    fibo_max = new_max


def run_fibo(host, port=LEDGER_PORT, iterations=1, num_fib_values=1, test_mode=True):
    '''
    Runs the fibonacci app on the DAML ledger.
    host/port: The host and port of the ledger api server.
    num_fib_values: How many fibonacci numbers to find.
    iterations: How many times to run it.
    WARNING: This must remain able to run in multiple threads or subprocesses.
    '''
    input_file = os.path.join(REQUEST_TOOL_DIR, "{}.txt".format(num_fib_values))
    cmd = [daml_sdk_path, "script", "--dar", ".daml/dist/ledgerclient-0.0.1.dar", "--script-name", "Fibo:run"]
    cmd.extend(["--ledger-host", host, "--ledger-port", str(port)])
    cmd.extend(["--input-file", input_file])

    if not os.path.exists(input_file):
        with open(input_file, "w") as f:
            f.write(str(num_fib_values))

    for i in range(0, iterations):
        # Note that the app can hang in some cases, so keep the timeout.
        log.debug("Running fibo app on host {} with value {}".format(host, num_fib_values))
        success, stdout = util.helper.execute_ext_command(cmd, timeout=120, working_dir=REQUEST_TOOL_DIR)
        log.debug("Fibonacci app iteration {} success: {}, stdout: {}".format(i, success, stdout))

        if test_mode:
            msg = "Running simple fibonacci with value '{}' failed".format(num_fib_values)
            assert success, msg
        else:
            return success, stdout


def run_request_tool(host, port=LEDGER_PORT, iterations=1):
    '''
    Runs the more realistic DAML request tool.
    host/port: The host and port of the ledger api server.
    iterations: How many times to run it.
    WARNING: This must remain able to run in multiple threads or subprocesses.
    '''
    cmd = [daml_sdk_path, "script", "--dar", ".daml/dist/ledgerclient-0.0.1.dar", "--script-name", "AssetCheck:asset_check"]
    cmd.extend(["--ledger-host", host, "--ledger-port", str(port)])

    for i in range(0, iterations):
        log.debug("Running request tool on host {}".format(host))
        success, stdout = util.helper.execute_ext_command(cmd, timeout=240, working_dir=REQUEST_TOOL_DIR)
        log.debug("Request tool app iteration {} stdout: {}".format(i, stdout))
        assert success, "Running request tool app failed"


def get_primary_ip(fxBlockchain, committers):
    '''
    fxBlockchain: Blockchain tuple of (blockchainId, consortiumId, replicas, clientNodes).
    committers: List of committers.
    Returns the IP of the primary.
    '''
    primary_rid = util.blockchain_ops.get_primary_rid(fxBlockchain) 
    arbitrary_committer_ip = committers[0]
    username, password = util.helper.getNodeCredentials(fxBlockchain.blockchainId, arbitrary_committer_ip)
    success = False
    msg = None
    e = None
    output = None

    output = util.helper.ssh_connect(arbitrary_committer_ip,
                                             username, password,
                                             "cat /config/concord/config-local/deployment.config",
                                             verbose=False)
    config = yaml.load(output, Loader=yaml.Loader)

    for item in config["node"]:
        if item["replica"][0]["principal_id"] == primary_rid:
            primary_ip = item["replica"][0]["replica_host"]

            if "localhost" in primary_ip or "127.0.0.1" in primary_ip:
                # The one we retrieved the config from *is* the primary.
                primary_ip = arbitrary_committer_ip

            return primary_ip

    raise("Could not find ip for replica '{}' in: '{}'".format(primary_rid, config))


def start_tx_failure_ok(blockchain_id, ledger):
    '''
    Start a tx on the given ledger and do not mind if it fails.
    Launched in a separate thread.
    Blockchain Id is required here to get strong password.
    '''
    username, password = util.helper.getNodeCredentials(blockchain_id, ledger) 

    try:
        timestamp = util.blockchain_ops.get_docker_timestamp(ledger, username, password, "daml_ledger_api")
        log.info("Starting fibo_max tx at ledger time {}".format(timestamp))
        run_fibo(ledger, num_fib_values=fibo_max)
    except Exception as e:
        timestamp = util.blockchain_ops.get_docker_timestamp(ledger, username, password, "daml_ledger_api")
        log.info("Fibo max failed at ledger time {}, but that is ok.".format(timestamp))


def sleep_and_kill(blockchain_id, sleep_time, committer, ledger):
    '''
    Sleep a while, then kill the given committer.
    Launched in a separate thread.
    Blockchain Id is required here to get strong password.
    '''
    log.info("Sleeping for {} seconds and then killing the primary, {}".format(sleep_time, committer))
    time.sleep(sleep_time)
    c_username, c_password = util.helper.getNodeCredentials(blockchain_id, committer)
    l_username, l_password = util.helper.getNodeCredentials(blockchain_id, ledger)
    c_timestamp = util.blockchain_ops.get_docker_timestamp(committer, c_username, c_password, "concord")
    l_timestamp = util.blockchain_ops.get_docker_timestamp(ledger, l_username, l_password, "daml_ledger_api")
    log.info("Stopping concord at concord time {} and ledger time {}".format(c_timestamp, l_timestamp))
    util.blockchain_ops.stop_services(committer, c_username, c_password, ["concord"])


@pytest.fixture
def fxSpiderDar(fxHermesRunSettings):
    dar_file = "spider-modules.dar"
    spider_tag = "1.33.98"
    spider_image_and_tag = util.daml.daml_helper.DA_SPIDER_IMAGE + ":" + spider_tag

    if not os.path.isfile(dar_file):
        # The dar isn't here.  Is the image present?
        log.info("Checking to see if the spider docker image is present.")
        cmd = ["docker", "images", "--format", "{{.Repository}}:{{.Tag}}"]
        status, output = util.helper.execute_ext_command(cmd)
        log.info(output)

        if not (status and spider_image_and_tag in output):
            # Looks like we need to fetch it.
            log.info("Fetching the spider docker image")
            username = fxHermesRunSettings["hermesCmdlineArgs"].dockerHubUser
            password = fxHermesRunSettings["hermesCmdlineArgs"].dockerHubPassword
            util.daml.daml_helper.download_spider_app(username, password, spider_tag)

        # Now extract the file.
        with open("spider-modules.dar", "w") as f:
            log.info("Extracting dar from spider docker image")
            cmd = ["docker", "run", "--rm", "--entrypoint", "cat", spider_image_and_tag, "/home/dlt/dar/{}".format(dar_file)]
            completedProcess = subprocess.run(cmd, stdout=f,
                                              stderr=subprocess.STDOUT,
                                              timeout=120)

    return dar_file


@describe("Send multiple very simple transactions.")
@pytest.mark.smoke
def test_simple_txs(fxBlockchain, fxInstallDamlSdk, fxAppSetup, fxFiboMax):
    '''
    Send a few short transactions, then a few long transactions.
    '''
    participants, committers = util.helper.extract_ip_lists_from_fxBlockchain(fxBlockchain)
    host = participants[0]
    run_fibo(host, iterations=5)
    run_fibo(host, iterations=5, num_fib_values=fibo_max)


@describe("Run a more realistic scenario.")
@pytest.mark.smoke
def test_realistic_txs(fxBlockchain, fxInstallDamlSdk, fxAppSetup):
    '''
    Run a more realistic scenario.
    python3 daml_requests.py --url https://10.72.230.8:6865 scenario --complex 1
    '''
    participants, committers = util.helper.extract_ip_lists_from_fxBlockchain(fxBlockchain)
    host = participants[0]
    run_request_tool(host, iterations=5)


@describe("Send one slow tx while running multiple realistic scenarios.")
@pytest.mark.smoke
def test_parallel_short_long_txs(fxBlockchain, fxInstallDamlSdk, fxAppSetup, fxFiboMax):
    '''
    Start the 30 second fibo transaction, then do a few iterations of the request tool.
    '''
    participants, committers = util.helper.extract_ip_lists_from_fxBlockchain(fxBlockchain)
    host = participants[0]
    futures = []
    num_slow_procs = 1
    num_realistic_procs = 5
    max_workers = num_slow_procs + num_realistic_procs

    with concurrent.futures.ProcessPoolExecutor(max_workers = max_workers) as executor:
        for _ in range(0, num_slow_procs):
            # Running in parallel can slow things down, so do not use fibo_max.
            future = executor.submit(run_fibo, host, iterations=1, num_fib_values=fibo_max-2)
            futures.append(future)
        for _ in range(0, num_realistic_procs):
            future = executor.submit(run_request_tool, host, iterations=3)
            futures.append(future)

    # This will trigger failures due to Pytest asserts.
    for f in futures:
        f.result()


@describe("Send only multiple realistic scenarios in parallel.")
@pytest.mark.smoke
def test_parallel_short_txs(fxBlockchain, fxInstallDamlSdk, fxAppSetup, fxFiboMax):
    '''
    Launch several iterations of the request tool, in parallel.
    '''
    participants, committers = util.helper.extract_ip_lists_from_fxBlockchain(fxBlockchain)
    host = participants[0]
    futures = []
    num_procs = 5

    with concurrent.futures.ProcessPoolExecutor(max_workers=num_procs) as executor:
        for _ in range(0, num_procs):
            future = executor.submit(run_request_tool, host, iterations=3)
            futures.append(future)

    # This will trigger failures due to Pytest asserts.
    for f in futures:
        f.result()


@describe("Send only multiple long transactions in parallel.")
@pytest.mark.smoke
def test_parallel_long_txs(fxBlockchain, fxInstallDamlSdk, fxAppSetup, fxFiboMax):
    '''
    Note that multiple transactions with fibo_max will result in timeouts.  So reduce
    the num_fib_values.
    '''
    participants, committers = util.helper.extract_ip_lists_from_fxBlockchain(fxBlockchain)
    host = participants[0]
    futures = []
    num_procs = 3

    with concurrent.futures.ProcessPoolExecutor(max_workers=num_procs) as executor:
        for _ in range(0, num_procs):
            future = executor.submit(run_fibo, host, iterations=1, num_fib_values=fibo_max-3)
            futures.append(future)

    # This will trigger failures due to Pytest asserts.
    for f in futures:
        f.result()


@describe("Stop f nonprimary nodes")
def test_stop_nonprimary_nodes(fxBlockchain, fxFiboMax, fxAppSetup, fxInstallDamlSdk):
    '''
    Stop f nonprimary nodes and verify that txs work.
    '''
    participants, committers = util.helper.extract_ip_lists_from_fxBlockchain(fxBlockchain)
    ledger = participants[0]
    util.blockchain_ops.wait_for_state_transfer_complete(fxBlockchain.blockchainId, committers)
    primary_ip = get_primary_ip(fxBlockchain, committers)
    log.info("primary: {}".format(primary_ip))
    f = (len(committers) - 1) / 3
    
    stopped_nodes = []

    for committer in committers:
        if not committer == primary_ip:
            c_username, c_password = util.helper.getNodeCredentials(fxBlockchain.blockchainId, committer) 
            timestamp = util.blockchain_ops.get_docker_timestamp(committer, c_username, c_password, "concord")
            log.info("Stopping Concord on {} at timestamp {}".format(committer, timestamp))
            stopped_nodes.append(committer)
            util.blockchain_ops.stop_services(committer, c_username, c_password, ["concord"])
            if len(stopped_nodes) == f:
                break

    l_username, l_password = util.helper.getNodeCredentials(fxBlockchain.blockchainId, ledger) 
    timestamp = util.blockchain_ops.get_docker_timestamp(ledger, l_username, l_password, "daml_ledger_api")
    log.info("Starting transactions on {} at timestamp {}".format(ledger, timestamp))
    run_fibo(ledger, num_fib_values=fibo_max)
    run_request_tool(ledger)
    timestamp = util.blockchain_ops.get_docker_timestamp(ledger, l_username, l_password, "daml_ledger_api")
    log.info("Finished transactions on {} at timestamp {}".format(ledger, timestamp))

    for committer in stopped_nodes:
        c_username, c_password = util.helper.getNodeCredentials(fxBlockchain.blockchainId, committer)
        timestamp = util.blockchain_ops.get_docker_timestamp(ledger, l_username, l_password, "daml_ledger_api")
        log.info("Restarting Concord {} at {}".format(committer, timestamp))
        util.blockchain_ops.start_services(committer, c_username, c_password, ["concord"])

    util.blockchain_ops.wait_for_state_transfer_complete(fxBlockchain.blockchainId, committers)
    run_fibo(ledger, num_fib_values=1)
    run_request_tool(ledger)


@describe("View change during preexecution")
def test_view_change_during_preexecution(fxBlockchain, fxInstallDamlSdk, fxAppSetup, fxFiboMax):
    '''
    Get the primary.
    Submit a very long transaction.
    Bring down the primary once the real pre-execution begins.
    Verify the transaction fails.
    Bring up that node and wait for view change to complete. (There is a function in Hermes to help with this. Could also use Wavefront. See the sample dashboard.)
    Resubmit the long transaction and verify that it works.
    '''
    participants, committers = util.helper.extract_ip_lists_from_fxBlockchain(fxBlockchain)
    orig_primary_ip = get_primary_ip(fxBlockchain, committers)
    ledger = participants[0]
    log.info("ledger: {}".format(ledger))
    log.info("primary ip: {}".format(orig_primary_ip))

    before = time.time()
    run_fibo(ledger, num_fib_values=fibo_max)
    after = time.time()
    sleep_time = int((after - before)) / 2 # Ledger has finished, should be around the time it gets pre-executed in Concord.
    futures = []

    with concurrent.futures.ThreadPoolExecutor(max_workers = 2) as executor:
        future = executor.submit(start_tx_failure_ok, fxBlockchain.blockchainId, ledger)
        futures.append(future)
        future = executor.submit(sleep_and_kill, fxBlockchain.blockchainId, sleep_time, orig_primary_ip, ledger)
        futures.append(future)

    # This will trigger failures if there are Pytest assertion failures.
    for f in futures:
        f.result(timeout=120)

    # Give plenty of time for nodes to notice that the primary is gone before restarting this one.
    time.sleep(30)
    username, password = util.helper.getNodeCredentials(fxBlockchain.blockchainId, orig_primary_ip)
    util.blockchain_ops.start_services(orig_primary_ip, username, password, ["concord"])
    new_primary_ip = get_primary_ip(fxBlockchain, committers)
    start_time = time.time()

    while new_primary_ip == orig_primary_ip and time.time() - start_time < 180:
        log.info("Waiting for primary to change. Current primary: {}".format(new_primary_ip))
        time.sleep(5)
        new_primary_ip = get_primary_ip(fxBlockchain, committers)

    run_fibo(ledger, num_fib_values=1)


@describe("Large execution output")
def test_large_execution_output(fxBlockchain, fxSpiderDar, fxInstallDamlSdk, fxAppSetup):
    '''
    Perform a transaction with large output.
    Simply uploading the CHESS+ dar file is sufficient per DA.
    We are not running the app; we are just uploading it. Hard coding the version
    can save a significant amount of time.
    '''
    # We have a timeout of 1 hour, yes.
    # We could scp it over, but then we would need to install the DAML SDK on the ledger system,
    # which seems to invalidate it.
    # We retry 10x because sometimes what times out is the creation of the GRPC connection,
    # which seems hard coded to ten seconds.
    log.info("Yes, despite all the problems with dar upload, we have to do one for a test case.  And check out this timeout!  Here we go...")
    timeout = 3600
    attempts = 10
    success = False
    participants, committers = util.helper.extract_ip_lists_from_fxBlockchain(fxBlockchain)
    host = participants[0]
    cmd = [daml_sdk_path, "ledger", "upload-dar", fxSpiderDar, "--timeout", str(timeout)]
    cmd.extend(["--host", host, "--port", str(LEDGER_PORT)])

    while attempts and not success:
        attempts -= 1
        start = time.time()
        success = False
        stdout = None

        try:
            success, stdout = util.helper.execute_ext_command(cmd, timeout=timeout)
        except subprocess.TimeoutExpired as e:
            log.info("    Timeout expired.  Trying again.")

        log.info("    Output: {}".format(stdout))
        log.info("    Time taken: {}".format(time.time() - start))

    assert success, "Failed to upload the chess+ dar file"


@describe("Send requests in parallel, but from different clients.")
def test_parallel_clients(fxBlockchain, fxInstallDamlSdk, fxAppSetup, fxFiboMax):
    participants, committers = util.helper.extract_ip_lists_from_fxBlockchain(fxBlockchain)
    alice = participants[0]
    bob = participants[1]
    futures = []

    with concurrent.futures.ProcessPoolExecutor(max_workers = 2) as executor:
        futures.append(executor.submit(run_fibo, alice, iterations=2, num_fib_values=fibo_max-3))
        futures.append(executor.submit(run_request_tool, alice, iterations=3))
        futures.append(executor.submit(run_request_tool, alice, iterations=3))

        futures.append(executor.submit(run_fibo, bob, iterations=2, num_fib_values=fibo_max-3))
        futures.append(executor.submit(run_request_tool, bob, iterations=3))
        futures.append(executor.submit(run_request_tool, bob, iterations=3))

    for f in futures:
        f.result()


def get_wf_measurements(committers, start_epoch, end_epoch):
    '''
    Fetches measurements needed for the test case for Wavefront metrics.
    committers: List of committers for which data is gathered
    start_epoch: Epoch time for the start of the window for Wavefront
    end_epoch: Epoch time for the end of the window for Wavefront
    Return: Tuple of (num_measurements, highest_measurement) for the
    metric we are testing in this test suite.
    '''
    num_measurements = 0
    total_measurement = 0
    metric_name = "vmware.blockchain.concord.concordbft.preProcReqReceived.counter"
    filt = "vm_ip"
    str_output = None

    for ip in committers:
        metric_query = "ts({},{}={})".format(metric_name, filt, ip)
        str_output = util.wavefront.call_wavefront_chart_api(metric_query, start_epoch, end_epoch)
        output = json.loads(str_output)

        for series in output["timeseries"]:
            if series["data"]:
                num_measurements += len(series["data"])
                for d in series["data"]:
                    total_measurement += d[1]

        log.debug("Wavefront data: {}".format(str_output))

    log.info("num_measurements: {}".format(num_measurements))
    log.info("total_measurement: {}".format(total_measurement))
    return num_measurements, total_measurement


@describe("Verify that pre-execution metrics are being sent.")
def test_metrics(fxBlockchain, fxInstallDamlSdk, fxAppSetup):
    '''
    Notes:
    - Metrics values are stored in memory in Concord, so if there has been a
    Concord restart, the counters all reset to 0.
    - Time skew between Concord and the system this test is running on can occur,
    so we cannot for certain query Wavefront for Concord's metrics at the time this
    test case starts.
    So, we verify that non-zero values are added between (a fixed point in the past and test start time)
    and (the same fixed point + time to run some transactions + some sleep time).
    '''
    participants, committers = util.helper.extract_ip_lists_from_fxBlockchain(fxBlockchain)
    ledger = participants[0]
    num_measurements_before = 0
    num_measurements_after = 0
    total_before = 0
    total_after = 0
    start_epoch = time.time() - 900 # Clock skew issues...just cover a long period.

    end_epoch = time.time()
    num_measurements_before, total_before = get_wf_measurements(committers, start_epoch, end_epoch)

    run_request_tool(ledger)
    time.sleep(65) # Wait for nodes to update Wavefront; they are on a heartbeat.

    end_epoch = time.time()
    num_measurements_after, total_after = get_wf_measurements(committers, start_epoch, end_epoch)

    assert num_measurements_after > num_measurements_before, "Did not find additional metrics"
    assert total_after > total_before, "Total number of pre-exec requests should have increased"
