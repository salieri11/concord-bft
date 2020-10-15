##########################################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# These are system tests for pre-execution.
# Test plan: https://confluence.eng.vmware.com/display/BLOC/Pre-Execution+System+Test+Plan
##########################################################################################
import concurrent.futures
import os
import pytest
import time
import util.daml.daml_requests
import util.helper
import yaml

from fixtures.common_fixtures import fxBlockchain, fxConnection, fxInitializeOrgs, fxProduct
from suites.case import describe, passed, failed

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
    host = fxBlockchain.replicas["daml_participant"][0]
    daml_sdk_version = util.daml.daml_helper.get_ledger_api_version(host)
    daml_sdk_path = util.daml.daml_helper.install_daml_sdk(daml_sdk_version)


@pytest.fixture(scope="module")
def fxAppSetup(fxBlockchain):
    host = fxBlockchain.replicas["daml_participant"][0]
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
    host = fxBlockchain.replicas["daml_participant"][0]

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
                 new_max += 1
                 log.info("new_max has been incremented to {}".format(new_max))
        else:
            failures += 1
            new_max -= 1
            log.info("new_max has been decremented to {}".format(new_max))

            if successes > 0 and "outside of range" in output:
                # It worked last time, not this time. That means we went too high.
                # We have already decremented; just save that value.
                log.info("Breaking, new_max is {}".format(new_max))
                break

    # For test stability, decrement one more time. It is not reliable to run at max.
    if new_max > fibo_absolute_max:
        new_max = fibo_absolute_max
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
    cmd = ["daml", "script", "--dar", ".daml/dist/ledgerclient-0.0.1.dar", "--script-name", "Fibo:run"]
    cmd.extend(["--ledger-host", host, "--ledger-port", str(port)])
    cmd.extend(["--input-file", input_file])

    if not os.path.exists(input_file):
        with open(input_file, "w") as f:
            f.write(str(num_fib_values))

    for i in range(0, iterations):
        # Note that the app can hang in some cases, so keep the timeout.
        success, stdout = util.helper.execute_ext_command(cmd, timeout=60, working_dir=REQUEST_TOOL_DIR)
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
    cmd = ["daml", "script", "--dar", ".daml/dist/ledgerclient-0.0.1.dar", "--script-name", "AssetCheck:asset_check"]
    cmd.extend(["--ledger-host", host, "--ledger-port", str(port)])

    for i in range(0, iterations):
        success, stdout = util.helper.execute_ext_command(cmd, timeout=240, working_dir=REQUEST_TOOL_DIR)
        log.debug("Request tool app iteration {} stdout: {}".format(i, stdout))
        assert success, "Running request tool app failed"


def get_primary_ip(fxBlockchain):
    '''
    Noopur is implementing this in blockchain_ops.py.  This is just a
    placeholder.
    '''
    primary_rid = util.blockchain_ops.get_primary_rid(fxBlockchain)
    arbitrary_committer_ip = fxBlockchain.replicas["daml_committer"][0]
    username, password = util.helper.getNodeCredentials()
    success = False
    msg = None
    e = None
    output = None

    output = util.helper.durable_ssh_connect(arbitrary_committer_ip,
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


@describe("Send multiple very simple transactions.")
@pytest.mark.smoke
def test_simple_txs(fxBlockchain, fxInstallDamlSdk, fxAppSetup, fxFiboMax):
    '''
    Send a few short transactions, then a few long transactions.
    '''
    host = fxBlockchain.replicas["daml_participant"][0]
    run_fibo(host, iterations=5)
    run_fibo(host, iterations=5, num_fib_values=fibo_max)


@describe("Run a more realistic scenario.")
@pytest.mark.smoke
def test_realistic_txs(fxBlockchain, fxInstallDamlSdk, fxAppSetup):
    '''
    Run a more realistic scenario.
    python3 daml_requests.py --url https://10.72.230.8:6865 scenario --complex 1
    '''
    host = fxBlockchain.replicas["daml_participant"][0]
    run_request_tool(host, iterations=5)


@describe("Send one slow tx while running multiple realistic scenarios.")
@pytest.mark.smoke
def test_parallel_short_long_txs(fxBlockchain, fxInstallDamlSdk, fxAppSetup, fxFiboMax):
    '''
    Start the 30 second fibo transaction, then do a few iterations of the request tool.
    '''
    host = fxBlockchain.replicas["daml_participant"][0]
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
    host = fxBlockchain.replicas["daml_participant"][0]
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
    host = fxBlockchain.replicas["daml_participant"][0]
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
    util.blockchain_ops.wait_for_state_transfer_complete(fxBlockchain)
    ledger = fxBlockchain.replicas["daml_participant"][0]
    primary_ip = get_primary_ip(fxBlockchain)
    log.info("primary: {}".format(primary_ip))
    f = (len(fxBlockchain.replicas["daml_committer"]) - 1) / 3
    username, password = util.helper.getNodeCredentials()
    stopped_nodes = []

    for committer in fxBlockchain.replicas["daml_committer"]:
        if not committer == primary_ip:
            timestamp = util.blockchain_ops.get_docker_timestamp(committer, username, password, "concord")
            log.info("Stopping Concord on {} at timestamp {}".format(committer, timestamp))
            stopped_nodes.append(committer)
            util.blockchain_ops.stop_services(committer, username, password, ["concord"])
            if len(stopped_nodes) == f:
                break

    timestamp = util.blockchain_ops.get_docker_timestamp(ledger, username, password, "daml_ledger_api")
    log.info("Starting transactions on {} at timestamp {}".format(ledger, timestamp))
    run_fibo(ledger, num_fib_values=fibo_max)
    run_request_tool(ledger)
    timestamp = util.blockchain_ops.get_docker_timestamp(ledger, username, password, "daml_ledger_api")
    log.info("Finished transactions on {} at timestamp {}".format(ledger, timestamp))

    for committer in stopped_nodes:
        timestamp = util.blockchain_ops.get_docker_timestamp(ledger, username, password, "daml_ledger_api")
        log.info("Restarting Concord {} at {}".format(committer, timestamp))
        util.blockchain_ops.start_services(committer, username, password, ["concord"])

    util.blockchain_ops.wait_for_state_transfer_complete(fxBlockchain)
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
    orig_primary_ip = get_primary_ip(fxBlockchain)
    ledger = fxBlockchain.replicas["daml_participant"][0]
    log.info("ledger: {}".format(ledger))
    log.info("primary ip: {}".format(orig_primary_ip))

    username, password = util.helper.getNodeCredentials()
    before = time.time()
    run_fibo(ledger, num_fib_values=fibo_max)
    after = time.time()
    sleep_time = int((after - before)) / 2 # Ledger has finished, should be around the time it gets pre-executed in Concord.
    futures = []

    with concurrent.futures.ThreadPoolExecutor(max_workers = 2) as executor:
        future = executor.submit(start_tx_failure_ok, ledger)
        futures.append(future)
        future = executor.submit(sleep_and_kill, sleep_time, orig_primary_ip, ledger)
        futures.append(future)

    # This will trigger failures if there are Pytest assertion failures.
    for f in futures:
        f.result(timeout=120)

    # Give plenty of time for nodes to notice that the primary is gone before restarting this one.
    time.sleep(30)
    util.blockchain_ops.start_services(orig_primary_ip, username, password, ["concord"])
    new_primary_ip = get_primary_ip(fxBlockchain)
    start_time = time.time()

    while new_primary_ip == orig_primary_ip and time.time() - start_time < 180:
        log.info("Waiting for primary to change. Current primary: {}".format(new_primary_ip))
        time.sleep(5)
        new_primary_ip = get_primary_ip(fxBlockchain)

    run_fibo(ledger, num_fib_values=1)


def start_tx_failure_ok(ledger):
    '''
    Start a tx on the given ledger and do not mind if it fails.
    Launched in a separate thread.
    '''
    username, password = util.helper.getNodeCredentials()

    try:
        timestamp = util.blockchain_ops.get_docker_timestamp(ledger, username, password, "daml_ledger_api")
        log.info("Starting fibo_max tx at ledger time {}".format(timestamp))
        run_fibo(ledger, num_fib_values=fibo_max)
    except Exception as e:
        timestamp = util.blockchain_ops.get_docker_timestamp(ledger, username, password, "daml_ledger_api")
        log.info("Fibo max failed at ledger time {}, but that is ok.".format(timestamp))


def sleep_and_kill(sleep_time, committer, ledger):
    '''
    Sleep a while, then kill the given committer.
    Launched in a separate thread.
    '''
    log.info("Sleeping for {} seconds and then killing the primary, {}".format(sleep_time, committer))
    time.sleep(sleep_time)
    username, password = util.helper.getNodeCredentials()
    c_timestamp = util.blockchain_ops.get_docker_timestamp(committer, username, password, "concord")
    l_timestamp = util.blockchain_ops.get_docker_timestamp(ledger, username, password, "daml_ledger_api")
    log.info("Stopping concord at concord time {} and ledger time {}".format(c_timestamp, l_timestamp))
    util.blockchain_ops.stop_services(committer, username, password, ["concord"])
