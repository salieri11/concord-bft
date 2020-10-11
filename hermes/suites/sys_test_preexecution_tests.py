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
fibo_max = 33
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
    success = True
    successes = 0
    failures = 0
    host = fxBlockchain.replicas["daml_participant"][0]

    while True:
        success, output = run_fibo(host, num_fib_values=fibo_max, test_mode=False)

        if success:
            successes += 1

            if failures > 0:
                # It failed last time, but not last time.
                # We have been decrementing.  Keep the current value and return
                log.info("Returning, fibo_max is {}".format(fibo_max))
                return
            else:
                 # It is either:
                 #   1. our first try or
                 #   2. we have been seeing only successes, which means
                 #      we have been incrementing from the initial value.
                 # Either way, keep incrementing and checking.
                 fibo_max += 1
                 log.info("fibo_max has been incremented to {}".format(fibo_max))
        else:
            failures += 1
            fibo_max -= 1
            log.info("fibo_max has been decremented to {}".format(fibo_max))

            if successes > 0 and "outside of range" in output:
                # It worked last time, not this time.
                # We have already decremented; just save
                # that value.
                log.info("Returning, fibo_max is {}".format(fibo_max))
                return


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
        success, stdout = util.helper.execute_ext_command(cmd, timeout=240, working_dir=REQUEST_TOOL_DIR)
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

# def run_request_tool(host, port=LEDGER_PORT, num_fib_values=1, iterations=1):
#     '''
#     Runs the daml request_tool, which is more realistic than the simple fib app, on the DAML ledger.
#     host/port: The host and port of the ledger api server.
#     num_fib_values: How many fibonacci numbers to find.  This is done in the midst
#         running the other transactions.
#     '''
#     from daml_requests import daml_request
#     from dazl_remote import Remote
#     from scenario import Scenario
#     import time

#     url = "https://{}:{}".format(host, port)

#     with open(Scenario.DEFAULT_DATA_FILE, "r") as yaml_file:
#         data = yaml.load(yaml_file, Loader=yaml.FullLoader)

#     for i in range(0, iterations):
#         def init_scenario(url, data):
#             nonlocal assets
#             nonlocal fibs
#             assets = 0
#             fibs = 0
#             remote = Remote(url, data['parties'])
#             scenario = Scenario(remote.parties, data, 'scenario', complex=True, exec_delay=num_fib_values)
#             return remote, scenario

#         def count_assets(_):
#             nonlocal assets
#             assets += 1

#         def count_fib_complete(_):
#             nonlocal fibs
#             fibs += 1

#         log.info("Starting request_tool iteration {}".format(i))
#         assets = 0
#         fibs = 0
#         remote, scenario = init_scenario(url, data)
#         probable_infra_issue = False
#         retries = 5
#         pause_between_txs = 1
#         num_requests = 1 # Hangs if > 1.

#         for i in range(0, retries):
#             scenario.party['client'].add_ledger_created('Asset.Quote', count_assets)

#             scenario.party['issuer'].add_ledger_exercised('Fibo.InefficientFibonacci',
#                                                           'InefficientFibonacciCompute',
#                                                           count_fib_complete)
#             try:
#                 daml_request(remote, scenario, num_requests, pause_between_txs, True)
#             except Exception as e:
#                 if "FINISHED" not in str(e):
#                     probable_infra_issue = True

#             if probable_infra_issue:
#                 if i < retries-1:
#                     info("Retrying because of a probable infra issue: {}".format(e))
#                     remote, scenario = init_scenario(url, data)
#                     time.sleep(1)
#                 else:
#                     assert False, "Failed, probably due to infra issues."
#             else:
#                 log.info("assets: {}".format(assets))
#                 log.info("fibs: {}".format(fibs))
#                 assert assets == 4, "Wrong number of assets"
#                 assert fibs == 1, "Wrong number of fibonacci exercises"
#                 break


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
                                             "cat /config/concord/config-local/deployment.config")
    config = yaml.load(output, Loader=yaml.Loader)

    for item in config["node"]:
        if item["replica"][0]["principal_id"] == primary_rid:
            primary_ip = item["replica"][0]["replica_host"]

            if "localhost" in primary_ip or "127.0.0.1" in primary_ip:
                # The one we retrieved the config from *is* the primary.
                primary_ip = arbitrary_committer_ip

            return primary_ip

    raise("Could not find ip for replica '{}' in: '{}'".format(primary_rid, config))


def get_docker_timestamp(host, username, password, service):
    '''
    Retrieves the most recent timestamp from `docker logs` for the given service.
    '''
    output = util.helper.durable_ssh_connect(host, username, password,
                                             "docker logs --tail 1 {}".format(service))
    return output.split("|")[0]


@describe()
@pytest.mark.smoke
def test_simple_txs(fxBlockchain, fxInstallDamlSdk, fxAppSetup, fxFiboMax):
    '''
    Send multiple very simple transactions.
    '''
    host = fxBlockchain.replicas["daml_participant"][0]
    run_fibo(host, iterations=5)
    run_fibo(host, iterations=5, num_fib_values=fibo_max)


@describe()
@pytest.mark.smoke
def test_realistic_txs(fxBlockchain, fxInstallDamlSdk, fxAppSetup):
    '''
    Run a more realistic scenario.
    python3 daml_requests.py --url https://10.72.230.8:6865 scenario --complex 1
    '''
    host = fxBlockchain.replicas["daml_participant"][0]
    run_request_tool(host, iterations=5)
    #run_request_tool(host, iterations=5, num_fib_values=fibo_max)


@describe()
@pytest.mark.smoke
def test_parallel_short_long_txs(fxBlockchain, fxInstallDamlSdk, fxAppSetup, fxFiboMax):
    host = fxBlockchain.replicas["daml_participant"][0]
    futures = []
    num_slow_procs = 1
    num_realistic_procs = 5
    max_workers = num_slow_procs + num_realistic_procs

    with concurrent.futures.ProcessPoolExecutor(max_workers = max_workers) as executor:
        for _ in range(0, num_slow_procs):
            future = executor.submit(run_fibo, host, iterations=1, num_fib_values=fibo_max)
            futures.append(future)
        for _ in range(0, num_realistic_procs):
            future = executor.submit(run_request_tool, host, iterations=3)
            futures.append(future)

    # This will trigger failures due to Pytest asserts.
    for f in futures:
        f.result()


@describe()
@pytest.mark.smoke
def test_parallel_short_txs(fxBlockchain, fxInstallDamlSdk, fxAppSetup, fxFiboMax):
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


@describe()
@pytest.mark.smoke
def test_parallel_long_txs(fxBlockchain, fxInstallDamlSdk, fxAppSetup, fxFiboMax):
    '''
    Always fails with n=3 finding 33 fibo numbers. Usually fails with 2.
    '''
    host = fxBlockchain.replicas["daml_participant"][0]
    futures = []
    num_procs = 1

    with concurrent.futures.ProcessPoolExecutor(max_workers=num_procs) as executor:
        for _ in range(0, num_procs):
            future = executor.submit(run_fibo, host, iterations=1, num_fib_values=fibo_max)
            futures.append(future)

    # This will trigger failures due to Pytest asserts.
    for f in futures:
        f.result()


@describe()
def test_stop_nonprimary_nodes(fxBlockchain, fxAppSetup, fxInstallDamlSdk):
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
            timestamp = get_docker_timestamp(committer, username, password, "concord")
            log.info("Stopping Concord on {} at timestamp {}".format(committer, timestamp))
            stopped_nodes.append(committer)
            output = util.helper.durable_ssh_connect(committer,
                                                     username, password,
                                                     "docker stop concord")
            if len(stopped_nodes) == f:
                break

    timestamp = get_docker_timestamp(ledger, username, password, "daml_ledger_api")
    log.info("Starting transactions on {} at timestamp {}".format(ledger, timestamp))
    run_fibo(ledger, num_fib_values=fibo_max)
    run_request_tool(ledger)

    for committer in stopped_nodes:
        timestamp = get_docker_timestamp(ledger, username, password, "daml_ledger_api")
        log.info("Restarting Concord {} at {}".format(committer, timestamp))
        output = util.helper.durable_ssh_connect(committer,
                                                 username, password,
                                                 "docker restart concord")

    util.blockchain_ops.wait_for_state_transfer_complete(fxBlockchain)
    run_fibo(ledger, num_fib_values=1)
    run_request_tool(ledger)
