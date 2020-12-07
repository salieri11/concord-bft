from base64 import b64decode
import json
import re
import util.blockchain.eth
import dateutil.parser
import time

import util.hermes_logging

log = util.hermes_logging.getMainLogger()


def run_conc_time(concordContainer=None, args=""):
    '''
   Run the `conc_time` utility in a concord container. Appends the
   string `args` to the command. Returns the text output.

   '''
    if not concordContainer:
        concordContainer = util.blockchain.eth.get_concord_container_name(1)

    return util.blockchain.eth.exec_in_concord_container(concordContainer,
                                                         "./conc_time -o json {}".format(args))


def run_conc_reconfig_pusher_period(concordContainers, newPeriod):
    '''
   Run the conc_reconfig utility to reconfigure --time_pusher_period_ms to
   change the time pusher period. concordContainers should be either None or a
   list of string container names specifying which container(s) to run
   conc_reconfig in; if None is given, conc_reconfig will be run on every
   Concord node in the cluster. newPeriod specifies the new time pusher period,
   in millisecond to set for the selected container(s). Returns a list of the
   output from conc_reconfig for each container it is run for; the output is in
   the same order as the input list of containers if a list was provided and in
   ascending order of node/replica ID if None was provided for
   concordContainers.
   '''
    if concordContainers is None:
        concordContainers = util.blockchain.eth.get_all_concord_container_names()

    output = list()
    for container in concordContainers:
        output.append(util.blockchain.eth.exec_in_concord_container(container,
                                                                    "./conc_reconfig --time_pusher_period_ms {}".format(
                                                                        newPeriod)))
    return output


def time_service_is_disabled():
    '''
   If we receive an error saying the time service is disabled, return
   true. Useful for skipping tests that need the time service to be
   enabled.
   '''
    output = run_conc_time()

    # text format uses snake_case, json format uses camelCase
    if re.findall("error_response", output) or re.findall("errorResponse", output):
        return bool(re.findall("Time service is disabled", output))


def iso_timestamp_to_seconds(timestamp):
    '''
   Convert ISO-8601 string to (floating point) seconds (including partial seconds in the decimal).
   '''
    datetimeObj = dateutil.parser.isoparse(timestamp)
    return datetimeObj.timestamp()


def extract_samples_from_response(output):
    '''
   Dig through conc_time output to find a TimeResponse message, and
   extract samples from it. The conc_time script should have been
   exected with `-l` and `-o json` flags.
   '''
    responseText = re.findall("Received response: (.*)", output)[0]
    responseJson = json.loads(responseText)
    sampleList = responseJson["timeResponse"]["sample"]

    sampleMap = {}
    for sample in sampleList:
        text_source = b64decode(sample["source"]).decode("utf-8")
        sampleMap[text_source] = iso_timestamp_to_seconds(sample["time"])

    return sampleMap


def get_samples(concordContainer=None):
    '''
   Use the `conc_time` tool to read the latest state of the time contract.
   '''
    sleep_time = 30

    attempt = 0
    max_tries = 4
    while attempt < max_tries:
        attempt += 1

        output = run_conc_time(concordContainer, "-l")

        # retry reading after sleep_time if docker exec command failed
        # temporary fix for BC-3865 due to intermittent open runc issue https://github.com/opencontainers/runc/issues/1326
        if re.findall("runtime exec failed", output):
            log.warning(
                "Unable to read the time contract from container {} due to \"{}\".".format(concordContainer, output))
            log.warning("Retry after {} seconds...".format(sleep_time))
            time.sleep(sleep_time)
        else:
            return extract_samples_from_response(output)
    raise RuntimeError(
        "Failed to read the time contract from container {} after {} tries".format(concordContainer, max_tries))


def extract_time_summary_response(output):
    '''
   Dig through conc_time output to find a TimeResponse message, and
   extract the summary from it. The conc_time script should have been
   exected with `-g` and `-o json` flags.
   '''
    responseText = re.findall("Received response: (.*)", output)[0]
    responseJson = json.loads(responseText)
    return iso_timestamp_to_seconds(responseJson["timeResponse"]["summary"])


def get_summary(concordContainer=None):
    '''
   Use the `conc_time` tool to read the latest state of the time contract.
   '''
    output = run_conc_time(concordContainer, "-g")
    return extract_time_summary_response(output)


def max_faulty_clocks():
    '''
    Find out how many sources we expect to be able to tolerate faults
    in. For the implementation based on median, this is less than 1/2.
    '''
    sampleCount = len(get_samples())
    return sampleCount // 2 - (1 if (sampleCount / 2 == sampleCount // 2) else 0)


def get_summary_and_samples():
    '''
    Read both the summary time and all samples.
    '''
    output = run_conc_time(args="-g -l")
    return extract_time_summary_response(output), extract_samples_from_response(output)


def find_faulty_clocks(expected_update_period):
    '''
    Look for clocks that are far in the future, or stuck in the past.
    '''
    start_summary, start_samples = get_summary_and_samples()

    # We are guaranteed to see enough updates from non-faulty nodes in
    # three update periods.
    time.sleep(expected_update_period*3)

    end_summary, end_samples = get_summary_and_samples()

    slow_sources = []
    healthy_sources = []
    fast_sources = []

    for source, end_time in end_samples.items():
        if end_time < start_summary:
            slow_sources.append(source)
        elif start_samples.get(source) > end_summary:
            fast_sources.append(source)
        else:
            healthy_sources.append(source)

    return slow_sources, healthy_sources, fast_sources
