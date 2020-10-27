#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
from datetime import datetime, timedelta
import time
from suites.case import describe
import pytest
from util import helper, hermes_logging
import json
from subprocess import check_output
from fixtures.common_fixtures import fxBlockchain, fxProduct

log = hermes_logging.getMainLogger()


def connect_to_log_insight(log_insight_host, username, password):
    '''
    Function to connect to LogInsight using sessions api.
    Args:
        log_insight_host: LogInsight host ip address
        username: LogInsight username
        password: LogInsight password
    API Reference:
        https://code.vmware.com/apis/50/log-insight
    Returns:
        True/False and session id message based on the outcome
    '''
    # Url to connect to Log Insight using sessions api
    authenticate_url = "https://{}/api/v1/sessions".format(log_insight_host)

    data = {"username": username, "password": password, "provider": "Local"}

    # Command to connect to loginsight
    connect_cmd = ["curl", "-k", "-X", "POST",
                  "-H", "Accept: application/json",
                  "-H", "Content-Type: application/json", authenticate_url,
                  "-d", json.dumps(data),
                  "--connect-timeout", "5",  # Connection timeout for each retry
                  "--max-time", "10",  # Wait time for each retry
                  "--retry", "5",  # Maximum retries
                  "--retry-delay", "0",  # Delay in next retry
                  "--retry-max-time", "60",  # Total time before this call is considered a failure
                  "-i"]
    cmd_output = check_output(connect_cmd).decode('utf8')

    log_insight_connect_output = cmd_output.split("\n", 1)
    log.info("\n\nStatus: {}".format(log_insight_connect_output[0]))

    if 'HTTP/1.1 200' in log_insight_connect_output[0]:
       log_insight_connect_output = cmd_output.split("\n", 1)
       log.info("Connection to Log Insight is successful")
       session = log_insight_connect_output[1].split('"sessionId":"')[1].split('"')
       return True, session[0]
    elif 'HTTP/1.1 400' or 'HTTP/1.1 401' or 'HTTP/1.1 403' in cmd_output:
       log.error(cmd_output)
       return False, "Field Error: Could not connect to LogInsight with the given credentials/provider"
    else:
       log.error(cmd_output)
       return False, "Could not connect to LogInsight. Check error message for more details"


@describe("Verify Log Insight is connecting")
@pytest.mark.smoke
def test_log_insight_smoke():
    '''
    Verify Log Insight is connecting using below steps:
    - Fetch LogInsight information from user_config.json.
    - Connect to LogInsight by calling local function connect_to_log_insight.
    API Reference:
        https://code.vmware.com/apis/50/log-insight
    '''
    # Config object injected by the Jenkins run, which is calling this script
    configObject = helper.getUserConfig()
    log_insight_details = configObject["logInsight"]
    success, session_id_msg = connect_to_log_insight(log_insight_details["address"], log_insight_details["username"],
                                                    log_insight_details["password"])
    assert success, session_id_msg


@describe("Verify Log Insight is running and collecting logs")
def test_log_insight_event_logs(fxBlockchain):
    '''
    Verify Log Insight is running and collecting logs using below steps:
    - Fetch LogInsight information from user_config.json.
    - Connect to LogInsight by calling local function connect_to_log_insight.
    - Fetch logs using events api by filtering with consortium id.
    API Reference:
        https://code.vmware.com/apis/50/log-insight
    Args:
       fxBlockchain: blockchain
    '''
    # Config object injected by the Jenkins run, which is calling this script
    configObject = helper.getUserConfig()
    log_insight_details = configObject["logInsight"]

    # Blockchain consortium id for which logs are to be verified
    bc_consortium_id =  fxBlockchain.consortiumId
    assert bc_consortium_id, "Consortium Id not found, can't proceed with this test"

    # Fetch session id by calling the function to connect to log insight
    success, session_id_msg = connect_to_log_insight(log_insight_details["address"], log_insight_details["username"],
                                                    log_insight_details["password"])
    log.info("\n\nSession ID: {}".format(session_id_msg))

    assert success, session_id_msg

    # Timestamp to fetch Latest 15 minutes of data
    timestamp = 900000  # Timestamp has to be provided in milliseconds

    start_time = datetime.now()
    log.info("\n\nStart time: {}".format(start_time))
    end_time = start_time + timedelta(milliseconds=timestamp)
    log.info("\n\nEnd time: {}".format(end_time))

    # Using hard coded value
    fetch_logs_url_1 = "https://{}:{}/api/v1/events/{}/CONTAINS%20{}/timestamp/LAST%20{}". \
       format(log_insight_details["address"], log_insight_details["port"], 'consortium_id',
              'f570b580-2d44-45a6-b670-c5487a4059e6',
              timestamp)

    log.info("\n\nLogInsight Url to fetch events logs: {}".format(fetch_logs_url_1))

    # Command to fetch logs
    consortium_filter_1 = ["curl", "-k", fetch_logs_url_1,
                         "-H", "Accept: application/json",
                         "-H", "Content-Type: application/json",
                         "-H", "Authorization: Bearer {}".format(session_id_msg)]

    consortium_filter_output = check_output(consortium_filter_1).decode('utf8')
    output = json.loads(consortium_filter_output)
    log.info("\n\nLength of events of hard code value: {}".format(len(output['events'])))

    time.sleep(180)

    # Url to verify logs using events api by filtering using consortium id for the given timestamp
    fetch_logs_url = "https://{}:{}/api/v1/events/{}/CONTAINS%20{}/timestamp/LAST%20{}". \
      format(log_insight_details["address"], log_insight_details["port"], 'consortium_id', bc_consortium_id,
             timestamp)

    log.info("\n\nLogInsight Url to fetch events logs: {}".format(fetch_logs_url))

    # Command to fetch logs
    consortium_filter = ["curl", "-k", fetch_logs_url,
                        "-H", "Accept: application/json",
                        "-H", "Content-Type: application/json",
                        "-H", "Authorization: Bearer {}".format(session_id_msg)]

    consortium_filter_output = check_output(consortium_filter).decode('utf8')
    output = json.loads(consortium_filter_output)
    log.info("\n\nLength of events of fxBlockchain: {}".format(len(output['events'])))
    #assert len(output['events']), "Logs not generated for the blockchain"
