#########################################################################
# Copyright 2018 - 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Utility to test the performance of the Helen+concord ecosystem
#########################################################################
import logging
import pytest
import requests
import json
import time

from uuid import UUID
from datetime import datetime, timedelta
from util.daml import daml_helper
from util.auth import getAccessToken
from fixtures.common_fixtures import (
    fxBlockchain, fxConnection, fxHermesRunSettings,
    fxInitializeOrgs, fxProduct, deployToSddc)

log = logging.getLogger(__name__)

log_insight_host = 'https://localhost.vmware.com'
log_insight_url = '{0}/api/lint/ops/query/log-query-tasks'.format(
    log_insight_host
)
wait_time = 15 * 60  # Fifteen Minutes

headers = {
    "Authorization": "Bearer {}".format(getAccessToken()),
    'accept': 'application/json',
    'content-type': 'application/json'
}


def li_sql_query_builder(
    where,
    level="(level = 'INFO' OR level = 'ERROR')"
):
    select = "SELECT * FROM logs WHERE"

    return "{0} {1} {2} ORDER BY ingest_timestamp DESC".format(
        select,
        where,
        level
    )

# Log Insight Cloud Query
# First it creates a query task with a replica ID
# Then polls on query task, until it's completed
@pytest.fixture
def li_cloud_query(fxConnection):
    def _init(data={}):
        log_dict = {}
        max_iterations = 7
        cur_iterations = 0

        # Get our replica id
        blockchain = fxConnection.request.getBlockchains()[0]
        replicas = blockchain.get('node_list', None)
        replica_id = replicas[0].get('node_id', None)
        replica_url = "{0}?replica_id={1}".format(
            log_insight_url,
            replica_id
        )

        # Set our time range
        now = datetime.utcnow()
        minutes_ago = now - timedelta(minutes=60)

        # Set our where clause

        where = ""
        extra_where = data.get('where', False)

        if extra_where:
            where = '{0} {1}'.format(where, extra_where)

        query = li_sql_query_builder(where)
        log.info('LI Cloud Query: {0}'.format(query))

        logs_query_job = requests.post(
            replica_url,
            headers=headers,
            data=json.dumps({
                "logQuery": query,
                "start": round(minutes_ago.timestamp()),
                "end": round(now.timestamp())
            }),
            verify=False
        )

        job_response = json.loads(logs_query_job.content)
        job_url = job_response.get('documentSelfLink', None)
        log.info('Job Response: {0}'.format(job_response))

        if job_url is None:
            log.error("Log Insight Task Response: {0}".format(job_response))
            raise Exception("Cannot generate query: {0}".format(job_response))

        # Query task until completed
        while True:
            cur_iterations += 1
            log_response = requests.get(
                '{0}/{1}'.format(log_insight_url, job_url),
                headers=headers,
                verify=False
            )

            log_dict = json.loads(log_response.content)
            state = log_dict.get('taskInfo', {}).get('stage', '')

            if (state in ['FINISHED', 'FAILED']
                    or cur_iterations > max_iterations):
                log.info('LI Cloud Task Result: {} --- {}'.format(
                    state, log_dict
                ))
                break
            else:
                time.sleep(3)

        return log_dict

    return _init


@pytest.mark.smoke
def deploy(deployToSddc):
    # Wait for tasks to finish
    blockchain_id, consortium_id = deployToSddc
    UUID(blockchain_id)
    UUID(consortium_id)

    # Wait for blockchain to deploy
    time.sleep(wait_time)


@pytest.mark.smoke
def test_log_insight_cloud_api(li_cloud_query):
    # Wait for logs to ingest
    time.sleep(wait_time)

    parsed_logs = li_cloud_query()
    results = parsed_logs.get('logQueryResults', None)
    log.info('Log Query Results: {0}'.format(results))
    assert len(results) != 0


@pytest.mark.smoke
def test_log_insight_concord_log(li_cloud_query):
    parsed_logs = li_cloud_query(data={
        "where": "service_name='concord AND'"
    })

    results = parsed_logs.get('logQueryResults', None)
    log.info('Log Query Results: {0}'.format(results))
    assert len(results) != 0


@pytest.mark.smoke
def test_log_insight_concord_log(li_cloud_query):
    parsed_logs = li_cloud_query(data={
        "where": "service_name='concord AND'"
    })

    results = parsed_logs.get('logQueryResults', None)
    log.info('Log Query Results: {0}'.format(results))
    assert len(results) != 0


@pytest.mark.smoke
def test_log_insight_concord_message(li_cloud_query):
    parsed_logs = li_cloud_query(data={
        "where": "service_name='concord' AND text='sends ReqMissingDataMsg AND'"
    })

    results = parsed_logs.get('logQueryResults', None)
    log.info('Log Query Results: {0}'.format(results))
    assert len(results) != 0


# @pytest.mark.smoke
# def test_log_insight_daml_api_log(li_cloud_query):
#     parsed_logs = li_cloud_query(data={
#         "where": "service_name='daml_ledger_api AND'"
#     })

#     results = parsed_logs.get('logQueryResults', None)
#     log.info('Log Query Results: {0}'.format(results))
#     assert len(results) != 0


# @pytest.mark.smoke
# def test_log_insight_daml_waiting(li_cloud_query):
#     # Deploy some DAML contracts
#     try:
#         daml_helper.upload_test_tool_dars(host='localhost', port='6861')
#         daml_helper.verify_ledger_api_test_tool(host='ledger', port='6865')
#     except Exception as e:
#         log.error(e)
#         pass
#     # Wait for logs to ingest
#     time.sleep(wait_time)

#     parsed_logs = li_cloud_query(data={
#         "where": "service_name='daml_ledger_api' AND message='Waiting for Concord to be ready AND'"
#     })

#     results = parsed_logs.get('logQueryResults', None)
#     log.info('Log Query Results: {0}'.format(results))
#     assert len(results) != 0


# @pytest.mark.smoke
# def test_log_insight_daml_api_correlation(li_cloud_query):

#     parsed_logs = li_cloud_query(data={
#         "where": "service_name='daml_ledger_api' AND text='correlationId AND'"
#     })

#     results = parsed_logs.get('logQueryResults', None)
#     log.info('Log Query Results: {0}'.format(results))
#     assert len(results) != 0


# @pytest.mark.smoke
# def test_log_insight_daml_index_db(li_cloud_query):

#     parsed_logs = li_cloud_query(data={
#         "where": "service_name='daml_index_db AND'"
#     })

#     results = parsed_logs.get('logQueryResults', None)
#     log.info('Log Query Results: {0}'.format(results))
#     assert len(results) != 0


@pytest.mark.smoke
def test_log_insight_daml_execution_engine(li_cloud_query):

    parsed_logs = li_cloud_query(data={
        "where": "service_name='daml_execution_engine' AND text='Server started, AND'"
    })

    results = parsed_logs.get('logQueryResults', None)
    log.info('Log Query Results: {0}'.format(results))
    assert len(results) != 0
