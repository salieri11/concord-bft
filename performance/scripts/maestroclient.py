##########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
##########################################################################

import argparse
import getpass
import json
import logging
import os
import subprocess
import traceback

import requests
import urllib3
import time


# suppress certs verification warnings
urllib3.disable_warnings()


class MaestroClient:
    """
    Client that interacts with Maestro
    """

    def __init__(self, api_url, reporting_url, username, password):
        """
        Args:
            api_url (string): URL to send api requests
            reporting_url (string): URL for getting reporting information for a run
            username (string): LDAP username
            password (string): LDAP password
        """
        self._username = username.strip()
        self._password = password.strip()
        self._auth = (self._username, self._password)
        self._api_url = api_url
        self._reporting_url = reporting_url

    def get_payload(self, bcversion, recipients, application, comments, noexec=False):
        """
        Return json payload to send to the api server.  Inserts the blockchain version,
        recipients, and comments.
        """
        if noexec:
            logging.debug("get_payload({}, {}, {})".format(bcversion, recipients, comments))

        (commit_ids, commit_summary, authors) = self.get_commit_info()

        return {
            "run_request": {
                "filters": {
                    "config": {
                        "region": "reg005",
                        "cacheType": "",
                        "cacheDisks": "",
                        "capacityType": "",
                        "capacityDisks": "",
                        "resourceGroup": "",
                        "cpuModel": ""
                    },
                    "hostType": "",
                    "useFilters": True,
                    "showOutOfService": False,
                    "resourceSelection": [],
                    "resourceTypeChosen": "",
                    "isSelectionDisabled": False
                },
                "config": {
                    "releaseVersion": bcversion,
                    "threads": 16,
                    "batching": False,
                    "application": application,
                    "damlVersion": "1.0.0",
                    "transactions": 1000,
                    "spiderVersion": "1.25.162",
                    "datasetFlavour": "cde7",
                    "clusterType": "Intel",
                    "vCPUCommitter": 8,
                    "vHDDCommitter": 64,
                    "vMemoryCommitter": 64,
                    "vCPUParticipant": 16,
                    "vHDDParticipant": 64,
                    "vMemoryParticipant": 64,
                    "additionalJson": "",
                    "maxQueueSize": 100,
                    "maxBatchSizeBytes": 4194304,
                    "maxWaitMillis": 100,
                    "maxConcurrentCommits": 5,
                    "operation": "RunTest",
                    "git_commits": {
                        "ids": commit_ids,
                        "commits": commit_summary,
                        "authors": authors
                    }
                },
                "workload": "ap-blockchain",
                "builds": {},
                "cc": recipients,
                "comments": comments,
                "testName": "Blockchain CI Runs"
            }
        }

    def submit_run(self, bcversion, recipients, app, comments=None, noexec=False):
        """
        Create a run for the given blockchain version.
        Results will be emailed to recipients.
        """
        endpoint = self._api_url + '/ap/submit_job/'
        payload = self.get_payload(bcversion, recipients, app, comments, noexec)
        logging.info(
            "Submitting performance run. Version: '{}', recipients: '{}', comments: '{}'".format(bcversion, recipients,
                                                                                                 comments))

        if noexec:
            return 1234
        else:
            response = requests.post(
                endpoint, auth=self._auth, verify=False, json=payload)
            response_json = response.json()

            try:
                response.raise_for_status()
                return response_json['results']
            except Exception:
                # get specific exception message from submit_template endpoint
                raise Exception(response.json())

    def start_run(self, run_id, noexec=False):
        """
        Start the maestro run based on run_id
        """
        endpoint = self._api_url + '/vdi/start_job/' + str(run_id)

        if noexec:
            logging.debug("start_run({})".format(run_id))
            pass
        else:
            response = requests.get(
                endpoint, auth=self._auth, verify=False)
            logging.debug(response.json())
            response.raise_for_status()

    def write_results(self, run_ids, apps, html_file, json_file):
        """
        Write results to an html file and a json file.
        """

        logging.info("Writing results to {}".format(html_file))
        urls = []
        with open(html_file, "a") as f:
            html_results = "<html>"
            for index in range(len(run_ids)):
                url = "{}/{}/".format(self._reporting_url, run_ids[index])
                html_results = html_results + "<a href={}>{} Performance run {}</a>".format(url, apps[index],
                                                                                            run_ids[index]) + "<br>"
                urls.append(url)

            html_results += "</html>"
            f.write(html_results)

        logging.info("Writing results to {}".format(json_file))
        results = {
            "run_ids": run_ids[index],
            "urls": urls
        }
        with open(json_file, "w") as f:
            json.dump(results, f)

    def get_commit_info(self):
        """
        Fetch commtit info in this master build
        Args:

        Returns:
            list of commits and summary
        """
        commits = []
        author_emails = []
        commit_ids = []
        try:
            commits_output = subprocess.run([
                'git', 'log', 'origin/master..',
                '--pretty=format:"%H%x09%aN%x09%aE%x09%aD%x09%s"'
                # More info: https://mirrors.edge.kernel.org/pub/software/scm/git/docs/git-log.html
                # %H = Full Commit Hash
                # %x09 = Tab Character (char code 9)
                # %aN = Author Name (respecting .mailmap)
                # %x09 = Tab Character (char code 9)
                # %aE = Author Email (respecting .mailmap)
                # %x09 = Tab Character (char code 9)
                # %aD = Author Date (RFC2822 style)
                # %x09 = Tab Character (char code 9)
                # %s = Commit Summary
            ], stdout=subprocess.PIPE).stdout.decode('utf-8')
            commits_output = commits_output.split('\n')

            for line in commits_output:
                commit_info_str = line.strip()
                # Ignore null line
                if commit_info_str is None or len(commit_info_str) == 0:
                    continue
                # strip unwanted quotes
                while commit_info_str.startswith('"') and commit_info_str.endswith('"'):
                    commit_info_str = commit_info_str[1:-1]
                commit_info = commit_info_str.split('\t')
                if len(commit_info) != 5:
                    logging.info("git log is not displaying results as expected, line: %s" % commit_info_str)
                    continue
                author_email = commit_info[2]
                summary = commit_info[4]
                if summary.startswith("Merge branch ") or author_email == "vmwathenabot@vmware.com":
                    logging.info("Merging commit skipped, '%s'" % summary)
                    continue

                author_emails.append(commit_info[2])
                commits.append(commit_info[4])
                commit_ids.append(commit_info[0])
            logging.info(commits)
            logging.info(author_emails)
            logging.info(commit_ids)
        except Exception as e:
            logging.info("Error while fetching commit information %s" % format(e))
        return commit_ids, commits, author_emails



def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--apiUrl",
                        help="Where to send API calls",
                        default="https://bc.maestro.eng.vmware.com:8001")
    parser.add_argument("--reportingUrl",
                        help="Where to get reporting information",
                        default="http://10.153.243.52/maestro_production")
    parser.add_argument("--username",
                        required=True,
                        help="The username")
    parser.add_argument("--password",
                        help="The password.  Will be prommpted if not entered.",
                        default=None)
    parser.add_argument("--recipients",
                        help="Comma delimited list of VMware IDs which will " \
                             "receive results.",
                        default=[])
    parser.add_argument("--comments",
                        help="Annotation sent to Maestro",
                        default=None)
    parser.add_argument("--bcversion",
                        required=True,
                        help="Blockchain version number to test")
    parser.add_argument("--noexec",
                        help="Don't send jobs to the server.  For testing.",
                        action='store_true',
                        default=False)
    parser.add_argument("--logLevel",
                        help="Set the log level.  Valid values:"
                             "'DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL'",
                        default="INFO")
    parser.add_argument("--htmlFile",
                        help="HTML file containing run information.",
                        default="results.html")
    parser.add_argument("--resultsFile",
                        help="JSON file containing run information.",
                        default="results.json")
    parser.add_argument("--testList",
                        help="list of tests like chess_plus, IOU etc",
                        default="IOU")

    args = parser.parse_args()

    if not args.password:
        args.password = getpass.getpass()

    logging.basicConfig(level=args.logLevel.upper())

    client = MaestroClient(args.apiUrl, args.reportingUrl, args.username, args.password)
    app_list = args.testList.split(",")
    run_ids = []

    for app in app_list:
        comment = app + ": " + args.comments
        logging.info("Starting performance run with build: {}, recipients: {}, comments: {}". \
                     format(args.bcversion,
                            args.recipients,
                            comment))
        run_id = client.submit_run(args.bcversion,
                                   args.recipients,
                                   app, comment,
                                   args.noexec)
        client.start_run(run_id, args.noexec)
        run_ids.append(run_id)
        time.sleep(10)

    client.write_results(run_ids, app_list, args.htmlFile, args.resultsFile)

main()