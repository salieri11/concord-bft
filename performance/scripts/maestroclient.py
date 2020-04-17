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

        (commit_summary, authors) = self.get_commit_info()

        return {
            "run_request": {
                "config": {
                    "vsanHFT": 0,
                    "vsanChecksum": False,
                    "vsanPrepDisk": True,
                    "vsanSnapshot": False,
                    "vsanDropCache": True,
                    "vsanRaidLevel": "RAID 0",
                    "vsanEncryption": False,
                    "vsanEsxcfgCmds": "",
                    "vsanStatsEsxtop": False,
                    "vsanStripeWidth": 1,
                    "vsanVmkbootCmds": "",
                    "vdiWorkloadTests": [{
                        "name": "t1_5i_0r",
                        "protocol": "PCOIP",
                        "rdshType": 0,
                        "iteration": 5,
                        "thinkTime": 5,
                        "rampUpTime": 5,
                        "blastCodecs": "H264",
                        "description": "runprofile for maestro run",
                        "workloadList": ["vp_SuperpositionPro"],
                        "defaultConfig": True,
                        "workGroupName": "workGroup_1",
                        "runProfileName": "runProfile_1",
                        "workProfileName": "standardTestProfile_chrome",
                        "vp_superPosition": {
                            "gpuSuperPositionApi": "Directx",
                            "gpuSuperPositionQuality": "High",
                            "gpuSuperPositionTexture": "High",
                            "gpuSuperPositionResolution": "1920X1080"
                        }
                    }],
                    "vsanPostbootCmds": "",
                    "vsanStatsPerfsvc": False,
                    "vsanStatsVmkperf": False,
                    "vsanStatsNetstats": False,
                    "vsanStatsObserver": False,
                    "vsanStatsVmkstats": False,
                    "vsanNumActiveHosts": 1,
                    "vsanPostenableCmds": "",
                    "vsanStatsVmSupport": False,
                    "vsanStatsVsantraces": False,
                    "vsanCacheReservation": 0,
                    "vsanDedupCompression": False,
                    "vsanForceProvisioning": False,
                    "vsanWorkloadCfgHostSize": 0,
                    "vsanWorkloadCfgVmdkType": "thick",
                    "vsanProportionalCapacity": 0,
                    "vsanStatsPerfsvcInterval": 300,
                    "vsanWorkloadCfgNumberOfVMs": 1,
                    "vsanWorkloadCfgSizePercent": 0,
                    "vsanWorkloadCfgNumberOfDisks": 1,
                    "vsanWorkloadCfgSnapshotPrepPct": 0,
                    "vsanWorkloadCfgSnapshotPrepRandPct": 0
                },
                "bcConfig": {
                    "BlockChainService": {
                        "releaseVersion": bcversion
                    },
                    "Benchmark": {
                        "application": application,
                        "daml_version": "0.13.56-snapshot.20200331.3729.0.b43b8d86",
                        "spider_version": "SNAPSHOT-1.25.0-20200414-1643-8e1346b1",
                        "dataset_flavour": "sample",
                        "transactions": 50000,
                        "threads": 16,
                        "batching": False
                    },
                    "git_commits": {
                        "commits": commit_summary,
                        "authors": authors
                    }
                },
                "viewplannerConfig": {
                    "RDSFarm": {
                        "farmType": "AUTOMATED",
                        "cloneType": "LINKED",
                        "maxRDSServers": 1,
                        "customSpecType": "SYS_PREP",
                        "maxSessionType": "UNLIMITED",
                        "numberOfSessions": 10,
                        "defaultDisplayProtocol": "PCOIP"
                    },
                    "runMode": "LOCAL",
                    "useVSAN": False,
                    "AppVolume": {
                        "buildId": "",
                        "buildUrl": "",
                        "releaseId": "",
                        "addWritable": False,
                        "customAppStackUrl": ""
                    },
                    "vdiServer": {
                        "viewBuild": "ob-14584133",
                        "viewAgentBuild": "ob-14590940"
                    },
                    "viewAgent": {
                        "cloneType": "LINKED"
                    },
                    "clientPool": {
                        "numberOfVMs": 2
                    },
                    "coreStorage": {
                        "dataStoreVersion": "vmfs6"
                    },
                    "desktopPool": {
                        "poolType": "AUTOMATED",
                        "cloneType": "INSTANT",
                        "userAssign": "FLOATING",
                        "numberOfVMs": 2,
                        "customSpecType": "CLONE_PREP",
                        "windowsVersion": "Desk",
                        "windowsVersionNumber": "Windows-10",
                        "defaultDisplayProtocol": "PCOIP"
                    },
                    "viewplanner": {
                        "version": 4,
                        "testMode": "local",
                        "cloneType": "INSTANT",
                        "vsanSetup": False,
                        "runProfile": {
                            "iteration": 1,
                            "workGroup": {
                                "addVdi": True,
                                "protocol": "PCOIP",
                                "rdshtype": 0,
                                "workprofile": {
                                    "name": "standardTestProfile_chrome",
                                    "workload": "",
                                    "description": ""
                                }
                            }
                        },
                        "collectVPStats": False,
                        "supportAppVolume": False,
                        "viewplannerBuild": "ob-15659297"
                    },
                    "uploadFileConfig": {
                        "rerunId": "810",
                        "runCommand": "",
                        "fileLocation": "http://10.153.243.52/rahulRuns/810/pc_logs/custom_script/",
                        "uploadedFiles": []
                    },
                    "gpuConfigurations": {
                        "useGPU": False,
                        "gpuType": "vGPU",
                        "gpuProfile": "grid_p40-1q",
                        "esxVibLocation": "ftp://prmh-gfxdev-nas02.eng.vmware.com/nvidia/signed/NVIDIA-VMware_ESXi_6.7_Host_Driver-440.43-1OEM.670.0.0.8169922.x86_64.vib",
                        "decodingGpuType": "vGPU",
                        "hardwareDecoding": False,
                        "hardwareEncoding": True,
                        "decodingGpuProfile": "",
                        "nvidiaDriverLocation": "ftp://prmh-gfxdev-nas02.eng.vmware.com/nvidia/signed/441.66_grid_win10_server2016_server2019_64bit_international.exe",
                        "decodingEsxVibLocation": "",
                        "decodingNvidiaDriverLocation": ""
                    },
                    "nsxConfigurations": {
                        "useNSX": False,
                        "nsxBuild": ""
                    },
                    "advanceHardwareConfig": {
                        "clientMemory": 4,
                        "clientStorage": 50,
                        "desktopMemory": 8,
                        "desktopStorage": 50,
                        "clientNumberCpu": 2,
                        "desktopVramSize": 16,
                        "viewClientBuild": "",
                        "vmwareToolBuild": "",
                        "desktopNumberCpu": 2,
                        "updateClientConfig": False,
                        "updateDesktopConfig": True,
                        "desktopGraphicsMemory": 500,
                        "provideViewClientBuild": False,
                        "provideVmwareToolBuild": False
                    }
                },
                "filters": {
                    "config": {
                        "region": "reg005",
                        "gpuType": "",
                        "cpuModel": "",
                        "cacheType": "INTEL-NVMe-1526185MB-SSD",
                        "cacheDisks": "1",
                        "gpuCapacity": "",
                        "capacityType": "INTEL-SATA-915715MB-SSD",
                        "clientRegion": "",
                        "capacityDisks": "1",
                        "clientGpuType": "",
                        "resourceGroup": "vsan-perf",
                        "clientCpuModel": "",
                        "clientCacheType": "",
                        "clientCacheDisks": "",
                        "clientGpuCapacity": "",
                        "clientCapacityType": "",
                        "clientCapacityDisks": "",
                        "clientResourceGroup": ""
                    },
                    "hostType": "",
                    "useFilters": True,
                    "showOutOfService": False,
                    "resourceSelection": [],
                    "resourceTypeChosen": "anyHosts",
                    "isSelectionDisabled": False,
                    "clientResourceTypeChosen": ""
                },
                "hostsCount": 1,
                "workload": "vdi-viewplanner",
                "builds": {
                    "vcBuild": "10.158.180.93",
                    "esxBuild": "ob-14320388"
                },
                "cc": recipients,
                "testName": "Performance CI Runs",
                "comments": comments,
                "resources": "1"
            }
        }

    def submit_run(self, bcversion, recipients, app, comments=None, noexec=False):
        """
        Create a run for the given blockchain version.
        Results will be emailed to recipients.
        """
        endpoint = self._api_url + '/vdi/submit_job/'
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
        commit_summary = []
        author_emails = []

        try:
            python = "python3"
            output = subprocess.run([python, "/vars/get-commits-blame.py"] ,stdout=subprocess.PIPE).stdout.decode('utf-8')
            logging.debug(output)

            commits_blame = json.load(open("/vars/commits_authors.json"))
            author_emails = commits_blame["authorsList"]

            for commit in commits_blame["commits"]:
                commit_summary.append(commit["summary"])

            logging.debug(author_emails)
            logging.debug(commit_summary)

        except Exception as e:
            logging.debug("Error while fetching commit information %s" % format(e))
            traceback.print_exc()
        return commit_summary, author_emails



def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--apiUrl",
                        help="Where to send API calls",
                        default="http://10.158.177.18:8001")
    parser.add_argument("--reportingUrl",
                        help="Where to get reporting information",
                        default="http://10.153.243.52/blockchainRuns")
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
