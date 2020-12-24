#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
# CLI version (standalone) of utility to monitor the health of replicas and
# also run periodic tests to verify health & availability of blockchain nodes
# When a crash is detected (replica status or transaction verification failure),
# support logs are collected from all the supplied replicas
#
# Example: python3 -m pytest suites/monitor_replicas_test.py --replicas
# daml_committer:10.70.30.226,10.70.30.225,10.70.30.227,10.70.30.228
# --replicas daml_participant:10.70.30.226,10.70.30.225,10.70.30.227,
# 10.70.30.228 --runDuration 1 --loadInterval 1

import json
import os
import shutil
import time
from tempfile import NamedTemporaryFile

import util.hermes_logging as hermes_logging
from suites.case import describe
from util import helper

log = hermes_logging.getMainLogger()
DEFAULT_SUPPORT_LOGS_DEST = "/var/log/blockchain_support"


@describe("Monitor Replica Tests")
def test_monitor_replicas(fxHermesRunSettings, fxBlockchain):
    args = fxHermesRunSettings["hermesCmdlineArgs"]
    log.info("fxBlockchain:::{}".format(fxBlockchain))
    log.info("args:::::{}".format(args))
    if not os.path.exists(args.resultsDir):
        os.makedirs(args.resultsDir)

    hermes_logging.setUpLogging(args)

    if args.su: helper.WITH_JENKINS_INJECTED_CREDENTIALS = True
    if not args.replicasConfig and not args.replicas:
        log.error("Usage: pass either --replicas (or) --replicasConfig")
        # sys.exit(1)

    all_replicas_and_type = {}

    if args.replicasConfig:
        replicas_config = args.replicasConfig
        log.info("Replica Configuration: {}".format(replicas_config))
        all_replicas_and_type = helper.parseReplicasConfig(args.replicasConfig)
        log.info("all_replicas_and_type:::{}".format(all_replicas_and_type))
    else:
        replicas_config_data = {}
        for item in args.replicas:
            blockchain_type, ips = item[0].split(':')
            replicas_details = []
            for ip in ips.split(','):
                replicas_details.append({"ip": ip})
            replicas_config_data[blockchain_type] = replicas_details

        replicas_config = NamedTemporaryFile(delete=False).name
        with open(replicas_config, "w") as tmp:
            json.dump(replicas_config_data, tmp, indent=True)
        all_replicas_and_type = helper.parseReplicasConfig(replicas_config)

    replica_json_data = json.dumps(all_replicas_and_type, sort_keys=True,
                                   indent=4)
    log.info(
        "Initializing & Monitoring for blockchain type/replicas: {}".format(
            replica_json_data))
    log.info("Run Duration: {} hrs".format(args.runDuration))
    log.info("Load Interval: {} mins".format(args.loadInterval))
    log.info("Support bundle destination: {}".format(args.resultsDir))

    log.info("")
    log.info("************************************************************")
    status = helper.installHealthDaemon(all_replicas_and_type)
    start_time = time.time()
    ret_status = 1
    if status:
        log.info(
            "Successfully instantiated health monitoring daemon on all replicas")
        monitor_status = helper.monitor_replicas(fxBlockchain,
                                                 replicas_config,
                                                 args.runDuration,
                                                 args.loadInterval,
                                                 args.resultsDir,
                                                 args.testlistFile,
                                                 args.testset,
                                                 args.notifyTarget,
                                                 args.notifyJobName)

        end_time = time.time()
        monitored_duration = (end_time - start_time) / 3600
        if monitor_status:
            log.info("")
            log.info("**** Blockchain successfully active for {} hrs".format(
                monitored_duration))
            ret_status = 0
        else:
            log.info("")
            log.error("**** Blockchain FAILED to be active for {} hrs".format(
                args.runDuration))
            log.error(
                "**** Blockchain sustained only for {} hrs".format(
                    monitored_duration))

        # record monitored_duration
        save_monitored_duration(start_time, monitored_duration)
        if args.replicasConfig:
            log.info("")
            log.info(helper.longRunningTestDashboardLink(args.replicasConfig))
    else:
        log.error(
            "**** Failed to install status monitoring daemon on nodes: {}".format(
                all_replicas_and_type))

    # sys.exit(ret_status)


def save_monitored_duration(start_time, monitored_duration):
    '''
   Record monitored duration in hermes-data repo for graph
   :param start_time: Monitoring start time
   :param monitored_duration: monitored duration
   '''
    jenkins_workspace = os.getenv("WORKSPACE")
    if jenkins_workspace:
        log.info("Record monitored duration...")
        file_not_found = False
        run_duration_record_file = "../../hermes-data/longrun_test" \
                                   "/longrun_duration_summary.csv "
        if not os.path.exists(run_duration_record_file):
            file_not_found = True

        with open(run_duration_record_file, "a+") as fp:
            if file_not_found:
                line = "\"Date\",\"Run Duration\"\n"
                fp.write(line)

            start_date = time.strftime('%m/%d/%y', time.localtime(start_time))
            line = "{},{}\n".format(start_date, int(monitored_duration))
            fp.write(line)

        # For graph in Jenkins
        shutil.copy(run_duration_record_file, jenkins_workspace)
