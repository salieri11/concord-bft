#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
# CLI version (standalone) of utility to monitor the health of replicas and
# also run periodic tests to verify health & availability of blockchain nodes
# When a crash is detected (replica status or transaction verification failure),
# support logs are collected from all the supplied replicas
#
# Example: python3 monitor_replicas.py
#  --replicas daml_committer:10.70.30.226,10.70.30.225,10.70.30.227,10.70.30.228
#  --replicas daml_participant:10.70.30.226,10.70.30.225,10.70.30.227,10.70.30.228
#  --runDuration 1
#  --loadInterval 1

import argparse
import json
import os
import sys
import time
from tempfile import NamedTemporaryFile
from util import helper
import util.hermes_logging as hermes_logging

log = hermes_logging.getMainLogger()
DEFAULT_SUPPORT_LOGS_DEST="/var/log/blockchain_support"

def main(args):
   parser = argparse.ArgumentParser()
   parser.add_argument("--logLevel",
                       help="Set the log level.  Valid values:"
                       "'DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL'",
                       default="INFO")
   parser.add_argument("--replicas", action='append', nargs='*',
                       help="Repeated set of blockchain type:<comma separated list of IPs>")
   parser.add_argument("--replicasConfig",
                       help="If replicas are not passed via --replicas, pass in replicas.json file via this option")
   parser.add_argument("--resultsDir",
                       default="{}/logs_{}".format(DEFAULT_SUPPORT_LOGS_DEST,
                                                   time.strftime(
                                                      "%Y-%m-%d-%H-%M-%S",
                                                      time.gmtime())),
                       help="Deployment support bundle archive path")
   parser.add_argument("--runDuration",
                       type=int,
                       default=6,
                       help="No. of hrs to monitor replicas (default 6 hrs)")
   parser.add_argument("--loadInterval",
                       type=int,
                       default=60,
                       help="Minutes to wait between monitors (default 60 mins)")
   parser.add_argument("--testset",
                       required=True,
                       help="Set of test sets to be picked up from testlist file.  e.g. " \
                       "'basic_tests'")
   parser.add_argument("--testlistFile",
                       help="json file containing the list of tests",
                       default=helper.LONG_RUN_TEST_FILE)
   parser.add_argument("--su",
                       action="store_true", # implies default=False
                       help="Super user privilege with all Jenkins injected credentials available.")
   parser.add_argument("--notifyTarget",
                       help="Slack channel name or email address, default will skip notification",
                       default=None)
   parser.add_argument("--notifyJobName",
                       help="Shortened job name running this monitoring script",
                       default=None)

   args = parser.parse_args()

   if not os.path.exists(args.resultsDir):
      os.makedirs(args.resultsDir)

   hermes_logging.setUpLogging(args)

   if args.su: helper.WITH_JENKINS_INJECTED_CREDENTIALS = True
   if not args.replicasConfig and not args.replicas:
      log.error("Usage: pass either --replicas (or) --replicasConfig")
      sys.exit(1)

   all_replicas_and_type = {}

   if args.replicasConfig:
      replicas_config = args.replicasConfig
      all_replicas_and_type = helper.parseReplicasConfig(args.replicasConfig)
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

   replica_json_data = json.dumps(all_replicas_and_type, sort_keys=True, indent=4)
   log.info("Initializing & Monitoring for blockchain type/replicas: {}".format(
      replica_json_data))
   log.info("Run Duration: {} hrs".format(args.runDuration))
   log.info("Load Interval: {} mins".format(args.loadInterval))
   log.info("Support bundle destination: {}".format(args.resultsDir))

   log.info("")
   log.info("************************************************************")
   status = helper.installHealthDaemon(all_replicas_and_type)
   start_time = time.time()
   if status:
      log.info("**** Successfuly instantiated health monitoring daemon on all replicas")
      if helper.monitor_replicas(replicas_config,
                                 args.runDuration,
                                 args.loadInterval,
                                 args.resultsDir,
                                 args.testlistFile,
                                 args.testset,
                                 args.notifyTarget,
                                 args.notifyJobName):
         log.info("**** Blockchain successfully active for {} hrs".format(
            args.runDuration))
         if args.replicasConfig:
            log.info(helper.longRunningTestDashboardLink(args.replicasConfig))
      else:
         end_time = time.time()
         log.error("**** Blockchain FAILED to be active for {} hrs".format(
            args.runDuration))
         log.error("**** Blockchain sustained only for {} hrs".format(
            (end_time - start_time) / 3600))
         if args.replicasConfig:
            log.info(helper.longRunningTestDashboardLink(args.replicasConfig))
         sys.exit(1)
   else:
      log.error(
         "**** Failed to install status monitoring daemon on nodes: {}".format(
            all_replicas_and_type))
      sys.exit(1)

if __name__ == '__main__':
   main(sys.argv)
