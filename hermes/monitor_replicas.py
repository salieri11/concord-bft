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
#  --replicas daml_committer:10.70.30.226,10.70.30.225,10.70.30.227,10.70.30.228
#  --blockchainLocation <sddc/onprem>
#  --runDuration 1
#  --loadInterval 1

import argparse
import json
import logging
import sys
import time
from util import helper

DEFAULT_SUPPORT_LOGS_DEST="/var/log/blockchain_support"

def main(args):
   parser = argparse.ArgumentParser()
   parser.add_argument("--replicas", action='append', nargs='*',
                       help="Repeated set of blockchain type:<comma separated list of IPs>")
   parser.add_argument("--replicasConfig",
                       help="If replicas are not passed via --replicas, pass in replicas.json file via this option")
   parser.add_argument("--blockchainLocation",
                       required=True,
                       help="Location ({}, {}) of the blockchain being tested".format(
                          util.helper.LOCATION_SDDC,
                          util.helper.LOCATION_ONPREM)),
   parser.add_argument("--saveSupportLogsTo",
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
   args = parser.parse_args()

   if not args.replicasConfig and not args.replicas:
      log.error("Usage: pass either --replicas (or) --replicasConfig")
      sys.exit(1)

   all_replicas = {}

   if args.replicasConfig:
      log.info("Using replicas config: {}".format(args.replicasConfig))
      with open(args.replicasConfig, "r") as fp:
         all_replicas_details = json.load(fp)
         for replica_type in all_replicas_details:
            all_replicas[replica_type] = []
            for replica_info in all_replicas_details[replica_type]:
               if "ip" in replica_info: # committer node returns "ip"
                  all_replicas[replica_type].append(replica_info["ip"])
               elif "public_ip" in replica_info: # participant node returns "public_ip" and "private_ip"
                  all_replicas[replica_type].append(replica_info["public_ip"])
               else:
                  log.error("replica config file should map replica info with property 'ip' or 'public_ip'")
                  sys.exit(1)
   else:
      log.info("Using replicas: {}".format(args.replicas))
      for item in args.replicas:
         blockchain_type, ips = item[0].split(':')
         all_replicas[blockchain_type] = ips.split(',')

   log.info("Initializing & Monitoring for blockchain type/replicas: {}".format(
      json.dumps(all_replicas, sort_keys=True, indent=4)))
   log.info("Run Duration: {} hrs".format(args.runDuration))
   log.info("Load Interval: {} mins".format(args.loadInterval))
   log.info("Support bundle destination: {}".format(args.saveSupportLogsTo))

   log.info("")
   log.info("************************************************************")
   status = helper.installHealthDaemon(all_replicas)
   start_time = time.time()
   if status:
      log.info("**** Successfuly instantiated health monitoring daemon on all replicas")
      if util.helper.monitor_replicas(all_replicas,
                                      args.blockchainLocation,
                                      args.runDuration,
                                      args.loadInterval,
                                      args.saveSupportLogsTo):
         log.info("**** Blockchain successfully active for {} hrs".format(
            args.runDuration))
      else:
         end_time = time.time()
         log.error("**** Blockchain FAILED to be active for {} hrs".format(
            args.runDuration))
         log.error("**** Blockchain sustained only for {} hrs".format(
            (end_time - start_time) / 3600))
         sys.exit(1)
   else:
      log.error(
         "**** Failed to install status monitoring daemon on nodes: {}".format(
            all_replicas))
      sys.exit(1)

if __name__ == '__main__':
   logging.basicConfig(level=logging.INFO,
                       format='%(asctime)s %(levelname)s %(message)s',
                       datefmt='%Y-%m-%d %H:%M:%S')
   log = logging.getLogger(__name__)

   main(sys.argv)
