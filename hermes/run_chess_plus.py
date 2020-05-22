#!/usr/bin/env python3
############################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential #
############################################################################
#
# CLI version (standalone) utility file to run chess plus on supplied
# daml participant node IPs


import argparse
import os
import sys
import time
import util.helper as helper
import util.daml.daml_helper as daml_helper
import util.hermes_logging as hermes_logging

log = hermes_logging.getMainLogger()

KNOWN_SDK_SPIDER_VERSION_MAPPINGS = daml_helper.get_sdk_spider_version_mappings()
DEFAULT_TEST_TIMEOUT = "1800" # 30 mins in seconds
DEFAULT_MARKET_FLAVOR = "sample"
DEFAULT_CONCURRENCY = "3"
DEFAULT_LOGS_DEST="/var/log/blockchain_support"


def parse_arguments():
   parser = argparse.ArgumentParser()
   parser.add_argument("--logLevel",
                       help="Set the log level.  Valid values:"
                       "'DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL'",
                       default="INFO")
   parser.add_argument("--damlParticipantIPs", required=True,
                       help="List of comma separated DAML participant node IPs")
   parser.add_argument("--spiderImageTag", default=None,
                       help="Spider image tag, optional. If not passed in, an appropriate one will be determined.")
   parser.add_argument("--marketFlavor", default=DEFAULT_MARKET_FLAVOR,
                       help="market flavor (sample, cde7, etc)")
   parser.add_argument("--concurrency", default=DEFAULT_CONCURRENCY,
                       help="Concurrency")
   parser.add_argument("--resultsDir",
                       default="{}/logs_{}".format(DEFAULT_LOGS_DEST,
                                                   time.strftime(
                                                      "%Y-%m-%d-%H-%M-%S",
                                                      time.gmtime())),
                       help="Chess+ run logs dir")
   parser.add_argument("--testTimeout", default=DEFAULT_TEST_TIMEOUT,
                       help="Max test timeout in seconds (default: {})".format(DEFAULT_TEST_TIMEOUT))
   parser.add_argument("--dockerHubUser", default="blockchainrepositoryreader",
                       help="DockerHub user which has read access to the digitalasset private repos. " \
                       "Only needed if the DAML SDK version is not one of {}.".format(list(KNOWN_SDK_SPIDER_VERSION_MAPPINGS.keys())))
   parser.add_argument("--dockerHubPassword",
                       help="DockerHub password which has read access to the digitalasset private repos. " \
                       "Only needed if the DAML SDK version is not in {}.".format(list(KNOWN_SDK_SPIDER_VERSION_MAPPINGS.keys())))

   return parser.parse_args()


def run_chess_plus(args, participant_ip, results_dir):
   '''
   Run chess plus hitting a participant IP
   :param args: Command line arguments
   :param participant_ip: Participant node IP
   :param results_dir: Results log directory
   :return: Chess plus run status
   '''
   try:
      daml_sdk_version = daml_helper.get_ledger_api_version(participant_ip)

      if args.spiderImageTag:
         spider_image_tag = args.spiderImageTag
      else:
         spider_image_tag = daml_helper.get_spider_version(daml_sdk_version,
                                                           args.dockerHubUser,
                                                           args.dockerHubPassword)
      daml_helper.download_spider_app(spider_image_tag)

   except Exception as e:
      log.error("Failed to get DAML SDK or Spider version.")
      raise

   cmd = [
      "timeout", args.testTimeout,
      "bash", "util/run_chess_plus.sh",
      "--ledgerHost", participant_ip,
      "--spiderImageTag", spider_image_tag,
      "--marketFlavor", args.marketFlavor,
      "--damlSDKVersion", daml_sdk_version,
      "--concurrency", args.concurrency,
      "--resultsDir", results_dir
   ]

   status, output = helper.execute_ext_command(cmd)
   return status


def main():
   args = parse_arguments()
   over_all_status = None
   args.logLevel = hermes_logging.logStringToInt(args.logLevel)

   if not os.path.isdir(args.resultsDir):
      os.makedirs(args.resultsDir, exist_ok=True)

   hermes_logging.setUpLogging(args)
   log.info("Args: {}".format(args))

   for ledger_api_host in args.damlParticipantIPs.split(","):
      log.info("**** DAML ledger host: {}".format(ledger_api_host))
      results_dir = args.resultsDir + "_{}".format(ledger_api_host.replace('.', '-'))
      status = run_chess_plus(args, ledger_api_host, results_dir)
      if status:
         log.info("**** Run Passed")
         if over_all_status is None:
            over_all_status = True
      else:
         log.error("**** Run Failed")
         over_all_status = False
      log.info("Logs at: {}/run.log".format(results_dir))

   if over_all_status:
      log.info("Overall Run status: PASS")
      sys.exit(0)
   else:
      log.info("Overall Run status: FAIL")
      sys.exit(1)


if __name__ == "__main__":
   main()
