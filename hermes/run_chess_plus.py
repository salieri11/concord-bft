#!/usr/bin/env python3
############################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential #
############################################################################
#
# CLI version (standalone) utility file to run chess plus on supplied
# daml participant node IPs


import argparse
import sys
import time
import util.helper as helper
import util.daml.daml_helper as daml_helper
import util.hermes_logging as hermes_logging

log = hermes_logging.getMainLogger()

DEFAULT_TEST_TIMEOUT = "1800" # 30 mins in seconds
DEFAULT_SPIDER_IMAGE_TAG = "1.25.300"
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
   parser.add_argument("--spiderImageTag", default=DEFAULT_SPIDER_IMAGE_TAG,
                       help="Spider image tag")
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

   return parser.parse_args()

def run_chess_plus(participant_ip, spider_image_tag, market_flavor,
                   concurrency, results_dir, test_timeout):
   '''
   Run chess plus hitting a participant IP
   :param participant_ip: Participant node IP
   :param spider_image_tag: Spider Image TAG
   :param market_flavor: Market flavor (sample, cde7)
   :param concurrency: Concurrency of transactions
   :param results_dir: Results log directory
   :return: Chess plus run status
   '''
   try:
      daml_sdk_version = daml_helper.get_ledger_api_version(participant_ip)
   except Exception as e:
      log.error("Failed to get DAML SDK Version: {}".format(e))
      sys.exit(1)

   cmd = [
      "timeout", test_timeout,
      "bash", "util/run_chess_plus.sh",
      "--ledgerHost", participant_ip,
      "--spiderImageTag", spider_image_tag,
      "--marketFlavor", market_flavor,
      "--damlSDKVersion", daml_sdk_version,
      "--concurrency", concurrency,
      "--resultsDir", results_dir
   ]

   status, output = helper.execute_ext_command(cmd)
   return status

def main():
   args = parse_arguments()
   over_all_status = None

   args.logLevel = hermes_logging.logStringToInt(args.logLevel)
   hermes_logging.setUpLogging(args)
   log.info("Args: {}".format(args))

   for ledger_api_host in args.damlParticipantIPs.split(","):
      log.info("**** DAML ledger host: {}".format(ledger_api_host))
      results_dir = args.resultsDir + "_{}".format(ledger_api_host.replace('.', '-'))

      status = run_chess_plus(ledger_api_host, args.spiderImageTag,
                     args.marketFlavor, args.concurrency,
                     results_dir, args.testTimeout)
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
