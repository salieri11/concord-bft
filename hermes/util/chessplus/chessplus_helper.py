#!/usr/bin/env python3
############################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential #
############################################################################
#
# CLI version (standalone) utility file to run chess plus on supplied
# daml participant node IPs

import util.helper as helper
import util.daml.daml_helper as daml_helper
import util.hermes_logging as hermes_logging

log = hermes_logging.getMainLogger()

KNOWN_SDK_SPIDER_VERSION_MAPPINGS = daml_helper.get_sdk_spider_version_mappings()
DEFAULT_TEST_TIMEOUT = "1800" # 30 mins in seconds
DEFAULT_MARKET_FLAVOR = "sample"
DEFAULT_CONCURRENCY = "3"
DEFAULT_NO_OF_REQUESTS = "1"
CHESSPLUS_TEST_LAUNCHER = "util/chessplus/chessplus_test_launcher.sh"
chess_plus_run_timeout = {
   DEFAULT_MARKET_FLAVOR: DEFAULT_TEST_TIMEOUT,
   "nfr": "432000"
}

def get_test_timeout(market_flavor):
   try:
      return chess_plus_run_timeout[market_flavor]
   except:
      log.warning("Market flavor '{}' not found. Returning default timeout {}".format(market_flavor, DEFAULT_TEST_TIMEOUT))
      return DEFAULT_TEST_TIMEOUT


def run_chess_plus(args, participant_ip):
   '''
   Run chess plus hitting a participant IP
   :param args: Command line arguments
   :param participant_ip: Participant node IP
   :param results_dir: Results log directory
   :return: Chess plus run status
   '''
   try:
      daml_sdk_version = daml_helper.get_ledger_api_version(participant_ip,
                                                            results_dir=args.resultsDir)
      log.debug("daml sdk version: {}".format(daml_sdk_version))

      if args.spiderImageTag and args.spiderImageTag != "autoFetch":
         spider_image_tag = args.spiderImageTag
      else:
         spider_image_tag = daml_helper.get_spider_version(daml_sdk_version,
                                                           args.dockerHubUser,
                                                           args.dockerHubPassword)
      log.debug("spider image tag: {}".format(spider_image_tag))
      if spider_image_tag:
         daml_helper.download_spider_app(spider_image_tag)
      else:
         raise Exception("No Spider image tag.")

   except Exception as e:
      log.error("Failed to get DAML SDK or Spider.")
      raise

   cmd = [
      "timeout", get_test_timeout(args.marketFlavor),
      "bash", CHESSPLUS_TEST_LAUNCHER,
      "--ledgerHost", participant_ip,
      "--spiderImageTag", spider_image_tag,
      "--marketFlavor", args.marketFlavor,
      "--damlSDKVersion", daml_sdk_version,
      "--concurrency", args.concurrency,
      "--noOfRequests", args.noOfRequests,
      "--resultsDir", args.resultsDir,
      "--logLevel", str(args.logLevel)
   ]

   status, output = helper.execute_ext_command(cmd)
   return status


