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


def run_chess_plus(args, participant_ip):
   '''
   Run chess plus hitting a participant IP
   :param args: Command line arguments
   :param participant_ip: Participant node IP
   :param results_dir: Results log directory
   :return: Chess plus run status
   '''
   try:
      daml_sdk_version = daml_helper.get_ledger_api_version(participant_ip)

      if args.spiderImageTag and args.spiderImageTag != "autoFetch":
         spider_image_tag = args.spiderImageTag
      else:
         spider_image_tag = daml_helper.get_spider_version(daml_sdk_version,
                                                           args.dockerHubUser,
                                                           args.dockerHubPassword)
      if spider_image_tag:
         daml_helper.download_spider_app(spider_image_tag)
      else:
         raise Exception("No Spider image tag.")

   except Exception as e:
      log.error("Failed to get DAML SDK or Spider.")
      raise

   cmd = [
      "timeout", DEFAULT_TEST_TIMEOUT,
      "bash", CHESSPLUS_TEST_LAUNCHER,
      "--ledgerHost", participant_ip,
      "--spiderImageTag", spider_image_tag,
      "--marketFlavor", args.marketFlavor,
      "--damlSDKVersion", daml_sdk_version,
      "--concurrency", args.concurrency,
      "--noOfRequests", args.noOfRequests,
      "--resultsDir", args.resultsDir
   ]

   status, output = helper.execute_ext_command(cmd)
   return status


