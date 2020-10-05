#################################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# This suite covers the tests related to long_running_tests
# Usage: ./main.py LongRunningTests --blockchainType daml
#                                  --blockchainLocation onprem/sddc
#                                  --numReplicas 7
#                                  --numParticipants 1
#                                  --deploymentOrg system_test_master
#                                  --deploymentService staging
#                                  --zoneConfig resources/zone_config_lrt.json
#
# Alternate Usage: ./main.py LongRunningTests --replicasConfig <path to replicasConfig.json>
# Test deploys Blockchain to sddc if --replicasConfig paramater not passed

#################################################################################

import time
import json
import pytest
import util.helper as helper
from suites.case import describe
from fixtures.common_fixtures import fxProduct, fxBlockchain
import util.hermes_logging

log = util.hermes_logging.getMainLogger()
helper.disable_duplicate_logs()

@describe("Long Running Tests")
def test_long_run(fxHermesRunSettings, fxBlockchain):
   args = fxHermesRunSettings["hermesCmdlineArgs"]
   if args.replicasConfig:
      replicas_config = args.replicasConfig
   else:
      blockchain_type = args.blockchainType.lower()
      blockchain_location = args.blockchainLocation.lower()
      if blockchain_type == util.helper.TYPE_DAML and \
            blockchain_location in [util.helper.LOCATION_SDDC, util.helper.LOCATION_ONPREM]:
         replicas_config = util.helper.REPLICAS_JSON_PATH
      else:
         log.warning(
            "Either --replicasConfig should be passed OR blockchainType must be 'daml' and blockchainLocation must be 'onprem / sddc'- Test Failed")

   all_replicas_and_type = helper.parseReplicasConfig(replicas_config)
   replica_json_data = json.dumps(all_replicas_and_type, sort_keys=True, indent=4)
   log.info("Initializing & Monitoring for blockchain type/replicas: {}".format(
      replica_json_data))

   log.info("Run Duration: {} hrs".format(args.runDuration))
   log.info("Load Interval: {} mins".format(args.loadInterval))
   log.info("Support bundle destination: {}".format(args.resultsDir))

   log.info("************************************************************")
   status = helper.installHealthDaemon(all_replicas_and_type)
   start_time = time.time()
   result = False
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
         result = True
      else:
         end_time = time.time()
         log.error("**** Blockchain FAILED to be active for {} hrs".format(
            args.runDuration))
         log.error("**** Blockchain sustained only for {} hrs".format(
            (end_time - start_time) / 3600))
         if args.replicasConfig:
            log.info(helper.longRunningTestDashboardLink(args.replicasConfig))
   else:
      log.error(
         "**** Failed to install status monitoring daemon on nodes: {}".format(
            all_replicas_and_type))
   log.info("Overall Run status: {}".format(result))
   assert result, "Long Run Failed"
