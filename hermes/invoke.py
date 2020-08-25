#!/usr/bin/python3

#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

# Invocation of standalone utility functions that often
# require Jenkins-injected credentials in user_config

import argparse
import logging
import traceback
import json
import os
import urllib
import tempfile
import base64
import time
from fixtures.common_fixtures import BlockchainFixture
from util import (auth, csp, hermes_logging, helper, slack, mailer, wavefront,
                 racetrack, jenkins, infra, blockchain_ops as ops, vault)
import util.hermes_logging
from suites import case

log = util.hermes_logging.getMainLogger()

def localTestFunction(args, options, secret):
  '''A placeholder function for local testing of new code, pre-commit'''
  # ! temp
  return

def slackDM(args, options, secret):
  a = prepareArgs(args)
  slack.sendMessageToPerson(email=a[0], message=a[1], ts=a[2], token=secret)

def slackPost(args, options, secret):
  a = prepareArgs(args)
  slack.postMessageOnChannel(channelName=a[0], message=a[1], ts=a[2], token=secret)

def slackUpload(args, options, secret):
  a = prepareArgs(args)
  slack.postFileUpload(channelNameOrEmail=a[0], message=a[1], fileName=a[2], filePath=a[3], token=secret)

def slackReportMonitoring(args, options, secret):
  a = prepareArgs(args)
  slack.reportMonitoring(target=a[0], message=a[1])

def emailSend(args, options, secret):
  a = prepareArgs(args)
  mailer.send(email=a[0], subject=a[1], message=a[2], senderName=a[3])

def racetrackSetBegin(args, options, secret):
  a = prepareArgs(args)
  repo = a[0] # default "athena"
  forBuilding = True if a[1] == "building" else False # for building or testing
  setId = racetrack.setStart(forBuilding=forBuilding)
  racetrack.setSetId(setId, forBuilding=forBuilding)
  info = racetrack.getTestingEnvironmentInfo()
  if info["branch"] == "MR" or info["branch"] == "master":
    if forBuilding:
      jenkins.autoSetRunDescription() # BUILDING...
    else:
      jenkins.replaceRunDescription([ # Images building finished, go into testing
        ("BUILDING...", ""),
        (jenkins.JENKINS_RACETRACK_HTML, '<a href="{}">Racetrack</a>'.format(racetrack.getResultSetLink(setId=setId)))
      ])

def racetrackSetEnd(args, options, secret):
  a = prepareArgs(args)
  repo = a[0] # default "athena"
  testResult = a[1] # a[0] = SUCCESS | FAILURE | ABORTED
  forBuilding = True if a[2] == "building" else False # for building or testing
  racetrack.finalize(testResult, forBuilding=forBuilding) # a[0] = SUCCESS | FAILURE | ABORTED
  if not forBuilding:
    info = racetrack.getTestingEnvironmentInfo()
    jenkins.publishRunsRetroactively(
      jobName = info["jobName"],
      limit = 3, # publish this one and 2 previous runs just in case
      startFromBuildNumber = info["build"],
      firstRunOverrideWith = a[1],
      verbose = False
    )
    jenkins.setFailureSummaryInDescription()

def publishRuns(args, options, secret):
  a = prepareArgs(args)
  jenkins.publishRunsRetroactively(jobName=a[0], limit=a[1], startFromBuildNumber=a[2])

def publishRunsMaster(args, options, secret):
  a = prepareArgs(args)
  jenkins.publishRunsMaster(limit=a[0], startFromBuildNumber=a[1])

def publishRunsReleases(args, options, secret):
  a = prepareArgs(args)
  jenkins.publishRunsReleases(releaseVersion=a[0], limit=a[1], startFromBuildNumber=a[2])

def publishRunsMR(args, options, secret):
  a = prepareArgs(args)
  jenkins.publishRunsMR(limit=a[0], startFromBuildNumber=a[1])

def printLongRunningTestDashboardLink(args, options, secret):
  link = helper.longRunningTestDashboardLink()
  log.info(link)

def ownAllJenkinsNodesWorkspace(args, options, secret):
  jenkins.ownAllJenkinsNodesWorkspace(blockchainWorkersOnly=True)

def resetBlockchain(args, options, secret):
  a = prepareArgs(args)
  replicas = helper.parseReplicasConfig(replicas=a[0])
  fxBlockchain = BlockchainFixture(blockchainId=None, consortiumId=None, replicas=replicas, clientNodes=None)
  ops.reset_blockchain(fxBlockchain)

def capturePipelineError(args, options, secret):
  a = prepareArgs(args)
  pipelineError = json.loads(a[0])
  case.extractAndSavePipelineFailurePoint(pipelineError=pipelineError)
  jenkins.setFailureSummaryInDescription()

def resetIPAM(args, options, secret):
  ''' Usage, DRY RUN, provide target segments:
      a[0]: targetSegments, e.g. "sddc1.mr.*, sddc4.mr.cloud" or "sddc1.*, sddc2.*, sddc3.*"
    
      Usage, actual reset, provide COMMENCE flag first and then target segments:
      a[0]: "COMMENCE"
      a[1]: targetSegments e.g. "sddc1.mr.*, sddc4.mr.cloud" or "sddc1.*, sddc2.*, sddc3.*"
  '''
  a = prepareArgs(args)
  dryRun = (a[0] != "COMMENCE")
  if dryRun: log.info("COMMENCE parameter is not specified; doing a DRY RUN...")
  targetSegments = a[0] if dryRun else a[1]
  infra.resetIPAM(targetSegments, dryRun=dryRun)

def removeOrphans(args, options, secret):
  '''Removes all orphan NAT/EIP on all active SDDCs'''
  a = prepareArgs(args)
  dryRun = (a[0] != "COMMENCE")
  if dryRun: log.info("COMMENCE parameter is not specified; doing a DRY RUN...")
  infra.deregisterOrphanResources(dryRun=dryRun)

def resolveIPConflict(args, options, secret):
  '''Resolves IP conflict by destroying conflict VMs (temp tool; IPAM not released)'''
  a = prepareArgs(args)
  dryRun = (a[0] != "COMMENCE")
  if dryRun: log.info("COMMENCE parameter is not specified; doing a DRY RUN...")
  infra.resolveConflictVMs(dryRun=dryRun)

def zonesOverride(args, options, secret):
  '''
    Overrides zone_config.json, re-target segments and folder
    a[0] : targetSegments e.g. "sddc1.mr.*, sddc4.mr.*"
    a[1] : targetFolder e.g. "HermesTesting"
  '''
  a = prepareArgs(args)
  infra.overrideDefaultZones(targetSegments=a[0], targetFolder=a[1])
  
def zonesRestore(args, options, secret):
  '''Restores the overridden zone_config.json'''
  a = prepareArgs(args)
  infra.restoreDefaultZones()

def vaultResolveFile(args, options, secret):
  '''
    Updates the file by given path, a[0],
    replacing all Vault identifier references to Vault-pulled values.
    a[0]: filename is the file path e.g. "resources/zone_config.json"
  '''
  a = prepareArgs(args)
  vault.resolveFile(filename=a[0])

def patchOrg(args, options, secret):
  '''
  Utility method to patch an org, defaulting to using Helen
  on staging to do so.

  args[0]: Org name
  args[1]: Action ("add" or "delete")
  args[2]: Key
  args[3]: Value
  args[4]: Service, optional.  Defaults to util.auth.SERVICE_STAGING.

  Sample invocation to add foo=bar to the org system_test_master:
    python invoke.py patchOrg --param system_test_master add foo bar staging
  '''
  a = prepareArgs(args)
  orgName = args[0]
  action = args[1]
  patchKey = args[2]
  patchValue = args[3]

  if len(args) > 4:
    if args[4] == "staging":
      service = util.auth.SERVICE_STAGING
    else:
      service = args[4]
  else:
    service = util.auth.SERVICE_STAGING

  util.csp.patch_org(orgName, action, patchKey, patchValue, service)


def deregisterBlockchain(args, options, secret):
  '''
  Utility method to deregister a blockchain.

  args[0]: Name of an org which contains the user vmbc_test_con_admin.
           This org must be part of the consortium whose blockchain is being deleted.
           There must be a consortium admin in the org named "vmbc_test_con_admin".
           This parameter is used to look up the API key in util/auth.py
  args[1]: Blockchain ID
  args[2]: Service, optional.  Defaults to util.auth.SERVICE_STAGING.

  Sample invocation to dergister blockchain 0123456789:
    python invoke.py deregisterBlockchain --param system_test_master 0123456789
  '''
  a = prepareArgs(args)
  orgName = args[0]
  bcId = args[1]

  if len(args) > 2:
    service = args[2]
  else:
    service = util.auth.SERVICE_STAGING

  ops.deregister_blockchain(orgName, bcId, service)


def getZones(args, options, secret):
  '''
  Utility method to get zones

  args[0]: Name of an org which contains the user vmbc_test_con_admin.
  args[1]: Service, optional.  Defaults to util.auth.SERVICE_STAGING.

  Sample invocation to dergister blockchain 0123456789:
    python invoke.py getZones --param system_test_master
  '''
  a = prepareArgs(args)
  orgName = args[0]

  if len(args) > 1:
    service = args[1]
  else:
    service = util.auth.SERVICE_STAGING

  util.helper.get_zones(orgName, service)


# Registry of callable standalone functions
DISPATCH = {
  # Test Function - for local testing only
  "_test": localTestFunction,

  # Communications
  "emailSend": emailSend,
  "slackDM": slackDM,
  "slackPost": slackPost,
  "slackUpload": slackUpload,
  "slackReportMonitoring": slackReportMonitoring,

  # CI/CD Dashboard data points publish
  "publishRuns": publishRuns,
  "publishRunsMaster": publishRunsMaster,
  "publishRunsReleases": publishRunsReleases,
  "publishRunsMR": publishRunsMR,

  # Jenkins-related functions
  "ownAllWorkspaces": ownAllJenkinsNodesWorkspace,
  "capturePipelineError": capturePipelineError,

  # Infra-related
  "resetIPAM": resetIPAM,
  "removeOrphans": removeOrphans,
  "resolveIPConflict": resolveIPConflict,
  "zonesOverride": zonesOverride,
  "zonesRestore": zonesRestore,

  # Secret & Config Management
  "vaultResolveFile": vaultResolveFile,

  # CI/CD Racetrack
  "racetrackSetBegin": racetrackSetBegin,
  "racetrackSetEnd": racetrackSetEnd,

  # Long-running Test
  "lrtPrintDashboardLink": printLongRunningTestDashboardLink,

  # Blockchain-related functions
  "resetBlockchain" : resetBlockchain,
  "deregisterBlockchain": deregisterBlockchain,

  # Patch an org
  "patchOrg": patchOrg,

  # Zones
  "getZones": getZones,
}


def main():
  parser = argparse.ArgumentParser()
  parser.add_argument("funcName", help="Name of the utility function.")
  parser.add_argument("--param",
                      help="arguments to the utility function call",
                      default=[],
                      nargs="*")
  parser.add_argument("--credentials",
                      help="optional credentials parameter to override default value in user_config.json",
                      default="")
  parser.add_argument("--options",
                      help="arguments to the utility function call",
                      default=[],
                      nargs="*")
  parser.add_argument("--su",
                      action="store_true", # implies default=False
                      help="Super user privilege with all Jenkins injected credentials available.")
  parser.add_argument("--logLevel",
                       help="Set the log level.  Valid values:"
                       "'DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL'",
                       default="INFO")
  parser.add_argument("--resultsDir",
                       default=tempfile.gettempdir(),
                       help="Results directory")
  args = parser.parse_args()
  hermes_logging.setUpLogging(args)
  if args.su:
    helper.WITH_JENKINS_INJECTED_CREDENTIALS = True
    userConfig = helper.getUserConfig()
    zoneConfig = helper.getZoneConfig()
  try:
    param = trimCmdlineArgs(args.param)
    options = trimCmdlineArgs(args.options)
    DISPATCH[args.funcName](param, options, args.credentials)
  except Exception as e:
    if not helper.thisHermesIsFromJenkins():
      traceback.print_exc()
    helper.hermesNonCriticalTrace(e)
  helper.hermesPreexitWrapUp()
  return


def trimCmdlineArgs(argList):
  # remove all leading/trailing quotes in params; groovy calls can inject those in sh block
  for i, param in enumerate(argList):
    while param.startswith('"') and param.endswith('"'):
      param = param[1:-1]
    param = param.strip()
    if param.startswith("__base64__"):
      param = base64.b64decode(param.split("__base64__")[1]).decode('utf-8')
    argList[i] = param
  return argList


def prepareArgs(args):
  '''
    All unsupplied slots are None
  '''
  safeArgs = [None]*32
  for i, arg in enumerate(args):
    safeArgs[i] = arg
  return safeArgs


if __name__ == "__main__":
  main()
