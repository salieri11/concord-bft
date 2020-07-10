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
from fixtures.common_fixtures import BlockchainFixture
from util import (hermes_logging, helper, slack, mailer, wavefront,
                 racetrack, jenkins, infra, blockchain_ops as ops)
import util.hermes_logging
from suites import case

log = util.hermes_logging.getMainLogger()

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
  setId = racetrack.setStart()
  racetrack.setSetId(setId)
  info = racetrack.getTestingEnvironmentInfo()
  if info["branch"] == "MR" or info["branch"] == "master":
    jenkins.autoSetRunDescription()

def racetrackSetEnd(args, options, secret):
  a = prepareArgs(args)
  racetrack.finalize(a[0]) # a[0] = SUCCESS | FAILURE | ABORTED
  info = racetrack.getTestingEnvironmentInfo()
  jenkins.publishRunsRetroactively(
    jobName = info["jobName"],
    limit = 3, # publish this one and 2 previous runs just in case
    startFromBuildNumber = info["build"],
    firstRunOverrideWith = a[0],
    verbose = False
  )
  jenkins.setFailureSummaryInDescription()

def racetrackCaseFailed(args, options, secret):
  a = prepareArgs(args)
  case.reportFailedCase(suiteName=a[0], caseName=a[1], description=[2])

def racetrackCasePassed(args, options, secret):
  a = prepareArgs(args)
  case.reportPassedCase(suiteName=a[0], caseName=a[1], description=[2])

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
  a = prepareArgs(args)
  link = helper.longRunningTestDashboardLink()
  log.info(link)

def ownAllJenkinsNodesWorkspace(args, options, secret):
  a = prepareArgs(args)
  jenkins.ownAllJenkinsNodesWorkspace(blockchainWorkersOnly=True)

def resetBlockchain(args, options, secret):
  a = prepareArgs(args)
  replicas = helper.parseReplicasConfig(replicas=a[0])
  fxBlockchain = BlockchainFixture(blockchainId=None, consortiumId=None, replicas=replicas)
  ops.reset_blockchain(fxBlockchain)

def localTestFunction(args, options, secret):
  '''A placeholder function for local testing of new code, pre-commit'''
  return


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

  # Jenkins Master/Executor Nodes Ops
  "ownAllWorkspaces": ownAllJenkinsNodesWorkspace,

  # CI/CD Racetrack
  "racetrackSetBegin": racetrackSetBegin,
  "racetrackSetEnd": racetrackSetEnd,
  "racetrackCasePassed": racetrackCasePassed,
  "racetrackCaseFailed": racetrackCaseFailed,

  # Long-running Test
  "lrtPrintDashboardLink": printLongRunningTestDashboardLink,

  # Reset Blockchain
  "resetBlockchain" : resetBlockchain,
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
  helper.hermesNonCriticalTraceFinalize()
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
