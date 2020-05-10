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
from util import helper, slack, mailer, wavefront, racetrack, jenkins
from suites import case

log = None

def slackDM(args, options, secret):
  a = prepareArgs(args)
  slack.sendMessageToPerson(email=a[0], message=a[1], ts=a[2], token=secret)

def slackPost(args, options, secret):
  a = prepareArgs(args)
  slack.postMessageOnChannel(channelName=a[0], message=a[1], ts=a[2], token=secret)

def slackUpload(args, options, secret):
  a = prepareArgs(args)
  slack.postFileUpload(channelNameOrEmail=a[0], message=a[1], fileName=a[2], filePath=a[3], token=secret)

def emailSend(args, options, secret):
  a = prepareArgs(args)
  mailer.send(email=a[0], subject=a[1], message=a[2], senderName=a[3])

def racetrackSetBegin(args, options, secret):
  setId = racetrack.setStart()
  with open(racetrack.getIdFilePath(), "w+") as f:
    f.write(json.dumps({"setId": setId}, indent = 4))

def racetrackSetEnd(args, options, secret):
  a = prepareArgs(args)
  racetrack.finalize(a[0]) # a[0] = SUCCESS | FAILURE | ABORTED
  info = racetrack.getTestingEnvironmentInfo()
  jenkins.publishRunsRetroactively(
    jobName = info["jobName"],
    limit = 3, # publish this one and 2 previous runs just in case
    startFromBuildNumber = info["build"],
    verbose = True
  )

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


def slackReportMonitoring(args, options, secret):
  a = prepareArgs(args)
  slack.reportMonitoring(message=a[0])


def printLongRunningTestDashboardLink(args, options, secret):
  '''
    Log output long-running test dashboard link (Wavefront)
  '''
  a = prepareArgs(args)
  link = helper.longRunningTestDashboardLink()
  log.info(link)


def ownAllJenkinsNodesWorkspace(args, options, secret):
  a = prepareArgs(args)
  jenkins.ownAllJenkinsNodesWorkspace(blockchainWorkersOnly=True)


# Registry of callable standalone functions
DISPATCH = {
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
}


def main():
  setUpLogging()
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
  args = parser.parse_args()
  setUpLogging()
  try:
    param = trimCmdlineArgs(args.param)
    options = trimCmdlineArgs(args.options)
    DISPATCH[args.funcName](param, options, args.credentials)
  except Exception as e:
    log.info(e); traceback.print_exc()
  helper.hermesNonCriticalTraceFinalize()
  return


def setUpLogging():
  try:
    logging.basicConfig(level=logging.INFO, format='%(asctime)s %(levelname)s %(message)s',
                        datefmt='%Y-%m-%d %H:%M:%S')
    global log
    log = logging.getLogger(__name__)
  except AttributeError:
    exit(1)


def trimCmdlineArgs(argList):
  # remove all leading/trailing quotes in params; groovy calls can inject those in sh block
  for i, param in enumerate(argList):
    while param.startswith('"') and param.endswith('"'):
      param = param[1:-1]
    param = param.strip()
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