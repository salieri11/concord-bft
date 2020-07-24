#!/usr/bin/python3

#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import jenkins
import requests
import json
import traceback
import os
import urllib
import re
from itertools import islice
from datetime import datetime
from . import helper, hermes_logging, wavefront, racetrack, gitlab, slack
from suites import case
log = hermes_logging.getMainLogger()

MAIN_JENKINS_URL = "blockchain.svc.eng.vmware.com"
EXISTING_CONNECTION = { "conn" : None }
REQ_SESSION = requests.Session()
EVENTS_RECORDER_TIME_FORMAT = "%Y-%m-%d_%H-%M-%S"
JOB_NAME_REPLACE_WITH = {
  # map of job names with slashes; escape slashes on URL
  '/releases/': '/releases%252F',
}
JENKINS_USER_OVERRIDE = os.getenv("JENKINS_USER")
JENKINS_TOKEN_OVERRIDE = os.getenv("JENKINS_TOKEN")
JENKINS_RACETRACK_HTML = '<span style="display:none">RACETRACK</span>'
JENKINS_SUMMARY_LOG_HTML = '<span style="display:none">SUMMARY_LOG</span>'
JENKINS_TEST_LOG_HTML = '<span style="display:none">TEST_LOG</span>'
JENKINS_ERRORS_ONLY_HTML = '<span style="display:none">ERRORS_ONLY</span>'


def publishRunsMaster(limit=100, startFromBuildNumber=None):
  '''
    Publish recent master runs (default 100 count, start from most recent run)
  '''
  jobName = helper.JENKINS_RUN_MASTER["exactly"]
  return publishRunsRetroactively(jobName, limit, startFromBuildNumber)


def publishRunsReleases(releaseVersion, limit=100, startFromBuildNumber=None):
  '''
    Publish ToT, release versions (default 100 count, start from most recent run)
    releaseVersion: 0.5, 0.6, ...
  '''
  # helper-defined full job name format: 
  # e.g. <RELEASE_VERSION> Branch Blockchain Run on GitLab/releases/<RELEASE_VERSION>
  jobNameFormat = helper.JENKINS_RUN_RELEASE_BRANCH["format"]
  jobName = jobNameFormat.replace("<RELEASE_VERSION>", str(releaseVersion))
  return publishRunsRetroactively(jobName, limit, startFromBuildNumber)


def publishRunsMR(limit=100, startFromBuildNumber=None):
  '''
    Publish recent MR runs (default 100 count, start from most recent run)
  '''
  jobName = helper.JENKINS_RUN_MAIN_MR["exactly"]
  return publishRunsRetroactively(jobName, limit, startFromBuildNumber)


def getTimesDataOfRun(jobName, buildNumber):
  ''' 
    Fetches times.json from the run and normalizes the data
    into seconds from human-readable time string
  '''
  if getConnection() is None: return outputGetConnectionError()
  timesData = getArtifact(jobName, buildNumber, "times.json")
  if timesData is None: return None
  timesData = normalizeTimesData(timesData)
  return timesData


def publishRunData(jobName=None, buildNumber=None, publishImmediately=True, runResultOverride=None, verbose=True):
  '''
    This function retroactively publishes run data to Wavefront endpoint
    It also can be used to retroactively fill in missed metrics during run time,
    which can be caused by endpoint unavailability, per-user submission throttle, etc.
  '''
  try:
    if getConnection() is None: return outputGetConnectionError()
    runInfo = helper.getJenkinsJobNameAndBuildNumber()
    if jobName is None: jobName = runInfo["jobName"]
    if buildNumber is None: buildNumber = runInfo["buildNumber"]
    runTypeInfo = helper.getJenkinsRunTypeInfo(jobName)
    runType = runTypeInfo["type"] if runTypeInfo else "OTHER"
    traceId = helper.getJenkinsBuildTraceId(jobName, buildNumber)
    if verbose: log.info("Publishing run '{}/{}' traceId is {}".format(jobName, buildNumber, traceId))

    metaData = getRunMetadata(jobName, buildNumber)
    if not metaData:
      if verbose: log.info("[ {}/{} ] metadata cannot be obtained".format(jobName, buildNumber))
      return None
    if metaData["building"]: metaData["result"] = "PENDING"
    ts = int(metaData["timestamp"] / 1000)
    
    # These are the fundamental tags
    # Query-able by job, run, or runtype
    baseTags = {
      wavefront.WF_TAGNAME_JOB: jobName,
      wavefront.WF_TAGNAME_BUILD: buildNumber,
      wavefront.WF_TAGNAME_RUNTYPE: runType,
    }

    runResult = metaData["result"]
    if runResultOverride: runResult = runResultOverride

    # If run is still building, set this run to pending and return
    if runResult == "PENDING":
      # Set to pending status for this run
      wavefront.queueMetric(
        name = wavefront.WF_METRIC_RUN_RESULT, value = 1, # set to 1
        timestamp = ts - (ts % 3600), tags = tagsAdd(baseTags, addedTags={
          wavefront.WF_TAGNAME_RESULT:'PENDING',
          wavefront.WF_TAGNAME_TIMESCOPE:'1h',
        }))
      if publishImmediately: wavefront.publish()
      if verbose: log.info("[ {}/{} ] is PENDING".format(jobName, buildNumber))
      return "PENDING"
    
    # If finished, remove pending status for this run
    wavefront.queueMetric(
      name = wavefront.WF_METRIC_RUN_RESULT, value = 0, # set to 0
      timestamp = ts - (ts % 3600), tags = tagsAdd(baseTags, addedTags={
        wavefront.WF_TAGNAME_RESULT:'PENDING',
        wavefront.WF_TAGNAME_TIMESCOPE:'1h',
      }))

    # Add result tag to baseTags for better sorting on Wavefront
    baseTags[wavefront.WF_TAGNAME_RESULT] = metaData["result"] # SUCCESS | FAILURE | ABORTED

    # Publish results in time buckets to avoid messy interpolation (1h, 4h, 12h, 1d, 1w, 1mo)
    queueToAllTimeIndexes(wavefront.WF_METRIC_RUN_RESULT, 1, ts, baseTags)

    # If not aborted, publish opposite result tags with INVERTED value (0) 
    # For example, if a run succeeded, you want to set SUCCESS metric of the run to 1, 
    # and FAILURE metric of the run to 0. This is to avoid Wavefront's auto-interpolation 
    # on charts; if a datapoint is not found, the chart will try to average neighboring values.
    # Publishing both SUCCESS and FAILURE metrics for the run gives more accurate representation.
    if metaData["result"] != "ABORTED":
      # temp change result tag
      baseTags[wavefront.WF_TAGNAME_RESULT] = "FAILURE" if metaData["result"] == "SUCCESS" else "SUCCESS"
      queueToAllTimeIndexes(wavefront.WF_METRIC_RUN_RESULT, 0, ts, baseTags)
      # revert back result tag
      baseTags[wavefront.WF_TAGNAME_RESULT] = metaData["result"]

    # Get times data in seconds for total run and all stages
    timesData = getTimesDataOfRun(jobName, buildNumber)
    if not timesData or "total" not in timesData or "Setup" not in timesData:
      if verbose: log.info("[ {}/{} ] timesData cannot be obtained. TIMES_MISSING".format(jobName, buildNumber))
      if publishImmediately: wavefront.publish()
      return "TIMES_MISSING"
    
    # Publish total run duration
    runDuration = timesData["total"]
    wavefront.queueMetric(
      name = wavefront.WF_METRIC_RUN_DURATION, # bc.devops.run.duration
      value = timesData["total"],
      timestamp = timesData["Setup"]["startTime"] + runDuration, # reported at the end
      tags = tagsAdd(baseTags, addedTags={})
    )

    # Publish stage duration for each stage, 
    # including non-test-suite stages as well (e.g. Setup, init node, python, etc.)
    for stageName in timesData:
      stageData = timesData[stageName]
      if type(stageData) != dict: continue # timesData["total"] is int; skip if not dictionary
      if "duration" not in stageData:
        if verbose: log.info("[ {}/{} ] Stage duration for '{}' not found".format(jobName, buildNumber, stageName))
        continue
      if stageData["duration"] < 0: stageData["duration"] = 0
      wavefront.queueMetric(
        name = wavefront.WF_METRIC_STAGE_DURATION, # bc.devops.stage.duration
        value = stageData["duration"],
        timestamp = stageData["startTime"] + stageData["duration"], # reported at the end
        tags = tagsAdd(baseTags, addedTags={
          wavefront.WF_TAGNAME_STAGE: stageName
        })
      )
      # Spans in miliseconds
      startTimeMilis = stageData["startTime"] * 1000
      durationMilis = stageData["duration"] * 1000
      runTypeInfo = helper.getJenkinsRunTypeInfo(jobName)
      runType = runTypeInfo["type"] if runTypeInfo else "OTHER"
      wavefront.queueSpan(
        name = wavefront.WF_JENKINS_STAGE_PREFIX + stageName,
        start = startTimeMilis,
        duration = durationMilis,
        traceId = traceId,
        tags = {
          wavefront.WF_TAGNAME_JOB: jobName,
          wavefront.WF_TAGNAME_BUILD: buildNumber,
          wavefront.WF_TAGNAME_RUNTYPE: runType # MAIN_MR | MASTER | RELEASE | OTHER | ...
        }
      )

    if publishImmediately: wavefront.publish()
    return "SUCCESS"

  except Exception as e:
    if verbose: log.info(e)
    helper.hermesNonCriticalTrace(e)
    return None


def publishRunsRetroactively(jobName, limit=None, startFromBuildNumber=None, firstRunOverrideWith=None, verbose=True):
  if limit is None: limit = 100
  limit = int(limit)
  if startFromBuildNumber is not None:
    topBuildNumber = startFromBuildNumber
  else:
    topBuildNumber = getTopBuildNumberForJob(jobName)
    if not topBuildNumber:
      log.info("Cannot get the most recent build number of {}".format(jobName))
      return None
  count = 0
  buildNumber = int(topBuildNumber)
  successCount = 0
  pendingCount = 0
  timesMissingCount = 0
  publishErrorCount = 0
  while buildNumber > 0 and count < limit:
    result = publishRunData(
      jobName, buildNumber, 
      runResultOverride= firstRunOverrideWith if count == 0 else None,
      verbose=verbose
    )
    if result == "SUCCESS": successCount += 1
    elif result == "TIMES_MISSING": timesMissingCount += 1
    elif result == "PENDING": pendingCount += 1
    elif not result: publishErrorCount += 1
    buildNumber -= 1
    count += 1
  failureCount = wavefront.getPublishFailureCount()
  if verbose:
    log.info("Retroactively published metrics data, total {} runs of {}".format(count, jobName))
    log.info("Build number from {} down to {}. SUCCESS={}, PENDING={}, TIMES_MISSING={}, ERRORS={}, PUBLISH_FAILURES={}".format(
      topBuildNumber, buildNumber+1, successCount, pendingCount, timesMissingCount, publishErrorCount, failureCount))


def getTopBuildNumberForJob(jobName):
  ''' 
    Get top build number for the supplied job
  '''
  if getConnection() is None: return outputGetConnectionError()
  res = treeQuery(jobName, query='nextBuildNumber')
  if res is None: return None
  return res["nextBuildNumber"] - 1


def getRunMetadata(jobName=None, buildNumber=None):
  try:
    if getConnection() is None: return outputGetConnectionError()
    if jobName is None or buildNumber is None:
      runInfo = helper.getJenkinsJobNameAndBuildNumber()
      jobName = runInfo["jobName"]
      buildNumber = runInfo["buildNumber"]
    res = treeQuery(jobName, buildNumber,
      query = 'displayName,fullDisplayName,description,timestamp,building,result,duration,url,'
            + 'actions[queuingDurationMillis,parameters[name,value],environment[*]]'
    )
    if res is None: return None
    metadata = {
      "displayName": res["displayName"],
      "fullDisplayName": res["fullDisplayName"],
      "description": res["description"],
      "timestamp": res["timestamp"],
      "building": res["building"],
      "result": res["result"],
      "duration": res["duration"],
      "queuedFor": 10,
      "params": {},
      "env": {},
    }
    for action in res["actions"]:
      keyCount = len(action.keys())
      if keyCount == 0 or keyCount == 1: continue
      elif "parameters" in action:
        for param in action["parameters"]:
          metadata["params"][param["name"]] = param["value"] if "value" in param else None
      elif "queuingDurationMillis" in action:
        metadata["queueFor"] = action["queuingDurationMillis"]
      elif "environment" in action:
        for envVarName in action["environment"]:
          envVarValue = action["environment"][envVarName]
          metadata["env"][envVarName] = envVarValue
    return metadata

  except Exception as e:
    log.info(e); traceback.print_exc()
    return None


def getConfigFromLatestGoodMaster():
  if not JENKINS_USER_OVERRIDE or not JENKINS_TOKEN_OVERRIDE:
    log.error("Your local environment is does not have the needed Jenkins environment variables.")
    log.error("In order to set environment variables (e.g. ~/.bash_profile), add:")
    log.error("export JENKINS_USER={LDAP_USERNAME} # e.g. email without '@vmware.com'")
    log.error("export JENKINS_TOKEN={TOKEN} # e.g. add new API token on https://blockchain.svc.eng.vmware.com/user/{LDAP_USERNAME}/configure")
    return False
  log.info("Getting user_config.json from the latest good master...")
  jobName = helper.JENKINS_RUN_MASTER["exactly"]
  buildNumber = getTopBuildNumberForJob(jobName)
  while True:
    metadata = getRunMetadata(jobName, buildNumber)
    if metadata["result"] == "SUCCESS": break
    buildNumber -= 1
  config = getArtifact(jobName, buildNumber, '/blockchain/hermes/resources/user_config.json')
  zoneConfig = getArtifact(jobName, buildNumber, '/blockchain/hermes/resources/zone_config.json')
  return { "userConfig": config, "zoneConfig": zoneConfig }


def configSubmit(jobName=None, buildNumber=None, displayName='', description='', descriptionAppend=False):
  '''
    Set `displayName` and/or `description` of a jenkins run
  '''
  if not displayName and not description: return
  if jobName is None or buildNumber is None:
    runInfo = helper.getJenkinsJobNameAndBuildNumber()
    jobName = runInfo["jobName"]
    buildNumber = runInfo["buildNumber"]
  metadata = getRunMetadata(jobName, buildNumber)
  before = { "displayName": metadata["displayName"], "description": metadata["description"] }
  if descriptionAppend or not displayName or not description: # one fields missing, get from metadata
    after = { "displayName": metadata["displayName"], "description": metadata["description"] }
  else:
    after = {}
  if displayName: after["displayName"] = displayName
  if description:
    if descriptionAppend: after["description"] += description
    else: after["description"] = description
  print("")
  print("Changing Jenkins build displayName and description...")
  print("Before: " + json.dumps(before, indent=4))
  print("After: " + json.dumps(after, indent=4))
  baseUrl = getJenkinRunBaseUrl(jobName, buildNumber, authenticated=True)
  res = REQ_SESSION.post(
    baseUrl + '/configSubmit', 
    data={"json": json.dumps(after)}
  )
  if res.status_code == 200:
    print("Successfully modified Jenkins build displayName and description.")
    return True
  else:
    log.info("Setting build displayName and description returned with {}".format(res.status_code))
    return None


def ownAllJenkinsNodesWorkspace(blockchainWorkersOnly=True):
  '''
    Owns /var/jenkins/workspaces of all nodes (or all blockchain-worker nodes)
  '''
  builderPW = helper.getUserConfig()["jenkins"]["builderPassword"]
  if builderPW.startswith("<"):
    log.error('Builder password is not set. Cannot sudo chown without builder password.')
    return
  cmd = 'echo "{}" | sudo -S chown -R builder:builder /var/jenkins/workspace/*'.format(builderPW)
  baseURL = getJenkinRunBaseUrl("ROOT", authenticated=True)
  response = REQ_SESSION.get(baseURL + "/computer/api/json?tree=computer[displayName]")
  count = 0
  if response.status_code == 200:
    resJSON = json.loads(response.content.decode('utf-8'), strict=False)
    nodeList = resJSON["computer"]
    for nodeInfo in nodeList:
      try:
        nodeName = nodeInfo["displayName"]
        if blockchainWorkersOnly and not nodeName.startswith("blockchain-worker."): continue
        response2 = REQ_SESSION.get(baseURL + "/computer/{}".format(nodeName))
        if response2.status_code == 200:
          ip = response2.content.decode('utf-8').split("<h2>IP</h2><p>")[1].split("</p>")[0]
          if ip:
            output = helper.ssh_connect(ip, "builder", builderPW, cmd)
            if output is not None:
              count += 1
              log.info("[ {} :: {} ] /var/jenkins/workspace/* has been recursively owned by builder:builder".format(nodeName, ip))
      except Exception as e:
        traceback.print_exc()
        pass
    log.info("Total of {} nodes are affected.".format(count))
    return True
  else: return None


def autoSetRunDescription():
  metadata = getRunMetadata()
  prevDesc = metadata["description"]
  if not prevDesc: prevDesc = ""
  newDesc = ""
  triggerSourceInfo = {}
  if "GitLab Merge Request #" in prevDesc: # From MR
    # e.g. Desc set by Gitlab kick off
    # Triggered by <a href="https://gitlab.eng.vmware.com/blockchain/vmwathena_blockchain/merge_requests/NNNN" target="_blank">
    # GitLab Merge Request #NNNN</a>: blockchain/user/BC-NNNN => master
    mrNumber = prevDesc.split("GitLab Merge Request #")[1].split("</a>")[0]
    branchInfo = prevDesc.split("</a>: blockchain/")[1].split(" => ")[0]
    jiraPrefixAndHyphen = helper.JIRA_PREFIX + "-"
    if jiraPrefixAndHyphen in branchInfo:
      jiraNumber = re.findall(r'\d+', branchInfo.split(jiraPrefixAndHyphen)[1])[0]
      jiraId = jiraPrefixAndHyphen + jiraNumber
      jiraStoryURL = helper.JIRA_BASE_URL + "/browse/" + jiraId
      jiraStoryLink = '<a href="{}">{}</a>'.format(jiraStoryURL, jiraId)
      branchInfo = branchInfo.replace(jiraId, jiraStoryLink)
      triggerSourceInfo["jira_ticket"] = jiraId
      triggerSourceInfo["jira_url"] = jiraStoryURL
    mrURL = helper.GITLAB_BASE_URL + "/merge_requests/" + mrNumber
    triggerSourceInfo["mr_number"] = mrNumber
    triggerSourceInfo["mr_url"] = mrURL
    newDesc += '[ <a href="{}">{}</a> ]:\n{}<br>\n'.format(mrURL, 'MR ' + mrNumber, branchInfo)

  defaultDesc = ("[\n" +
                  'BUILDING...\n' +
                  JENKINS_RACETRACK_HTML + '\n' + 
                  JENKINS_SUMMARY_LOG_HTML + '\n' + 
                  JENKINS_TEST_LOG_HTML + '\n' + 
                  JENKINS_ERRORS_ONLY_HTML + '\n' + 
                "]\n<br>\n")
  newDesc += defaultDesc
  configSubmit(description=newDesc)
  if "mr_number" in triggerSourceInfo:
    mrData = gitlab.getMRDetails(triggerSourceInfo["mr_number"])
    if mrData:
      if "author" in mrData:
        authorEmail = mrData["author"]["username"] + "@vmware.com"
        authorSlackUserId = slack.getUserIdByEmail(authorEmail)
        triggerSourceInfo["author_email"] = authorEmail
        triggerSourceInfo["author_slack_user_id"] = authorSlackUserId
      triggerSourceInfo["mr_data"] = mrData
    with open(os.getenv("WORKSPACE") + '/summary/trigger_info.json', 'w+') as f:
      f.write(json.dumps(triggerSourceInfo, indent=4, default=str))


def replaceRunDescription(replaceList):
  metadata = getRunMetadata()
  desc = metadata["description"]
  if not desc: desc = ""
  for replaceInfo in replaceList:
    old = replaceInfo[0]
    new = replaceInfo[1]
    desc = desc.replace(old, new)
  configSubmit(description=desc)


def setFailureSummaryInDescription():
  if not helper.thisHermesIsFromJenkins(): return
  summaryJsonPath = helper.getJenkinsWorkspace() + '/summary/failure_summary.json'
  if not os.path.isfile(summaryJsonPath):
    log.info("Cannot find failure_summary.json file, skipping setting summary link.")
    return
  with open(summaryJsonPath, 'r') as f:
    metadata = getRunMetadata()
    currentDescription = metadata["description"]
    if not currentDescription or \
       JENKINS_SUMMARY_LOG_HTML not in currentDescription: # summary already set
      return
    newDescription = currentDescription
    summaryObj = json.loads(f.read(), strict=False)
    hasErrorLines = False
    if summaryObj["from"] == "Hermes":
      lookFor = ["ERROR", "FAIL", "FATAL", "FAILURE"]
      ignoreFor = ["ERRORS:"]
      filteredLogText = case.filterErrorsInLogs(summaryObj["log_path_local"], lookFor, ignoreFor)
      if filteredLogText:
        hasErrorLines = True
        filteredLogFilePath = helper.getJenkinsWorkspace() + '/summary/errors_only.log'
        with open(filteredLogFilePath, 'w+') as w: w.write(filteredLogText)
    elif summaryObj["from"] == "Build":
      hasErrorLines = True
    elif summaryObj["from"] == "Pipeline":
      hasErrorLines = False
    if summaryObj["summary_url"]:
      summaryFileHTML = '| <a href="{}">Summary</a>'.format(summaryObj["summary_url"])
      newDescription = newDescription.replace(JENKINS_SUMMARY_LOG_HTML, summaryFileHTML)
    if summaryObj["log_path"]:
      logFileHTML = '| <a href="{}">Logs</a>'.format(summaryObj["log_path"])
      newDescription = newDescription.replace(JENKINS_TEST_LOG_HTML, logFileHTML)
    if hasErrorLines: # file with lines contaning ERROR
      errorsOnlyHTML = '| <a href="{}">Filtered</a>'.format(summaryObj["filtered_url"])
      newDescription = newDescription.replace(JENKINS_ERRORS_ONLY_HTML, errorsOnlyHTML)
    configSubmit(description=newDescription)
    




#========================================================================================
#
#   Helper Functions
#
#========================================================================================

def getConnection():
  if JENKINS_TOKEN_OVERRIDE:
    username = JENKINS_USER_OVERRIDE
    token = JENKINS_TOKEN_OVERRIDE
  else:
    configObject = helper.getUserConfig()
    username = configObject["jenkins"]["username"]
    token = configObject["jenkins"]["token"]
  if EXISTING_CONNECTION["conn"] is not None:
    return EXISTING_CONNECTION["conn"]
  conn = jenkins.Jenkins('https://' + MAIN_JENKINS_URL, username=username, password=token)
  if conn:
    version = conn.get_version()
    log.info("Connection to Jenkins established; endpoint's Jenkins verion is {}".format(version))
  EXISTING_CONNECTION["conn"] = conn
  return conn

def normalizeTimesData(timesData):
  '''
    Replace human readable time to UNIX timestamps
    for each stage / test suite duration records
  '''
  normalized = {}
  for stageName in timesData:
    stageData = timesData[stageName]
    if "events" not in stageData or "elapsed" not in stageData: continue
    firstEventKey = next(islice(stageData["events"], 0, None))
    firstEventValue = stageData["events"][firstEventKey]
    stageStartDatetime = datetime.strptime(firstEventValue, EVENTS_RECORDER_TIME_FORMAT)
    stageStartTime = int(stageStartDatetime.timestamp())
    elapsedSeconds = getSecondsFromTimeStr(stageData["elapsed"])
    normalized[stageName] = {
      "startTime": stageStartTime,
      "duration": elapsedSeconds
    }
  if "total" in timesData:
    normalized["total"] = getSecondsFromTimeStr(timesData["total"])
  return normalized

def getJenkinRunBaseUrl(jobName, buildNumber='', authenticated=False):
  '''
    If jobName is "ROOT" it will default to the very root URL
  '''
  if JENKINS_TOKEN_OVERRIDE:
    username = JENKINS_USER_OVERRIDE
    token = JENKINS_TOKEN_OVERRIDE
  else:
    configObject = helper.getUserConfig()
    username = configObject["jenkins"]["username"]
    token = configObject["jenkins"]["token"]
  accessor = username + ':' + token
  if jobName == "ROOT": return 'https://{}@{}'.format(accessor, MAIN_JENKINS_URL)
  jobNameEscaped = jobName
  for pattern in JOB_NAME_REPLACE_WITH: # escape slashes in job names
    jobNameEscaped = jobNameEscaped.replace(pattern, JOB_NAME_REPLACE_WITH[pattern])
  jobsPath = '/job/'.join(jobNameEscaped.split('/'))
  if buildNumber: jobsPath = jobsPath + '/' + str(buildNumber)
  if authenticated:
    baseUrl = 'https://{}@{}/job/{}'.format(accessor, MAIN_JENKINS_URL, jobsPath)
  else:
    baseUrl = 'https://{}/job/{}'.format(MAIN_JENKINS_URL, jobsPath)
  return baseUrl

def getArtifact(jobName, buildNumber, path, isJSON=True):
  baseUrl = getJenkinRunBaseUrl(jobName, buildNumber, authenticated=True)
  artifactsPath = baseUrl + '/artifact/'
  response = REQ_SESSION.get(artifactsPath + path)
  if response.status_code == 200:
    if isJSON:
      return json.loads(response.content.decode('utf-8'), strict=False)
    else:
      return response.content.decode('utf-8')
  else:
    log.debug("Cannot fetch artifact named, '{}'".format(path))
    return None

def treeQuery(jobName, buildNumber='', query='', params={}):
  baseUrl = getJenkinRunBaseUrl(jobName, buildNumber, authenticated=True)
  jsonApiPath = baseUrl + '/api/json?'
  otherParamsStr = urllib.parse.urlencode(params) + "&" if len(params.keys()) > 0 else ""
  fullRequestUrl = jsonApiPath + otherParamsStr + 'tree=' + query if query else jsonApiPath
  response = REQ_SESSION.get(fullRequestUrl)
  if response.status_code == 200:
    try:
      return json.loads(response.content.decode('utf-8'), strict=False)
    except Exception as e:
      log.info(e); traceback.print_exc()
      log.info("Mal-formed JSON from {}".format(fullRequestUrl))
      # Mal-formed JSON
      return None
  else:
    return None

def outputGetConnectionError():
  log.info("Cannot get connection to Jenkins instance at {}".format(MAIN_JENKINS_URL))
  return None

def getSecondsFromTimeStr(timeStr):
  if "day" in timeStr: timeStr = timeStr.split(", ")[1]
  h, m, s = timeStr.split(':')
  return int(h) * 3600 + int(m) * 60 + int(s)

def tagsAdd(base, addedTags):
  copy = json.loads(json.dumps(base), strict=False)
  for tagName in addedTags:
    copy[tagName] = addedTags[tagName]
  return copy

def queueToAllTimeIndexes(name, value, ts, baseTags):
  # Publish results in time buckets to avoid messy interpolation (1h, 4h, 12h, 1d, 1w, 1mo)
  timeBuckets = { "1h":3600, "4h":14400, "12h":43200, "1d":86400, "1w":604800, "1mo":2592000 }
  for timeScope in timeBuckets:
    bucketSpan = timeBuckets[timeScope]
    wavefront.queueMetric(
      name = name,
      value = value,
      timestamp = ts - (ts % bucketSpan), 
      tags = tagsAdd(baseTags, addedTags={
        # 1h, 4h, 12h, 1d, 1w, 1mo
        wavefront.WF_TAGNAME_TIMESCOPE: timeScope
      }))

def overrideOnlyDefaultConfig(target, src):
  '''
    Default config without sed replace is usually "<SOME_CONFIG_NAME>"
    Recursively find and replace with matching configs from src
  '''
  if not target or not src: return
  if isinstance(target, dict) and isinstance(src, dict):
    for key, value in target.items():
      if not value: continue
      if isinstance(value, str):
        if value.startswith("<") and value.endswith(">"):
          if key in src: target[key] = src[key]
      if isinstance(value, dict) or isinstance(value, list): 
        if key in src: overrideOnlyDefaultConfig(value, src[key])
  elif isinstance(target, list) and isinstance(src, list):
    for idx, value in enumerate(target):
      if not value: continue
      if isinstance(value, str):
        if value.startswith("<") and value.endswith(">"):
          if idx < len(src): target[idx] = src[idx]
      if isinstance(value, dict) or isinstance(value, list):
        if idx < len(src): overrideOnlyDefaultConfig(value, src[idx])
  return target
