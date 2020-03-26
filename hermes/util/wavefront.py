#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
# More info: https://github.com/wavefrontHQ/wavefront-sdk-python

from uuid import UUID
import logging
import time
import json
import importlib
from . import hermes_logging, helper, cert
log = hermes_logging.getMainLogger()


SENDER = { "conn": None, "queueLength": 0 }
WF_DEFAULT_DEVOPS_SOURCE = "bc.devops.hermes"
WF_DEFAULT_DEVOPS_APP = "Blockchain"
WF_DEFAULT_DEVOPS_SERVICE = "Jenkins"
WF_JENKINS_STAGE_PREFIX = "stage."
WF_TAGNAME_JOB = "bc.job"
WF_TAGNAME_BUILD = "bc.build"
WF_TAGNAME_RUNTYPE = "bc.runtype"
WF_TAGNAME_STAGE = "bc.stage"
WF_TAGNAME_RESULT = 'bc.result'
WF_TAGNAME_TIMESCOPE = 'bc.timescope'
WF_METRIC_STAGE_DURATION = "bc.devops.stage.duration"
WF_METRIC_RUN_DURATION = "bc.devops.run.duration"
WF_METRIC_RUN_RESULT = "bc.devops.run.result"
WF_DEFAULT_STATIC_METRIC_TAGS = {}
WF_DEFAULT_STATIC_SPAN_TAGS = {
  # application, service, cluster, shards are MANDATORY span tags in Wavefront
  "application" : WF_DEFAULT_DEVOPS_APP,
  "service": WF_DEFAULT_DEVOPS_SERVICE,
  "cluster": "none", 
  "shard": "none",
}


DEP_INSTALLED = importlib.util.find_spec("wavefront_sdk") is not None
if DEP_INSTALLED:
  from wavefront_sdk import WavefrontDirectClient
else:
  class WavefrontDirectClientMock:
    '''
      When `event_recorder.py` is called before python3.7 init and
      when none of the requirements.txt deps are available, python will
      complain not finding WavefrontDirectClient class. In this case,
      supply with no-dependency class that will satisfy python parser.
      TODO: populate with no-SDK, pure REST-based publish
    '''
    def __init__(self, server, token, max_queue_size, batch_size, flush_interval_seconds): return
    def send_metric(self, name, value, timestamp, source, tags): return 
    def send_span(self, name, start_millis, duration_millis, source, trace_id, span_id, parents, follows_from, tags, span_logs): return
    def flush_now(self): return
  WavefrontDirectClient = WavefrontDirectClientMock


def queueMetric(name, value, timestamp=None, tags={}):
  '''
    Add metric to publish queue using Wavefront SDK
  '''
  try:
    conn = getConnection()
    if conn is None:
      log.info("Bad connection to Wavefront endpoint")
      return False
    timestamp = getTime() if timestamp is None else timestamp
    value = float(value)
    tags = tagsAppend(tags, defaultTags=WF_DEFAULT_STATIC_METRIC_TAGS)
    tagsStr = []
    for tagName in tags: tagsStr.append('{}="{}"'.format(tagName, tags[tagName]))
    tagsStr = ' '.join(tagsStr)

    # output for info
    log.debug("Queueing Wavefront metric for publish: {} {} {}".format(name, value, timestamp))
    log.debug("tags: {}".format(tagsStr))

    conn.send_metric(
      name = name,
      value = value,
      timestamp = timestamp,
      source = WF_DEFAULT_DEVOPS_SOURCE,
      tags = tags
    )
    SENDER["queueLength"] += 1
    return True
  except Exception as e:
    log.info(e); traceback.print_exc()
    return False


def queueSpan(name, start, duration, traceId=None, parents=[], tags={}):
  '''
    Add span data (used for trace view) to publish queue using Wavefront SDK
  '''
  try:
    conn = getConnection()
    if conn is None:
      log.info("Bad connection to Wavefront endpoint")
      return False
    if start is None: start = getTime() * 1000
    if duration is None: duration = 1000
    traceId = traceId if traceId else helper.getJenkinsBuildTraceId()
    spanId = helper.getJenkinsBuildSpanId(traceId, name, "")
    runInfo = helper.getJenkinsJobNameAndBuildNumber()
    tags = tagsAppend(tags, defaultTags=WF_DEFAULT_STATIC_SPAN_TAGS)
    tagsStr = []
    for tagName in tags: tagsStr.append('{}="{}"'.format(tagName, tags[tagName]))
    tagsStr = ' '.join(tagsStr)
    
    # output for info
    log.debug("Queueing Wavefront span for publish: [{}/{}] {} {} {}".format(
      runInfo["jobName"], runInfo["buildNumber"], name, start, duration))
    log.debug("traceId: {}".format(traceId))
    log.debug("spanId: {}".format(spanId))
    log.debug("parents: {}".format(parents))
    log.debug("tags: {}".format(tagsStr))

    tagsInTuplesList = []
    for tagName in tags:
      tagValue = tags[tagName]
      tagsInTuplesList.append((tagName, tagValue))

    conn.send_span(
      name = name,
      start_millis = start,
      duration_millis = duration,
      source = WF_DEFAULT_DEVOPS_SOURCE,
      trace_id = UUID(traceId),
      span_id = UUID(spanId),
      parents = parents,
      tags = tagsInTuplesList,
      follows_from = None,
      span_logs = None
    )
    SENDER["queueLength"] += 1
    return True
  except Exception as e:
    log.info(e)
    traceback.print_exc()
    return False


def publish():
  '''
    Flush all in publish queue to Wavefront endpoint
  '''
  try:
    conn = getConnection()
    if conn is None:
      log.info("Bad connection to Wavefront endpoint")
      return False
    queueLength = SENDER["queueLength"]
    if queueLength == 0:
      log.info("Nothing to publish to Wavefront on the queue.")
      return False
    conn.flush_now()
    SENDER["queueLength"] = 0
    log.debug("Published {} queued Wavefront publish items.".format(queueLength))
    return queueLength
  except Exception as e:
    log.info(e); traceback.print_exc()
    return False


def getPublishFailureCount():
  '''
    Get failure count of the publish
  '''
  try:
    conn = getConnection()
    if conn is None:
      log.info("Bad connection to Wavefront endpoint")
      return False
    count = int(conn.get_failure_count())
    log.debug("Failure count is {}, for Wavefront publish items.".format(count))
    return count
  except Exception as e:
    log.info(e); traceback.print_exc()
    return False




#==================================================================
# Other helper functions
#==================================================================

def getConnection():
  '''
    Create client to Wavefront endpoint
  '''
  try:
    if SENDER["conn"] is None:
      configObject = helper.getUserConfig()
      wfConfig = configObject["dashboard"]["devops"]["wavefront"]
      url = wfConfig["url"]
      token = wfConfig["token"]
      conn = WavefrontDirectClient(
        server=url,
        token=token,
        max_queue_size=50000,
        batch_size=10000,
        flush_interval_seconds=5
      )
      SENDER["conn"] = conn
      return conn
    return SENDER["conn"]
  except Exception as e:
    log.info(e); traceback.print_exc()
    return None

def getTime():
  '''get UNIX timestamp in seconds'''
  return int(time.time())

def getName(setName, caseName):
  '''
    Wavefront metrics are dot concatenated, all other special character will be
    auto-replaced with a hypen on their server; honor the delimter format with dot.
    e.g. "Some Set Name" "Some Case Name" => Some-Set-Name.Some-Case-Name
  '''
  name = None
  if not caseName: name = setName
  else: name = setName + '.' + caseName
  return name

def tagsAppend(tags, defaultTags={}):
  '''
    Populate default tags that are used to look-up data sets which
    is then used for generating charts. 
  '''
  result = json.loads(json.dumps(defaultTags), strict=False) # copy of default static tags
  # default dynamically added tags
  info = helper.getJenkinsJobNameAndBuildNumber()
  result[WF_TAGNAME_JOB] = info["jobName"]
  result[WF_TAGNAME_BUILD] = info["buildNumber"]
  runTypeInfo = helper.getJenkinsRunTypeInfo(info["jobName"])
  if runTypeInfo: # see top of helper.py which lists all major run types
    result[WF_TAGNAME_RUNTYPE] = runTypeInfo["type"] # MAIN_MR | MASTER | RELEASE
  for tagName in tags:
    tagValue = tags[tagName]
    if type(tagValue) == int or type(tagValue) == float:
      tagValue = str(tagValue)
    if type(tagValue) != str:
      log.info("Only string type tag values are allowed; " + 
        "tag name {} has value {}, which is not str.".format(tagName, tagValue))
      continue
    result[tagName] = tagValue
  return result
