#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import logging
import traceback
import time
import requests
import os
import json
import urllib.parse
from . import helper, cert, hermes_logging
log = hermes_logging.getMainLogger()

BASE_PUBLISH_URL = "https://racetrack.eng.vmware.com"
PUBLISH_USER = "svc.blockchain_1"
PUBLISH_DEFAULT_PRODUCT = "blockchain"
PUBLISH_DEFAULT_HOST_OS = "Linux"
PUBLISH_DEFAULT_SET_TYPE = "Smoke"
REQ_SESSION = requests.Session() # keep connection alive to avoid excessive HTTPS handshakes if possible
DEFAULT_SET_ID = { "setId" : None, "buildSetId": None, "errored": False }


def getTestingEnvironmentInfo():
  configObject = helper.getUserConfig()
  runInfo = helper.getJenkinsJobNameAndBuildNumber()
  jobName = runInfo["jobName"]
  buildNumber = runInfo["buildNumber"]
  # Main MR run
  if helper.jenkinsRunTypeIs(helper.JENKINS_RUN_MAIN_MR):
    # Racetrack Test Results Table for Main MR
    # https://racetrack.eng.vmware.com/resultsetlist.php?type=advanced&product=blockchain&branch=MR
    return {
      "jobName": jobName,
      "build": buildNumber,
      "buildType": "Dev",
      "branch": "MR",
      "description": "MR Run GitLab",
    }
  # Master run
  elif helper.jenkinsRunTypeIs(helper.JENKINS_RUN_MASTER):
    # Racetrack Test Results Table for Master
    # https://racetrack.eng.vmware.com/resultsetlist.php?type=advanced&product=blockchain&branch=master
    return {
      "jobName": jobName,
      "build": buildNumber,
      "buildType": "Candidate",
      "branch": "master",
      "description": "Master GitLab",
    }
  # Release branch run
  elif helper.jenkinsRunTypeIs(helper.JENKINS_RUN_RELEASE_BRANCH):
    # Racetrack Test Results Table for Master
    # https://racetrack.eng.vmware.com/resultsetlist.php?type=advanced&product=blockchain&branch=release
    productVersion = os.getenv("product_version")
    if productVersion is None: productVersion = "None"
    return {
      "jobName": jobName,
      "build": buildNumber,
      "buildType": "Release",
      "branch": "release",
      "description": productVersion,
    }
  else:
    # Racetrack Test Results Table for All Other Runs
    # https://racetrack.eng.vmware.com/resultsetlist.php?type=advanced&product=blockchain&branch=other
    return {
      "jobName": jobName,
      "build": buildNumber,
      "buildType": "Other",
      "branch": "other",
      "description": jobName + '/' + buildNumber,
    }


def finalize(result, forBuilding=False):
  try:
    setId = getSetId(forBuilding=forBuilding)
    
    if not setId:
      print("Racetrack set cannot finalize; the set did not start and save the setId to file.")
      return

    resultSetLink = getResultSetLink(setId)
    if resultSetLink:
      print("")
      if not forBuilding:
        print("Testing-stage Racetrack result set for this run:")
      else:
        print("Build-stage only result set:")
      print(resultSetLink)

    if result == "ABORTED":
      setUpdate(setId, status="Aborted")
    elif result == "FAILURE":
      setUpdate(setId, status="Waiting for Triage")
      if not os.path.exists(helper.getJenkinsWorkspace() + "/failure_summary.log"):
        # Failed but none of the cases reported as failed == likely pipeline error
        # Report this case to Racetrack so that failed runs are ALWAYS marked as failed.
        caseId = caseStart("_Pipeline", "check_pipeline_result", "The run has failed.", forBuilding=forBuilding)
        caseEnd(caseId, 'FAIL')
    setEnd(setId)
  except Exception as e:
    helper.hermesNonCriticalTrace(e)


def setStart(testSetType="Smoke", forBuilding=False):
  '''
    Starts a stage or a set of tests
    https://wiki.eng.vmware.com/RacetrackWebServices#TestSetBegin  
  '''
  info = getTestingEnvironmentInfo()
  product = info["product"] if "product" in info else PUBLISH_DEFAULT_PRODUCT
  user = info["user"] if "user" in info else PUBLISH_USER
  setId = requestWithPathAndParams("/TestSetBegin.php", {
    "BuildID" : info["build"],
    "User": user,
    "Product": product,
    "Description": info["description"],
    "BuildType": info["buildType"] if not forBuilding else info["buildType"] + "-build",
    "Branch": info["branch"] if not forBuilding else info["branch"] + "-build",
    "HostOS": PUBLISH_DEFAULT_HOST_OS,
    "TestType": testSetType
  })
  return setId


def setUpdate(setId, buildId=None, user=None, description=None, hostOS=None,
              branch=None, buildType=None, testType=None, status=None, language=None):
  '''
    Update a racetrack test set
    https://wiki.eng.vmware.com/RacetrackWebServices#TestSetUpdate
  '''
  updateObj = { "ID" : setId }
  if buildId: updateObj["BuildID"] = buildId
  if user: updateObj["User"] = user
  if description: updateObj["Description"] = description
  if hostOS: updateObj["HostOS"] = hostOS
  if branch: updateObj["Branch"] = branch
  if buildType: updateObj["BuildType"] = buildType
  if testType: updateObj["TestType"] = testType
  if status: updateObj["Status"] = status
  if language: updateObj["Language"] = language

  requestWithPathAndParams("/TestSetUpdate.php", updateObj)
  

def setEnd(setId):
  '''
    Ends the initiated set, given setId.
    https://wiki.eng.vmware.com/RacetrackWebServices#TestSetEnd  

    Parameters:  
      `setId` (str): Unique setId returned by setStart
  '''
  return requestWithPathAndParams("/TestSetEnd.php", {
    "ID" : setId
  })


def caseStart(suiteName, caseName, description=None, setId=None, startTime=None, 
              machineName=None, TCMSID=None, inputLanguage="EN", guestOS=None, forBuilding=False):
  '''
    Creates and returns caseId with supplied parameters
    https://wiki.eng.vmware.com/RacetrackWebServices#TestCaseBegin  

    Parameters:  
      `suiteName` (str): The feature that is being tested (TEST SUITE)
      `caseName` (str): The name of the test case (TEST CASE)
      `setId` (str): setId of this run, if unsupplied, automatically extracted where Jenkins had set it
      `startTime` (int): If not provided, Now() is used.
      `description` (str): A description of this test case (defaults to whatever was provided for Name)
      `machineName` (str): The host that the test is running against
      `TCMSID` (str): A comma-separated set of values that correspond to the Testlink Test Case Management System Id's (TCMSID) of this test case.
      `guestOS` (str): The Guest Operating System
  '''
  # if setId is None, try to get it from setIdFile 
  # which was created when this Jenkins run passes python init point
  if setId is None:
    setId = getSetId(forBuilding=forBuilding)
    if not setId: return None # still no setId found.
      
  caseId = requestWithPathAndParams("/TestCaseBegin.php", {
    "ResultSetID" : setId,
    "Feature": suiteName,
    "Description": description,
    "Name": caseName,
    "StartTime": startTime,
    "MachineName": machineName,
    "TCMSID": TCMSID,
    "InputLanguage": inputLanguage,
    "GOS": guestOS
  })
  return caseId


def caseEnd(caseId, result, endTime=None):
  '''
    Ends a case with result and end time
    https://wiki.eng.vmware.com/RacetrackWebServices#TestCaseEnd  

    Parameters:  
      `caseId` (str): Unique case Id returned by caseStart
      `result` (str): The result of the test. Enum of 'PASS','FAIL','RUNNING','CONFIG','SCRIPT','PRODUCT','PRODCHANGE','RERUNPASS','BLOCKED','UNSUPPORTED'  
      `endTime` (int): If not provided, Now() is used.  
  '''
  return requestWithPathAndParams("/TestCaseEnd.php", {
    "ID" : caseId,
    "Result": result,
    "EndTime": endTime,
  })


def perfLog(setId, featureName, caseName, value, perfType=None):
  '''
    Ends a case with result and end time
    https://wiki.eng.vmware.com/RacetrackWebServices#PerfData  

    Parameters:  
      `setId` (str): Unique set Id returned by setStart
      `featureName` (str): The feature being tested
      `caseName` (str): The specific performance case being reported
      `perfType` (str): Enum of 'Seconds', 'Memory', 'CPU', 'NetworkIO', 'DiskIO'
      `value` (num): Numeric value of the reported performance metric
  '''
  if perfType is None: perfType = 'Seconds'
  return requestWithPathAndParams("/PerfData.php", {
    "ResultSetID" : setId,
    "Feature": featureName,
    "Measure": caseName,
    "Type": perfType,
    "Value": str(value)
  })


def requestWithPathAndParams(requestPath, params):
  '''
    Racetrack WebService are interactable with POST and REQUEST.
    Since {reqPath + param} format is recurring in all publish methods
    Make a helper function to reduce code size & complexity
    e.g. requestWithPathAndParams("/someAPIPath.php", {
      "SomeCorrespondingArg1" : v1,
      "SomeCorrespondingArg2" : v2,
      ...
      "SomeCorrespondingArgN" : vN,
    })
    TODO Also selective implement http REQUEST method for paths (
      TestCaseLog, TestCaseTriage, UpdateRecommended, TestCaseLogViewer
    )
  '''
  try: 
    paramsValidated = {}
    for paramName in params: # ignore all dict entry with None value
      if params[paramName] is not None:
        paramsValidated[paramName] = params[paramName]
    urlEncodedParams = urllib.parse.urlencode(paramsValidated)
    response = REQ_SESSION.post( url = BASE_PUBLISH_URL + requestPath + "?" + urlEncodedParams )
    if response.status_code == 200:
      return str(response.content.decode('utf-8'))
    else:
      content = response.content.decode('utf-8')
      log.info("Racetrack {} has returned with {}, content: {}".format(requestPath, response.status_code, content))
      return None
  except Exception as e:
    helper.hermesNonCriticalTrace(e)
    return None


def getIdFilePath():
  if helper.getJenkinsWorkspace(): # Jenkins env
    return helper.getJenkinsWorkspace() + helper.RACETRACK_SET_ID_PATH
  else: # local env
    return "../vars/" + helper.RACETRACK_SET_ID_FILE

def setSetId(setId, forBuilding=False):
  if not setId: return None
  path = getIdFilePath()
  data = json.load(open(path, 'r')) if os.path.isfile(path) else {}
  if not forBuilding:
    data["setId"] = setId
    DEFAULT_SET_ID["setId"] = setId
  else:
    data["buildSetId"] = setId
    DEFAULT_SET_ID["buildSetId"] = setId
  with open(getIdFilePath(), "w+") as f:
    f.write(json.dumps(data, indent = 4))
    return True

def getSetId(forBuilding=False):
  # if errored once, no need to retry file read many times
  if DEFAULT_SET_ID["errored"]: return None
  # if exists, cached setId in this Jenkins run
  if not forBuilding:
    if DEFAULT_SET_ID["setId"]: return DEFAULT_SET_ID["setId"]
  else:
    if DEFAULT_SET_ID["buildSetId"]: return DEFAULT_SET_ID["buildSetId"]
  # else, fetch from id file
  idFilePath = getIdFilePath()
  if not os.path.exists(idFilePath): return None
  try:
    with open(idFilePath, "r") as f:
      data = json.loads(f.read())
      DEFAULT_SET_ID["setId"] = data["setId"] if "setId" in data else None
      DEFAULT_SET_ID["buildSetId"] = data["buildSetId"] if "buildSetId" in data else None
      if not forBuilding:
        return DEFAULT_SET_ID["setId"]
      else:
        return DEFAULT_SET_ID["buildSetId"]
  except Exception as e:
    DEFAULT_SET_ID["errored"] = True
    helper.hermesNonCriticalTrace(e)
  return None


def getResultSetLink(setId=None, forBuilding=False):
  if not setId: setId = getSetId(forBuilding=forBuilding)
  if setId: return BASE_PUBLISH_URL + "/result.php?id=" + setId
  else: return None
