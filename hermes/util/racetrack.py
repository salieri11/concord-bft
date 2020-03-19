#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import logging
import traceback
import time
import requests
import os
import urllib.parse
from . import helper, cert, hermes_logging
log = hermes_logging.getMainLogger()

BASE_PUBLISH_URL = "https://racetrack.eng.vmware.com"
PUBLISH_USER = "svc.blockchain_1"
PUBLISH_DEFAULT_PRODUCT = "VMware Blockchain"
PUBLISH_DEFUALT_DESC = "Test Run for Racetrack Publish"
PUBLISH_DEFAULT_HOST_OS = "Linux Photon OS"
PUBLISH_DEFAULT_SET_TYPE = "Smoke"
REQ_SESSION = requests.Session() # keep connection alive to avoid excessive HTTPS handshakes if possible


def getTestingEnvironmentInfo():
  configObject = helper.getUserConfig()
  runInfo = helper.getJenkinsJobNameAndBuildNumber()
  jobName = runInfo["jobName"]
  buildNumber = runInfo["buildNumber"]
  # Main MR run
  if helper.jenkinsRunTypeIs(helper.JENKINS_RUN_MAIN_MR):
    # Racetrack Test Results Table for Main MR
    # https://racetrack.eng.vmware.com/resultsetlist.php?type=product&product=VMware%20Blockchain%20MR&day=today
    runInfo = helper.getJenkinsJobNameAndBuildNumber()
    return {
      "product": "VMware Blockchain MR", 
      "description": "Main MR run tests",
      "buildType": "MR",
      "branch": buildNumber # be able to sort by build number
    }
  # Master run
  elif helper.jenkinsRunTypeIs(helper.JENKINS_RUN_MASTER):
    # Racetrack Test Results Table for Master
    # https://racetrack.eng.vmware.com/resultsetlist.php?type=product&product=VMware%20Blockchain%20Master&day=today
    return {
      "product": "VMware Blockchain Master",
      "description": "Full-coverage testing for product",
      "buildType": "Master",
      "branch": buildNumber # be able to sort by build number (e.g. 4821)
    }
  # Release branch run
  elif helper.jenkinsRunTypeIs(helper.JENKINS_RUN_RELEASE_BRANCH):
    # Racetrack Test Results Table for Master
    # https://racetrack.eng.vmware.com/resultsetlist.php?type=product&product=VMware%20Blockchain%20Release&day=today
    runTypeInfo = helper.getJenkinsRunTypeInfo()
    productVersion = os.getenv("product_version")
    return {
      "product": "VMware Blockchain Release",
      "description": "Official release selected from release candidates",
      "buildType": "Release",
      "branch": productVersion # be able to sort by productVersion (e.g. 0.5.0.1123, 0.6.1.1233, ...)
    }
  else:
    # Racetrack Test Results Table for All Other Runs
    # https://racetrack.eng.vmware.com/resultsetlist.php?type=product&product=VMware%20Blockchain&day=today
    return {
      "product": "VMware Blockchain",
      "description": "Other misc. builds",
      "buildType": "Other",
      "branch": jobName + '/' + buildNumber # be able to sort by run unique indentifier
    }


def setStart(setName, setType="Smoke"):
  '''
    Starts a stage or a set of tests
    https://wiki.eng.vmware.com/RacetrackWebServices#TestSetBegin  

    Parameters:  
      `setName` (str): Name of a stage or test suite
  '''
  jenkinsBuildId = helper.getJenkinsBuildId()
  buildNumber = jenkinsBuildId.split("/")[-1]
  info = getTestingEnvironmentInfo() 
  setId = requestWithPathAndParams("/TestSetBegin.php", {
    "BuildID" : jenkinsBuildId + '/' + setName,
    "User": PUBLISH_USER,
    "Product": info["product"],
    "Description": info["description"],
    "BuildType": info["buildType"],
    "Branch": info["branch"],
    "HostOS": PUBLISH_DEFAULT_HOST_OS,
    "ServerBuildID": buildNumber,
    "TestType": setType
  })
  return setId
  

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


def caseStart(setId, featureName, caseName, startTime=None, description=None, machineName=None, TCMSID=None, inputLanguage=None, guestOS=None):
  '''
    Creates and returns caseId with supplied parameters
    https://wiki.eng.vmware.com/RacetrackWebServices#TestCaseBegin  

    Parameters:  
      `setId` (str): Unique setId returned by setStart
      `featureName` (str): The feature that is being tested
      `caseName` (str): The name of the test case
      `startTime` (int): If not provided, Now() is used.
      `description` (str): A description of this test case (defaults to whatever was provided for Name)
      `machineName` (str): The host that the test is running against
      `TCMSID` (str): A comma-separated set of values that correspond to the Testlink Test Case Management System Id's (TCMSID) of this test case.
      `guestOS` (str): The Guest Operating System
  '''
  caseId = requestWithPathAndParams("/TestCaseBegin.php", {
    "ResultSetID" : setId,
    "Feature": featureName,
    "Name": caseName,
    "StartTime": startTime,
    "Description": description,
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
    log.info(e); traceback.print_exc()
    return None


