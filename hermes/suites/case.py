#################################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# cases.py 
# Individual Test Cases Reporting (Test Functions Hijacking)
#
#################################################################################

import inspect
import traceback
import json
import hashlib
import base64
import urllib.parse
import os
import re
from util import jenkins
from inspect import getframeinfo, stack
from enum import Enum
from functools import wraps

from util import hermes_logging, helper, wavefront, racetrack

log = hermes_logging.getMainLogger()

FAILURE_SUMMARY_FILENAME = "failure_summary"
FAILURE_FOLDER = "otherFailures"
SUITENAME_OVERRIDE = {
  # EthCoreVMTests have over 500+ cases; hard to scroll down.
  # prepend 'Z_' for Racetrack to show this suite last on the table
  "EthCoreVmTests": "Z_EthCoreVmTests"
}


class CaseType(Enum):
  SMOKE = "SMOKE"


def getInvocationProxy(testFunc):
  '''
    Returnes a wrapped proxy function of supplied testFunc
    This enables triggering pre/post script execution with
    having clear test context (used to auto publish test cases
    on to Racetrack or Wavefront)
  '''
  def pre(*args, **kwargs):
    '''
      Script to be executed before test function is called
    '''
    try:
      setattr(testFunc, "_suite_name", helper.CURRENT_SUITE_NAME)
      if helper.thisHermesIsFromJenkins():
        dontReport = hasattr(testFunc, "_dont_report")
        suiteName = helper.CURRENT_SUITE_NAME
        caseName = testFunc._short_name
        description = testFunc._description
        overrides = None
        if hasattr(testFunc, "_dynamic_report_override"):
          overrides = getattr(testFunc, "_dynamic_report_override")(*args, **kwargs)
          if overrides:
            if "dontReport" in overrides: dontReport = overrides["dontReport"]
            if "suiteName" in overrides:
              suiteName = overrides["suiteName"]; setattr(testFunc, "_suite_name", suiteName)
            if "caseName" in overrides: caseName = overrides["caseName"]
            if "description" in overrides: description = overrides["description"]
        if suiteName in SUITENAME_OVERRIDE:
          suiteName = SUITENAME_OVERRIDE[suiteName]
        if dontReport:
          setattr(testFunc, "_case_id", False)
          return
        caseId = racetrack.caseStart(
          suiteName = suiteName,
          caseName = caseName,
          description = description,
        )
        setattr(testFunc, "_case_id", caseId)
    except Exception as e:
      helper.hermesNonCriticalTrace(e, "Cannot execute pre function of {}".format(testFunc._name))
      return
      
  def post(result, errorMessage, stackInfo, originalE, *args, **kwargs):
    '''
      Script to be executed after test case function finished
    '''
    try:
      if helper.thisHermesIsFromJenkins() and hasattr(testFunc, "_case_id") and testFunc._case_id:
        racetrack.caseEnd(
          caseId = testFunc._case_id,
          result = "PASS" if result else "FAIL"
        )
      dontReport = hasattr(testFunc, "_dont_report")
      if result is False and not dontReport and helper.CURRENT_SUITE_LOG_FILE:
        extractAndSaveFailurePoint(testFunc, errorMessage, stackInfo, originalE, args, kwargs)
    except Exception as e:
      helper.hermesNonCriticalTrace(e, "Cannot execute post function of {}".format(testFunc._name))
      return

  @wraps(testFunc)
  def callHijacker(*args, **kwargs):
    '''
      Wraps the supplied test function in order to intercept the supplied
      arguments, and execute pre/post script 
    '''
    try:
      
      pre(*args, **kwargs) # set test case as started

      result = testFunc(*args, **kwargs) # invoke test function; stack trace below if errored.
      
      booleanResult = True if result else False
      if result is None: booleanResult = True # for pytests, no assertion created means success
      errorMessage = ""
      stackInfo = None

      # non-pytest result always tuple (True, None) or (False, message)
      if type(result) is tuple or type(result) is list:
        booleanResult = result[0]
        errorMessage = result[1]
        stackInfo = result[2]
      
      post(booleanResult, errorMessage, stackInfo, None, *args, **kwargs) # report the test result (PASS/FAIL)
      
      # return original result as it is for other original scripts depending on it
      return result

    except Exception as e:
      try:
        # Create stack trace for this exception
        failurePoint = traceback.extract_tb(e.__traceback__)[1]
        stackInfo = stack()[1:]
        stackInfo.insert(0, failurePoint)
        errorMessage = e.__class__.__name__ + ': ' + str(e)
        post(False, errorMessage, stackInfo, e, *args, **kwargs) # report as FAIL when exception raised
      except Exception as e:
        helper.hermesNonCriticalTrace(e)
      raise # raise the same exception/assertion so TestSuite/Pytest can handle it
  
  setattr(callHijacker, "_is_invocation_proxy", True)
  return callHijacker


def getStackInfo(e=None):
  if not e:
    return stack()[1:] # stack info not including this function
  else:
    failurePoint = traceback.extract_tb(e.__traceback__)[1]
    stackInfo = stack()[1:]
    stackInfo.insert(0, failurePoint)
    return stackInfo


def passed(message=None):
  '''
    Captures stack infomation before returning (True, message)
    Used to pin-point the exact point of test passing
  '''
  return (True, message, stack()[1:])


def failed(message):
  '''
    Captures stack infomation before returning (False, message)
    Used to pin-point the exact point of test failure
  '''
  return (False, message, stack()[1:])


def describe(description="", casetype=CaseType.SMOKE, dontReport=False, dynamicReportOverride=None):
  '''
    Decorator function to describe the test case and hijack the test function
    to make it automatically publish caseBegin/caseEnd with the case outcome
    
    Decorator usage:
    ```python
    @describe("Should deploy 7 nodes and pass sanity check")
    def test_function_name(arg1, arg2):
    ```

    `dynamicReportOverride` function can be supplied to uniquely report cases 
    where the same test function is used multiple times with different arguments
    by overriding `suiteName`, `caseName`, or `description` of the case based on 
    provided arguments to the test function. `dynamicReportOverride` function
    must have the same argument signature as the test function it is trying to
    override (e.g. `def dynamicReportOverride_func(arg1, arg2):` )
  '''
  def wrapper(func):
    setTestCaseAttributes(func, description, casetype, stack())
    if dontReport: setattr(func, "_dont_report", True)
    if dynamicReportOverride: setattr(func, "_dynamic_report_override", dynamicReportOverride)
    proxy = getInvocationProxy(func)
    return proxy
  return wrapper



def catchFailurePoint(func):
  '''
    Decorator function to capture and exception when func throw it
    without external reporting (e.g. Racetrack, Wavefront)
    Used for saving failure summary of non-test function (e.g. main)
  '''
  @wraps(func)
  def wrapper(*args, **kwargs):
    try:
      return func(*args, **kwargs)
    except Exception as e:
      try:
        # Create stack trace for this exception
        failurePoint = traceback.extract_tb(e.__traceback__)[1]
        stackInfo = stack()[1:]
        stackInfo.insert(0, failurePoint)
        errorMessage = e.__class__.__name__ + ': ' + str(e)
        extractAndSaveFailurePoint(func, errorMessage, stackInfo, e, args, kwargs)
      except Exception as e:
        helper.hermesNonCriticalTrace(e)
      raise
  return wrapper


def setTestCaseAttributes(func, description, casetype, stackInfo):
  decoratorsList = getDecorators(func)
  shortCaseName = getCaseNameShort(func.__name__)
  setattr(func, "_stack", stackInfo)
  setattr(func, "_name", func.__name__)
  setattr(func, "_short_name", shortCaseName)
  setattr(func, "_description", description)
  setattr(func, "_decorators", decoratorsList)
  setattr(func, "_casetype", casetype)


def extractAndSaveFailurePoint(func, errorMessage, stackInfo, originalE, args, kwargs):
  '''
    Based on captured stackInfo and argument to test function output
    failure point summary to `failure_summary` json and log. This is
    designed for pipeline notification to provide a way to one-click
    access the summary and target log file. 
  '''
  try:
    # Save arguments supplied to the test function
    testArgsMap = kwargs
    nofixtureArgsMap = {} # avoid randomized props in fixtures hindering signature pinpointing
    funcArgs = inspect.getfullargspec(func)[0]
    for i, argName in enumerate(funcArgs):
      if i >= len(args): break
      testArgsMap[argName] = args[i]
      if not argName.startswith("fx"): nofixtureArgsMap[argName] = args[i]
    testArgsMap = filterOutUnnecessaryArguments(testArgsMap)
    testArgsSerial = json.dumps(nofixtureArgsMap, indent=4, default=lambda o: '<NOT_SERIALIZABLE>')
    testArgsSummary = []
    for testArgName in testArgsMap:
      testArgValue = testArgsMap[testArgName]
      if type(testArgValue) in [dict, list, tuple]:
        try: testArgValue = json.dumps(testArgValue, indent=4, default=str)
        except: pass
      testArgValue = re.sub(r" at 0x[0-9a-fA-F]+", "", str(testArgValue)) # remove object refs (e.g."at 0x7f8f7a70d898")
      testArgsSummary.append("{} = {}".format(testArgName, testArgValue))
    testArgsSummary = "\n".join(testArgsSummary)
    
    # If from Jenkins, output directly to workspace, otherwise defulat log path
    artifactSummaryLogPath = ""
    artifactFilteredLogPath = ""
    artifactTestFolderPath = ""
    artifactProductFolderPath = ""
    if helper.thisHermesIsFromJenkins():
      run = helper.getJenkinsJobNameAndBuildNumber()
      # link to test log should be changed to Jenkins artifacts path
      workspaceTestFolder = "/".join(helper.CURRENT_SUITE_LOG_FILE.split("/")[:-1])
      basePath = jenkins.getJenkinRunBaseUrl(run["jobName"], run["buildNumber"])
      testLogPath = '/artifact/testLogs/' + helper.CURRENT_SUITE_LOG_FILE.split('/testLogs/')[1]
      testLogFolder = "/".join(testLogPath.split("/")[:-1]) if "/" in testLogPath else "" # drop filename
      attemptFolder = "/product_logs_attempt_" + str(helper.CURRENT_SUITE_PRODUCT_ATTEMPT_NUMBER)
      if os.path.isdir(workspaceTestFolder + attemptFolder):
        productLogFolder = testLogFolder + attemptFolder
      elif os.path.isdir(workspaceTestFolder + "/product_logs"):
        productLogFolder = testLogFolder + "/product_logs"
      else:
        productLogFolder = ""
      testLogPath = basePath + testLogPath.replace(' ', '%20') # encodeURL
      artifactSummaryLogPath = basePath + '/artifact/summary/' + FAILURE_SUMMARY_FILENAME + '.log'
      artifactFilteredLogPath = basePath + '/artifact/summary/errors_only.log'
      artifactTestFolderPath = basePath + testLogFolder.replace(' ', '%20')
      artifactProductFolderPath = basePath + productLogFolder.replace(' ', '%20')
      outputPath = helper.getJenkinsWorkspace() + '/summary/' # summary folder
    elif helper.CURRENT_SUITE_LOG_FILE:
      testLogPath = helper.CURRENT_SUITE_LOG_FILE
      outputPath = "/".join(testLogPath.split("/")[:-1]) + "/"
    else:
      testLogPath = ""
      outputPath = ""
    
    failurePointInfo = traceStackData(stackInfo[0], func)

    # Capture unique signature of the failure point
    suiteName = func._suite_name if hasattr(func, "_suite_name") else helper.CURRENT_SUITE_NAME
    returnCodeLine = failurePointInfo["line"].strip()
    caseName = func.__name__
    caseNameShort = getCaseNameShort(caseName)
    suiteAndCase = suiteName + '::' + caseName
    if hasattr(func, "_dynamic_report_override"): # implies multiple calls to single test function
      # For varying arguments like `EthCoreVMTests`, since there are multiple cases
      # tested within the same test function argument itself is included to uniquely
      # identify the specfic test case varying by the argument and where it returned.
      ingested = "hermes::" + suiteAndCase + "::" + testArgsSerial + "::" + returnCodeLine
    else:
      # Otherwise, HASH(suiteName + caseName + returnCodeLineString)
      # to uniquely identify the failure return code line
      ingested = "hermes::" + suiteAndCase + "::" + returnCodeLine
    longSignature = hashlib.sha256(ingested.encode()) 
    shortSignature = base64.b64encode(longSignature.digest()[:6]) # short, 6-byte
    longSignature = longSignature.hexdigest().upper()[:32] # 32-hex = 16-byte (e.g. F4E9A7CADAF8A0B9359740D5F84D118E)
    shortSignature = shortSignature.decode("utf-8").replace("+", "A").replace("/", "A") # (e.g. F9Omnytr4)
    
    stackTraceList = traceback.format_stack(limit=10)
    stackTraceList.reverse()
    stackTraceList = stackTraceList[2:]
    while len(stackTraceList) > 0 and "post(" in stackTraceList[0]:
      stackTraceList = stackTraceList[1:]
    
    if originalE:
      stackTrace = "\n".join(traceback.format_exception(
        originalE.__class__, originalE, originalE.__traceback__
      ))
    else:
      stackTrace = "".join(stackTraceList)

    cmdlineArgsCopy = json.loads(json.dumps(helper.CMDLINE_ARGS, indent=4, default=str))
    if "userConfig" in cmdlineArgsCopy: del cmdlineArgsCopy["userConfig"] # displayed separately
    if "zoneConfig" in cmdlineArgsCopy: del cmdlineArgsCopy["zoneConfig"] # displayed separately

    replicasConfigOutput = ""
    try: # display replicas config if replicas.json exists
      repliacasConfigPath = helper.CMDLINE_ARGS["replicasConfig"]
      replicas = helper.parseReplicasConfig(repliacasConfigPath) # check default path (/tmp/replicas.json)
      if replicas:
        replicasConfigOutput = "{}replicasConfigPath = {}\n{}\n\n\n".format(
          "============================================= Replicas Config ===========================================================\n",
          repliacasConfigPath, json.dumps(replicas, indent=4, default=str))
    except Exception as e: pass

    failureSummaryLog = "\n{}\n\n\n{}\n\n\n{}\n\n\n{}\n\n\n{}\n\n\n{}\n\n\n{}".format(
      "{}{} :: {}\n{}".format(
        "=============================================== Context =================================================================\n",
        helper.CURRENT_SUITE_NAME, func.__name__,
        testLogPath
      ),
      "{}{}\n\n{}Failed at {}:{}   (Signature={})\n\n{}{}".format(
        "=========================================== Failure Message =============================================================\n",
        errorMessage,
        "=========================================== Failed Function =============================================================\n",
        failurePointInfo["file"], failurePointInfo["lineno"], shortSignature,
        testArgsSummary + "\n\n" if testArgsSummary else "",
        failurePointInfo["body"],
      ),
      "{}{}".format(
        "============================================= Stack Trace ===============================================================\n",
        stackTrace
      ),
      "{}{}".format(
        "=========================================== Cmdline Arguments ===========================================================\n",
        json.dumps(cmdlineArgsCopy, indent=4, default=str)
      ),
      "{}{}".format(
        "=============================================== User Config =============================================================\n",
        json.dumps(helper.getUserConfig(), indent=4, default=str)
      ),
      "{}{}".format(
        "=============================================== Zone Config =============================================================\n",
        json.dumps(helper.getZoneConfig(), indent=4, default=str)
      ),
      replicasConfigOutput
    )

    failureSummaryJson = {
      "from": "Hermes",
      "suite": helper.CURRENT_SUITE_NAME,
      "function_name": func.__name__,
      "file": failurePointInfo["file"],
      "line_number": failurePointInfo["lineno"],
      "line": returnCodeLine,
      "suite_name": suiteName,
      "test_name": caseName,
      "test_name_short": caseNameShort,
      "message": errorMessage,
      "sig": { "long": longSignature, "short": shortSignature },
      "stack_trace": stackTrace,
      "args": testArgsMap,
      "body": failurePointInfo["body"],
      "log_path": testLogPath,
      "cmdline_args": helper.CMDLINE_ARGS,
      "user_config": helper.getUserConfig(),
      "zone_config": helper.getZoneConfig(),
      "log_path_local": helper.CURRENT_SUITE_LOG_FILE,
      "summary_url": artifactSummaryLogPath,
      "filtered_url": artifactFilteredLogPath,
      "test_folder_url": artifactTestFolderPath,
      "product_folder_url": artifactProductFolderPath,
    }

    # There may be multiple failures per run or test suite
    if outputPath:
      if not os.path.exists(outputPath + FAILURE_SUMMARY_FILENAME + '.log'):
        # first failure, save info direclty to workspace
        outputFileName = outputPath + FAILURE_SUMMARY_FILENAME
        # in Jenkins context, also save first failure summary JSON to vars directory
        if os.path.exists(outputPath + 'blockchain/vars'):
          with open(outputPath + 'blockchain/vars/' + FAILURE_SUMMARY_FILENAME + '.json', "w+") as f:
            f.write(json.dumps(failureSummaryJson, indent=4, default=str))
      else: # other failures, save to `otherFailures` directory
        if not os.path.exists(outputPath + FAILURE_FOLDER):
          os.makedirs(outputPath + FAILURE_FOLDER)
        outputFileName = outputPath + FAILURE_FOLDER + "/" + FAILURE_SUMMARY_FILENAME + "_" + shortSignature
      
      # Save to .log and .json
      with open(outputFileName + '.log', "w+") as f:
        f.write(failureSummaryLog)
      with open(outputFileName + '.json', "w+") as f:
        f.write(json.dumps(failureSummaryJson, indent=4, default=str))

  except Exception as e:
    helper.hermesNonCriticalTrace(e)


def extractAndSavePipelineFailurePoint(pipelineError):
  '''
    Captures pipeline error reported from groovy file.
    ```python
      pipelineError = {
        "stage_name": stageName,
        "build_url": env.BUILD_URL,
        "error_message": e.toString(),
        "stack_trace": ExceptionUtils.getStackTrace(e),
      }
    ```
  '''
  try:
    if not helper.thisHermesIsFromJenkins(): return
    stageName = pipelineError["stage_name"]
    buildURL = pipelineError["build_url"] # ends with /
    errorMessage = pipelineError["error_message"]
    filename = "(anonymous function)"
    lineNumber = "(unknown)"
    stackTrace = pipelineError["stack_trace"]
    stackTraceOriginal = stackTrace
    try: # try to parse only groovy errors if possible
      stackTrace = pipelineError["stack_trace"]
      stackTraceLines = stackTrace.split("\n")
      targetLines = []; targetLines.append(stackTraceLines[0])
      alreadyDetected = False
      for stackTraceLine in stackTraceLines:
        if ".groovy:" in stackTraceLine:
          if not alreadyDetected:
            lineNumber = int(re.search(r'\d+', stackTraceLine.split(".groovy:")[1]).group(0))
            filename = stackTraceLine.split("/JenkinsLibOnGitlab/")[1].split(":")[0]
            alreadyDetected = True
          targetLines.append(stackTraceLine)
      stackTrace = "\n".join(targetLines)
    except Exception as e: helper.hermesNonCriticalTrace(e)
    workspace = helper.getJenkinsWorkspace()
    imageNamePath = workspace + '/summary/failed_build_name.log'
    outputFileName = workspace + '/summary/failure_summary'
    # Image Build Failures
    if stageName == "Build" and os.path.isfile(imageNamePath):
      imageName = ""; buildLogPath = ""; buildErrorLog = ""
      with open(workspace + '/summary/failed_build_name.log', 'r') as f: imageName = f.read().strip()
      with open(workspace + '/summary/failed_build_path.log', 'r') as f: buildLogPath = f.read().strip()
      with open(workspace + '/summary/failed_build_error.log', 'r') as f: buildErrorLog = f.read().strip()
      buildErrorLogOriginal = buildErrorLog
      deepScanErrorLines = scanBuildErrorLines(buildLogPath, buildErrorLogOriginal)
      if deepScanErrorLines:
        buildErrorLog = deepScanErrorLines
        with open(workspace + '/summary/failed_build_error.log', 'w+') as w: w.write(buildErrorLog)
      buildErrorLogSummary = buildErrorLog
      buildErrorLogLines = buildErrorLog.split("\n")
      if len(buildErrorLogLines) > 15: # error log too long to display, cut it
        buildErrorLogSummary = "\n".join(buildErrorLogLines[-15:]) # last 15 lines
      originalErrorMessage = errorMessage
      errorMessage = "Error while building Docker image for '" + imageName + "'"
      buildLogFile = buildLogPath.split('/blockchain/')[1]
      failureSummaryLog = "\n{}\n\n\n{}\n\n\n".format(
        "{}{} :: {}\n{}".format(
          "=============================================== Context =================================================================\n",
          "Build", imageName,
          buildURL + 'artifacts/blockchain/' + buildLogFile
        ),
        "{}{}\n\n{}\n{}".format(
          "=========================================== Failure Message =============================================================\n",
          errorMessage,
          "============================================ Error Vicinity =============================================================\n",
          buildErrorLog
        ),
      )
      failureSummaryJson = {
        "from": "Build",
        "stage_name": "Build",
        "message": errorMessage,
        "original_error": originalErrorMessage,
        "build_error_log": buildErrorLog,
        "build_error_log_short": buildErrorLogSummary,
        "image": imageName,
        "log_path": buildURL + 'artifact/blockchain/' + buildLogFile,
        "summary_url": buildURL + 'artifact/summary/failure_summary.log',
        "filtered_url": buildURL + 'artifact/summary/failed_build_error.log',
        "file": filename,
        "line_number": lineNumber,
        "stack_trace": stackTrace,
        "stack_trace_original": stackTraceOriginal,
      }
      with open(outputFileName + '.log', "w+") as f:
        f.write(failureSummaryLog)
      with open(outputFileName + '.json', "w+") as f:
        f.write(json.dumps(failureSummaryJson, indent=4, default=str))
    
    # Other Pipeline Failures
    else:
      originalErrorMessage = errorMessage
      errorMessage = "Pipeline error: " + originalErrorMessage
      if stageName == "Run tests in containers" and "script returned exit code" in errorMessage:
        errorMessage = "Uncaptured error while running test suites."
      if filename.startswith("("): errorMessage += " (Couldn't find useful info to report in stack trace.)"
      failureSummaryLog = "\n{}\n\n\n{}\n\n\n{}\n\n\n{}\n\n\n".format(
        "{}{} :: {}".format(
          "=============================================== Context =================================================================\n",
          "Pipeline Stage", stageName,
        ),
        "{}{}\n\n{}Failed at {}:{}".format(
          "=========================================== Failure Message =============================================================\n",
          errorMessage,
          "========================================== Failure Location =============================================================\n",
          filename, lineNumber
        ),
        "{}{}".format(
          "============================================= Stack Trace ===============================================================\n",
          stackTrace
        ),
        "{}{}".format(
          "======================================== Original Stack Trace ===========================================================\n",
          stackTraceOriginal
        ),
      )
      failureSummaryJson = {
        "from": "Pipeline",
        "stage_name": stageName,
        "message": errorMessage,
        "original_error": originalErrorMessage,
        "log_path": None,
        "summary_url": buildURL + 'artifact/summary/failure_summary.log',
        "file": filename,
        "line_number": lineNumber,
        "stack_trace": stackTrace,
        "stack_trace_original": stackTraceOriginal,
      }
      with open(outputFileName + '.log', "w+") as f:
        f.write(failureSummaryLog)
      with open(outputFileName + '.json', "w+") as f:
        f.write(json.dumps(failureSummaryJson, indent=4, default=str))

  except Exception as e:
    helper.hermesNonCriticalTrace(e)


def traceStackData(stackData, func):
  '''
    Get failure point information based on stackData
    and target function (test function)
  '''
  try:
    failurePoint = stackData
    failureLineNumber = failurePoint.lineno
    failureLineNumber = int(failureLineNumber)
    if "/hermes/" in failurePoint.filename:
      failureTestFile = "hermes/" + failurePoint.filename.split("/hermes/")[1]
    else:
      failureTestFile = failurePoint.filename
    testFuncName = func.__name__
    sourceInfo = inspect.getsourcelines(func)
    sourceCodeLines = sourceInfo[0]
    sourceLineNumber = int(sourceInfo[1])
    revisedLines = []
    failureLine = ""
    defEncountered = False
    doTruncate = (len(sourceCodeLines) > 40)
    truncationLimit = 12
    for i, line in enumerate(sourceCodeLines):
      lineNumber = sourceLineNumber + i
      if lineNumber == failureLineNumber:
        failureLine = line
      if doTruncate and defEncountered and failureLineNumber - lineNumber > truncationLimit:
        continue
      ellipsesLines = ""
      if doTruncate and not defEncountered and line.strip().endswith("):"):
        if failureLineNumber - lineNumber > truncationLimit + 1:
          frontSpaceCount = len(line) - len(line.lstrip())
          frontSpaces = " " * (frontSpaceCount + 2)
          ellipsesLines += '\n'
          ellipsesLines += frontSpaces + '...\n'
          ellipsesLines += '\n'
        defEncountered = True
      if lineNumber >= failureLineNumber:
        # On and after failure point mark with E
        # Just like how Pytest would output
        if line == '\n': line = 'E\n'
        else: line = line[1:]; line = 'E' + line
        if doTruncate and lineNumber >= failureLineNumber + truncationLimit: break
      revisedLines.append(line)
      if ellipsesLines: revisedLines.append(ellipsesLines)
    revisedFunctionBody = ''.join(revisedLines)
    return {
      "name": testFuncName, # e.g. test_example_with_fixture
      "file": failureTestFile, # e.g. hermes/suites/sample_suite.py
      "lineno": failureLineNumber, # e.g. 40
      "line": failureLine, # e.g. assert 1 == 1
      "body": revisedFunctionBody, # post error point marked E
    }
  except Exception as e:
    helper.hermesNonCriticalTrace(e)
    return None


def getDecorators(obj, decoratorsList=None):
  '''
    Get decorators of given object, used to get list of
    decorators (e.g. @pytest.mark.smoke, etc.), which can be useful
    when publishing the type of the test or specific conditions
  '''
  decoratorsList = [] if decoratorsList is None else decoratorsList
  if type(obj).__name__ not in ["module", "class", "type", "function"]:
    return decoratorsList
  sourcelines = inspect.getsourcelines(obj)[0]
  for line in sourcelines:
    line = line.strip()
    if line.startswith("def "): break # past decorator section
    if line.startswith("@"):
      # only interested in decorator name
      if "(" in line:
        decoratorsList.append(line.split("(")[0][1:])
      else:
        decoratorsList.append(line.split("@")[1])
  return decoratorsList


def getCaseNameShort(caseName):
  if caseName.startswith("test_"): caseName = caseName[5:]
  elif caseName.startswith("_test_"): caseName = caseName[6:]
  return caseName


def reportFailedCase(suiteName, caseName, description=None):
  caseId = racetrack.caseStart(suiteName=suiteName, caseName=caseName, description=description)
  racetrack.caseEnd(caseId = caseId, result = "FAIL")


def reportPassedCase(suiteName, caseName, description=None):
  caseId = racetrack.caseStart(suiteName=suiteName, caseName=caseName, description=description)
  racetrack.caseEnd(caseId = caseId, result = "PASS")


def filterOutUnnecessaryArguments(testArgsMap):
  testArgsMap = json.loads(json.dumps(testArgsMap, default=str)) # copy
  if "self" in testArgsMap: # self argument is not needed
    testArgsMap.pop("self", None)
  # run settings are just too long to display on top; display separately later.
  if "fxHermesRunSettings" in testArgsMap:
    testArgsMap["fxHermesRunSettings"] = "<HermesRunSettings object; displayed at the bottom>"
  if "hermes_settings" in testArgsMap:
    testArgsMap["hermes_settings"] = "<HermesRunSettings object; displayed at the bottom>"
  return testArgsMap


def cleanBashColors(line):
  line = re.sub(r'\[.*?;.*?m', '', line) # remove bash colors
  line = line.replace("[m]", " ")
  line = line.replace("[m", " ")
  line = line.replace("[1m]", " ")
  line = line.replace("[1m", " ")
  line = ' '.join(line.split()) # remove multiple spaces
  return line


def scanBuildErrorLines(buildLogPath, originalLogs):
  if not os.path.isfile(buildLogPath): return None
  workspace = helper.getJenkinsWorkspace()
  # Note: just for easier matching, line content is turned to upper.
  lookFor = ["ERROR", "ERR!", "FAIL", "FATAL", "FAILURE", "FAILED"]
  ignoreFor = ["DEBUG", "WARN"]
  preErrorLinesKept = 4; postErrorLinesKept = 5 # how wide vicinity to keep
  allLines = []; keptLines = []; warm = 0; errorFound = False
  with open(buildLogPath, 'r') as f:
    for line in f:
      try:
        line = line.strip()
        if len(line) == 0: continue
        line = cleanBashColors(line)
        lineKept = False; prependList = []
        lineAllCaps = line.upper()
        if any(x in lineAllCaps for x in lookFor) and \
           not any(x in lineAllCaps for x in ignoreFor):
          errorFound = True
          warm = postErrorLinesKept; lineKept = True
          lastIndex = len(allLines) - 1
          for i in range(preErrorLinesKept): # go back and get prev lines if not kept
            idx = lastIndex - i
            if idx < 0: break
            if not allLines[idx]["kept"]:
              prependList.append(allLines[idx]["line"])
              allLines[idx]["kept"] = True
          if len(prependList) > 0: prependList = list(reversed(prependList))
        elif warm > 0: # error is still warm; keep a few lines after the error line
          warm -= 1; lineKept = True
        if len(prependList) > 0:
          for lineToBePrepended in prependList: keptLines.append(lineToBePrepended)
        if lineKept: keptLines.append(line)
        allLines.append({"line": line, "kept": lineKept})
      except Exception as e: helper.hermesNonCriticalTrace(e)
    newlyFilteredErrorLog = "\n".join(keptLines)
    if errorFound:
      return newlyFilteredErrorLog
    else:
      return "No error lines are detected. Here are the last few lines:\n\n" + originalLogs

