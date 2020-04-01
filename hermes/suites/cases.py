#################################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# cases.py 
# Individual Test Cases Reporting (Test Functions Hijacking)
#
#################################################################################

import inspect
import traceback
from enum import Enum
from functools import wraps

from util import hermes_logging, helper, wavefront, racetrack

log = hermes_logging.getMainLogger()


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
    try:
      log.info("Case: \x1b[33;1m[{}]\x1b[0m {}".format(testFunc._name, testFunc._description))
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
            if "suiteName" in overrides: suiteName = overrides["suiteName"]
            if "caseName" in overrides: caseName = overrides["caseName"]
            if "description" in overrides: description = overrides["description"]
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
      log.info("Cannot execute pre function of {}".format(testFunc._name))
      log.info(e); traceback.print_exc()
      return
      
  def post(result):
    try:
      if helper.thisHermesIsFromJenkins() and hasattr(testFunc, "_case_id") and testFunc._case_id:
        racetrack.caseEnd(
          caseId = testFunc._case_id,
          result = "PASS" if result else "FAIL"
        )
    except Exception as e:
      log.info("Cannot execute post function of {}".format(testFunc._name))
      log.info(e); traceback.print_exc()
      return

  @wraps(testFunc)
  def callHijacker(*args, **kwargs):
    try:
      pre(*args, **kwargs) # begin test case
      result = testFunc(*args, **kwargs)
      booleanResult = result
      if booleanResult is None: booleanResult = True # no assertion created means success
      if type(booleanResult) is tuple or type(booleanResult) is list: booleanResult = booleanResult[0]
      post(booleanResult) # report the test result (PASS/FAIL)
      # return original result as it is for other scripts depending on it
      return result
    except Exception as e:
      post(False) # report as FAIL when exception raised
      raise
  
  setattr(callHijacker, "_is_invocation_proxy", True)
  return callHijacker


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
    setTestCaseAttributes(func, description, casetype)
    if dontReport: setattr(func, "_dont_report", True)
    if dynamicReportOverride: setattr(func, "_dynamic_report_override", dynamicReportOverride)
    proxy = getInvocationProxy(func)
    return proxy
  return wrapper


def setTestCaseAttributes(func, description, casetype):
  decoratorsList = getDecorators(func)
  shortCaseName = func.__name__
  if shortCaseName.startswith("test_"): shortCaseName = shortCaseName[5:]
  if shortCaseName.startswith("_test_"): shortCaseName = shortCaseName[6:]
  setattr(func, "_name", func.__name__)
  setattr(func, "_short_name", shortCaseName)
  setattr(func, "_description", description)
  setattr(func, "_decorators", decoratorsList)
  setattr(func, "_casetype", casetype)


def getDecorators(obj, decoratorsList=None):
  decoratorsList = [] if decoratorsList is None else decoratorsList
  if type(obj).__name__ not in ["module", "class", "type", "function"]:
    return decoratorsList
  sourcelines = inspect.getsourcelines(obj)[0]
  for line in sourcelines:
    line = line.strip()
    if line.startswith("def "): break
    if line.startswith("@"):
      if "(" in line:
        decoratorsList.append(line.split("(")[0][1:])
      else:
        decoratorsList.append(line.split("@")[1])
  return decoratorsList
