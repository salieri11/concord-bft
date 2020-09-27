#!/usr/bin/python3

#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import json
import os
import sys
from . import helper, jenkins, hermes_logging

log = hermes_logging.getMainLogger()

# Whether this Hermes is qualified to skip test suite logic
DRY_RUN_MUST_RUN_SUITES = True


def invocationJsonPath():
    return helper.getJenkinsWorkspace() + '/summary/invocations.json'


def dryRunJsonPath():
    return helper.getJenkinsWorkspace() + '/summary/dry_run.json'


def targetComparisonJsonPath():
    return helper.getJenkinsWorkspace() + '/summary/invocations_comparison.json'


def dockerDirtyFlagFile():
    '''After launching product and testing, docker may leave residue'''
    return helper.getJenkinsWorkspace() + '/summary/docker/dirty_flag.log'


def isDryRun():
    '''
      PipelineWorks CI does not need to actually run the test suite.
      This is used to skip actual testing.
    '''
    if not helper.thisHermesIsFromJenkins(): return False
    return os.path.exists(dryRunJsonPath())


def dryRunSaveInvocation(runSuite=False):
    '''Save cmdline arguments metadata and signature for dry run comparison'''
    saveInvocationSignature(dryRunJsonPath(), runSuite)


def isQualifiedToSkip():
    try:
        suiteName = helper.CURRENT_SUITE_NAME
        global DRY_RUN_MUST_RUN_SUITES
        if not DRY_RUN_MUST_RUN_SUITES:
            log.info(f"already disqualified to skip once, running rest of the suite; "
                    f"running {suiteName}...")
            return False
        with open(dryRunJsonPath(), 'r') as f:
            dryRunJson = json.load(f)
            if os.path.exists(targetComparisonJsonPath()):
                with open(targetComparisonJsonPath(), 'r') as f2:
                    comparisonJson = json.load(f2)
            else:
                if "comparison_job" not in dryRunJson:
                    # if there is no target job to compare, just skip as dry run
                    return True
                comparisonTargetJob = dryRunJson["comparison_job"]
                buildNumber = jenkins.getTopBuildNumberForJob(comparisonTargetJob)
                while buildNumber >= 0:
                    metadata = jenkins.getRunMetadata(comparisonTargetJob, buildNumber)
                    if metadata["result"] == "SUCCESS":
                       comparisonJson = jenkins.getArtifact(comparisonTargetJob, buildNumber,
                                                            '/summary/invocations.json')
                       if comparisonJson and comparisonJson != 'null': # has invocations
                          break
                    buildNumber -= 1
                if buildNumber < 0:
                    log.info(f"cannot find latest good run to compare the sigs; "
                            f"running {suiteName}...")
                    return False
                comparisonJson['comparison_source_job'] = comparisonTargetJob
                comparisonJson['comparison_source_build_number'] = buildNumber
                with open(targetComparisonJsonPath(), 'w+') as f2:
                    f2.write(json.dumps(comparisonJson, indent=4))
            currentSig, currentArgstr = getInvocationSignature()
            if suiteName not in comparisonJson['hermes']:
                log.info(f"comparison target job doesn't have this suite {suiteName}, "
                         f"running {suiteName}...")
                return False
            targetSuite = comparisonJson['hermes'][suiteName]
            targetSig = targetSuite['sig']
            if targetSig != currentSig:
                log.info(f"comparison target signaure does not match for {suiteName}, "
                         f"{targetSig} != {currentSig}\n"
                         f"Comparison job invocation: {targetSuite['args']}\n"
                         f"Current invocation: {currentArgstr}\n"
                         f"running {suiteName}...")
                return False
            return True # signature is the same; no need to test
    except Exception as e:
      helper.hermesNonCriticalTrace(e)
      return False


def getInvocationIgnoreParams():
    '''Selective ignore main.py invoke params since some naturally change every time'''
    try:
        jobName = helper.getJenkinsJobNameAndBuildNumber()['jobName']
        desc = jenkins.getJobDescription(jobName)
        if not desc: return {}
        keyword = 'hermes invocation signature ignore params:'
        if keyword in desc:
            ignoreParams = json.loads(desc.split(keyword)[1].split('</div>')[0].strip())
            return ignoreParams
        return {}
    except Exception as e:
        helper.hermesNonCriticalTrace(e)
        return {}


def getInvocationSignature(ignoreParams={}):
    '''Get hash of the argv parameters for comparison'''
    argstr = ' '.join(sys.argv)
    workspace = helper.getJenkinsWorkspace()
    argstr = argstr.replace(workspace, '<WORKSPACE>')
    jobName = helper.getJenkinsJobNameAndBuildNumber()["jobName"]
    argstr = argstr.replace(jobName, '<JOB_NAME>')
    if helper.CURRENT_SUITE_NAME in ignoreParams:
        params = ignoreParams[helper.CURRENT_SUITE_NAME]
        params = params.strip().replace(' ', '').split(',')
        for param in params:
            param = ' --' + param
            if param in argstr:
                front = argstr.split(param)[0]
                back = argstr.split(param)[1]
                if ' --' in back:
                    backParams = ' --'.join(back.split(' --')[1:])
                    argstr = front + param + ' <IGNORED> --' + backParams
                else:
                    argstr = front + param + ' <IGNORED>'
    longSig, shortSig = helper.getContentSignature(argstr)
    return shortSig, argstr


def saveInvocationSignature(filepath=None, runSuite=False):
    '''Save cmdline arguments metadata and signature for dry run comparison'''
    try:
        if not filepath:
            filepath = invocationJsonPath()
        invokeJson = {}
        if os.path.exists(filepath):
            with open(filepath, 'r') as f:
                content = f.read()
                if content.startswith('{'):
                    invokeJson = json.loads(content)
        if 'hermes' not in invokeJson:
            invokeJson['hermes'] = {}
        ignoreParams = {}
        if filepath == invocationJsonPath(): # normal run
            ignoreParams = getInvocationIgnoreParams()
            invokeJson['ignore_params'] = ignoreParams
        shortSig, argstr = getInvocationSignature(ignoreParams)
        invokeJson['hermes'][helper.CURRENT_SUITE_NAME] = {
            'sig': shortSig,
            'args': argstr
        }
        invokeJson['hermes'][helper.CURRENT_SUITE_NAME]['ran'] = runSuite
        with open(filepath, 'w+') as f:
            f.write(json.dumps(invokeJson, indent=4))
    except Exception as e:
        helper.hermesNonCriticalTrace(e)


def markInvocationAsExecuted():
    try:
        if not os.path.exists(dockerDirtyFlagFile()):
            with open(dockerDirtyFlagFile(), 'w+') as f:
                f.write('hermes_executed')
    except Exception as e:
        helper.hermesNonCriticalTrace(e)


def markInvocationAsNotExecuted():
    try:
        if os.path.exists(dockerDirtyFlagFile()):
            os.remove(dockerDirtyFlagFile())
    except Exception as e:
        helper.hermesNonCriticalTrace(e)
