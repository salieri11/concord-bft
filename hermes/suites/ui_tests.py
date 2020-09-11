#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Utility to test the performance of the Helen+concord ecosystem
# Converted to pytest for Jira BC-496
#########################################################################
import logging
import os
import pathlib
import traceback
import subprocess
import pytest
import collections
import util.json_helper
import util.hermes_logging

from fixtures.common_fixtures import fxProduct, fxConnection, fxBlockchain
from suites.case import describe
from xvfbwrapper import Xvfb

log = util.hermes_logging.getMainLogger()
LocalSetupFixture = collections.namedtuple(
    "LocalSetupFixture", "testLogDir, uiPath, vdisplay")
pytest.XvfbPresent = False

#start vdisplay for the test
describe()
def startVdisplay(vdisplay):
    try:
        vdisplay.start()
        pytest.XvfbPresent = True
    except Exception as e:
        log.info(str(e))

#stop vdisplay if it is running
describe()
def stopVDisplay(vdisplay):
    if pytest.XvfbPresent:
        vdisplay.stop()

@pytest.fixture(scope="function")
@describe("fixture; local setup for given test suite")
def fxLocalSetup(request, fxBlockchain, fxHermesRunSettings, fxProduct, fxConnection):
    testName = fxConnection.request.testName
    testName = testName.replace('test_','')
    testLogDir = os.path.join(
        fxHermesRunSettings["hermesTestLogDir"], testName)
    pathlib.Path(testLogDir).mkdir(parents=True, exist_ok=True)
    repoPath = os.getcwd().split('/')[:-1]
    repoPath.append("ui")
    uiPath = '/'.join(repoPath)
    vdisplay = Xvfb(width=1600, height=2500)
    return LocalSetupFixture(testLogDir=testLogDir, uiPath=uiPath, vdisplay=vdisplay)

@describe("run all UI unit tests")
def test_ui_unit(fxLocalSetup):
    startVdisplay(fxLocalSetup.vdisplay)
    cmd = ["npm", "run", "test:build", ]
    logFilePath = os.path.join(fxLocalSetup.testLogDir, "unit_tests.log")
    with open(logFilePath, "wb+") as logFile:
        procOutput = subprocess.run(cmd,
                                        stdout=logFile,
                                        stderr=subprocess.STDOUT,
                                        cwd=fxLocalSetup.uiPath, )
    stopVDisplay(fxLocalSetup.vdisplay)
    assert procOutput.returncode == 0, "UI Unit tests failed, please see {}".format(logFilePath)
    
@describe("run UI e2e tests")
def test_ui_e2e(fxLocalSetup):
    startVdisplay(fxLocalSetup.vdisplay)
    cmd = ["npm", "run", "e2e:build", ]
    logFilePath = os.path.join(fxLocalSetup.testLogDir, "e2e.log")
    # Add the path to logs output to a file so we can save screenshot
    # from e2e tests in a directory that is familiar to other users
    fileTxtDirPath = os.path.join(fxLocalSetup.uiPath, "ui_e2e_path.txt")
    savePath = fxLocalSetup.testLogDir
    with open(fileTxtDirPath, "w") as fileDir:
        fileDir.write(savePath)
    with open(logFilePath, "wb+") as logFile:
        procOutput = subprocess.run(cmd,
                                        stdout=logFile,
                                        stderr=subprocess.STDOUT,
                                        cwd=fxLocalSetup.uiPath, )
    stopVDisplay(fxLocalSetup.vdisplay)
    assert procOutput.returncode == 0, "UI E2E tests failed, please see {}".format(logFilePath)

@describe("run UI lint tests")
def test_ui_lint(fxLocalSetup):
    startVdisplay(fxLocalSetup.vdisplay)
    cmd = ["npm", "run", "lint", ]
    logFilePath = os.path.join(fxLocalSetup.testLogDir, "lint.log")
    with open(logFilePath, "wb+") as logFile:
        procOutput = subprocess.run(cmd,
                                        stdout=logFile,
                                        stderr=subprocess.STDOUT,
                                        cwd=fxLocalSetup.uiPath, )
    stopVDisplay(fxLocalSetup.vdisplay)
    assert procOutput.returncode == 0, "UI linter failed, please see {}".format(logFilePath)