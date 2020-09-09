#########################################################################
# Copyright 2018 - 2020 VMware, Inc. All rights reserved. -- VMware Confidential
#
# Utility to test the performance of the Helen+concord ecosystem
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

from fixtures.common_fixtures import fxHermesRunSettings, fxProduct, fxConnection, fxBlockchain
from suites.case import describe
from xvfbwrapper import Xvfb

log = util.hermes_logging.getMainLogger()
LocalSetupFixture = collections.namedtuple(
    "LocalSetupFixture", "testLogDir, uiPath, vdisplay")
pytest.XvfbPresent = False


def startVDisplay(vdisplay):
    try:
        vdisplay.start()
        pytest.XvfbPresent = True
    except Exception as e:
        log.info(str(e))


def stopVDisplay(vdisplay):
    if pytest.XvfbPresent:
        vdisplay.stop()


@pytest.fixture(scope="function")
@describe("fixture; local setup for given test suite")
def fxLocalSetup(request, fxBlockchain, fxHermesRunSettings, fxProduct, fxConnection):
    testName = fxConnection.request.testName
    testName = testName.replace('test_', '')
    testLogDir = os.path.join(
        fxHermesRunSettings["hermesTestLogDir"], testName)
    pathlib.Path(testLogDir).mkdir(parents=True, exist_ok=True)
    repoPath = os.getcwd().split('/')[:-1]
    repoPath.append("ui")
    uiPath = '/'.join(repoPath)
    vdisplay = Xvfb(width=4000, height=4000)
    yield LocalSetupFixture(testLogDir=testLogDir, uiPath=uiPath, vdisplay=vdisplay)
    stopVDisplay(vdisplay)

# =============================================================================================
# Actual Test Functions
# =============================================================================================
@describe("run UI  lint e2e tests")
def test_lint_e2e(fxLocalSetup):
    vdisplay = uiPath = save_path = testLogDir = ""
    for vLocalSetup in fxLocalSetup:
        vdisplay = vLocalSetup.vdisplay
        uiPath = vLocalSetup.uiPath
        save_path = vLocalSetup.testLogDir
        testLogDir = vLocalSetup.testLogDir
    startVDisplay(vdisplay)
    cmd = ["npm", "run", "e2e:integration"]
    # Add the path to logs output to a file so we can save screenshot
    # from e2e tests in a directory that is familiar to other users
    fileTxtDirPath = os.path.join(uiPath, "ui_e2e_path.txt")
    with open(fileTxtDirPath, "w") as fileDir:
        fileDir.write(save_path)
    logFilePath = os.path.join(testLogDir, "e2e.log")
    with open(logFilePath, "wb+") as logFile:
        procOutput = subprocess.run(cmd,
                                    stdout=logFile,
                                    stderr=subprocess.STDOUT,
                                    cwd=uiPath, )
    assert procOutput.returncode == 0, "UI Integration E2E tests failed, please see {}".format(
        logFilePath)
