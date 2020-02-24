#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Utility to test the performance of the Helen+concord ecosystem
#########################################################################
import logging
import os
import pathlib
import traceback
import subprocess

from xvfbwrapper import Xvfb

from . import test_suite

log = logging.getLogger("main")


class UiTests(test_suite.TestSuite):
    _args = None
    _userConfig = None
    _productMode = True
    _resultFile = None
    _xvfb_present = False
    _testCaseDir = None

    def __init__(self, passedArgs, product):
        super(UiTests, self).__init__(passedArgs, product)

    def getName(self):
        return "UiTests"

    def run(self):
        repo_path = os.getcwd().split('/')[:-1]
        repo_path.append("ui")
        self.ui_path = '/'.join(repo_path)
        self._start_vdisplay()
        self.launchProduct()
        tests = self._get_tests()
        for (testName, testFun) in tests:
            result = False
            self._testCaseDir = os.path.join(self._testLogDir, testName)
            pathlib.Path(self._testCaseDir).mkdir(parents=True, exist_ok=True)

            try:
                result, info = getattr(self, "_test_{}".format(testName))()
            except Exception as e:
                result = False
                info = str(e) + "\n" + traceback.format_exc()
                log.error("Exception running UI test: '{}'".format(info))

            self.writeResult(testName, result, info)

        log.info("UI tests are done.")

        if self._xvfb_present:
            self.vdisplay.stop()

        relativeLogDir = self.makeRelativeTestPath(self._testLogDir)
        info += "Log: <a href=\"{}\">{}</a>".format(relativeLogDir,
                                                    self._testLogDir)
        return super().run()

    def _start_vdisplay(self):
        # Checking to see if xvfb is installed.
        # If not, we will try to run the tests without a virtual
        # display
        try:
            self.vdisplay = Xvfb(width=1600, height=2500)
            self.vdisplay.start()
            self._xvfb_present = True
        except Exception as e:
            log.info(str(e))

    def _get_tests(self):
        return [("ui_lint", self._test_ui_lint),
                ("ui_unit", self._test_ui_unit),
                ("ui_e2e", self._test_ui_e2e)]

    def _test_ui_unit(self):
        cmd = ["npm", "run", "test:build", ]
        logFilePath = os.path.join(self._testCaseDir, "unit_tests.log")

        with open(logFilePath, "wb+") as logFile:
            proc_output = subprocess.run(cmd,
                                         stdout=logFile,
                                         stderr=subprocess.STDOUT,
                                         cwd=self.ui_path, )

        if proc_output.returncode != 0:
            return False, "UI Unit tests failed, please see {}".format(logFilePath)

        return True, "UI unit tests passed"

    def _test_ui_e2e(self):
        cmd = ["npm", "run", "e2e:build", ]
        logFilePath = os.path.join(self._testCaseDir, "e2e.log")

        # Add the path to logs output to a file so we can save screenshot
        # from e2e tests in a directory that is familiar to other users
        fileTxtDirPath = os.path.join(self.ui_path, "ui_e2e_path.txt")
        save_path = self._testCaseDir
        with open(fileTxtDirPath, "w") as fileDir:
            fileDir.write(save_path)

        with open(logFilePath, "wb+") as logFile:
            proc_output = subprocess.run(cmd,
                                         stdout=logFile,
                                         stderr=subprocess.STDOUT,
                                         cwd=self.ui_path, )

        if proc_output.returncode != 0:
            return False, "UI E2E tests failed, please see {}".format(logFilePath)

        return True, "UI E2E passed"

    def _test_ui_lint(self):
        cmd = ["npm", "run", "lint", ]
        logFilePath = os.path.join(self._testLogDir, "lint.log")
        with open(logFilePath, "wb+") as logFile:
            proc_output = subprocess.run(cmd,
                                         stdout=logFile,
                                         stderr=subprocess.STDOUT,
                                         cwd=self.ui_path, )

        if proc_output.returncode != 0:
            return False, "UI linter failed, please see {}".format(logFilePath)

        return True, "UI linter passed"
