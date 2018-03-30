#!/usr/bin/python3

#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import argparse
import logging
import os
from tempfile import mkdtemp
from time import strftime, localtime

from suites import core_vm_tests
from util import html, json_helper

log = None

def main():
   parser = argparse.ArgumentParser()
   parser.add_argument("suite", help="Test suite name")
   parser.add_argument("--ethereumMode",
                       help="Run tests against Ethereum",
                       default=False,
                       action='store_true')
   parser.add_argument("--logLevel",
                       help="Set the log level.  Valid values:"
                       "'DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL'",
                       default="INFO")
   parser.add_argument("--resultsDir",
                       help="Results directory")
   args = parser.parse_args()

   if (args.resultsDir == None):
      args.resultsDir = createResultsDir(args.suite)

   setUpLogging(args)
   log.info("Results directory: {}".format(args.resultsDir))
   suite = createTestSuite(args)
   log.info("Running suite {}".format(suite.getName()))
   processResults(suite.run())

def setUpLogging(args):
   '''
   Checks the log level passed in by the user and sets up logging.
   After running, get a logger and use it like this:

   log = logging.getLogger(__name__)
   log.info("Two things: {} {}".format("pen", "pencil"))
   '''
   stringLevel = args.logLevel.upper()

   try:
      intLevel = getattr(logging, stringLevel)
      logging.basicConfig(level=intLevel, format='%(message)s')
      global log
      log = logging.getLogger(__name__)
   except AttributeError:
      print("Log level", args.logLevel, "not valid.  See the help for valid",
            "values. Exiting")
      exit(1)

def createTestSuite(args):
   if (args.suite == "CoreVMTests"):
      return core_vm_tests.CoreVMTests(args)

def createResultsDir(suiteName):
   prefix = suiteName + "_" + strftime("%Y%m%d_%H%M_", localtime())

   return mkdtemp(prefix=prefix)

def processResults(resultsFile):
   '''
   Process a result file, outputting an html file.  If we ever need to output
   a file in another format (such as a CI tool), do that here.
   '''
   log.debug("Processing results for '{}'".format(resultsFile))
   results = json_helper.readJsonFile(resultsFile)
   testCount, passCount, failCount = tallyResults(results)
   fileContents = html.createResultHeader(results,
                                  testCount,
                                  passCount,
                                  failCount)
   fileContents += html.createResultTable(results)
   fileLocation = os.path.join(os.path.dirname(resultsFile), "results.html")

   with open(fileLocation, "w") as f:
      f.write(fileContents)

   log.info("Results written to '{}'".format(fileLocation))

def tallyResults(results):
   '''
   Given the results structure, returns:
   - Total number of tests run.
   - Passes
   - Failures
   (It may return the number of skips eventually, so total - passes != failures.)
   '''
   suiteName = list(results.keys())[0]
   testCases = results[suiteName]["tests"]
   passCount = 0
   failCount = 0
   totalCount = 0

   for test in testCases:
      totalCount += 1

      if testCases[test]["result"] == "PASS":
         passCount += 1
      elif testCases[test]["result"] == "FAIL":
         failCount += 1

   return totalCount, passCount, failCount

main()
