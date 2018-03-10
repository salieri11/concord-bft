#!/usr/bin/python3

#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import argparse
from tempfile import mkdtemp
from time import strftime, localtime

from suites import core_vm_tests

def main():
   parser = argparse.ArgumentParser()
   parser.add_argument("suite", help="Test suite name")
   parser.add_argument("--resultsDir", help="Results directory (optional)")
   args = parser.parse_args()

   if (args.resultsDir == None):
      args.resultsDir = createResultsDir(args.suite)

   print("Results directory:", args.resultsDir)
   suite = createTestSuite(args)
   print("Running suite", suite.getName())
   processResults(suite.run())

def createTestSuite(args):
   if (args.suite == "CoreVMTests"):
      return core_vm_tests.CoreVMTests(args)

def createResultsDir(suiteName):
   prefix = suiteName + "_" + strftime("%Y%m%d_%H%M_", localtime())

   return mkdtemp(prefix=prefix)

def processResults(resultsFile):
   print(resultsFile)

main()
