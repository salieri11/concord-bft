#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Tasks:
# - A few ethereum tests are bundled with this.  Instead, create a config
#   file which contains a reference to the ethererum tests.  The license
#   seems permissive, but let's just not store any of that stuff here.
# - Use Python's logger instead of print().
# - Launch geth when running in ethereumMode.
# - We need to be given the user hash/pwd to use. (Config file) Hard code
#   values right now just work on my system.
# - Need to unlock the user. (I just unlock for the entire session manually.)
# - Detect product startup instead of sleeping.
# - Save results to a JSON file.
# - Return a path to a JSON file.
#########################################################################
import argparse
import json
import os
import tempfile
import time

from . import test_suite
from rpc.rpc_call import RPC
from util.product import Product

PRODUCT_CONFIG_JSON = "resources/product_launch_config.json"
TEST_SOURCE_CODE_SUFFIX = "Filler.json"
VM_TEST_DIR = "suites/ethereum_vm_tests/vmArithmeticTest"

class CoreVMTests(test_suite.TestSuite):
   _args = None
   _apiServerUrl = None
   _ethereumMode = False
   _productMode = True

   def __init__(self, passedArgs):
      self._args = passedArgs
      self._ethereumMode = self._args.ethereumMode

      if self._ethereumMode:
         print("Running in ethereum mode")
         self._productMode = False
         self._apiServerUrl = "http://localhost:8545"
      else:
         self._apiServerUrl = "http://localhost:8080/api/athena/eth/"


   def getName(self):
      return "CoreVMTests"

   def run(self):
      ''' Runs all of the tests. '''
      if self._productMode:
         p = Product(self._args.resultsDir)
         p.launchProduct(PRODUCT_CONFIG_JSON)

      print("Pretending to wait for startup.")
      time.sleep(5)
      print("The product is running.")

      tests = os.listdir(VM_TEST_DIR)

      for test in tests:
         if not self._isSourceCodeTestFile(test):
            testCompiled = self._loadCompiledTest(test)
            testSource = self._loadTestSource(test)
            print(self._runRpcTest(testSource, testCompiled))

      print("Tests are done.")

      if self._productMode:
         p.stopProduct()

      return {
         "results": "foo"
      }

   def _loadCompiledTest(self, test):
      '''
      Reads the compiled test from a file and returns it.
      '''
      testCompiledFile = os.path.join(VM_TEST_DIR, test)
      testCompiled = None

      with open(testCompiledFile) as f:
         testCompiled = json.load(f)

      return testCompiled

   def _loadTestSource(self, test):
      '''
      Reads the test source code from a file and returns it.
      '''
      testSourceFile = os.path.join(VM_TEST_DIR, test)
      testSourceFile = os.path.splitext(testSourceFile)[0]
      testSourceFile += TEST_SOURCE_CODE_SUFFIX
      testSource = None

      with open(testSourceFile) as f:
         testSource = json.load(f)

      return testSource

   def _isSourceCodeTestFile(self, name):
      '''
      Determines if a test file name is source code or a real test.
      '''
      return TEST_SOURCE_CODE_SUFFIX in name

   def _runRpcTest(self, testSource, testCompiled):
      ''' Runs one test. '''
      success = False
      testName = list(testSource.keys())[0]
      caller = "0x4291294e5ddd2f50ac9c575e3cf1455b060fd404"
      data = testCompiled[testName]["exec"]["code"]

      # For some tests, expected results are only in the "src" files which
      # are used to generate the tests.  The compiled files contain no
      # expected storage values.  The trend so far, from manual inspection, is
      # that this is when storage at 0x0 is expected to equal 0. I prefer to
      # have something more explicit, as is found in the src files.
      expectSection = testSource[testName]["expect"]
      expectedStorage = expectSection[list(expectSection.keys())[0]]["storage"]

      print("Starting test", testName)

      rpc = RPC(self._args.resultsDir,
                testName,
                self._apiServerUrl)
      txHash = rpc.sendTransaction(caller, data)

      if txHash:
         contractAddress = rpc.getContractAddrFromTxHash(txHash,
                                                         self._ethereumMode)

         if contractAddress:
            if (len(expectedStorage) < 1):
               print("No expected storage detected.  Skipping")
            else:
               for storageLoc in expectedStorage:
                  actualValue = rpc.getStorageAt(contractAddress, storageLoc)
                  actualValue = int(actualValue, 16)
                  expectedValue = int(expectedStorage[storageLoc], 16)
                  print("expectedValue:", expectedValue,
                        "actualValue", actualValue)
                  if expectedValue == actualValue:
                     success = True
                  else:
                     success = False
                     break
         else:
            print("Never got a contract address.")

      return success
