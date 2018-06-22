#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Tests covering Helen's non-ethereum ReST API.
# (i.e. everything under /api/, excluding /api/athena/eth)
#########################################################################
import collections
import json
import logging
import os
import traceback
import string
import urllib.request
import gzip
import io
import time
import sys
import statistics
import numpy as np

from . import test_suite
from util.product import Product
from rest.request import Request
import util.json_helper
from rpc.rpc_call import RPC
from matplotlib import pyplot as plt

log = logging.getLogger(__name__)


class PerformanceTests(test_suite.TestSuite):
   _args = None
   _apiBaseServerUrl = "http://localhost:8080"
   _userConfig = None
   _ethereumMode = False
   _productMode = True
   _resultFile = None
   p = None

   def __init__(self, passedArgs):
      super(PerformanceTests, self).__init__(passedArgs)

   def getName(self):
      return "PerformanceTests"

   def run(self):
      ''' Runs all of the tests. '''
      if self._productMode:
         global p
         try:
            p = self.launchProduct(self._args.resultsDir,
                                   self._apiBaseServerUrl + "/api/athena/eth",
                                   self._userConfig["product"])
         except Exception as e:
            log.error(traceback.format_exc())
            return self._resultFile

      if self._ethereumMode:
         info = "PerformanceTests are not applicable to ethereumMode."
         log.warn(info)
         self.writeResult("All tests", None, info)
         return self._resultFile

      tests = self._getTests()

      for (testName, testFun) in tests:
         testLogDir = os.path.join(self._testLogDir, testName)

         try:
            result, info = self._runRestTest(testName,
                                             testFun,
                                             testLogDir)
         except Exception as e:
            result = False
            info = str(e)
            traceback.print_tb(e.__traceback__)
            log.error("Exception running performance test: '{}'".format(info))

         if info:
            info += "  "
         else:
            info = ""

         relativeLogDir = self.makeRelativeTestPath(testLogDir)
         info += "Log: <a href=\"{}\">{}</a>".format(relativeLogDir,
                                                     testLogDir)
         self.writeResult(testName, result, info)

      log.info("Tests are done.")

      if self._productMode:
         p.stopProduct()

      return self._resultFile

   def _runRestTest(self, testName, testFun, testLogDir):
      log.info("Starting test '{}'".format(testName))
      return testFun()

   def _getTests(self):
      return [("Performance", self._test_performance)]


   def plotData(self, period_list, avg_latency_list, peak_latency_list, stdev_latency_list, var_latency_list, percentile_list_99):
      avg_latency, = plt.plot(period_list, avg_latency_list, label="Average")
      peak_latency, = plt.plot(period_list, peak_latency_list, label="Peak")
      stdev_latency, = plt.plot(period_list, stdev_latency_list, label="Std Deviation")
      var_latency, = plt.plot(period_list, var_latency_list, label="Variance")
      percentile_99, = plt.plot(period_list, percentile_list_99, label="99th percentile")
      plt.title("Latency vs Time")
      plt.xlabel("Time (seconds)")
      plt.ylabel("Latency (seconds)")
      plt.grid(color='r', linestyle='-', linewidth=0.5)
      plt.legend()
      #plt.show()
      path = os.path.join(self._testLogDir, "latencyVStime.png")
      plt.savefig(path)


   def parse_results(self, result_file):
      print("Parsing results..")
      period_list = []
      avg_latency_list = []
      peak_latency_list = []
      stdev_latency_list = []
      var_latency_list = []
      percentile_list_99 = []

      with open(result_file, 'r') as result_file_obj:
         lines = result_file_obj.readlines()
         current_latency_vals = []
         
         #Ignore 0th element as that contains column headers
         first_val = lines[1]
         first_line = first_val.rstrip('\n')
         first_line_vals = first_line.split('|')
         current_period = first_line_vals[1]
         previous_period = first_line_vals[1]
         current_latency_vals.append(float(first_line_vals[2]))

         for line in lines[2:]:
            line = line.rstrip('\n')
            vals = line.split('|')
            current_period = vals[1]

            if current_period != previous_period:
               period_list.append(previous_period)
               avg_latency_list.append(statistics.mean(current_latency_vals))
               peak_latency_list.append(max(current_latency_vals))
               stdev_latency_list.append(statistics.pstdev(current_latency_vals))
               var_latency_list.append(statistics.pvariance(current_latency_vals))
               percentile_list_99.append(np.percentile(current_latency_vals, 99))
               current_latency_vals[:] = []
               previous_period = current_period

            current_latency_vals.append(float(vals[2]))

         period_list.append(previous_period)
         avg_latency_list.append(statistics.mean(current_latency_vals))
         peak_latency_list.append(max(current_latency_vals))
         stdev_latency_list.append(statistics.pstdev(current_latency_vals))
         var_latency_list.append(statistics.pvariance(current_latency_vals))
         percentile_list_99.append(np.percentile(current_latency_vals, 99))

      self.plotData(period_list, avg_latency_list, peak_latency_list, stdev_latency_list, var_latency_list, percentile_list_99)
      

   def _test_performance(self):
      if self._productMode:
         filename = self._userConfig["performance"]["filename"]
         url = self._userConfig["performance"]["url"]

         print("Accessing test file...")
         with urllib.request.urlopen(url + filename) as response:
            print("Decompressing test file...")
            compressedFile = io.BytesIO(response.read())
            decompressedFile = gzip.GzipFile(fileobj=compressedFile)

            testName = "performance"
            testLogDir = os.path.join(self._testLogDir, testName)
            rpc = RPC(testLogDir, testName, self._apiServerUrl)

            print("Sending requests..")
            count = 1
            t_peak_response = sys.float_info.min

            result_file = os.path.join(self._testLogDir, "performance_logs.csv")
            with open(result_file, 'w') as result_file_obj:
               result_file_obj.write("ReqNo|Clock|ExecutionTime\n")
               
               t_start_test = time.time()
               t_end_test = None
               for row in decompressedFile:
                  line = row.rstrip()
                  request_type = line[0:2].decode()
                  from_addr = line[2:42].decode()
                  to_addr = line[42:82].decode()
                  value = line[82:146].decode()
                  data = line[146:].decode()
                  if data == "":
                     data = None

                  t_start_req = time.time()
                  if request_type == "01": #78
                     #ignoring 'to'
                     if data == None:
                        continue

                     rpc.sendTransaction(from_addr,
                                         data,
                                         self._getGas(),
                                         value = value)

                  else:
                     rpc.sendTransaction(from_addr,
                                         data,
                                         self._getGas(),
                                         to_addr,
                                         value)
                  t_req = time.time() - t_start_req
                  
                  if t_req > t_peak_response:
                     t_peak_response = t_req

                  result_file_obj.write(str(count) + "|" + \
                     str(int(time.time()) - int(t_start_test)) + "|" + 
                     str(t_req) + "\n")
                  
                  count += 1
                  if count % 100 == 0:
                     print(str(count) + " requests done")
                  
                  if count > 500:
                     break

               t_end_test = time.time()
            self.parse_results(result_file)
      
         t_total = t_end_test - t_start_test
         print()
         print("Peak Response Time = " + str(t_peak_response) + " seconds")
         print("Total Time = " + str(t_total) + " seconds")
         print("Throughput = " + str(count/t_total) + " RPS")
         print()

      return (True, None)
