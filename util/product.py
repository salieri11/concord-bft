#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import atexit
import json
import logging
import os
from rpc.rpc_call import RPC
import subprocess
import time

PRODUCT_LOGS_DIR = "product_logs"

log = logging.getLogger(__name__)

class Product():
   '''
   Represents an instance of the product.  That includes all of the processes
   needed to have "the product" running.
   '''
   _apiServerUrl = None
   _logs = []
   _processes = []
   _userProductConfig = None
   _resultsDir = None

   def __init__(self, resultsDir, apiServerUrl, userProductConfig):
      self._resultsDir = resultsDir
      self._apiServerUrl = apiServerUrl
      self._userProductConfig = userProductConfig

   def launchProduct(self):
      '''
      Given the user's product config section, launch the product.
      Raises an exception if it cannot start.
      '''
      atexit.register(self.stopProduct)
      productLogsDir = os.path.join(self._resultsDir, PRODUCT_LOGS_DIR)
      os.makedirs(productLogsDir, exist_ok=True)

      # since we change directories while launching products, save cwd here
      # and chdir to that once all launches are done
      original_cwd = os.getcwd()

      for launchElement in self._userProductConfig["launch"]:
         for project in launchElement:
            projectSection = launchElement[project]
            buildRoot = projectSection["buildRoot"]
            buildRoot = os.path.expanduser(buildRoot)

            for executable in projectSection:
               if executable == "buildRoot":
                  os.chdir(buildRoot)
               elif executable != "buildRoot":
                  executableSection = launchElement \
                                      [project][executable]
                  # cmd = [os.path.join(buildRoot,
                  #                     executableSection["launchCommand"])]
                  cmd = [os.path.join(executableSection["launchCommand"])]

                  # Add paramters.
                  # If it is a replica and we see the "-d" parameter, the next
                  # parameter needs to have the results directory prepended to it.
                  previousParam = None
                  for param in executableSection["parameters"]:
                     if executable.startswith("replica") and previousParam == "-d":
                        param = os.path.join(self._resultsDir, param)
                        os.makedirs(param)
                     elif (not executable.startswith("athena") and
                           previousParam == "-p"):
                        # -p is "path" for geth, but "port" for athena
                        param = os.path.join(buildRoot, param)
                     elif previousParam in ["-c", "-e", "-k"]:
                        param = os.path.join(buildRoot, param)

                     cmd.append(os.path.expanduser(param))
                     previousParam = param

                  print ("Running:", cmd, " from: ", os.getcwd())
                  log = open(os.path.join(productLogsDir, executable + ".log"),
                             "wb+")
                  self._logs.append(log)
                  p = subprocess.Popen(cmd,
                                       stdout=log,
                                       stderr=subprocess.STDOUT)
                  self._processes.append(p)

      # All pieces should be launched now.
      if not self._waitForProductStartup():
         raise Exception("The product did not start. Exiting.")

      # switch back to original cwd
      os.chdir(original_cwd)

   def stopProduct(self):
      '''
      Stops the product executables and closes the logs.
      '''
      for p in self._processes[:]:
         p.terminate()
         self._processes.remove(p)

      for log in self._logs[:]:
         log.close()
         self._logs.remove(log)

   def _waitForProductStartup(self):
      '''
      Issues a test transaction to see if the product has started up.
      Retries a few times.
      Returns whether the product started up.

      Note: For now, simply sends an empty contract:

      pragma solidity ^0.4.19;

      contract x {
      }

      If the account unlocking API becomes available, performing that unlock may
      be better, as it will also confirm that the user(s) we were given are valid
      and unlock them.
      '''
      retries = 10
      attempts = 0
      sleepTime = 1
      startupLogDir = os.path.join(self._resultsDir, PRODUCT_LOGS_DIR,
                                   "waitForStartup")
      rpc = RPC(startupLogDir, "waitForStartup", self._apiServerUrl)
      caller = self._userProductConfig["users"][0]["hash"]
      data = ("0x60606040523415600e57600080fd5b603580601b6000396000f3006060604"
              "052600080fd00a165627a7a723058202909725de95a67cf9907b67867deb3f7"
              "7096fdd38a55e7ac790117d50be1b3830029")
      txHash = None

      while attempts < retries and not txHash:
         if attempts != 0:
            time.sleep(sleepTime)

         attempts += 1

         try:
            txHash = rpc.sendTransaction(caller, data)
         except Exception as e:
            log.debug("Waiting for product startup...")
            log.debug(e)

      return txHash != None
