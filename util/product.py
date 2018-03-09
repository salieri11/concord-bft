#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import atexit
import json
import os
import subprocess

CMD_KEY = "launchCommand"
PARAMS_KEY = "parameters"

class Product():
   '''
   Represents an instance of the product.  That includes all of the processes
   needed to have "the product" running.
   '''
   _logs = []
   _processes = []
   _resultsDir = None

   def __init__(self, resultsDir):
      self._resultsDir = resultsDir

   def launchProduct(self, configFile):
      '''
      Given a config file containing information on how to launch the product,
      launch it.
      '''
      atexit.register(self.stopProduct)

      config = json.load(open(configFile))

      for name in config:
         log = open(os.path.join(self._resultsDir, name + ".log"), "wb+")
         self._logs.append(log)

         cmd = [os.path.expanduser(config[name][CMD_KEY])]

         # Add paramters.
         # If it is a replica and we see the "-d" parameter, the next
         # parameter needs to have the results directory prepended to it.
         previousParam = None
         for param in config[name][PARAMS_KEY]:
            if name.startswith("replica") and previousParam == "-d":
               param = os.path.join(self._resultsDir, param)
               os.makedirs(param)

            cmd.append(os.path.expanduser(param))
            previousParam = param

         p = subprocess.Popen(cmd,
                              stdout=log,
                              stderr=subprocess.STDOUT)
         self._processes.append(p)

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
