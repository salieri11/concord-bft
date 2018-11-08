#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import atexit
import json
import logging
import os
import pathlib
from rpc.rpc_call import RPC
import shutil
import socket
import subprocess
import time
import yaml

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
   _cmdlineArgs = None

   def __init__(self, cmdlineArgs, apiServerUrl, userConfig):
      self._cmdlineArgs = cmdlineArgs
      self._apiServerUrl = apiServerUrl
      self._userConfig = userConfig
      self._userProductConfig = userConfig["product"]

   def launchProduct(self):
      '''
      Given the user's product config section, launch the product.
      Raises an exception if it cannot start.
      '''
      atexit.register(self.stopProduct)
      productLogsDir = os.path.join(self._cmdlineArgs.resultsDir, PRODUCT_LOGS_DIR)
      os.makedirs(productLogsDir, exist_ok=True)

      if self._cmdlineArgs.dockerComposeFile:
         self._launchViaDocker(productLogsDir)
      else:
         self._launchViaCmdLine(productLogsDir)

   def _launchViaDocker(self, productLogsDir):
      dockerCfg = None

      if os.path.isfile(self._cmdlineArgs.dockerComposeFile):
         with open(self._cmdlineArgs.dockerComposeFile, "r") as f:
            dockerCfg = yaml.load(f)

         if not self._cmdlineArgs.keepAthenaDB:
            self.clearDBsForDockerLaunch(dockerCfg)
            self.initializeHelenDockerDB(dockerCfg)

         cmd = ["docker-compose",
                "--file", self._cmdlineArgs.dockerComposeFile,
                "up"]
         logFile = open(os.path.join(productLogsDir, "athena.log"),
                    "wb+")
         self._logs.append(logFile)
         log.debug("Launching via docker-compose with {}".format(cmd))
         p = subprocess.Popen(cmd,
                              stdout=logFile,
                              stderr=subprocess.STDOUT)
         self._processes.append(p)

         if not self._waitForProductStartup():
            raise Exception("The product did not start. Exiting.")
      else:
         raise Exception("The docker compose file '{}' does not " \
                         "exist. Exiting.".format(self._cmdlineArgs.dockerComposeFile))

   def _launchViaCmdLine(self, productLogsDir):
      # Since we change directories while launching products, save cwd here
      # and chdir to that once all launches are done
      original_cwd = os.getcwd()

      for launchElement in self._userProductConfig["launch"]:
         for project in launchElement:
            projectSection = launchElement[project]
            buildRoot = projectSection["buildRoot"]
            buildRoot = os.path.expanduser(buildRoot)

            if not self._cmdlineArgs.keepAthenaDB and \
               project.lower().startswith("athena"):
               self.clearAthenaDBForCmdlineLaunch(launchElement[project])

            for executable in projectSection:
               if executable == "buildRoot":
                  os.chdir(buildRoot)
               else:
                  executableSection = launchElement \
                                      [project][executable]
                  cmd = [os.path.join(executableSection["launchCommand"])]

                  # Add paramters.
                  # If it is a replica and we see the "-d" parameter, the next
                  # parameter needs to have the results directory prepended to it.
                  previousParam = None
                  for param in executableSection["parameters"]:
                     if executable.startswith("replica") and previousParam == "-d":
                        param = os.path.join(self._cmdlineArgs.resultsDir, param)
                        os.makedirs(param)
                     cmd.append(os.path.expanduser(param))
                     previousParam = param

                  pathlib.Path(productLogsDir).mkdir(parents=True, exist_ok=True)
                  logFile = open(os.path.join(productLogsDir, executable + ".log"),
                             "wb+")
                  self._logs.append(logFile)
                  log.debug("Launching via command line with {}".format(cmd))
                  p = subprocess.Popen(cmd,
                                       stdout=logFile,
                                       stderr=subprocess.STDOUT)
                  self._processes.append(p)
            # switch back to original cwd
            os.chdir(original_cwd)

      # All pieces should be launched now.
      if not self._waitForProductStartup():
         raise Exception("The product did not start. Exiting.")

   def clearAthenaDBForCmdlineLaunch(self, athenaSection):
      '''
      Deletes the Athena DB so we get a clean start.
      Other test suites can leave it in a state that makes
      it fail.
      Note that Helen's DB is cleared by the cockroach shell script
      when the product is launched via command line.
      '''
      params = None

      for subSection in athenaSection:
         if subSection.lower().startswith("athena"):
            params = athenaSection[subSection]["parameters"]

      isConfigParam = False

      for param in params:
         if isConfigParam:
            configFile = os.path.join(athenaSection["buildRoot"],
                                      param)
            configFile = os.path.expanduser(configFile)
            subPath = None

            with open (configFile, "r") as props:
               for prop in props:
                  prop = prop.strip()
                  if prop and not prop.startswith("#") and \
                     prop.lower().startswith("blockchain_db_path"):
                     subPath = prop.split("=")[1]

            dbPath = os.path.join(athenaSection["buildRoot"], subPath)
            dbPath = os.path.expanduser(dbPath)
            if os.path.isdir(dbPath):
               log.debug("Clearing Athena DB directory '{}'".format(dbPath))
               shutil.rmtree(dbPath)

         if param == "-c":
            isConfigParam = True


   def startHelenDockerDB(self, dockerCfg):
      ''' Starts the Helen DB.  Returns True if able to start, False if not.'''
      cmd = ["docker-compose",
             "--file", self._cmdlineArgs.dockerComposeFile,
             "up", "db-server"]
      log.debug("Launching Helen DB with command '{}'".format(cmd))
      subprocess.Popen(cmd,
                       stdout=subprocess.PIPE,
                       stderr=subprocess.STDOUT)
      sleepTime = 3
      maxTries = 10
      numTries = 0
      dbRunning = False
      dbPort = int(dockerCfg["services"]["db-server"]["ports"][0].split(":")[0])
      sock = socket.socket()

      while numTries < maxTries and not dbRunning:
         try:
            log.debug("Attempting to connect to the Helen DB server on port {}.".format(dbPort))
            sock.connect(("localhost", dbPort)) # Product may have a remote DB someday.
            log.debug("Helen DB is up.")
            sock.close()
            dbRunning = True
         except Exception as e:
            log.debug("Waiting for the Helen DB server: '{}'".format(e))

            if numTries < maxTries:
               numTries += 1
               log.debug("Will try again in {} seconds.".format(sleepTime))
               time.sleep(sleepTime)

      return dbRunning


   def getHelenDBContainerId(self, dockerCfg):
      '''Returns the running Helen DB container's ID, or None if it cannot be found.'''
      dbImageName = dockerCfg["services"]["db-server"]["image"]
      containerId = None
      sleepTime = 3
      maxTries = 10
      numTries = 0

      while numTries < maxTries and not containerId:
         # The processes cmd gives us a string like:
         # CONTAINER ID        IMAGE                          COMMAND ...
         # 21d37f282847        cockroachdb/cockroach:v2.0.2   "/cockroach/cockroac…" ...
         cmd = ["docker", "ps"]
         log.debug("Looking for the running Helen DB container with command '{}'".format(cmd))
         completedProcess = subprocess.run(cmd,
                                           stdout=subprocess.PIPE,
                                           stderr=subprocess.STDOUT)
         psOutput = completedProcess.stdout.decode("UTF-8")
         lines = psOutput.split("\n")

         for line in lines:
            if dbImageName in line:
               fields = line.split(" ")
               containerId = fields[0]

         if not containerId:
            log.debug("The docker ps command is not listing the Helen DB container yet.")
            log.debug("stdout: '{}', stderr: '{}'".format(completedProcess.stdout, completedProcess.stderr))

            if numTries < maxTries:
               numTries += 1
               log.debug("Will try again in {} seconds.".format(sleepTime))
               time.sleep(sleepTime)

      return containerId


   def configureHelenDockerDB(self, containerId):
      '''Runs the SQL commands to set up the Helen DB.  Returns whether the command
         exit codes indicate success.
      '''
      schema = None

      with open("resources/schema.sql", "r") as f:
         schema = f.read()

      commands = [
         ["docker", "exec", containerId, "./cockroach", "user", "set", "helen_admin", "--insecure"],
         ["docker", "exec", containerId, "./cockroach", "sql", "--insecure", "-e", schema],
      ]

      for cmd in commands:
         completedProcess = subprocess.run(cmd,
                                           stdout=subprocess.PIPE,
                                           stderr=subprocess.STDOUT)
         try:
            completedProcess.check_returncode()
         except subprocess.CalledProcessError as e:
            log.error("Command '{}' to configure the Helen DB failed.  Exit code: '{}'".format(cmd, e.returncode))
            log.error("stdout: '{}', stderr: '{}'".format(completedProcess.stdout, completedProcess.stderr))
            return False

      return True


   def stopDockerContainer(self, containerId):
      '''Stops the given docker container. Returns whether the exit code indicated success.'''
      cmd = ["docker", "kill", containerId]
      completedProcess = subprocess.run(cmd,
                                        stdout=subprocess.PIPE,
                                        stderr=subprocess.STDOUT)
      try:
         completedProcess.check_returncode()
      except subprocess.CalledProcessError as e:
         log.error("Command '{}' to stop container '{}' failed.  Exit code: '{}'".format(cmd,
                                                                                         containerId,
                                                                                         e.returncode))
         log.error("stdout: '{}', stderr: '{}'".format(completedProcess.stdout, completedProcess.stderr))
         return False

      return True


   def initializeHelenDockerDB(self, dockerCfg):
      '''When the product as Docker containers, we need to initialize the Helen DB
         in a different way than when launched via command line.  Raises an exception
         on error.'''
      if not self.startHelenDockerDB(dockerCfg):
         raise Exception("The Helen DB failed to come up.")

      containerId = self.getHelenDBContainerId(dockerCfg)
      if not containerId:
         raise Exception("Unable to get the running Helen DB's docker container ID.")

      if not self.configureHelenDockerDB(containerId):
         raise Exception("Unable to configure the Helen DB.")

      if not self.stopDockerContainer(containerId):
         raise Exception("Failure trying to stop the Helen DB.")


   def clearDBsForDockerLaunch(self, dockerCfg):
      for service in dockerCfg["services"]:
         serviceObj = dockerCfg["services"][service]
         if "volumes" in serviceObj:
            for v in serviceObj["volumes"]:
               if "rocksdbdata" in v or \
                  "cockroachDB" in v:
                  yamlDir = os.path.dirname(self._cmdlineArgs.dockerComposeFile)
                  deleteMe = os.path.join(yamlDir, v.split(":")[0])
                  log.debug("Deleting: {}".format(deleteMe))

                  if os.path.isdir(deleteMe):
                     try:
                        shutil.rmtree(deleteMe)
                     except PermissionError as e:
                        log.error("Could not delete {}. Try running with sudo " \
                                  "when running tests in docker mode.".format(deleteMe))
                        raise e

   def stopProduct(self):
      '''
      Stops the product executables and closes the logs.
      '''
      if self._cmdlineArgs.dockerComposeFile:
         cmd = ["docker-compose",
                "--file", self._cmdlineArgs.dockerComposeFile,
                "down"]
         print("Stopping the product with command '{}'".format(cmd))
         p = subprocess.run(cmd)
      else:
         for p in self._processes[:]:
            if p.poll() is None:
               p.terminate()
               print("Terminating {}.".format(p.args))

         for p in self._processes[:]:
            while p.poll() is None:
               print("Waiting for process {} to exit.".format(p.args))
               time.sleep(1)

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
      # Waiting for 10 seconds for 5 times is enough
      retries = 5
      attempts = 0
      # Helen now takes ~7-8 seconds to boot so we should wait for around 10 seconds
      sleepTime = 10
      startupLogDir = os.path.join(self._cmdlineArgs.resultsDir, PRODUCT_LOGS_DIR,
                                   "waitForStartup")
      rpc = RPC(startupLogDir,
                "waitForStartup",
                self._apiServerUrl,
                self._userConfig)
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
            rpc.addUser()
            txHash = rpc.sendTransaction(caller, data)
         except Exception as e:
            log.debug("Waiting for product startup...")
            log.debug(e)

      return txHash != None
