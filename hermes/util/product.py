#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import atexit
import collections
import json
import logging
import os
import os.path
import pathlib
from rpc.rpc_call import RPC
import shutil
import socket
import subprocess
import time
import yaml
import signal

PRODUCT_LOGS_DIR = "product_logs"

log = logging.getLogger(__name__)

class ConcordInstsanceMetaData():
    _processIndex = None
    _processCmd = None
    _logFile = None
    _instanceId = None
    _path = None
    _containerName = None

    def __init__(self, pIndex, pCmd, logFile, id, path):
        self._processIndex = pIndex
        self._processCmd = pCmd
        self._logFile = logFile
        self._instanceId = id
        self._path = path

class Product():
   '''
   Represents an instance of the product.  That includes all of the processes
   needed to have "the product" running.
   '''
   _apiServerUrl = None
   _logs = []
   _processes = []
   _concordProcessesMetaData = []
   _userProductConfig = None
   _cmdlineArgs = None
   _baseUrl = None

   def __init__(self, cmdlineArgs, apiServerUrl, userConfig, baseUrl):
      self._cmdlineArgs = cmdlineArgs
      self._apiServerUrl = apiServerUrl
      self._userConfig = userConfig
      self._userProductConfig = userConfig["product"]
      self._baseUrl = baseUrl

   def launchProduct(self):
      '''
      Given the user's product config section, launch the product.
      Raises an exception if it cannot start.
      '''
      atexit.register(self.stopProduct)
      productLogsDir = os.path.join(self._cmdlineArgs.resultsDir, PRODUCT_LOGS_DIR)
      os.makedirs(productLogsDir, exist_ok=True)

      # Workaround for intermittent product launch issues.
      numAttempts = 0
      launched = False

      while (not launched) and (numAttempts < self._cmdlineArgs.productLaunchAttempts):
         try:
            if self._cmdlineArgs.dockerComposeFile:
               self._launchViaDocker(productLogsDir)
            else:
               self._launchViaCmdLine(productLogsDir)

            launched = True
         except Exception as e:
            numAttempts += 1
            log.info("Attempt {} to launch the product failed. Exception: '{}'".format(
               numAttempts, str(e)))

            if numAttempts < self._cmdlineArgs.productLaunchAttempts:
               log.info("Stopping whatever was launched and attempting to launch again.")
               self.stopProduct()

      if not launched:
         raise Exception("Failed to launch the product after {} attempt(s). Exiting".format(numAttempts))


   def validatePaths(self, paths):
      '''Make sure the given paths are all valid.'''
      for path in paths:
         if not os.path.isfile(path):
            log.error("The file '{}' does not exist.".format(path))
            return False

      return True


   def mergeDictionaries(self, orig, new):
      '''Python's update() simply replaces keys at the top level.'''
      for newK, newV in new.items():
         if newK in orig:
            if isinstance(newV, collections.Mapping):
               self.mergeDictionaries(orig[newK], newV)
            elif isinstance(newV, list):
               orig[newK] = orig[newK] + newV
            else:
               orig[newK] = newV
         else:
            orig[newK] = newV


   def _launchViaDocker(self, productLogsDir):
      dockerCfg = {}

      if self.validatePaths(self._cmdlineArgs.dockerComposeFile):
         for cfgFile in self._cmdlineArgs.dockerComposeFile:
            with open(cfgFile, "r") as f:
               newCfg = yaml.load(f)
               self.mergeDictionaries(dockerCfg, newCfg)

         self.copyEnvFile()

         if not self._cmdlineArgs.keepconcordDB:
            self.clearDBsForDockerLaunch(dockerCfg)
            self.initializeHelenDockerDB(dockerCfg)

         cmd = ["docker-compose"]

         for cfgFile in self._cmdlineArgs.dockerComposeFile:
            cmd += ["--file", cfgFile]

         cmd += ["up"]

         logFile = open(os.path.join(productLogsDir, "concord.log"),
                    "wb+")
         self._logs.append(logFile)
         log.debug("Launching via docker-compose with {}".format(cmd))

         p = subprocess.Popen(cmd,
                              stdout=logFile,
                              stderr=subprocess.STDOUT)
         self._processes.append(p)

         if not self._waitForProductStartup():
            raise Exception("The product did not start.")
      else:
         raise Exception("The docker compose file list contains an invalid value.")


   def copyEnvFile(self):
      # This file contains variables fed to docker-compose.yml.  It is picked up from the
      # location of the process which invokes docker compose.
      if not os.path.isfile(".env"):
         log.debug("Copying .env file from Concord.")
         shutil.copyfile("../concord/docker/.env", "./.env")


   def _launchViaCmdLine(self, productLogsDir):
      # Since we change directories while launching products, save cwd here
      # and chdir to that once all launches are done
      original_cwd = os.getcwd()

      for launchElement in self._userProductConfig["launch"]:
         for project in launchElement:
            projectSection = launchElement[project]
            buildRoot = projectSection["buildRoot"]
            buildRoot = os.path.expanduser(buildRoot)

            if not self._cmdlineArgs.keepconcordDB and \
               project.lower().startswith("concord"):
               self.clearconcordDBForCmdlineLaunch(launchElement[project])

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

                     hermes_home = self._cmdlineArgs.hermes_dir
                     concord_home = os.path.join(hermes_home, '..', 'concord')
                     helen_home = os.path.join(hermes_home, '..', 'helen')
                     param = param.replace('${HERMES_HOME}', hermes_home)
                     param = param.replace('${CONCORD_HOME}', concord_home)
                     param = param.replace('${HELEN_HOME}', helen_home)

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
                  if project.lower().startswith("concord"):
                      cm = ConcordInstsanceMetaData(
                        len(self._processes) - 1,
                        cmd,
                        logFile,
                        int(project[len("concord"):]),
                        os.getcwd()
                      )
                      self._concordProcessesMetaData.append(cm)
            # switch back to original cwd
            os.chdir(original_cwd)

      # All pieces should be launched now.
      if not self._waitForProductStartup():
         raise Exception("The product did not start.")


   def clearconcordDBForCmdlineLaunch(self, concordSection, serviceName=None):
      '''
      Deletes the concord DB so we get a clean start.
      Other test suites can leave it in a state that makes
      it fail.
      Note that Helen's DB is cleared by the cockroach shell script
      when the product is launched via command line.
      '''
      params = None

      log.debug("serviceName{}".format(serviceName))

      for subSection in concordSection:
         if serviceName is None:
             if subSection.lower().startswith("concord"):
                 params = concordSection[subSection]["parameters"]
         elif subSection.lower() == serviceName:
             params = concordSection[subSection]["parameters"]

      isConfigParam = False
      buildRoot = None

      for param in params:
         if isConfigParam:
            configFile = os.path.join(concordSection["buildRoot"],
                                      param)
            configFile = os.path.expanduser(configFile)
            subPath = None

            with open (configFile, "r") as props:
               for prop in props:
                  prop = prop.strip()
                  if prop and not prop.startswith("#") and \
                     prop.lower().startswith("blockchain_db_path"):
                     subPath = prop.split("=")[1]

            buildRoot = os.path.abspath(concordSection["buildRoot"])
            dbPath = os.path.join(concordSection["buildRoot"], subPath)
            dbPath = os.path.expanduser(dbPath)
            if os.path.isdir(dbPath):
               log.debug("Clearing concord DB directory '{}'".format(dbPath))
               shutil.rmtree(dbPath)
            isConfigParam = False

         if param == "-c":
            isConfigParam = True

      return buildRoot


   def pullHelenDBImage(self, dockerCfg):
      '''This is the cockroach DB.  Make sure we have it before trying to start
         the product so we don't time out while downloading it.'''
      image = dockerCfg["services"]["db-server"]["image"]
      image_name = image.split(":")[0]
      pull_cmd = ["docker", "pull", image]
      find_cmd = ["docker", "images", "--filter", "reference="+image]
      found = False

      completedProcess = subprocess.run(pull_cmd,
                                        stdout=subprocess.PIPE,
                                        stderr=subprocess.STDOUT)
      # Sleep just in case there is a gap between the time "pull" finishes
      # and "images" can find it. In testing, it looks immediate.
      time.sleep(1)
      completedProcess = subprocess.run(find_cmd,
                                        stdout=subprocess.PIPE,
                                        stderr=subprocess.STDOUT)
      psOutput = completedProcess.stdout.decode("UTF-8")

      if image_name in psOutput:
         found = True

      return found


   def startHelenDockerDB(self, dockerCfg):
      ''' Starts the Helen DB.  Returns True if able to start, False if not.'''
      cmd = ["docker-compose"]

      for cfgFile in self._cmdlineArgs.dockerComposeFile:
         cmd += ["--file", cfgFile]

      cmd += ["up", "db-server"]
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


   def getRunningContainerIds(self, searchString):
      '''
      Return the docker container Id(s) which are running and whose "docker ps" output
      contains the given search string.
      '''
      containerIds = []

      # The processes cmd gives us a string like:
      # CONTAINER ID        IMAGE                          COMMAND ...
      # 21d37f282847        cockroachdb/cockroach:v2.0.2   "/cockroach/cockroac…" ...
      cmd = ["docker", "ps", "--filter", "status=running"]
      log.debug("Getting running containers with command '{}'".format(cmd))
      completedProcess = subprocess.run(cmd,
                                        stdout=subprocess.PIPE,
                                        stderr=subprocess.STDOUT)
      psOutput = completedProcess.stdout.decode("UTF-8")
      lines = psOutput.split("\n")

      for line in lines:
         if searchString in line:
            log.debug("Found container '{}' with search string '{}' in the ps output.".format(line, searchString))
            fields = line.strip().split(" ")
            containerIds.append(fields[0])

      if not containerIds:
         log.debug("Unable to find a running container for '{}'.".format(searchString))
         log.debug("stdout: '{}', stderr: '{}'".format(completedProcess.stdout, completedProcess.stderr))

      return containerIds


   def getHelenDBContainerId(self, dockerCfg):
      '''Returns the running Helen DB container's ID, or None if it cannot be found.'''
      dbImageName = dockerCfg["services"]["db-server"]["image"]
      containerId = None
      sleepTime = 3
      maxTries = 10
      numTries = 0

      while numTries < maxTries and not containerId:
         containerIds = self.getRunningContainerIds(dbImageName)

         if containerIds:
            containerId = containerIds[0]
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

      with open("../helen/src/main/resources/database/schema.sql", "r") as f:
         schema = f.read()

      commands = [
         ["docker", "exec", containerId, "./cockroach", "user", "set", "helen_admin", "--insecure"],
         ["docker", "exec", containerId, "./cockroach", "sql", "--insecure", "-e", schema],
      ]

      for cmd in commands:
         log.info("running '{}'".format(cmd))
         completedProcess = subprocess.run(cmd,
                                           stdout=subprocess.PIPE,
                                           stderr=subprocess.STDOUT)
         try:
            completedProcess.check_returncode()
            log.info("stdout: {}, stderr: {}".format(completedProcess.stdout, completedProcess.stderr))
         except subprocess.CalledProcessError as e:
            log.error("Command '{}' to configure the Helen DB failed.  Exit code: '{}'".format(cmd, e.returncode))
            log.error("stdout: '{}', stderr: '{}'".format(completedProcess.stdout, completedProcess.stderr))
            return False

      return True


   def stopDockerContainer(self, containerId):
      '''Stops the given docker container. Returns whether the exit code indicated success.'''
      log.info("Stopping '{}'".format(containerId))
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
      if not self.pullHelenDBImage(dockerCfg):
         raise Exception("Unable to pull the Helen DB image.")

      if not self.startHelenDockerDB(dockerCfg):
         raise Exception("The Helen DB failed to come up.")

      containerId = self.getHelenDBContainerId(dockerCfg)
      if not containerId:
         raise Exception("Unable to get the running Helen DB's docker container ID.")

      if not self.configureHelenDockerDB(containerId):
         raise Exception("Unable to configure the Helen DB.")

      if not self.stopDockerContainer(containerId):
         raise Exception("Failure trying to stop the Helen DB.")


   def clearDBsForDockerLaunch(self, dockerCfg, serviceName=None):
      concordDbPath = None
      for service in dockerCfg["services"]:
         if serviceName is None or  service == serviceName:
             serviceObj = dockerCfg["services"][service]
             if "volumes" in serviceObj:
                for v in serviceObj["volumes"]:
                   if "rocksdbdata" in v or \
                      "cockroachDB" in v:
                      yamlDir = os.path.dirname(self._cmdlineArgs.dockerComposeFile[0])
                      deleteMe = os.path.join(yamlDir, v.split(":")[0])
                      log.info("Deleting: {}".format(deleteMe))
                      if serviceName == service and "rocksdbdata" in v:
                        concordDbPath = os.path.abspath(yamlDir)
                      if os.path.isdir(deleteMe):
                         try:
                            shutil.rmtree(deleteMe)
                         except PermissionError as e:
                            log.error("Could not delete {}. Try running with sudo " \
                                      "when running in docker mode.".format(deleteMe))
                            raise e
      return concordDbPath


   def stopProcessesInContainers(self, containerSearchString, processSearchString):
      '''
      containerSearchString: A string appearing in the "docker ps" output for the container
      to look for.
      processSearchSring: A string appearing in the docker container's "ps" output for
      the process to look for.
      Sends a process (processSearchString) in a container (containerSearchString)
      a polite request to stop before we kill the container in which it is running.
      This is needed because a "docker kill" of a container abruptly terminates a utility
      such as Valgrind, which prevents it from summarizing memory leak data.
      '''
      containerIds = self.getRunningContainerIds(containerSearchString)

      for containerId in containerIds:
         cmd = ["docker", "exec", containerId, "ps", "-x"]
         completedProcess = subprocess.run(cmd,
                                        stdout=subprocess.PIPE,
                                        stderr=subprocess.STDOUT)
         psOutput = completedProcess.stdout.decode("UTF-8")
         lines = psOutput.split("\n")

         for line in lines:
            if processSearchString in line and not "/bin/sh" in line:
               fields = line.strip().split(" ")
               processId = fields[0]
               log.info("Killing process id '{}' in container '{}' because we were " \
                        "asked to kill processes containing '{}' in containers " \
                        "containing '{}'".format(processId, containerId,
                                               processSearchString, containerSearchString))
               cmd = ["docker", "exec", containerId, "kill", processId]
               completedProcess = subprocess.run(cmd,
                                              stdout=subprocess.PIPE,
                                              stderr=subprocess.STDOUT)
               psOutput = completedProcess.stdout.decode("UTF-8")
               log.info("Kill command output: {}".format(psOutput))


   def stopMemoryLeakNode(self):
      '''
      We need to send Valgrind a polite request to terminate before
      killing the container it is running in so that it can summarize
      memory leak information.  Then we give it a few seconds to do so.
      '''
      self.stopProcessesInContainers("memleak", "valgrind")
      time.sleep(10)


   def stopProduct(self):
      '''
      Stops the product executables and closes the logs.
      '''
      if self._cmdlineArgs.dockerComposeFile:
         self.stopMemoryLeakNode()
         cmd = ["docker-compose"]

         for cfgFile in self._cmdlineArgs.dockerComposeFile:
            cmd += ["--file", cfgFile]

         cmd += ["down"]
         print("Stopping the product with command '{}'".format(cmd))
         p = subprocess.run(cmd)
      else:
         for p in self._processes[:]:
            if p.poll() is None:
               p.terminate()
               print("Terminating {}.".format(p.args))

         for p in self._processes[:]:
            if "docker" in p.args:
               cmd = ["docker", "ps", "-q", "-f",
                      "name=reverse-proxy-hermes-test"]
               ps_output = subprocess.run(cmd,
                                          stdout=subprocess.PIPE,
                                          stderr=subprocess.STDOUT)
               container_ids = ps_output.stdout.decode("UTF-8").split("\n")
               print ("Container IDs found: {0}".format(container_ids))

               for container_id in container_ids:
                  if container_id:
                     print("Terminating container ID: {0}".format(container_id))
                     if not self.stopDockerContainer(container_id):
                        raise Exception("Failure trying to stop docker container.")

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
      retries = 10
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
            rpc.addUser(self._baseUrl)
            txHash = rpc.sendTransaction(caller, data)
         except Exception as e:
            log.debug("Waiting for product startup...")
            log.debug(e)
      return txHash != None

   def cleanConcordDb(self, instanceId):
      if len(self._concordProcessesMetaData) == 0:
         if os.path.isfile(self._cmdlineArgs.dockerComposeFile[0]):
            with open(self._cmdlineArgs.dockerComposeFile[0], "r") as f:
               dockerCfg = yaml.load(f)
               res= self.clearDBsForDockerLaunch(dockerCfg, "concord{}".format(instanceId))
               return res
      else:
        for launchElement in self._userProductConfig["launch"]:
           for project in launchElement:
              projectSection = launchElement[project]
              buildRoot = projectSection["buildRoot"]
              buildRoot = os.path.expanduser(buildRoot)

              if not self._cmdlineArgs.keepconcordDB and \
                 project.lower() == "concord" + str(instanceId):
                 path = self.clearconcordDBForCmdlineLaunch(launchElement[project], "concord" + str(instanceId))
                 return path

   def get_concord_container_name(self, replicaId):
      command = 'docker ps --format "{0}" | grep concord{1}'.format("{{ .Names }}", replicaId)
      output = subprocess.Popen(command,stderr=subprocess.PIPE, shell=True, stdout=subprocess.PIPE).stdout.read().decode().replace(os.linesep,"")
      return output

   def action_on_concord_container(self, containerName, action):
      command = "docker {0} {1}".format(action, containerName)
      output = subprocess.Popen(command,stderr=subprocess.PIPE, shell=True, stdout=subprocess.PIPE).stdout.read().decode().replace(os.linesep,"")
      if output != containerName:
        return False
      return True

   def start_concord_replica(self, id):
       if len(self._concordProcessesMetaData) == 0:
          containerName = "docker_concord{}_1".format(id)
          return self.action_on_concord_container(containerName, "start")

       originalCwd = os.getcwd()
       result = False
       for idx, meta in enumerate(self._concordProcessesMetaData):
           if meta._instanceId == id:
               log.info("Starting concord replica{}".format(id))
               os.chdir(meta._path)
               p = subprocess.Popen(meta._processCmd,
                                    stdout=meta._logFile,
                                    stderr=subprocess.STDOUT)
               log.info("Concord replica{} started".format(id))
               self._processes.append(p)
               meta._processIndex = len(self._processes) - 1
               self._concordProcessesMetaData[idx] = meta
               result = True
               break
       os.chdir(originalCwd)
       return result

   def kill_concord_replica(self, id):
       if len(self._concordProcessesMetaData) == 0:
          containerName = self.get_concord_container_name(id)
          if len(containerName) == 0:
             return False
          return self.action_on_concord_container(containerName, "kill")

       for meta in self._concordProcessesMetaData:
           if meta._instanceId == id:
               log.info("Killing concord replica with ID {}".format(id))
               self._processes[meta._processIndex].kill()
               meta._processIndex = None
               log.info("Killed concord replica with ID {}".format(id))
               return True
       return False

   def pause_concord_replica(self, id):
       if len(self._concordProcessesMetaData) == 0:
          containerName = self.get_concord_container_name(id)
          if len(containerName) == 0:
             return False
          return self.action_on_concord_container(containerName, "pause")

       for meta in self._concordProcessesMetaData:
           if meta._instanceId == id:
               log.info("Suspending concord replica with ID {}".format(id))
               os.kill(self._processes[meta._processIndex].pid, signal.SIGSTOP)
               log.info("Suspended concord replica with ID {}".format(id))
               return True
       return False

   def resume_concord_replica(self,id):
       if len(self._concordProcessesMetaData) == 0:
          containerName = "docker_concord{}_1".format(id)
          return self.action_on_concord_container(containerName, "unpause")

       for meta in self._concordProcessesMetaData:
           if meta._instanceId == id:
               log.info("Resuming concord replica with ID {}".format(id))
               os.kill(self._processes[meta._processIndex].pid, signal.SIGCONT)
               log.info("Resumed concord replica with ID {}".format(id))
       return True
