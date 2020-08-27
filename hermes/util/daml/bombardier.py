##########################################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#
#   DAML Bombardier Client
#
##########################################################################################

import requests
import subprocess
import atexit
import logging
import os
import errno
import time
import json
import threading
import traceback
import random
from util import helper

log = logging.getLogger(__name__)

# Older working version: 0.0.57
BOMBARDIER_VERSION = "0.1.3"
BOMBARDIER_VERSION_CHECKED = False

class BombardierFleet:
  '''
    Tx-load controller
    This class contains both client and server spawn logic
  '''

  def __init__(self, for_test_name, count=1, portStart=8865, check_install=True):
    '''
      Initialize for a single test case (log output to a specific folder)
    '''
    self.for_test_name = for_test_name
    self.clients = []

    if not BOMBARDIER_VERSION_CHECKED and check_install:
      self.check_version()

    for i in range(count):
      self.fresh_worker(i, portStart+i)

    time.sleep(3)

    def terminateOnParentExit(): self.terminate_all()
    atexit.register(terminateOnParentExit)


  def fresh_worker(self, i, port):
    '''
      Spawns a fresh worker dedicated to a specific test case.
      tx-loader log output will be redirected to:
      {CURRENT_SUITE_LOG_PATH}/{CASE_NAME}/bombardier.log
      for debugging tx-loader when something seems to be up.
      This terminates old instance of tx-loader
    '''
    logFile = "{}/bombardier{}.log".format(self.for_test_name, i)
    print("logFile:{}".format(logFile))

    self.clients.append({
      "index": i,
      "logFile": logFile,
      "port": port,
      "session": requests.Session()
    })

    if not os.path.exists(os.path.dirname(logFile)):
      try:
        os.makedirs(os.path.dirname(logFile))
      except OSError as exc: # Guard against race condition
        if exc.errno != errno.EEXIST:
          raise

    self.terminate(port)
    time.sleep(1)

    with open(logFile, "w+") as f:
      subprocess.call("daml-bombardier {} {} &".format(port, i), stdout=f, shell=True)

    log.info("DAML Bombardier client worker (id={}) started on port {}".format(i, port))
    log.info("To see the logs: tail -f {}".format(logFile))


  def txLoad(self, client, endpoint, count=1, connections=1, asynchronous=False, timeout=30,
             party=None, txIds=False):
    '''
      Proceed to load tx based on:
      count: number of IOU transactions to submit
      connection: number of concurrency live connections to use.
      asynchronous: rather than waiting for the entire job, return `task_id` right away
                    This unblocks the process to do other things while transactions
                    are kept being submitted and processed. (e.g. mid-tx interruption)
      Returns False when any error was encountered
    '''
    try:
      clientBaseUrl = "http://localhost:{}".format(client["port"])
      params={
        "endpoint": endpoint,
        "count": count,
        "concurrency": connections,
        "async": 1 if asynchronous else 0
      }

      if party:
        params["party"] = party

      if txIds:
        params["getTxIds"] = 1

      res = client["session"].get(clientBaseUrl + "/tx-load",
                                  params = params,
                                  timeout = timeout)

      if res.status_code == 200:
        resObj = json.loads(res.content.decode('utf-8'), strict=False)
        if "status" in resObj and resObj["status"] == "ok":
          return resObj

      return False
    except Exception as e:
      log.error("       {}".format(e))
      return False


  def txGet(self, client, endpoint, txIds, parties, connections=1, timeout=30):
    '''
      Retrieve transactions via their transaction IDs.
      txIds: List of transaction IDs.
    '''
    try:
      clientBaseUrl = "http://localhost:{}".format(client["port"])
      tx_string = ",".join(txIds)
      params={
        "endpoint": endpoint,
        "ids": tx_string,
        "concurrency": connections
      }

      if parties:
        params["parties"] = parties

      res = client["session"].get(clientBaseUrl + "/tx-get",
                                  params = params)

      if res.status_code == 200:
        resObj = json.loads(res.content.decode('utf-8'), strict=False)

        if "status" in resObj and resObj["status"] == "ok":
          return resObj

      return False
    except Exception as e:
      log.error("       {}".format(e))
      return False


  def reserveConnections(self, client, endpoint, count=1, timeout=360):
    '''
      Reserve n connections on a given endpoint for immediate usage.
      Returns False when any error was encountered
    '''
    try:
      clientBaseUrl = "http://localhost:{}".format(client["port"])
      res = client["session"].get(clientBaseUrl + "/connection-pool-reserve", params={
        "endpoint": endpoint, "count": count
      }, timeout = timeout)
      if res.status_code == 200:
        resObj = json.loads(res.content.decode('utf-8'), strict=False)
        if "status" in resObj and resObj["status"] == "ok":
          return resObj
      return False
    except Exception as e:
      log.error("       {}".format(e))
      return False


  def connectionPoolInfo(self, client, endpoint, timeout=30):
    '''
      Query for connection pool information.
    '''
    try:
      clientBaseUrl = "http://localhost:{}".format(client["port"])
      res = client["session"].get(clientBaseUrl + "/connection-pool-info", params={
        "endpoint": endpoint
      }, timeout = timeout)
      if res.status_code == 200:
        resObj = json.loads(res.content.decode('utf-8'), strict=False)
        if "status" in resObj and resObj["status"] == "ok":
          return resObj
      return None
    except Exception as e:
      log.error("       {}".format(e))
      return None


  def terminate(self, port):
    '''
      Terminate the worker server process.
      Returns False when any error was encountered
    '''
    try:
      clientBaseUrl = "http://localhost:{}".format(port)
      res = requests.get(clientBaseUrl + "/server/terminate", timeout=10)
      if res.status_code == 200:
        resObj = json.loads(res.content.decode('utf-8'), strict=False)
        if "status" in resObj and resObj["status"] == "ok":
          return resObj
      return False
    except Exception as e:
      return False


  def reset(self, port):
    '''
      This will purge all connections and cached parties/package/templates
      on the tx-load server. After blockchain reset this must be called.
      Returns False when any error was encountered
    '''
    try:
      clientBaseUrl = "http://localhost:{}".format(port)
      res = requests.get(clientBaseUrl + "/server/reset", timeout=10)
      if res.status_code == 200:
        resObj = json.loads(res.content.decode('utf-8'), strict=False)
        if "status" in resObj and resObj["status"] == "ok":
          return resObj
      return False
    except Exception as e:
      return False


  def waitAndCheck(self, client, taskId, timeout=600, max_stale=60):
    '''
      This will wait for a single task_id to finish.
      If task is tx loading, it will also output progress of the tx-loading job
      `timeout` is timeout for the waiting
      `max_stale` used to terminate after no tx go trhough for n seconds
      Returns False when any error was encountered
    '''
    try:
      lastAnnounced = time.time()
      lastHandledUpdated = time.time()
      handledCount = -1
      while True:
        clientBaseUrl = "http://localhost:{}".format(client["port"])
        res = client["session"].get(clientBaseUrl + "/task-check",
                                    params={ "taskId": taskId }, timeout = timeout)
        now = time.time()
        if handledCount >= 0 and now - lastHandledUpdated > max_stale: # tx couldn't pass for 60 seconds
          log.error("       tx not going through for {} seconds. Terminating this task...".format(max_stale))
          return False
        if res.status_code == 200:
          resObj = json.loads(res.content.decode('utf-8'), strict=False)
          if "status" in resObj and resObj["status"] == "ok":
            if resObj["state"] == "finished":
              outcome = resObj["result"]
              return outcome
            elif now - lastAnnounced > (6 + random.uniform(0.0, 3.0)):
              lastAnnounced = now
              if "result" in resObj:
                data = resObj["result"]
                if handledCount != data["handled"] + data["rejected"]:
                  handledCount = data["handled"] + data["rejected"]
                  lastHandledUpdated = now
                log.info("       all({}) = good({}) + bad({}), tps: {:.1f}, taskId={}".format(
                                  data["total"], data["handled"], data["rejected"], data["tps"], taskId))
        time.sleep(random.uniform(1.0, 1.5))
    except Exception as e:
      log.error("       {}".format(e))
      return False


  def waitAndCheckMultiple(self, taskIds, timeout=600):
    '''
      This will await multiple task_ids to finish (join)
      Returns False when any error was encountered
    '''
    try:
      results = []; threads = []
      def checkOne(taskId):
        result = self.waitAndCheck(taskId, timeout)
        results.append({"outcome": result, "taskId": taskId})
      for taskId in taskIds:
        thr = threading.Thread( target = lambda taskId: checkOne(taskId), args = (taskId, ))
        threads.append(thr); thr.start()
      for thd in threads: thd.join() # wait for all checks to return
      allPassed = True
      for result in results:
        if not result["outcome"]:
          allPassed = False; break
      if allPassed:
        return results
      else:
        return False
    except Exception as e:
      log.error("       {}".format(e))
      return False




  # ===================================================================================
  #   For All Workers
  # ===================================================================================
  def fresh_workers(self):
    for worker in self.clients: self.fresh_worker(worker["index"], worker["port"])

  def terminate_all(self):
    for worker in self.clients: self.terminate(worker["port"])
    time.sleep(1)

  def reset_all(self):
    for worker in self.clients: self.reset(worker["port"])




  # ===================================================================================
  #   Initialization Checks
  # ===================================================================================
  def install(self):
    '''
      NPM install tx-loader package
      Does not work with sudo.  To work with sudo, we will have to do this:
        npm config set registry http://build-artifactory.eng.vmware.com:80/artifactory/api/npm/npm
      but even then, things will not work.  So not doing the above until we have a path
      to success with sudo.
    '''
    returnCode = subprocess.call(
      "npm install -g @vdaml/daml-bombardier@{} --unsafe > bombardier_install.log 2>&1".format(BOMBARDIER_VERSION),
      shell=True
    )
    if returnCode == 0:
      log.info("Successfully imported DAML bombardier v{}."
                .format(BOMBARDIER_VERSION))
      time.sleep(5)
    else:
      raise Exception("Unable to import DAML bombardier.")


  def check_version(self):
    '''
      Check and install right version of tx-loader
    '''
    diffOutput = None

    try:
      diffOutput = subprocess.run(['daml-bombardier-installed'], stdout=subprocess.PIPE).stdout.decode('utf-8')
    except Exception as e:
      log.info("Exception")
      log.info(e)
      pass

    diffObj = None

    if diffOutput:
      diffObj = json.loads(diffOutput)

    if not diffObj or not diffObj["installed"]:
      log.info("DAML Bombardier is not yet imported, resolving...")
      self.install()
    subprocess.call("daml-bombardier > /dev/null 2>&1 &", shell=True)
    version = None; tries = 0
    while tries < 10:
      try:
        tries += 1
        res = requests.get("http://localhost:8865/version", timeout=5)
        if res.status_code == 200:
          resObj = json.loads(res.content.decode('utf-8'), strict=False)
          if "status" in resObj and resObj["status"] == "ok":
            version = resObj["version"]
            break
      except Exception as e:
        log.error("Could not launch bombardier.")
        pass
      time.sleep(1)
    if version != BOMBARDIER_VERSION:
      log.info("DAML bombardier version does not match ({} != {}), importing..."
                .format(version, BOMBARDIER_VERSION))
      self.install()
    global BOMBARDIER_VERSION_CHECKED
    BOMBARDIER_VERSION_CHECKED = True
