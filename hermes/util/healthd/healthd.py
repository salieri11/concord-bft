#!/usr/bin/python3

#########################################################################
# Copyright 2018 - 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import time
import os
import json
import requests
import logging
import subprocess
import re
import copy
import resource
from collections import namedtuple

DEFAULT_REPORTING_INTERVAL = 20 # report and log every 20 seconds by default (mininum 3s)
DEFAULT_ANNOUNCE_INTERVAL = 21600 # 6-hours; announce stats that stay relatively static

AGENT_DOWN_MAX_FORGIVE = 120 # how long should the daemon forgive agent from being down before reporting crash

CONFIG = {
  # Output files, and reporting intervals
  "logFile": "",
  "crashReportFile": "",
  "sleepTime": DEFAULT_REPORTING_INTERVAL,
  "announceInterval": DEFAULT_ANNOUNCE_INTERVAL,

  # Node identifying information
  "ip": "",
  "buildId": "",
  "uuid": "",
  "blockchainId": "",
  "blockchainType": "",
  "nodeType": "",

  # Which containers to look for
  "componentsWatchList": [],
  
  # Is crash announced already?
  "crashAnnounced": False,
}

log = None
def setUpLogging():
  global log
  log = logging.getLogger('healthd')
  log.setLevel(logging.INFO)
  handler=logging.FileHandler(CONFIG["logFile"], mode='a+')
  formatter = logging.Formatter('%(asctime)s %(levelname)s %(message)s', datefmt='%Y-%m-%d %H:%M:%S')
  handler.setFormatter(formatter)
  log.addHandler(handler)


def loadProperties(filepath, sep='=', comment_char='#'):
  """
  Read the file passed as parameter as a properties file.
  """
  props = {}
  with open(filepath, "rt") as f:
    for line in f:
      l = line.strip()
      if l and not l.startswith(comment_char):
        key_value = l.split(sep)
        key = key_value[0].strip()
        value = sep.join(key_value[1:]).strip().strip('"') 
        props[key] = value 
  return props


def parseColumnViewToJson(cmds, columns):
  result = subprocess.run(cmds, stdout=subprocess.PIPE)
  header, *rows = result.stdout.decode("utf-8").splitlines()
  ColumnRange = namedtuple('ColumnRange', ['column', 'start_idx', 'stop_idx'])
  def find_column_range(header, column):
    """Determines the string indexes that the given column lies between."""
    column_idx = columns.index(column)
    start_idx = header.index(column)
    try:
      next_column = columns[column_idx + 1]
      stop_idx = header.index(next_column)
    except IndexError:
      # There is no next column (i.e. we're on the last one).
      stop_idx = None
    column_range = ColumnRange(column, start_idx, stop_idx)
    return column_range
  def row_to_container(row, column_ranges):
    """Extracts the column values from a container row."""
    container = {column_range.column: extract_field(row, column_range)
                 for column_range in column_ranges}
    return container
  def extract_field(row, column_range):
    """Pulls the value of a field from a given row."""
    start_idx = column_range.start_idx
    stop_idx = column_range.stop_idx or len(row)
    field = row[start_idx:stop_idx].strip()
    return field
  column_ranges = [find_column_range(header, column) for column in columns]
  running_containers = [row_to_container(row, column_ranges) for row in rows]
  return running_containers


class CPULoadViewer():
  def __init__(self, percentage=True):
    self.percentage = percentage
    self.cpustat = '/proc/stat'
    self.sep = ' ' 

  def getTime(self):
    '''
    http://stackoverflow.com/questions/23367857/accurate-calculation-of-cpu-usage-given-in-percentage-in-linux
    Idle=idle+iowait
    NonIdle=user+nice+system+irq+softirq+steal
    Total=Idle+NonIdle # first line of file for all cpus
    CPU_Percentage=((Total-PrevTotal)-(Idle-PrevIdle))/(Total-PrevTotal)
    '''
    cpu_infos = {} #collect here the information
    with open(self.cpustat,'r') as f_stat:
      lines = [line.split(self.sep) for content in f_stat.readlines() for line in content.split('\n') if line.startswith('cpu')]

      #compute for every cpu
      for cpu_line in lines:
        if '' in cpu_line: cpu_line.remove('')#remove empty elements
        cpu_line = [cpu_line[0]]+[float(i) for i in cpu_line[1:]]#type casting
        cpu_id,user,nice,system,idle,iowait,irq,softrig,steal,guest,guest_nice = cpu_line

        Idle=idle+iowait
        NonIdle=user+nice+system+irq+softrig+steal

        Total=Idle+NonIdle
        #update dictionionary
        cpu_infos.update({cpu_id:{'total':Total,'idle':Idle}})
      return cpu_infos

  def getLoad(self):
    '''
    CPU_Percentage=((Total-PrevTotal)-(Idle-PrevIdle))/(Total-PrevTotal)
    '''
    start = self.getTime() 
    time.sleep(CONFIG["sleepTime"] - 2)
    stop = self.getTime()
    cpu_load = {}
    for cpu in start:
      Total = stop[cpu]['total']
      PrevTotal = start[cpu]['total']
      Idle = stop[cpu]['idle']
      PrevIdle = start[cpu]['idle']
      CPU_Percentage=((Total-PrevTotal)-(Idle-PrevIdle))/(Total-PrevTotal)*100
      cpu_load.update({cpu: CPU_Percentage})
    return cpu_load


def getMemoryFootprint():
  mem=str(os.popen('free -t -m').readlines())
  T_ind=mem.index('T')
  mem_G=mem[T_ind+14:-4]
  S1_ind=mem_G.index(' ')
  mem_T=mem_G[0:S1_ind]
  mem_G1=mem_G[S1_ind+8:]
  S2_ind=mem_G1.index(' ')
  mem_U=mem_G1[0:S2_ind]
  mem_F=mem_G1[S2_ind+8:]
  return { # in MB
    "total" : float(mem_T.strip()),
    "used" : float(mem_T.strip()) - float(mem_F.strip()),
    "free" : float(mem_F.strip()),
    "percent" : (float(mem_T.strip()) - float(mem_F.strip())) / float(mem_T.strip()) * 100
  }


def getDiskFootprint():
  result = subprocess.run(["df"], stdout=subprocess.PIPE)
  header, *rows = result.stdout.decode("utf-8").splitlines()
  values = re.split('\s+', rows[0]) # remove all multiple spaces
  diskInfo = {
    "total": float3(float(values[1]) / 1024 / 1024), # total RAM in GB
    "percent": float3(float(values[4][:-1])) # percent usage
  }
  return diskInfo


def checkContainerStatus(checkDisk=False):
  '''
    --size option this may use a lot of system io, 
    must be run only once in a while to check mounted path utilization
  '''
  command = ["docker", "ps", "-a"]
  columns = ['CONTAINER ID', 'IMAGE', 'COMMAND', 'CREATED', 'STATUS', 'PORTS', 'NAMES']
  if checkDisk: 
    command.append("--size")
    columns.append("SIZE")
  return parseColumnViewToJson(command, columns)


def parseCPUInfo():
  with open("/proc/cpuinfo") as fileIn:
    content = fileIn.read().splitlines()
    cpus = []
    currentCPU = {}
    for line in content:
      pos = line.find(":")
      if pos >= 0:
        if pos < len(line) - 1:
          propname = line[:pos].strip()
          value = line[pos+1:].strip()
          if propname != "processor":
            currentCPU[propname] = value
      else:
        cpus.append(currentCPU)
        currentCPU = {}
    fileIn.close()
    return cpus


def getDaemonProcessUsage():
  res = resource.getrusage(resource.RUSAGE_SELF)
  return {
    "utime": res.ru_utime, # user CPU time used
    "stime": res.ru_stime, # system CPU time used
    "maxrss": res.ru_maxrss, # maximum resident set size
    "ixrss": res.ru_ixrss, # integral shared memory size
    "idrss": res.ru_idrss, # integral unshared data size
    "isrss": res.ru_isrss, # integral unshared stack size
    "minflt": res.ru_minflt, # page reclaims (soft page faults)
    "majflt": res.ru_majflt, # page faults (hard page faults)
    "nswap": res.ru_nswap, # swaps
    "inblock": res.ru_inblock, # block input operations
    "oublock": res.ru_oublock, # block output operations
    "msgsnd": res.ru_msgsnd, # IPC messages sent
    "msgrcv": res.ru_msgrcv, # IPC messages received
    "nsignals": res.ru_nsignals, # signals received
    "nvcsw": res.ru_nvcsw, # voluntary context switches
    "nivcsw": res.ru_nivcsw, # involuntary context switches
  }


def getContainerFootprint(prevFootprints=None, prevFootprintsTimeMS=0):
  rawData = parseColumnViewToJson(
    ["docker", "stats", "-a", "--no-stream"],
    ['CONTAINER ID', 'NAME', 'CPU %', 'MEM USAGE / LIMIT', 'MEM %', 'NET I/O', 'BLOCK I/O', 'PIDS']
  )
  footprints = []
  for i, svc in enumerate(rawData):
    containerId = svc['CONTAINER ID']
    contLogFilePath = subprocess.run(
      ["docker", "inspect", "--format='{{.LogPath}}'", containerId], 
      stdout=subprocess.PIPE
    ).stdout.decode("utf-8").strip().replace("'", "")
    contLogSize = os.path.getsize(contLogFilePath) if os.path.exists(contLogFilePath) else 0
    footprint = {
      "n": svc["NAME"],
      "c": float3(svc["CPU %"][:-1]), # cpu %
      "m": float3(svc["MEM %"][:-1]), # mem %
      "d": (toKB(svc["NET I/O"].split(' / ')[0])), # total net read ("download")
      "u": (toKB(svc["NET I/O"].split(' / ')[1])), # total net send ("upload")
      "r": (toKB(svc["BLOCK I/O"].split(' / ')[0])), # total fs read
      "w": (toKB(svc["BLOCK I/O"].split(' / ')[1])), # total fs write
      "l": int(contLogSize / 1000), # container log size in kB
    }
    now = int(round(time.time() * 1000)) # in ms
    tDelta = float(now - prevFootprintsTimeMS) / 1000
    prevFootprint = prevFootprints[i] if prevFootprints else None
    if prevFootprint and tDelta > 0:
      footprint["lr"] = ((footprint["l"] - prevFootprint["l"]) / tDelta) # log rate (kB/s)
      if footprint["lr"] < 0: footprint["lr"] = 0 # negative log rate means, the log file is truncated
      footprint["dr"] = ((footprint["d"] - prevFootprint["d"]) / tDelta) # log rate (kB/s)
      if footprint["dr"] < 0: footprint["dr"] = 0 # negative rate means, container restarted
      footprint["ur"] = ((footprint["u"] - prevFootprint["u"]) / tDelta) # upload rate (kB/s)
      if footprint["ur"] < 0: footprint["ur"] = 0 # negative rate means, container restarted
      footprint["rr"] = ((footprint["r"] - prevFootprint["r"]) / tDelta) # disk read rate (kB/s)
      if footprint["rr"] < 0: footprint["rr"] = 0 # negative rate means, container restarted
      footprint["wr"] = ((footprint["w"] - prevFootprint["w"]) / tDelta) # disk write rate (kB/s)
      if footprint["wr"] < 0: footprint["wr"] = 0 # negative rate means, container restarted
    else:
      footprint["lr"] = 0.0
      footprint["dr"] = 0.0
      footprint["ur"] = 0.0
      footprint["rr"] = 0.0
      footprint["wr"] = 0.0
    footprints.append(footprint)
  return footprints


def trimFootprints(footprints):
  trimmedFootprints = []
  for footprint in footprints:
    trimmedFootprints.append({
      "name": footprint["n"], # container name
      "cpu": footprint["c"], # cpu %
      "mem": footprint["m"], # mem %
      "log": {
        "total": float3(footprint["l"], True),
        "rate": float3(footprint["lr"], True), # kB/s
      },
      "net":{
        "in":{
          "total": float3(footprint["d"], True),
          "rate": float3(footprint["dr"], True), # kB/s
        },
        "out":{
          "total": float3(footprint["u"], True),
          "rate": float3(footprint["ur"], True), # kB/s
        }
      },
      "disk":{
        "read":{
          "total": float3(footprint["r"], True),
          "rate": float3(footprint["rr"], True), # kB/s
        },
        "write":{
          "total": float3(footprint["w"], True),
          "rate": float3(footprint["wr"], True), # kB/s
        }
      },
    })
  return trimmedFootprints




def main():
  config = loadProperties('healthd.conf')
  
  # Configuration information
  CONFIG["logFile"] = config["logFile"] if "logFile" in config else ""
  CONFIG["crashReportFile"] = config["crashReportFile"] if "crashReportFile" in config else ""
  CONFIG["sleepTime"] = int(config["reportingInterval"]) if "reportingInterval" in config else DEFAULT_REPORTING_INTERVAL
  if CONFIG["sleepTime"] < 3: CONFIG["sleepTime"] = 3 # if less than 3 seconds, it would start affecting CPU time
  CONFIG["announceInterval"] = int(config["announceInterval"]) if "announceInterval" in config else DEFAULT_ANNOUNCE_INTERVAL
  CONFIG["ip"] = config["ip"] if "ip" in config else ""
  CONFIG["buildId"] = config["buildId"] if "buildId" in config else ""
  CONFIG["uuid"] = config["uuid"] if "uuid" in config else ""
  CONFIG["blockchainId"] = config["blockchainId"] if "blockchainId" in config else ""
  CONFIG["blockchainType"] = config["blockchainType"] if "blockchainType" in config else ""
  CONFIG["nodeType"] = config["nodeType"] if "nodeType" in config else ""
  CONFIG["componentsWatchList"] = config["componentsWatchList"].split(",") if "componentsWatchList" in config else []

  # Simple metics monitor for near real time reports
  # (Will be deprecated once all production components are fully equipped with metrics in July+)
  CONFIG["wavefrontUrl"] = config["wavefrontUrl"] if "wavefrontUrl" in config else ""
  CONFIG["wavefrontToken"] = config["wavefrontToken"] if "wavefrontToken" in config else ""
  CONFIG["wavefrontPublishQueue"] = []

  # initialize logging
  setUpLogging()
  log.info("Deamon started; log location = {}; config = {}".format(CONFIG["logFile"], json.dumps(CONFIG)))

  # for keep-alive persistent requests so HTTPS handshake doesn't happen every time
  reqSession = requests.Session()
  wfSession = requests.Session()
  
  cpuChecker = CPULoadViewer()

  # Last time relatively static info was announced.
  lastAnnounced = 0
  
  # Check iteration counter
  counter = 0

  # Previous data used for metric comparison
  prevContainersFootprints = None
  prevContainersFootprintsTimeMS = 0  
  prevDaemonLogSize = int(os.path.getsize(CONFIG["logFile"]) / 1024)

  # Node-indentifying information
  job = ""
  buildNumber = ""
  if len(CONFIG["buildId"]) > 0:
    pieces = CONFIG["buildId"].split("/")
    if len(pieces) >= 2:
      buildNumber = pieces.pop()
      job = '/'.join(pieces)
  ip = CONFIG["ip"]
  blockchainId = CONFIG["blockchainId"]
  blockchainType = CONFIG["blockchainType"]
  nodeType = CONFIG["nodeType"]

  # Was node OK even before daemon started?
  statusWasOk = True if not os.path.exists(CONFIG["crashReportFile"]) else False
  if not statusWasOk: CONFIG["crashAnnounced"] = True

  # Agent down timestamp used to forgive agent down for n seconds
  agentDown = 0

  # Wavefront publish helper function
  def wavefrontPublish(data, trailing=""):
    if "wavefrontBadToken" in CONFIG: return
    data += f' source=bc.hermes.healthd bc.ip="{ip}"'
    if blockchainId is not None: data += f' bc.id="{blockchainId}"'
    if blockchainType is not None: data += f' bc.type="{blockchainType}"'
    if nodeType is not None: data += f' bc.nodetype="{nodeType}"'
    if job is not None: data += f' bc.job="{job}"'
    if buildNumber is not None: data += f' bc.build="{buildNumber}"'
    data = data + ((" "+trailing) if len(trailing) > 0 else "")
    CONFIG["wavefrontPublishQueue"].append(data)
  
  # Wavefront publish batch flush to avoid mutiple HTTPS POST calls
  def wavefrontPublishFlush():
    if len(CONFIG["wavefrontPublishQueue"]) == 0: return
    try:
      with open("healthd_wf_batch_publish.txt", "w+") as f:
        f.write("\n".join(CONFIG["wavefrontPublishQueue"]))
      url = CONFIG["wavefrontUrl"] + '/report'
      token = CONFIG["wavefrontToken"]
      os.system(f'cat healthd_wf_batch_publish.txt | curl -H "Authorization: Bearer {token}" -F file=@- {url}')
      CONFIG["wavefrontPublishQueue"].clear()
    except Exception as e:
      log.info(e)


  # Main daemon logic
  while(True):

    # Get system CPU usage
    cpuData = cpuChecker.getLoad() # this call includes sleeping (t - 2)
    cpuInfo = {"core": [None]*(len(cpuData)-1)}
    for cpuId in cpuData:
      if cpuId == "cpu":
        cpuInfo["avg"] = float3(cpuData[cpuId])
      else:
        cpuInfo["core"][int(cpuId[3:])] = float3(cpuData[cpuId])
    # Get system memory usage
    memInfo = getMemoryFootprint()
    # Get system disk usage
    diskInfo = getDiskFootprint()
    
    # get containers information
    containers = checkContainerStatus()
    svcErrors = []
    svcErroredContainers = []
    svcAccountedFor = {}
    svcGood = True
    for svcName in CONFIG["componentsWatchList"]: svcAccountedFor[svcName] = False
    anomaly = "" # abnormal state that is not considered "crashed" yet
    for svc in containers:
      if svc["STATUS"].startswith("Up "): # if service up and good, check true
        svcAccountedFor[svc["NAMES"]] = True
      if svc["NAMES"] == "agent": # agent might be special case, forgive for n seconds
        if agentDown == 0:
          if not svc["STATUS"].startswith("Up "):
            log.warning("Agent container is not up")
            anomaly = "Agent container is not up"
            svcErrors.append({"n":svc["NAMES"], "s":svc["STATUS"]})
            svcErroredContainers.append(svc["NAMES"])
            agentDown = int(time.time())
        else:
          if svc["STATUS"].startswith("Up "): # agent has come back
            agentDown = 0
          else:
            anomaly = "Agent container is not up"
            svcErrors.append({"n":svc["NAMES"], "s":svc["STATUS"]})
            svcErroredContainers.append(svc["NAMES"])
            if int(time.time()) - agentDown >= AGENT_DOWN_MAX_FORGIVE: # agent hasn't come back in time
              svcGood = False; markNodeCrashedIfNotAlready()
      elif not svc["STATUS"].startswith("Up "): # other containers up?
        svcErrors.append({"n":svc["NAMES"], "s":svc["STATUS"]})
        svcErroredContainers.append(svc["NAMES"])
        svcGood = False; markNodeCrashedIfNotAlready()
    
    # if any one of watched services are faulty, notify with crash condition
    for svcName in svcAccountedFor:
      if not svcAccountedFor[svcName]:
        svcErrors.append({"n":svcName, "s":"not_found"})
        svcErroredContainers.append(svcName)
        svcGood = False; markNodeCrashedIfNotAlready()
    
    # only emit `thisIsThePointOfFailure` if services were good before, but turned bad.
    thisIsThePointOfFailure = False
    if statusWasOk and not svcGood:
      statusWasOk = False
      thisIsThePointOfFailure = True
      crashTime = int(time.time())
      wavefrontPublish(f'bc.replica.crash 1 {crashTime}', 'bc.crashed="true" bc.crashed_components="{}"'.format(','.join(svcErroredContainers)))

    # This docker native call takes 2 seconds to poll resource usage polling (compare values from prev snapshot)
    # All footprints are container specific.
    containersFootprints = getContainerFootprint(prevContainersFootprints, prevContainersFootprintsTimeMS)
    prevContainersFootprintsTimeMS = int(round(time.time() * 1000)) # in ms
    prevContainersFootprints = containersFootprints
    trimmedContainersFootprints = trimFootprints(containersFootprints)

    now = int(time.time())

    # verbose format for reporting to a highly active colleciton endpoint
    reportData = {
      "t" : now,
      "ip" : CONFIG["ip"],
      "buildId" : CONFIG["buildId"],
      "uuid" : CONFIG["uuid"],
      "cpu" : cpuInfo,
      "mem" : float3(memInfo["percent"]),
      "disk" : float3(diskInfo["percent"]),
      "status" : "ok" if svcGood else "bad",
      "anomaly" : anomaly,
      "footprints": trimmedContainersFootprints
    }
    if thisIsThePointOfFailure: reportData["thisIsThePointOfFailure"] = True

    # More concise format for local log dump
    loggedData = {
      "cpu" : reportData["cpu"],
      "mem" : reportData["mem"],
      "disk" : reportData["disk"],
      "status" : reportData["status"],
      "footprints": trimmedContainersFootprints
    }
    if thisIsThePointOfFailure: loggedData["thisIsThePointOfFailure"] = True

    # Announce relatively static information once in a while (reports when daemon starts, and interval'ed)
    if now - lastAnnounced >= CONFIG["announceInterval"]:
      daemonLogSize = int(os.path.getsize(CONFIG["logFile"]) / 1024.0) # in KB
      daemonLogRate = float(prevDaemonLogSize - daemonLogSize) / float(now - lastAnnounced)
      if daemonLogSize > 30 * 1024: truncateDaemonLogs() # truncate when more than 30 MB; ~ 5-days-worth (20s interval)
      daemonProcessUsage = getDaemonProcessUsage()
      announceData = {
        "log_size": daemonLogSize, # log output size caused by deamon 
        "log_rate": daemonLogRate if lastAnnounced > 0 else 0, # this daemon log output rate (kB/s)
        "daemon": daemonProcessUsage,
        "system": {
          "mem_total_GB": float3(memInfo["total"] / 1024), # RAM total capacity of VM in GB
          "disk_total_GB": diskInfo["total"], # disk total capacity of VM in GB
          "cpuinfo": parseCPUInfo(), # is it AMD? Intel? Ghz? Available CPU instruction sets?
        },
        "containers": containers, # container information (docker ps) in JSON
      }
      loggedData["announce"] = announceData
      reportData["announce"] = announceData
      prevDaemonLogSize = daemonLogSize
      lastAnnounced = now

    # Output metric to log file
    log.info(json.dumps(loggedData))
    
    # Wavefront publish
    if CONFIG["wavefrontUrl"]:
      # publish VM footprint
      good = 1 if reportData["status"] == "ok" else 0
      vmCPU = reportData["cpu"]["avg"]
      vmMEM = reportData["mem"]
      vmDISK = reportData["disk"]
      wavefrontPublish(f'bc.healthd.good {good} {now}', f'bc.service="VM"')
      wavefrontPublish(f'bc.healthd.cpu {vmCPU} {now}', f'bc.service="VM"')
      wavefrontPublish(f'bc.healthd.mem {vmMEM} {now}', f'bc.service="VM"')
      wavefrontPublish(f'bc.healthd.disk {vmDISK} {now}', f'bc.service="VM"')
      for i, coreCPU in enumerate(reportData["cpu"]["core"]):
        wavefrontPublish(f'bc.healthd.cpu.{i} {coreCPU} {now}', f'bc.service="VM"')
      # publish footprint of each container
      for footprint in trimmedContainersFootprints:
        svcName = footprint["name"]
        cpu = footprint["cpu"]
        mem = footprint["mem"]
        log_total = footprint["log"]["total"]
        log_rate = footprint["log"]["rate"]
        net_in_total = footprint["net"]["in"]["total"]
        net_in_rate = footprint["net"]["in"]["rate"]
        net_out_total = footprint["net"]["out"]["total"]
        net_out_rate = footprint["net"]["out"]["rate"]
        disk_read_total = footprint["disk"]["read"]["total"]
        disk_read_rate = footprint["disk"]["read"]["rate"]
        disk_write_total = footprint["disk"]["write"]["total"]
        disk_write_rate = footprint["disk"]["write"]["rate"]
        wavefrontPublish(f'bc.healthd.cpu {cpu} {now}', f'bc.service="{svcName}"')
        wavefrontPublish(f'bc.healthd.mem {mem} {now}', f'bc.service="{svcName}"')
        wavefrontPublish(f'bc.healthd.log {log_total} {now}', f'bc.service="{svcName}"')
        wavefrontPublish(f'bc.healthd.log_rate {log_rate} {now}', f'bc.service="{svcName}"')
        wavefrontPublish(f'bc.healthd.net_in {net_in_total} {now}', f'bc.service="{svcName}"')
        wavefrontPublish(f'bc.healthd.net_in_rate {net_in_rate} {now}', f'bc.service="{svcName}"')
        wavefrontPublish(f'bc.healthd.net_out {net_out_total} {now}', f'bc.service="{svcName}"')
        wavefrontPublish(f'bc.healthd.net_out_rate {net_out_rate} {now}', f'bc.service="{svcName}"')
        wavefrontPublish(f'bc.healthd.disk_read {disk_read_total} {now}', f'bc.service="{svcName}"')
        wavefrontPublish(f'bc.healthd.disk_read_rate {disk_read_rate} {now}', f'bc.service="{svcName}"')
        wavefrontPublish(f'bc.healthd.disk_write {disk_write_total} {now}', f'bc.service="{svcName}"')
        wavefrontPublish(f'bc.healthd.disk_write_rate {disk_write_rate} {now}', f'bc.service="{svcName}"')

    # Update check iteration count
    counter += 1

    # Flush Wavefront publishes in this iteration in a single POST call
    wavefrontPublishFlush()

    # Let outside procs know healthd is alive and well
    try:
      f = open('/var/log/healthd_last_seen', 'w+')
      f.write(str(int(time.time())))
      f.close()
    except Exception as e:
      log.info(e)





# Float at most 3 significant digits
def float3(s, flattenNearZero=False):
  n = float(format(float(s), '.3g'))
  if flattenNearZero and n < 0.01 and n >= 0: n = 0
  return n

# Convert to KB unit value
def toKB(s):
  n = float(re.findall('\d+', s)[0])
  if 'M' in s: return n * 1024
  if 'k' in s or 'K' in s: return n
  return n / 1024

# truncate logs by simple rotation
def truncateDaemonLogs():
  logPath = CONFIG["logFile"]
  archivePath = CONFIG["logFile"] + '_old' # e.g. /var/log/healthd.log_old
  subprocess.run(["cp", logPath, archivePath]) # overwrite on archive with current
  with open(logPath, 'w') as f: pass # truncate current

# Mark this node as crashed if haven't already
def markNodeCrashedIfNotAlready():
  if CONFIG["crashAnnounced"]: return
  if not os.path.exists(CONFIG["crashReportFile"]):
    try:
      f = open(CONFIG["crashReportFile"], 'w+')
      f.write(str(int(time.time()))) # mark first crash timestamp in file
      f.close()
      CONFIG["crashAnnounced"] = True
    except Exception as e:
      log.info(e)
    





if __name__ == "__main__":
  main()