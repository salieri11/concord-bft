#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import os
import json
import logging
import traceback
import time
import threading
from . import helper, vsphere
log = helper.hermes_logging_util.getMainLogger()

# Node type pretty name print for annotation
PRETTY_TYPE_COMMITTER = "Committer" # eth/daml regular nodes are committers
PRETTY_TYPE_PARTICIPANT = "Participant" # applicable to daml only, for now
PRETTY_TYPE_REPLICA = "Replica"

# all open infra sessions for accessing VMs & Folders on the datacenter connection
# e.g. vm = INFRA["SDDC3"].getByIP("10.69.100.46")
#     populated by `getConnection` (ConnectionToSDDC objects defined in vsphere.py)
INFRA = {}

# list of all deployed replicas by Hermes from this particular build
#     auto populated by `giveDeploymentContext` with replicaInfo
DEPLOYED_REPLICAS = []


def credentialsAreGood(sddcName, sddcInfo):
   c = sddcInfo
   if not c["username"] or not c["password"]: 
      log.error("Target 'vSphere/{}' is not well-defined in user_config.json".format(sddcName))
      return False
   if c["username"].startswith("<") and c["username"].endswith(">"): # user_config not injected correctly with credential
      log.error("vSphere/{}: username credential is not injected (user_config.json)".format(sddcName))
      return False
   if c["password"].startswith("<") and c["password"].endswith(">"): # user_config not injected correctly with credential
      log.error("vSphere/{}: password credential is not injected (user_config.json)".format(sddcName))
      return False
   return True


def getConnection(sddcName, skipMapping=False):
   '''
      Initializes vSphere connection to the target SDDC.
      Host and Credentials are fed from `zone_config.json`, for example: {
        ...
        "infra":{
          "SDDC3":{
            "name": "SDDC3",
            "type": "vSphere",
            "orgId": "c56e116e-c36f-4f7d-b504-f9a33955b853",
            "sddcId": "6db19f8f-cde6-4151-88e5-a3b0d6aead6a",
            "publicIP": "35.156.16.204",
            "username": "<VMC_SDDC3_VC_CREDENTIALS_USERNAME>",
            "password": "<VMC_SDDC3_VC_CREDENTIALS_PASSWORD>",
            "vmcToken": "<VMC_API_TOKEN>",
            "active": true
          }
          ...
        }
      }
      where Jenkins-kept credentials brought from `withCredentials` call,
      which get injected to `user_config.json` in `gitlabBuildSteps.groovy` 
   '''
   # get config from user_config.json
   zoneConfigObject = helper.getZoneConfig()
   try:
      if sddcName not in INFRA:
         if sddcName not in zoneConfigObject["infra"]:
           log.debug("Cannot open session to {}, no vSphere credential in config object.".format(sddcName))
           return None
         sddcInfo = zoneConfigObject["infra"][sddcName]
         if not sddcInfo["active"]:
           log.debug("Cannot open session to {}, this SDDC is inactive".format(sddcName))
           return None
         if not credentialsAreGood(sddcName, sddcInfo):
           log.debug("Cannot open session to {}, credentials are bad".format(sddcName))
           return None
         conn = vsphere.ConnectionToSDDC(sddcInfo, skipMapping=skipMapping)
         if not conn.ready:
           log.debug("Cannot open session to {}, connection is not ready".format(sddcName))
           return None
         INFRA[sddcName] = conn
         return conn
      return INFRA[sddcName]

   except Exception as e:
     log.debug(str(e))
     return None


def prepareConnections(sddcs):
  if not sddcs: return
  threads = []
  def connect(sddcName):
    getConnection(sddcName, skipMapping=True)
  for sddcName in sddcs:
    if sddcName in INFRA: continue
    thr = threading.Thread(
        target = lambda sddcName: connect(sddcName),
        args = (sddcName, )
    )
    threads.append(thr); thr.start()
  for thd in threads: thd.join(timeout=15) # wait for all to return
  initializerThreads = []
  for sddcName in sddcs:
    if not getConnection(sddcName).entitiesMapped:
      getConnection(sddcName).updateAllEntityHandles(initialFetch=True)


def getListFromZoneConfig(configObject = None):
  '''
      Returns sddcNumber list from config object
      (List of all SDDCs affected by Hermes testing)
      :param configObject: (optional), if not given, auto-resolved by loadConfigFile
  '''
  if configObject is None:
    configObject = helper.getZoneConfig()
  sddcs = []
  # Can be vSphere SDDC or other on-prem locations
  #  e.g. Other on-prem location name with any string val
  for sddcName in configObject["infra"]:
    sddcs.append(sddcName)
  return sddcs


def findVMByReplicaId(replicaId, sddcs = None):
  '''
      Returns VM by the supplied replicaId
      :param sddc: (optional), if not given, all SDDCs defined in zone_config.json used
  '''
  # if narrow sddcs search not given, search in all SDDCs in zone_config
  sddcs = sddcs if sddcs is not None else getListFromZoneConfig()
  prepareConnections(sddcs)
  threads = []; results = []
  def findInSDDC(sddcName):
    if getConnection(sddcName):
      vmHandle = INFRA[sddcName].getByNameContaining(replicaId, getAsHandle=True)
      if vmHandle: results.append(vmHandle)
      else: results.append(None)
  for sddcName in sddcs:
    thr = threading.Thread(target = lambda sddcName: findInSDDC(sddcName), args = (sddcName, ))
    threads.append(thr); thr.start()
  for thd in threads: thd.join() # wait for all to return
  for result in results:
    if result: return result # only 1 unique replica id possible for all SDDCs
  log.debug("Cannot find vm with its name containing '{}' in datacenters [{}]".format(replicaId, ', '.join(sddcs)))
  return None


def findVMByInternalIP(ip, sddcs = None):
  '''
      Returns VM by the supplied internal IP (e.g. 10.*.*.*)
      :param sddc: (optional), if not given, all SDDCs defined in zone_config.json used
  '''
  # if narrow sddcs search not given, search in all SDDCs in zone_config
  sddcs = sddcs if sddcs is not None else getListFromZoneConfig()
  prepareConnections(sddcs)
  threads = []; results = []
  def findInSDDC(sddcName):
    if getConnection(sddcName):
      vmHandle = INFRA[sddcName].getByInternalIP(ip, getAsHandle=True)
      if vmHandle: results.append(vmHandle)
      else: results.append(None)
  for sddcName in sddcs:
    thr = threading.Thread(target = lambda sddcName: findInSDDC(sddcName), args = (sddcName, ))
    threads.append(thr); thr.start()
  for thd in threads: thd.join() # wait for all to return
  for result in results:
    if result: return result # result vmHandle
  log.info("Cannot find vm with internal IP '{}' in datacenters {}".format(ip, sddcs))
  return None


def giveDeploymentContext(blockchainDetails, otherMetadata=""):
   '''
      Add detailed deployment context to the Hermes-deployed VMs
      ```python
      blockchainDetails = {
          "id": "c035100f-22e9-4596-b9d6-5daa349db342",
          "consortium_id": "bfaa0041-8ab2-4072-9023-4cedd0e81a78",
          "blockchain_type": "ETHEREUM",
          "nodes_type": "Committer" | "Participant",
          "node_list": [ ... ], # `NOT USED`
          "replica_list": [{
            "ip": "52.63.165.178",
            "url": "https://52.63.165.178:8545", # `NOT USED`
            "cert": "", # `NOT USED`
            "zone_id": "6adaf48a-9075-4e35-9a71-4ef1fb4ac90f", # `NOT USED`
            "replica_id": "a193f7b8-6ec5-4802-8c2f-33cb33516c3c"
          }, ...]
      }
      ```
      TODO: user this on persephone deployment test as well
   '''
   # get config from zone_config.json
   try: 
      configObject = helper.getUserConfig()
      sddcs = getListFromZoneConfig()
      jobName = configObject["metainf"]["env"]["jobName"]
      buildNumber = configObject["metainf"]["env"]["buildNumber"]
      jenkinsBuildId = helper.getJenkinsBuildId()
      dockerTag = configObject["metainf"]["env"]["dockerTag"]
      pytestContext = os.getenv("PYTEST_CURRENT_TEST") if os.getenv("PYTEST_CURRENT_TEST") is not None else ""
      runCommand = os.getenv("SUDO_COMMAND") if os.getenv("SUDO_COMMAND") is not None else ""

      for replicaInfo in blockchainDetails["replica_list"]:
        alreadyRegistered = [replica for replica in DEPLOYED_REPLICAS if replica.get("replica_id") == replicaInfo["replica_id"]]
        if len(alreadyRegistered) > 0: continue # this replica is already registered to DEPLOYED_REPLICAS
        DEPLOYED_REPLICAS.append(replicaInfo)

        vmAndSourceSDDC = findVMByReplicaId(
          replicaId = replicaInfo["replica_id"],
          sddcs = sddcs
          # above "sddcs": Perhaps, this can be abtracted later to "datacenters" and also support AWS/Azure/GCP/etc
          # It would be interesting to test concord with various mixed cloud environment set-up.
          # They all have their own version of inventory system with: instance notes, descriptions and/or tags, etc.
        )
        if vmAndSourceSDDC is None: continue # vm with the given replicaId is not found
        vmHandle = vmAndSourceSDDC["vmHandle"]
        if "realm" in vmHandle["attrMap"]: # already has context given (not possible for fresh deployment)
          log.error("VM ({}) has deployment context annotations already\n".format(replicaInfo["ip"]))
          log.error("This is a sign of a previous VM clean-up failure while IPAM thinks this IP ({}) is released for use."
                      .foramt(replicaInfo["ip"]))
          continue
        vm = vmHandle["entity"]
        sddc = vmAndSourceSDDC["sddc"]
        ipType = "Private" if replicaInfo["ip"].startswith("10.") else "Public"
        notes = "{} IP: {}\nReplica ID: {}\nBlockchain: {}\nConsortium: {}\nNetwork Type: {}\nNode Type: {}\n".format(
                ipType,
                replicaInfo["ip"],
                replicaInfo["replica_id"],
                blockchainDetails["id"],
                blockchainDetails["consortium_id"],
                blockchainDetails["blockchain_type"],
                blockchainDetails["nodes_type"]
              ) + "\nDeployed By: Hermes\nJob Name: {}\nBuild Number: {}\nDocker Tag: {}\n".format(
                # Below 3 attributes: if run locally, user_config will have default "<VAR_NAMES>", in this case use "None"
                jobName if not jobName.startswith("<") else "None",
                buildNumber if not buildNumber.startswith("<") else "None",
                dockerTag if not dockerTag.startswith("<") else "None"
              ) + "\nPytest Context: {}\n\nRun Command: {}\n\nOther Metadata: {}\n\n".format(
                pytestContext,
                runCommand,
                otherMetadata
              )
        log.info("Annotating VM ({}) for better tracking & life-cycle management...".format(replicaInfo["ip"]))
        # edit VM Notes with detailed deployment context
        sddc.vmAnnotate(vm, notes)
        # Add custom attributes
        sddc.vmSetCustomAttribute(vm, "up_since", str(int(time.time()))) # UNIX timestamp in seconds
        sddc.vmSetCustomAttribute(vm, "realm", "testing")
        # below 3 attributes: if run locally, user_config will have default "<VAR_NAME>", in this case use ""
        sddc.vmSetCustomAttribute(vm, "docker_tag", dockerTag if not dockerTag.startswith("<") else "")
        sddc.vmSetCustomAttribute(vm, "jenkins_build_id", jenkinsBuildId if not jenkinsBuildId.startswith("<") else "")
        sddc.vmSetCustomAttribute(vm, "job_name", jobName if not jobName.startswith("<") else "")
        sddc.vmSetCustomAttribute(vm, "replica_id", replicaInfo["replica_id"])
        sddc.vmSetCustomAttribute(vm, "blockchain_id", blockchainDetails["id"])
        sddc.vmSetCustomAttribute(vm, "consortium_id", blockchainDetails["consortium_id"])
        sddc.vmSetCustomAttribute(vm, "blockchain_type", blockchainDetails["blockchain_type"])
        sddc.vmSetCustomAttribute(vm, "node_type", blockchainDetails["nodes_type"].lower())
        sddc.vmSetCustomAttribute(vm, "other_metadata", otherMetadata)
        if ipType == "Public":
          sddc.vmSetCustomAttribute(vm, "public_ip", replicaInfo["ip"])
        else:
          sddc.vmSetCustomAttribute(vm, "private_ip", replicaInfo["ip"])

   except Exception as e:
     helper.hermesNonCriticalTrace(e)


def getVMsByAttribute(attrName, matchValue, mapBySDDC=False):
  '''
      Returns list of VMs (or map if mapBySDDC set to True)
      That satisfies the supplied attribute value condition
  '''
  sddcs = getListFromZoneConfig()
  prepareConnections(sddcs)
  vms = []
  resultMap = {}
  for sddcName in sddcs:
    if getConnection(sddcName):
      vmHandles = INFRA[sddcName].vmFilterByAttributeValue(attrName, matchValue, getAsHandle=True)
      if mapBySDDC:
        resultMap[sddcName] = vmHandles
      else:
        for vmHandle in vmHandles:
          vms.append(vmHandle)
  return resultMap if mapBySDDC else vms


def fetch_vm_handles(ips):
   '''
   Fetch vm handles for the supplied IPs
   :param ips: list of ips
   :return: vm handles
   '''
   log.info("Fetching vm handles...")
   vm_handles = {}
   for ip in ips:
      vm_handle = findVMByInternalIP(ip)
      if vm_handle:
         vm_handles[ip] = vm_handle
      else:
         log.warning("Unable to fetch VM handle for IP: {}".format(ip))
         vm_handles[ip] = None

   return vm_handles

