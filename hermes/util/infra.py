#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import os
import json
import logging
import traceback
import time
import threading
import enum
from . import helper, vsphere
log = helper.hermes_logging_util.getMainLogger()

class INVENTORY_ERRORS(enum.Enum):
    IP_CONFLICT = "IP_CONFLICT"
    NAME_CONFLICT = "NAME_CONFLICT"

# Node type pretty name print for annotation
PRETTY_TYPE_COMMITTER = "Committer" # eth/daml regular nodes are committers
PRETTY_TYPE_PARTICIPANT = "Participant" # applicable to daml only, for now
PRETTY_TYPE_REPLICA = "Replica"

# all open infra sessions for accessing VMs & Folders on the datacenter connection
# e.g. vm = INFRA["SDDC3"].getByIP("10.69.100.46")
#     populated by `getConnection` (ConnectionToSDDC objects defined in vsphere.py)
INFRA = {}

# list of all deployed replicas by Hermes from this particular build
# auto populated by `giveDeploymentContext` with replicaInfo
DEPLOYED_REPLICAS = []


def credentialsAreGood(sddcName, sddcInfo):
    c = sddcInfo
    if not c["username"] or not c["password"]: 
        log.debug("Target 'vSphere/{}' is not well-defined in user_config.json".format(sddcName))
        return False
    if c["username"].startswith("<") and c["username"].endswith(">"):
        log.debug("vSphere/{}: username credential"
                  "is not injected (user_config.json)".format(sddcName))
        return False
    if c["password"].startswith("<") and c["password"].endswith(">"):
        log.debug("vSphere/{}: password credential "
                  "is not injected (user_config.json)".format(sddcName))
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
      which get injected to `zone_config.json` in `gitlabBuildSteps.groovy` 
   '''
   # get SDDCs list and config from zone_config.json
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


def findVMByReplicaId(replicaId, sddcs=None, checkNew=False):
  '''
      Returns VM by the supplied replicaId (partial string match to VM name)
      :param sddc: (optional), if not given, all SDDCs defined in zone_config.json used
  '''
  # if narrow sddcs search not given, search in all SDDCs in zone_config
  sddcs = sddcs if sddcs is not None else getListFromZoneConfig()
  prepareConnections(sddcs)
  threads = []; results = []
  def findInSDDC(sddcName):
    if getConnection(sddcName):
      if checkNew: INFRA[sddcName].checkForNewEntities()
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


def findVMByInternalIP(ip, sddcs=None, checkNew=False):
  '''
      Returns VM by the supplied internal IP (e.g. 10.*.*.*)
      (Note: if DHCP or static-IP is not set, the VM won't have an internal IP attached.)
      :param sddc: (optional), if not given, all SDDCs defined in zone_config.json used
  '''
  # if narrow sddcs search not given, search in all SDDCs in zone_config
  sddcs = sddcs if sddcs is not None else getListFromZoneConfig()
  prepareConnections(sddcs)
  threads = []; results = []
  def findInSDDC(sddcName):
    if getConnection(sddcName):
      if checkNew: INFRA[sddcName].checkForNewEntities()
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


def giveDeploymentContext(blockchainFullDetails, otherMetadata="", sddcs=None):
    '''
      Add detailed deployment context to the Hermes-deployed VMs
      ```python
      e.g. blockchainFullDetails = {
          "id": "c035100f-22e9-4596-b9d6-5daa349db342",
          "consortium_id": "bfaa0041-8ab2-4072-9023-4cedd0e81a78",
          "blockchain_type": "ETHEREUM",
          "deployed_from": "Helen",
          "nodes_list": [{
              "ip": "52.63.165.178",
              "replica_id": "a193f7b8-6ec5-4802-8c2f-33cb33516c3c",
              ...
            }, ...],
          
          # (optional info; will be added only if they exist)
          "version": "Blockchain Version: 0.0.0.1635, DAML SDK Version: 1.0.0", # (optional)
          "created_by": "vmbc_test_con_admin@csp.local", # (optional)
          "created": 1593749320175, # (optional)
      }
      ```
    '''
    # get config from zone_config.json
    fatalErrors = []
    try: 
      # if narrow sddcs search not given, search in all SDDCs in zone_config
      sddcs = sddcs if sddcs is not None else getListFromZoneConfig()
      configObject = helper.getUserConfig()
      jobName = configObject["metainf"]["env"]["jobName"]
      buildNumber = configObject["metainf"]["env"]["buildNumber"]
      jenkinsBuildId = helper.getJenkinsBuildId()
      dockerTag = configObject["metainf"]["env"]["dockerTag"]
      pytestContext = os.getenv("PYTEST_CURRENT_TEST") if os.getenv("PYTEST_CURRENT_TEST") is not None else ""
      runCommand = os.getenv("SUDO_COMMAND") if os.getenv("SUDO_COMMAND") is not None else ""
      DEPLOYED_REPLICAS.append(blockchainFullDetails)
      
      prepareConnections(sddcs) # connect to applicable SDDCs if not already connected
      for sddcName in sddcs: INFRA[sddcName].checkForNewEntities()
      
      for replicaInfo in blockchainFullDetails["nodes_list"]:
        try:   
          isParticipant = False
          if "type_name" in replicaInfo: # must be from getBlockchainFullDetails function
            isParticipant = replicaInfo["type_name"] == helper.TYPE_DAML_PARTICIPANT
          else: # from new persephone test, v1 or v2
            if "node_type" in replicaInfo:
              if isinstance(replicaInfo["node_type"], int):
                # 0 committer, 1 participant, from NodeInfo class in persephone_tests_new.py (BC-3529)
                isParticipant = True if replicaInfo["node_type"] == 1 else False
          replicaTypeDisplayName = PRETTY_TYPE_COMMITTER if not isParticipant else PRETTY_TYPE_PARTICIPANT
          if "id" not in replicaInfo: # from new persephone test
            if "node_id" in replicaInfo: replicaInfo["id"] = replicaInfo["node_id"]
          versionInfo = "Version Info: " + blockchainFullDetails["version"] + "\n" if "version" in blockchainFullDetails else ""
          createdBy = "Deployer: " + blockchainFullDetails["created_by"] + "\n" if "created_by" in blockchainFullDetails else ""

          vmHandle = findVMByReplicaId(replicaId = replicaInfo["id"], sddcs = sddcs)
          if not vmHandle: continue # vm with the given replicaId is not found
          if "realm" in vmHandle["attrMap"]: # already has context given? (not possible for fresh deployment)
            log.error("VM ({}) has deployment context annotations already. This should not be possible with fresh deployment."
                        .format(replicaInfo["id"]))
            fatalErrors.append({"type": INVENTORY_ERRORS.NAME_CONFLICT, 
                              "node": replicaInfo, "occupant": vmHandle})
            continue
          vm = vmHandle["entity"]
          sddc = vmHandle["sddc"]
          sddcName = vmHandle["sddcName"]
          ipInfo = ""
          if "private_ip" in replicaInfo and replicaInfo["private_ip"]: # VM should ALWAYS have this
            ipInfo += replicaInfo["private_ip"] + " (Private)"
            handlesFound = sddc.getByInternalIP(replicaInfo["private_ip"], getAsHandle=True, getFullList=True)
            conflictHandle = None
            if handlesFound:
              for handleFound in handlesFound:
                if vmHandle["uid"] != handleFound["uid"]: conflictHandle = handleFound
              if conflictHandle:
                replicaInfo["ip"] = replicaInfo["private_ip"]
                fatalErrors.append({"type": INVENTORY_ERRORS.IP_CONFLICT, "node": replicaInfo, "occupant": conflictHandle})
          if "public_ip" in replicaInfo and replicaInfo["public_ip"]: # Cloud deployment with public IP
            ipInfo += ", " + replicaInfo["public_ip"] + " (Public)"; anyIP = replicaInfo["public_ip"]
            handlesFound = getVMsByAttribute("public_ip", replicaInfo["public_ip"])
            conflictHandle = None
            if handlesFound:
              for handleFound in handlesFound:
                if vmHandle["uid"] != handleFound["uid"]: conflictHandle = handleFound
              if conflictHandle:
                replicaInfo["ip"] = replicaInfo["public_ip"]
                fatalErrors.append({"type": INVENTORY_ERRORS.IP_CONFLICT, "node": replicaInfo, "occupant": conflictHandle})
          notes = ("Location: {}\nIP: {}\nPassword: {}\nReplica ID: {}\nBlockchain: {}"
                    + "\nConsortium: {}\nNetwork Type: {}\nNode Type: {}\n{}{}").format(
                  sddcName,
                  ipInfo,
                  replicaInfo["password"] if "password" in replicaInfo else "(Unknown)",
                  replicaInfo["id"],
                  blockchainFullDetails["id"],
                  blockchainFullDetails["consortium_id"],
                  blockchainFullDetails["blockchain_type"].upper(),
                  replicaTypeDisplayName,
                  versionInfo, # (optional prop, concord & exec engine version)
                  createdBy, # (optional prop, email of the csp account who created the blockchain)
                ) + "\nDeployed By: Hermes ({})\nJob Name: {}\nBuild Number: {}\nDocker Tag: {}\n".format(
                  # Below 3 attributes: if run locally, user_config will have default "<VAR_NAMES>", in this case use "None"
                  "through " + blockchainFullDetails["deployed_from"],
                  jobName if not jobName.startswith("<") else "None",
                  buildNumber if not buildNumber.startswith("<") else "None",
                  dockerTag if not dockerTag.startswith("<") else "None"
                ) + "\nPytest Context: {}\n\nRun Command: {}\n\nOther Metadata: {}\n\n".format(
                  pytestContext,
                  runCommand,
                  otherMetadata
                )
          log.info("Annotating VM ({}) for better tracking & life-cycle management... (location: {})"
                    .format(replicaInfo["private_ip"], sddcName))
          # edit VM Notes with detailed deployment context
          sddc.vmAnnotate(vm, notes)
          # Add custom attributes
          sddc.vmSetCustomAttribute(vm, "up_since", str(int(time.time()))) # UNIX timestamp in seconds
          sddc.vmSetCustomAttribute(vm, "realm", "testing")
          # below 3 attributes: if run locally, user_config will have default "<VAR_NAME>", in this case use ""
          sddc.vmSetCustomAttribute(vm, "docker_tag", dockerTag if not dockerTag.startswith("<") else "")
          sddc.vmSetCustomAttribute(vm, "jenkins_build_id", jenkinsBuildId if not jenkinsBuildId.startswith("<") else "")
          sddc.vmSetCustomAttribute(vm, "job_name", jobName if not jobName.startswith("<") else "")
          sddc.vmSetCustomAttribute(vm, "replica_id", replicaInfo["id"])
          sddc.vmSetCustomAttribute(vm, "blockchain_id", blockchainFullDetails["id"])
          sddc.vmSetCustomAttribute(vm, "consortium_id", blockchainFullDetails["consortium_id"])
          sddc.vmSetCustomAttribute(vm, "blockchain_type", blockchainFullDetails["blockchain_type"])
          sddc.vmSetCustomAttribute(vm, "node_type", replicaTypeDisplayName.lower())
          sddc.vmSetCustomAttribute(vm, "other_metadata", otherMetadata)
          if "private_ip" in replicaInfo and replicaInfo["private_ip"]:
            sddc.vmSetCustomAttribute(vm, "private_ip", replicaInfo["private_ip"])
          if "public_ip" in replicaInfo and replicaInfo["public_ip"]:
            sddc.vmSetCustomAttribute(vm, "public_ip", replicaInfo["public_ip"])
        except Exception as e:
          helper.hermesNonCriticalTrace(e)
    except Exception as e:
        helper.hermesNonCriticalTrace(e)
    return fatalErrors


def getVMsByAttribute(attrName, matchValue, matchExactly=True, mapBySDDC=False):
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
      vmHandles = INFRA[sddcName].vmFilterByAttributeValue(
        attrName, matchValue,
        matchExactly=matchExactly,
        getAsHandle=True
      )
      if mapBySDDC:
        resultMap[sddcName] = vmHandles
      else:
        for vmHandle in vmHandles:
          vms.append(vmHandle)
  return resultMap if mapBySDDC else vms


def save_fatal_errors_to_summary(fatal_errors):
    if fatal_errors:
        for current_error in fatal_errors:
            node_data = current_error["node"]; occupant_handle = current_error["occupant"]
            please_contact_msg = "Please contract concierge about this as soon as possible; this is a serious issue."
            if current_error["type"] == INVENTORY_ERRORS.NAME_CONFLICT:
                msg = "FATAL !!  Node ID: '{}' already occupied by {} on {}. {}.".format( 
                        node_data["id"], occupant_handle["uid"], occupant_handle["sddcName"], please_contact_msg)
                raise Exception(msg) # Raise Name Conflict
            elif current_error["type"] == INVENTORY_ERRORS.IP_CONFLICT:
                msg = "FATAL !!  Node IP: '{}' already occupied by {} on {}. {}.".format(
                        node_data["id"], occupant_handle["uid"], occupant_handle["sddcName"], please_contact_msg)
                log.error(msg)
                raise Exception(msg) # Raise IP Conflict


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

