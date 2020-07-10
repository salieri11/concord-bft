#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

from pyVim.connect import SmartConnect, Disconnect
from pyVmomi import vim

import atexit
import ssl
import threading
import time
import requests
import traceback

import logging

import util.hermes_logging
log = util.hermes_logging.getMainLogger()

REQ_SESSION = requests.Session()
CSP_SESSION = { "active": False }

class ConnectionToSDDC:
  
  def __init__(self, sddcInfo, skipMapping=False):
    '''
        Connect to vSphere given host and credentials.
        After establishing connection successfully, fetches and maps all
        VMs and Folders in the SDDC by name for easy look-up.
        (Establishing connection takes about 2 ~ 5 seconds)

        Using the library, `pyvmomi`, which allows access to most functionalities
        of vSphere running on the SDDC (about 80% feature coverage; e.g. attaching
        tags and categories are missing in the lib, and must be done with vapi.cis)
    '''
    self.sddcName = sddcInfo["name"]
    self.publicIP = sddcInfo["publicIP"]; publicIPHyphen = self.publicIP.replace('.', '-')
    self.hostnameVCenter = "vcenter.sddc-{}.vmwarevmc.com".format(publicIPHyphen)
    self.hostnameNSX = "nsx-{}.rp.vmwarevmc.com".format(publicIPHyphen)
    self.reqSession = requests.Session()
    self.orgId = sddcInfo["orgId"]
    self.sddcId = sddcInfo["sddcId"]
    self.baseNSXT = "https://{}/vmc/reverse-proxy/api/orgs/{}/sddcs/{}/sks-nsxt-manager".format(
      self.hostnameNSX, self.orgId, self.sddcId
    )
    self.headersNSXT = {}
    self.username = sddcInfo["username"]
    self.password = sddcInfo["password"]
    self.vmcToken = sddcInfo["vmcToken"] if not sddcInfo["vmcToken"].startswith("<") else ""
    self.ready = False
    self.entitiesMapped = False
    tryCount = 0
    maxRetries = 3
    while tryCount < maxRetries:
      try:
        tryCount += 1
        sslContext = None # skip cert checking
        if hasattr(ssl, '_create_unverified_context'): 
          sslContext = ssl._create_unverified_context()
        log.debug("Connecting to vSphere on {}...".format(self.sddcName))
        self.connection = SmartConnect(host = self.hostnameVCenter,
                                      user = self.username,
                                      pwd = self.password,
                                      sslContext = sslContext)
        if not self.connection:
          log.debug("Cannot connect to {} as {} ".format(self.hostnameVCenter, self.username))
          continue
        else:
          log.debug("Successfully connected to vSphere on {}".format(self.sddcName))
          self.content = self.connection.RetrieveContent() # main content obj of vSphere
          self.services = self.connection.RetrieveServiceContent() # all managers
          self.attrByName = {}
          self.attrByKey = {}
          if not skipMapping:
            self.allEntityHandlesByName = {} # by VM name
            self.allEntityHandlesByUID = {} # by VM id (e.g. vim.VirtualMachine:vm-28102)
            self.allEntityHandlesByAttribute = {} # by custom attritube and its value
            self.updateAllEntityHandles() # populate `allEntityHandlesByName` with all VMs & Folders in this SDDC
          self.ready = True
          break
      except Exception as e:
        traceback.print_exc()
        log.debug("Failed to initialize session to vSphere on {}".format(self.sddcName))
        log.debug(e)
    atexit.register(Disconnect, self.connection) # clean up connection    


  def getByName(self, name, getAsHandle=False):
    '''
        Returns VM or Folder that matches the name exactly
    '''
    if name in self.allEntityHandlesByName:
      if getAsHandle:
        return self.allEntityHandlesByName[name]
      else:
        return self.allEntityHandlesByName[name]["entity"]
    else:
      return None


  def getByNameContaining(self, substring, getAsHandle=False):
    '''
        Returns first VM or Folder that has substring in the name
        (mainly used for matching `replicaId`, which is the second
        part of the deployed VMs on the SDDC)
    '''
    for name in self.allEntityHandlesByName:
      if substring in name:
        if getAsHandle:
          return self.allEntityHandlesByName[name]
        else:
          return self.allEntityHandlesByName[name]["entity"]
    return None


  def getByInternalIP(self, ipAddress, getAsHandle=False):
    '''
        Returns a VM (vim.VirtualMachine) matching the supplied IP
    '''
    matchedList = self.content.searchIndex.FindAllByIp(ip=ipAddress, vmSearch=True)
    if len(matchedList) == 0: return None
    vm = matchedList[0]
    handle = self.vmRegisterIfNew(vm)
    if getAsHandle: return handle
    else: return vm
    

  def getEntityType(self, entity):
    if entity.__class__ is vim.VirtualMachine:
      return "VM"
    elif entity.__class__ is vim.Folder:
      return "Folder"
    else:
      return None
  

  def entityDefaultProperties(self, entity):
    t = self.getEntityType(entity)
    if t == "VM": return ["name", "customValue"]
    elif t == "Folder": return ["name"]
    return []


  def entityGetUID(self, entity):
    return str(entity).replace("'", "")

  
  def vmRegisterIfNew(self, vm):
    vmUID = self.entityGetUID(vm)
    if vmUID in self.allEntityHandlesByUID:
      return self.allEntityHandlesByUID[vmUID]
    try:
      props = self.entityDefaultProperties(vm)
      handle = { "entity": vm }
      for propname in props:
        if hasattr(vm, propname):
          handle[propname] = getattr(vm, propname)
      self.initializeHandle(handle)
      return handle
    except Exception as e:
      log.debug(e)
      return None


  def vmFilterByNames(self, names, matchExactly=True, getAsHandle=False):
    result = []
    for name in names:
      vm = None
      if matchExactly:
        vm = self.getByName(name, getAsHandle=getAsHandle)
      else:
        vm = self.getByNameContaining(name, getAsHandle=getAsHandle)
      if vm is not None:
        result.append(vm)
    return result


  def vmFilterByAttributeValue(self, attrName, attrValue, matchExactly=True, getAsHandle=False):
    result = []
    if attrName not in self.allEntityHandlesByAttribute: return []
    if matchExactly:
      if type(attrValue) in [list]:
        for attrValueN in attrValue:
          if attrValueN in self.allEntityHandlesByAttribute[attrName]:
            handles = self.allEntityHandlesByAttribute[attrName][attrValueN]
            for handle in handles:
              if getAsHandle:
                result.append(handle)
              else:
                result.append(handle["entity"])
      else:
        if attrValue not in self.allEntityHandlesByAttribute[attrName]:
          return []
        handles = self.allEntityHandlesByAttribute[attrName][attrValue]
        for handle in handles:
          if getAsHandle:
            result.append(handle)
          else:
            result.append(handle["entity"])
    else:
      for value in self.allEntityHandlesByAttribute[attrName]:
        if type(attrValue) in [list]:
          for attrValueN in attrValue:
            if attrValueN in value:
              for handle in self.allEntityHandlesByAttribute[attrName][value]:
                if getAsHandle:
                  result.append(handle)
                else:
                  result.append(handle["entity"])
        else:
          if attrValue in value:
            for handle in self.allEntityHandlesByAttribute[attrName][value]:
              if getAsHandle:
                result.append(handle)
              else:
                result.append(handle["entity"])
    return result


  def vmAnnotate(self, vm, noteContent):
    '''
        Changes `Notes` content on the supplied VM
    '''
    try:
      if self.getEntityType(vm) is not "VM":
        log.debug("Cannot annotate; supplied argument is not a VM")
        return False
      spec = vim.vm.ConfigSpec()
      spec.annotation = noteContent
      vm.ReconfigVM_Task(spec)
    except Exception as e:
      log.debug("Cannot annotate VM {}".format(vm.name))
      log.debug(e)


  def vmSetCustomAttribute(self, vm, name, value):
    '''
        Attaches a key-value custom attribute on the VM
        (If the field is unregistered in the SDDC, this will create one in global space)
        
        Note: 
        once set, there is no pyvmomi way to DELETE the attribute;
        you can only modify the value of the custom attribute
    '''
    try:
      if self.getEntityType(vm) is not "VM":
        log.debug("Cannot add custom value; supplied argument is not a VM")
        return False
      manager = self.services.customFieldsManager
      attr = self.findRegisteredAttributeByName(name)
      if not attr:
        try:
          attr = manager.AddCustomFieldDef(name = name, moType = vim.VirtualMachine)
          self.attrByName[name] = attr
        except Exception as e:
          log.debug(e)
          attr = self.findRegisteredAttributeByName(name)
      
      manager.SetField( entity = vm, key = attr.key, value = value)

    except Exception as e:
      log.debug("Cannot attach tag {}={} to VM ".format(name, value, vm))
      log.debug(e)
  

  def vmGetCustomAttribute(self, vm, name):
    '''
        Gets custom attribute applied to the supplied VM
    '''
    # find attr key registered in SDDC
    attr = self.findRegisteredAttributeByName(name)
    if attr is None:
      log.debug("Attribute by name '{}' is not found in this SDDC.".format(name))
      return None
    if self.entityGetUID(vm) in self.allEntityHandlesByUID:
      vmHandle = self.allEntityHandlesByUID[self.entityGetUID(vm)]
      if name in vmHandle["attrMap"]:
        return vmHandle["attrMap"][name]
      else:
        return None
    for attrOnVM in vm.customValue:
      if attr.key == attrOnVM.key:
        return attrOnVM.value
    return None


  def getAllNATRules(self):
    '''
      Get all NAT rules in this SDDC
    '''
    if not cspGetConnection(self.vmcToken, self.headersNSXT): return False
    response = REQ_SESSION.post(
      f"{self.baseNSXT}/policy/api/v1/search/querypipeline?page_size=1000&cursor=0&sort_by=sequence_number&sort_ascending=true",
      json = {
        "query_pipeline":[
          {"query": "resource_type:PolicyNATRule AND path:\/infra\/tier-1s\/cgw\/nat\/USER\/nat-rules* "}
        ],
      },
      headers = self.headersNSXT
    )
    return response.json()["results"]


  def getAllPublicIPs(self):
    '''
      Get all public IPs allocated in this SDDC
    '''
    if not cspGetConnection(self.vmcToken, self.headersNSXT): return False
    response = REQ_SESSION.get(
      f"{self.baseNSXT}/cloud-service/api/v1/infra/public-ips",
      headers = self.headersNSXT
    )
    return response.json()["results"]


  def findRegisteredAttributeByName(self, name):
    manager = self.services.customFieldsManager
    if name in self.attrByName:
      return self.attrByName[name]
    else:
      found = None
      for attr in manager.field: 
        if attr.name not in self.attrByName:
          self.attrByName[attr.name] = attr
        if attr.key not in self.attrByKey:
          self.attrByKey[attr.key] = attr
        if attr.name == name:
          found = attr
      return found


  def initializeHandle(self, handle):
    entity = handle["entity"] if "entity" in handle else None
    if not entity: return
    entityUID = self.entityGetUID(entity)
    handle["isHandle"] = True
    handle["attrMap"] = {}
    handle["sddcName"] = self.sddcName
    handle["sddc"] = self
    handle["uid"] = entityUID
    self.allEntityHandlesByUID[entityUID] = handle
    # Folder or VM
    if "name" in handle:
      entityName = handle["name"]
      self.allEntityHandlesByName[entityName] = handle
      if entityName.count('-') == 9: # get replicaId by dropping first uuid (blockchainId)
        handle["replicaId"] = '-'.join(entityName.split('-')[5:])
    # VM with custom attributes
    if self.getEntityType(entity) == "VM" and "customValue" in handle:
      attrs = handle["customValue"]
      for attr in attrs:
        if attr.key not in self.attrByKey:
          log.debug("Custom attr key '{}' not found on {}".format(attr.key, entity))
          continue
        attrDef = self.attrByKey[attr.key]
        attrName = attrDef.name
        handle["attrMap"][attrName] = attr.value
        if attrName not in self.allEntityHandlesByAttribute:
          self.allEntityHandlesByAttribute[attrName] = {}
        if attr.value not in self.allEntityHandlesByAttribute[attrName]:
          self.allEntityHandlesByAttribute[attrName][attr.value] = []
        self.allEntityHandlesByAttribute[attrName][attr.value].append(handle)


  def updateAllEntityHandles(self, initialFetch=False):
    '''
        Fetches and maps all VMs and Folders in the SDDC by name for easy look-up.
    '''
    t1 = time.time()
    try:
      # clear and get latest
      log.debug("Fetching all VMs and Folders on {}...".format(self.sddcName))
      self.allEntityHandlesByName = {} # by VM name
      self.allEntityHandlesByUID = {} # by VM id (e.g. vim.VirtualMachine:vm-28102)
      self.allEntityHandlesByAttribute = {} # by custom attritube and its value
      containerView = self.getDefaultContainerView()
      handles = self.parallelIteratePropsFetchedByAPI(
        iterable = containerView.view
      )
      self.findRegisteredAttributeByName("public_ip") # map out attr keys
      for handle in handles: self.initializeHandle(handle)
      if initialFetch:
        t2 = time.time()
        log.debug("All entities mapped; vSphere on {} ready. (took {:.1f} seconds, total {} entities)"
                    .format(self.sddcName, t2-t1, len(handles)))
      self.entitiesMapped = True
    except Exception as e:
      print(e)


  def filterHandleList(self, handleList):
    if len(handleList) == 0: return handleList
    if handleList[0].__class__ is vim.VirtualMachine:
      return handleList
    newList = []
    for handle in handleList:
      newList.append(handle["entity"])
    return newList


  def checkForNewEntities(self):
    '''
      This updates the VM & Folder mapping with any new VMs
      deployed AFTER initial entities mapping when the connection
      was first established (e.g. new deployment or node added after).
    '''
    log.debug("Checking for new entities on SDDC ({}) inventory...".format(self.sddcName))
    containerView = self.getDefaultContainerView()
    unregistered = []
    for entity in containerView.view:
      if self.getEntityType(entity) in ["VM", "Folder"]:
        entityUID = self.entityGetUID(entity)
        if entityUID not in self.allEntityHandlesByUID:
          unregistered.append(entity)
    if len(unregistered) > 0:
      handles = self.parallelIteratePropsFetchedByAPI(iterable=unregistered)
      for handle in handles: self.initializeHandle(handle)
      log.debug("Added {} new entities to local inventory cache of entities.".format(len(handles)))
    else:
      log.debug("There are no new entities on SDDC ({}) inventory.".format(self.sddcName))


  def parallelIteratePropsFetchedByAPI(self, iterable):
    '''
        Fetches all VMs and Folders in parallel. This function is needed because
        serial entity prop iteration is blocking; it takes 20 ~ 120 seconds to
        fetch 100+ entities (just to look up their names!)
        
        This version takes less than 3 seconds to map all VMs & Folders in the SDDC
        (Each fetch gets its own thread; non-blocking)

        In pyvmomi, each attribute getter on ManagedEntity triggers a vSphere API fetch,
            e.g. `entity.name` triggers an API call to fresh fetch 'name' value from server
            e.g. `entity.config` triggers an API call to fresh fetch 'config' value from server
        This blocks the main thread until resolution in single thread mode, and if you
        have 100+ vSphere entities to look up, then, process will take a very long time.
        (The effect is amplified if SDDC is located in a far-away datacenter)
        
        This can be resolved by fetching everything in parallel.
    '''
    # fetch function (the thread function)
    handles = []
    def parallelGetFetchedProp(entity):
      handle = { "entity": entity }
      propnames = self.entityDefaultProperties(entity)
      for propname in propnames:
        counter = 0; retries = 3
        while counter < retries:
          try:
            counter += 1
            if hasattr(entity, propname):
              fetched = getattr(entity, propname) # --> this triggers API call
              handle[propname] = fetched
              if counter > 1:
                log.debug("Retry, fetching property '{}' from {} (try {}/{}). Succeeded"
                                      .format(propname, entity, counter, retries))
              break
          except Exception as e:
            notReadyYetMessage = "has already been deleted or has not been completely created"
            if hasattr(e, 'msg') and e.msg.endswith(notReadyYetMessage):
              log.debug("VM '{}' has been deleted or creation is not complete".format(entity))  
              return
            log.debug("Errorm fetching property '{}' from {} (try {}/{}). Error: {}"
                                      .format(propname, entity, counter, retries, e))
            time.sleep(1)
      handles.append(handle)
      time.sleep(0.1)
    # parallel fetch all VMs and Folders, give each fetch its own thread
    threads = []
    for entity in iterable:
      # Only VM or Folder (skip resource group, datastores, etc.)
      if self.getEntityType(entity) in ["VM", "Folder"]:
        thd = threading.Thread(
          target = lambda entity: parallelGetFetchedProp(entity), 
          args = (entity, )
        )
        threads.append(thd); thd.start()
    for thd in threads: thd.join(timeout=600) # wait for all API calls to return
    return handles


  def getDefaultContainerView(self):
    '''
      Get default container view of the SDDC, containerView.view is
      the iterable list which container ALL SDDC entities (of all types)
    '''
    return self.content.viewManager.CreateContainerView(
      container = self.content.rootFolder, 
      type = [vim.Folder, vim.VirtualMachine], # VMs & Folders Only
      recursive = True
    )





def cspGetConnection(vmcToken, headers={}):
  try:
    if not vmcToken: return False
    now = int(time.time())
    if not CSP_SESSION["active"] or now >= CSP_SESSION["expires"]:
      response = REQ_SESSION.post(
        "https://console.cloud.vmware.com/csp/gateway/am/api/auth/api-tokens/authorize",
        data = { "refresh_token": vmcToken }
      )
      if response.status_code != 200: return False
      authInfo = response.json()
      CSP_SESSION["id_token"] = authInfo["id_token"]
      CSP_SESSION["access_token"] = authInfo["access_token"]
      CSP_SESSION["expires"] = int(time.time()) + authInfo["expires_in"] - 30 # expire 30s early
      headers["csp-auth-token"] = CSP_SESSION["access_token"]
      headers["csp-open-id-token"] = CSP_SESSION["id_token"]
      return True
  except Exception as e:
    log.debug(e)
    return False
