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

class ConnectionToSDDC:
  
  def __init__(self, sddcInfo):
    '''
        Connect to vSphere given host and credentials.
        After establishing connection successfully, fetches and maps all
        VMs and Folders in the SDDC by name for easy look-up.
        (Establishing connection takes about 2 ~ 5 seconds)

        Using the library, `pyvmomi`, which allows access to most functionalities
        of vSphere running on the SDDC (about 80% feature coverage; e.g. attaching
        tags and categories are missing in the lib, and must be done with vapi.cis)
    '''
    try:
      self.sddcName = sddcInfo["name"]
      self.publicIP = sddcInfo["publicIP"]; publicIPHyphen = self.publicIP.replace('.', '-')
      self.hostnameVCenter = f"vcenter.sddc-{publicIPHyphen}.vmwarevmc.com"
      self.hostnameNSX = f"nsx-{publicIPHyphen}.rp.vmwarevmc.com"
      self.reqSession = requests.Session()
      self.orgId = sddcInfo["orgId"]
      self.sddcId = sddcInfo["sddcId"]
      self.baseNSXT = f"https://{self.hostnameNSX}/vmc/reverse-proxy/api/orgs/{self.orgId}/sddcs/{self.sddcId}/sks-nsxt-manager"
      self.headersNSXT = {}
      self.username = sddcInfo["username"]
      self.password = sddcInfo["password"]
      self.vmcToken = sddcInfo["vmcToken"] if not sddcInfo["vmcToken"].startswith("<") else ""
      self.ready = False
      sslContext = None # skip cert checking
      if hasattr(ssl, '_create_unverified_context'): 
        sslContext = ssl._create_unverified_context()
      log.info("Connecting to vSphere on {}...".format(self.sddcName))
      self.connection = SmartConnect(host = self.hostnameVCenter,
                                    user = self.username,
                                    pwd = self.password,
                                    sslContext = sslContext)
      if not self.connection:
        log.debug("Cannot connect to {} as {} ".format(self.hostnameVCenter, self.username))
      else:
        atexit.register(Disconnect, self.connection) # clean up connection
        log.info("Successfully connected to vSphere on {}".format(self.sddcName))
        self.content = self.connection.RetrieveContent() # main content obj of vSphere
        self.services = self.connection.RetrieveServiceContent() # all managers
        self.attrByName = {}
        self.attrByKey = {}

        log.info("Fetching all VMs and Folders on {}...".format(self.sddcName))
        self.allEntityHandlesByName = {} # by VM name
        self.allEntityHandlesByUID = {} # by VM id (e.g. vim.VirtualMachine:vm-28102)
        self.allEntityHandlesByAttribute = {} # by custom attritube and its value
        self.updateAllEntityHandles() # populate `allEntityHandlesByName` with all VMs & Folders in this SDDC
        self.ready = True
    
    except Exception as e:
      traceback.print_exc()
      log.debug("Failed to initialize session to vSphere on {}".format(self.sddcName))
      log.debug(e)


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


  def vmBasicPropertiesToFetch(self):
    return ["name", "customValue"]


  def vmGetUID(self, vm):
    return str(vm).replace("'", "")

  
  def vmRegisterIfNew(self, vm):
    vmUID = str(vm)
    if vmUID in self.allEntityHandlesByUID:
      return self.allEntityHandlesByUID[vmUID]
    try:
      handle = { "entity": vm }
      props = self.vmBasicPropertiesToFetch()
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
    if self.vmGetUID(vm) in self.allEntityHandlesByUID:
      vmHandle = self.allEntityHandlesByUID[self.vmGetUID(vm)]
      if name in vmHandle["attrMap"]:
        return vmHandle["attrMap"][name]
      else:
        return None
    for attrOnVM in vm.customValue:
      if attr.key == attrOnVM.key:
        return attrOnVM.value
    return None


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
    handle["isHandle"] = True
    handle["attrMap"] = {}
    handle["sddc"] = self.sddcName
    if "name" in handle:
      name = handle["name"]
      entity = handle["entity"]
      if name.count('-') == 9: # get replicaId by dropping first uuid (blockchainId)
        handle["replicaId"] = '-'.join(name.split('-')[5:])
      self.allEntityHandlesByName[name] = handle
      if self.getEntityType(entity) == "VM":
        vmUID = self.vmGetUID(entity)
        handle["uid"] = vmUID
        self.allEntityHandlesByUID[vmUID] = handle
        attrs = handle["customValue"]
        for attr in attrs:
          attrDef = self.attrByKey[attr.key]
          attrName = attrDef.name
          handle["attrMap"][attrName] = attr.value
          if attrName not in self.allEntityHandlesByAttribute:
            self.allEntityHandlesByAttribute[attrName] = {}
          if attr.value not in self.allEntityHandlesByAttribute[attrName]:
            self.allEntityHandlesByAttribute[attrName][attr.value] = []
          self.allEntityHandlesByAttribute[attrName][attr.value].append(handle)


  def updateAllEntityHandles(self):
    '''
        Fetches and maps all VMs and Folders in the SDDC by name for easy look-up.
    '''
    try:
      # clear and get latest
      self.allEntityHandlesByName = {} # by VM name
      self.allEntityHandlesByUID = {} # by VM id (e.g. vim.VirtualMachine:vm-28102)
      self.allEntityHandlesByAttribute = {} # by custom attritube and its value
      containerView = self.content.viewManager.CreateContainerView(
        container = self.content.rootFolder, 
        type = [vim.Folder, vim.VirtualMachine], # VMs & Folders Only
        recursive = True
      )
      handles = self.parallelIteratePropsFetchedByAPI(
        iterable = containerView.view,
        props = self.vmBasicPropertiesToFetch()
      )
      self.findRegisteredAttributeByName("public_ip") # map out attr keys
      for handle in handles: self.initializeHandle(handle)
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


  def parallelIteratePropsFetchedByAPI(self, iterable, props):
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
    def parallelGetFetchedProp(entity, props, results):
      try:
        handle = { "entity": entity }
        for propname in props:
          if hasattr(entity, propname):
            handle[propname] = getattr(entity, propname) # --> this triggers API call
        results.append(handle) # list append is thread-safe
      except Exception as e:
        log.debug(e)

    # parallel fetch all VMs and Folders, give each fetch its own thread
    results = []; threads = []
    for entity in iterable:
      t = threading.Thread(
        target = lambda entity: parallelGetFetchedProp(entity, props, results), 
        args = (entity, )
      )
      threads.append(t)
      t.start()

    for thd in threads: thd.join() # wait for all API calls to return
    return results

