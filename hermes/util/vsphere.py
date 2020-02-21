from pyVim.connect import SmartConnect, Disconnect
from pyVmomi import vim

import atexit
import ssl
import threading

import logging

log = logging.getLogger(__name__)

class ConnectionToSDDC:
  
  def __init__(self, sddcName, hostname, username, password):
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
      self.sddcName = sddcName
      self.ready = False
      sslContext = None # skip cert checking
      if hasattr(ssl, '_create_unverified_context'): 
        sslContext = ssl._create_unverified_context()
      log.info("Connecting to vSphere on SDDC {}...".format(sddcName))
      self.connection = SmartConnect(host = hostname,
                                    user = username,
                                    pwd = password,
                                    sslContext = sslContext)
      if not self.connection:
        log.debug("Cannot connect to {} as {} ".format(hostname, username))
      else:
        atexit.register(Disconnect, self.connection) # clean up connection
        log.info("Successfully connected to vSphere on {}".format(sddcName))
        self.content = self.connection.RetrieveContent() # main content obj of vSphere
        self.services = self.connection.RetrieveServiceContent() # all managers
        self.attrByName = {}

        log.info("Fetching all VMs and Folders on {}...".format(sddcName))
        self.updateAllEntityHandles() # populate `allEntityHandlesByName` with all VMs & Folders in this SDDC
        self.ready = True
    
    except Exception as e:
      log.debug("Failed to initialize session to vSphere on {}".format(sddcName))
      log.debug(e)


  def getByName(self, name):
    '''
        Returns VM or Folder that matches the name exactly
    '''
    if name in self.allEntityHandlesByName:
      return self.allEntityHandlesByName[name]["entity"]
    else:
      return None


  def getByNameContaining(self, substring):
    '''
        Returns first VM or Folder that has substring in the name
        (mainly used for matching `replicaId`, which is the second
        part of the deployed VMs on the SDDC)
    '''
    for name in self.allEntityHandlesByName:
      if substring in name:
        return self.allEntityHandlesByName[name]["entity"]
    return None


  def getByIP(self, ipAddress):
    '''
        Returns VMs (vim.VirtualMachine) matching the supplied IP
    '''
    return self.content.searchIndex.FindAllByIp(ip=ipAddress, vmSearch=True)


  def getEntityType(self, entity):
    if entity.__class__ is vim.VirtualMachine:
      return "VM"
    elif entity.__class__ is vim.Folder:
      return "Folder"
    else:
      return None


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
    for attrOnVM in vm.customValue:
      if attr.key == attrOnVM.key:
        return attrOnVM.value
    return None

  
  def vmMoveToFolderByName(self, folderName, entityNames, matchExactly=True, suspendVMs=False, appendAnnotation="", attrSet=[]):
    '''
        Moves VMs with matching names to another folder.
        (Mainly used for keeping suspect VMs alive for further investigation)

        ```
        # e.g.: Move select `replicaIds` to another folder
          sddcConn.moveVMsToFolder("HermesTesting-LongInvestigation", [
            "fb50a73f-605f-4b39-9d13-9b1abfc52471",
            "fb50a73f-605f-4b39-9d13-9b1abfc52472",
            "fb50a73f-605f-4b39-9d13-9b1abfc52473",
            "fb50a73f-605f-4b39-9d13-9b1abfc52474"
          ], matchExactly = False)
        ```
    '''
    if len(entityNames) == 0:
      log.debug("Supplied entity name list is empty")
      return False
    
    folder = self.getByName(folderName)
    if folder is None:
      log.debug("Folder not found (name='{}')".format(folderName))
      return False
    
    vmNames = []
    vmList = []
    for entityName in entityNames:
      vm = None
      if matchExactly: 
        vm = self.getByName(entityName)
      else:
        vm = self.getByNameContaining(entityName)
      if vm is not None:
        vmList.append(vm)
        vmNames.append(vm.name)
    
    if len(vmList) == 0:
      log.debug("Cannot find any VM matching the name description")
      return False
    
    log.info("Moving VMs [{}] to {}".format(', '.join(vmNames), folderName))
    folder.MoveIntoFolder_Task(list = vmList)

    # While moving, additioanlly append annotation why it was moved
    if len(appendAnnotation) > 0:
      for vm in vmList:
        currentAnnotation = vm.config.annotation
        self.vmAnnotate(vm, currentAnnotation + "\n\nMoved, Reason: " + appendAnnotation)

    # While moving, additioanlly set/modify VM attributes (e.g. "moved" : yes)
    if len(attrSet) > 0:
      for vm in vmList:
        for attr in attrSet:
          self.vmSetCustomAttribute(vm, attr["name"], attr["value"])

    # While moving additional option to suspend the VM
    #    note: `suspending` actually takes the VM down (even IP assignment)
    #    if you want TRUE `pausing` you gotta pause the VM with esxcli command
    #    kill -STOP [VMX_PROCESS_PID] # pauses
    #    kill -CONT [VMX_PROCESS_PID] # resumes
    #    more info: https://www.virtuallyghetto.com/2013/03/how-to-pause-not-suspend-virtual.html
    if suspendVMs:
      for vm in vmList:
        vm.SuspendVM_Task()

    return True


  def findRegisteredAttributeByName(self, name):
    manager = self.services.customFieldsManager
    if name in self.attrByName:
      return self.attrByName[name]
    else:
      found = None
      for attr in manager.field: 
        if attr.name not in self.attrByName:
          self.attrByName[attr.name] = attr
        if attr.name == name: 
          found = attr
      return found


  def updateAllEntityHandles(self):
    '''
        Fetches and maps all VMs and Folders in the SDDC by name for easy look-up.
    '''
    self.allEntityHandlesByName = {} # clear array and get latest
    containerView = self.content.viewManager.CreateContainerView(
      container = self.content.rootFolder, 
      type = [vim.Folder, vim.VirtualMachine], # VMs & Folders Only
      recursive = True
    )
    handles = self.parallelIteratePropsFetchedByAPI(
      iterable = containerView.view,
      props = ["name"]
    )
    for handle in handles:
      if "name" in handle:
        name = handle["name"]
        entity = handle["entity"]
        self.allEntityHandlesByName[name] = handle


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

