#########################################################################
# Copyright 2018-2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Wrapper for ReST API calls to Helen.
#########################################################################
import json
import os
import pprint
import subprocess
import threading

from util.auth import getAccessToken, tokens, SERVICE_DEFAULT
import util.json_helper
import util.product
import util.generate_zones_migration as migration
import util.helper as helper
import util.hermes_logging
log = util.hermes_logging.getMainLogger()

class Request():
   # Class
   # Incremented for every call, even across test cases, to have
   # something unique to prepended to the output file names for the calls.
   _idCounter = 1

   # Instance
   logDir = None
   testName = None
   _endpointName = None
   _responseFile = None
   _outputFile = None
   _baseUrl = None
   _subPath = None
   _params = ""
   _data = None
   _accessToken = None

   def __init__(self, logDir, testName, baseUrl, userConfig,
                tokenDescriptor=None,
                forceNewToken=False,
                service=SERVICE_DEFAULT):
      self.logDir = logDir
      os.makedirs(self.logDir, exist_ok=True)

      self.testName = testName
      self._baseUrl = baseUrl
      self._subPath = ""
      self._params = ""
      self._userConfig = userConfig
      self._tokenDescriptor = tokenDescriptor
      self._accessToken = getAccessToken(tokenDescriptor, forceNewToken, service=service)
      self._service = service


   def __str__(self):
      return "testName: {}\n" \
         "_baseUrl: {}\n" \
         "_subPath: {}\n" \
         "_params: {}\n" \
         "_tokenDescriptor: {}".format(self.testName, self._baseUrl, self._subPath, self._params, self._tokenDescriptor)


   def newWithToken(self, tokenDescriptor, forceNewToken=False):
      '''
      Create a new of the this, with a new token.
      '''
      return Request(self.logDir,
                     self.testName,
                     self._baseUrl,
                     self._userConfig,
                     tokenDescriptor,
                     forceNewToken)


   def _send(self, verb=None):
      '''
      Makes the actual ReST request by invoking curl.  Returns the raw json
      of the response.
      '''
      response = None
      exception = None

      # Protect use of Request._idCounter.
      lock = threading.Lock()
      lock.acquire()
      self._setUpOutput(self._endpointName)
      Request._idCounter += 1
      lock.release()
      url = '{0}{1}'.format(self._baseUrl, self._subPath)

      if self._params:
         if "?" in url:
            url += "&" + self._params
         else:
            url += "?" + self._params

      if verb is None:
         if self._data is None:
            curlCmd = ["curl",
                       "-H", "Accept: application/json",
                       "-H", "Authorization: Bearer {0}".format(self._accessToken),
                       url,
                       "--output", self._responseFile,
                       "--verbose",
                       "--insecure"]
         else:
            curlCmd = ["curl",
                       "-H", "Accept: application/json",
                       "-H", "Content-Type: application/json",
                       "-H", "Authorization: Bearer {0}".format(self._accessToken),
                       "--data", json.dumps(self._data),
                       url,
                       "--output", self._responseFile,
                       "--verbose",
                       "--insecure"]
      else:
         curlCmd = ["curl",
                    "--request", verb,
                    "-H", "Accept: application/json",
                    "-H", "Content-Type: application/json",
                    "-H", "Authorization: Bearer {0}".format(self._accessToken),
                    "--data", json.dumps(self._data),
                    url,
                    "--output", self._responseFile,
                    "--verbose",
                    "--insecure"]

      with open (self._outputFile, "a") as f:
         # Make people's lives easier by printing a copy/pastable command.
         f.write("Command: \n'" + "' '".join(curlCmd) + "'\n\n")
         f.flush()
         log.debug("Sending REST command (see {})".format(self._outputFile))
         curlProc = subprocess.run(curlCmd,
                                   stdout=f,
                                   stderr=subprocess.STDOUT)
         log.debug("REST response received.")

      if os.path.isfile(self._responseFile):
         response = util.json_helper.readJsonFile(self._responseFile)

         if response is None:
            exception = "Couldn't read ReST response.\n"
         elif "error" in response:
            exception = "ReST response contained an error.\n" \
                        "Response: '{}'".format(response)
      else:
         exception = "No response for a ReST request was received.  Is the " \
                     "server running?"

         if os.path.isfile(self._outputFile):
            exception += "  Details:\n"
            with open (self._outputFile, "r") as f:
               exception += f.read()

      if exception:
         raise Exception(exception)
      else:
         return response

   def _addParam(self, param):
      '''
      Adds the given parameter.  e.g. count=2
      '''
      if param:
         if self._params:
            self._params += "&"

         self._params += param

   def _setUpOutput(self, method):
      '''
      Creates the log directory and sets the response/output files for a
      ReST request.
      '''
      fileRoot = os.path.join(self.logDir, str(Request._idCounter) + "_" +
                              method)
      self._responseFile = fileRoot + ".json"
      self._outputFile = fileRoot + ".log"

   # Deprecated; will be replaced with getReplicas.
   def getMemberList(self, blockchainId, certs=False):
      '''
      Get the list of nodes in the concord cluster
      '''
      self._subPath = "/api/blockchains/" + blockchainId + "/concord/members"
      if certs:
         self._params = "certs=true"
      else:
         self._params = ""
      self._endpointName = "members"

      return self._send()

   # Will replace /concord/members.  Not implemented in the product yet.
   def getReplicas(self, blockchainId, certs=False):
      '''
      Get the list of replicas in the concord cluster
      '''
      self._subPath = "/api/blockchains/" + blockchainId + "/replicas"
      if certs:
         self._params = "certs=true"
      else:
         self._params = ""
      self._endpointName = "replicas"

      return self._send()

   def getNodeCredentials(self, blockchainId, nodeId, nodeType, certs=False):
      '''
      Get strong password (SSH) of the node with given replica id
      '''
      nodeCategoryPath = 'replicas'
      if nodeType == helper.TYPE_DAML_COMMITTER: nodeCategoryPath = 'replicas'
      elif nodeType == helper.TYPE_DAML_PARTICIPANT: nodeCategoryPath = 'clients'
      self._subPath = "/api/blockchains/" + blockchainId + "/" + nodeCategoryPath + "/" + nodeId+ "/credentials"
      if certs:
         self._params = "certs=true"
      else:
         self._params = ""
      self._endpointName = "replicas"

      return self._send()

   def getBlockchains(self, certs=False):
      '''
      Get the list of blockchains
      '''
      self._subPath = "/api/blockchains"
      if certs:
         self._params = "certs=true"
      else:
         self._params = ""
      self._endpointName = "blockchains"

      return self._send(verb="GET")

   def createBlockchain(self, consortiumId,  siteIds, client_nodes=[],
                        blockchainType=helper.TYPE_ETHEREUM.upper()):
      '''
      Create a blockchain.  Values are simply passed through to persephone.
      consortiumId: The consoritum UUID.
      f: # of faulty nodes.
      c: # of slow nodes.
      siteIds: Array of SDDC IDs to use.  e.g. Go to vmc.vmware.com, pick an SDDC, Support tab, SDDC ID field.
      fixed: Affects the way Persehone distributes nodes. If false, "UNSPECIFIED" is used, and there is debate
             about whether that is a use case.
      '''
      self._subPath = "/api/blockchains"
      self._params = ""
      self._data = {
         "consortium_id": consortiumId,
         "replica_zone_ids": siteIds,
         "client_nodes": client_nodes,
         "blockchain_type": blockchainType
      }

      self._endpointName = "create_blockchain"

      return self._send()

   def getBlockchainDetails(self, blockchainId):
      '''
      Get the details for a given blockchain ID.
      '''
      self._subPath = "/api/blockchains/" + blockchainId
      self._params = ""
      self._data = None
      self._endpointName = "blockchain"
      return self._send()

   def get_participant_details(self, blockchain_id):
      """
      Returns participants details
      :param blockchain_id: he blockchain id the participants are in
      :return: List of ReplicaGetResponse
      """
      self._subPath = "/api/blockchains/{}/clients".format(blockchain_id)
      self._params = ""
      self._endpointName = "participants"
      self._data = None
      return self._send()


   def getBlockList(self, blockchainId, nextUrl=None, latest=None, count=None):
      '''
      Get the list of blocks for the passed in blockchain.
      '''
      self._subPath = nextUrl or "/api/blockchains/" + blockchainId + "/concord/blocks"
      self._params = ""

      if latest != None:
         self._addParam("latest=" + str(latest))

      if count != None:
         self._addParam("count=" + str(count))

      self._endpointName = "blocklist"

      return self._send()

   def getBlockByUrl(self, url):
      '''
      Get a specific block
      '''
      self._subPath = url
      self._params = ""
      self._endpointName = "block"

      return self._send()

   def getBlockByNumber(self, blockchainId, number):
      '''
      Get a specific block, by its number
      '''
      self._subPath = "/api/blockchains/" + blockchainId + "/concord/blocks/{}".format(number)
      self._params = ""
      self._endpointName = "block"

      return self._send()

   def getTransaction(self, blockchainId, txhash):
      '''
      Get a specific transaction
      '''
      self._subPath = "/api/blockchains/" + blockchainId + "/concord/transactions/" + txhash
      self._params = ""
      self._endpointName = "transaction"

      return self._send()

   def getNextTransactionList(self, nextUrl):
      '''
      Calls the given nextUrl to get list of next transactions
      '''
      self._subPath = nextUrl
      self._params = ""
      self._endpointName = "transactionList"
      return self._send()


   def getTransactionList(self, blockchainId, latest=None, count=None):
      '''
      Get a list of transactions
      '''
      self._subPath = "/api/blockchains/" + blockchainId + "/concord/transactions"
      if latest:
         self._addParam("latest=" + latest)

      if count:
         self._addParam("count={}".format(count))

      self._endpointName = "transactionList"

      return self._send()

   def getUsers(self):
       self._subPath = '/api/users'
       self._params = ""
       self._endpointName = "users"

       return self._send()

   def getWallet(self, userId, address):
      '''
      Get the wallet of the user & address
      :param userId:
      :param address:
      :return:
      '''

      self._subPath = '/api/users/{}'.format(userId)
      self._subPath += '/wallet/' + address
      self._params = ""
      self._endpointName = "wallet"

      return self._send()

   def uploadContract(self, blockchainId, data):
      '''
      Does an upload new contract POST request
      '''
      self._subPath = "/api/blockchains/" + blockchainId + "/concord/contracts"
      self._params = ""
      self._endpointName = "postContract"
      self._data = data

      return self._send()

   def getContracts(self, blockchainId):
      '''
      Returns a list of all contracts.  Does a GET request.
      '''
      self._subPath = "/api/blockchains/" + blockchainId + "/concord/contracts"
      self._params = ""
      self._endpointName = "getContract"
      self._data = None

      return self._send()

   def getAllContractVersions(self, blockchainId, contractId):
      '''
      Returns all versions of a contract.
      '''
      self._subPath = "/api/blockchains/" + blockchainId + "/concord/contracts/" \
                      + contractId
      self._params = ""
      self._endpointName = "getAllContractVersions"
      self._data = None

      return self._send()

   def getContractVersion(self, blockchainId, contractId, contractVersion):
      '''
      Returns a contract.
      '''
      self._subPath = "/api/blockchains/" + blockchainId + "/concord/contracts/" \
                      + contractId + "/versions/" + contractVersion
      self._params = ""
      self._endpointName = "getContractVersion"
      self._data = None

      return self._send()

   def callContractAPI(self, apiPath, params):
      '''
      Calls a contract management API. Does a GET request.
      '''
      self._subPath = apiPath
      self._params = params
      self._endpointName = "contractsManagement"
      self._data = None

      return self._send()

   def callUserAPI(self, apiPath, verb=None, params=None, data=None):
      '''
      Calls a user management API. Send the request based on verb value
      '''
      self._subPath = '/api' + apiPath
      self._endpointName = "userManagement"
      self._data = None
      self._params = ""
      if data:
         self._data = data
      if params:
         self._params = params
      return self._send(verb)

   def compileContract(self, data):
      '''
      Does a compile contract POST request
      '''
      self._subPath = '/contracts/compile'
      self._params = ""
      self._endpointName = "microserviceCompileContract"
      self._data = data

      return self._send()

   def verifyContract(self, data):
      '''
      Does a verify contract POST request
      '''
      self._subPath = '/contracts/verify'
      self._params = ""
      self._endpointName = "microserviceVerifyContract"
      self._data = data

      return self._send()

   def getTaskStatus(self, taskId):
      '''
      Returns the given task's status.
      '''
      self._subPath = "/api/tasks/{}".format(taskId)
      self._params = ""
      self._endpointName = "get_task"
      self._data = None
      return self._send(verb = "GET")

   def createConsortium(self, conName):
      '''
      Given a consortium name, creates it.
      '''
      self._subPath = "/api/consortiums/"
      self._params = ""
      self._endpointName = "create_consortiums"
      self._data = {}

      if conName != None:
         self._data["consortium_name"] = conName

      return self._send()

   def getConsortium(self, conId):
      '''
      Retrieve one consortium.
      '''
      self._subPath = "/api/consortiums/{}".format(conId)
      self._params = ""
      self._data = None
      self._endpointName = "retrieve_consortium"
      return self._send()

   def getConsortiums(self):
      '''
      Retrieve a list of all consortiums.
      '''
      self._subPath = "/api/consortiums/"
      self._params = ""
      self._data = None
      self._endpointName = "retrieve_consortiums"
      return self._send()

   def patchConsortium(self, conId, newName=None, newType=None,
                      orgsToAdd=None, orgsToRemove=None):
      '''
      Modify a consortium.  The consortium's ID is mandatory;
      other fields are optional.
      '''
      self._subPath = "/api/consortiums/{}".format(conId)
      self._params = ""
      self._data = {}

      if newName != None:
         self._data["consortium_name"] = newName
      if newType != None:
         self._data["consortium_type"] = newType
      if orgsToAdd != None:
         self._data["orgs_to_add"] = orgsToAdd
      if orgsToRemove != None:
         self._data["orgs_to_remove"] = orgsToRemove

      self._endpointName = "patch_consortium"
      return self._send(verb="PATCH")

   def getConsortiumOrgs(self, conId):
      '''
      Given a consortium ID, get its organizations.
      '''
      self._subPath = "/api/consortiums/{}/organizations".format(conId)
      self._params = ""
      self._data = None
      self._endpointName = "get_consortium_orgs"
      return self._send()

   def getOrg(self, orgId):
      '''
      Given an organization ID, return the organization.
      This
      '''
      self._subPath = "/api/organizations/{}".format(orgId)
      self._params = ""
      self._data = {}
      self._endpointName = "get_organization"
      return self._send(verb="GET")


   def patchOrg(self, orgId, addProperties = None, delProperties = None):
      '''
      Given an organization ID, add or delete properties.
      Format of addProperties and delProperties:
      {
         "additionalProp1": "string",
         "additionalProp2": "string",
         "additionalProp3": "string"
      }
      '''
      self._subPath = "/api/organizations/{}".format(orgId)
      self._params = ""
      self._data = {}

      if addProperties != None:
         self._data["add_properties"] = addProperties
      if delProperties != None:
         self._data["delete_properties"] = delProperties

      self._endpointName = "patch_organization"
      return self._send(verb="PATCH")

   def createZone(self, zoneInfo):
      '''
      Creates a zone.
      '''
      self._subPath = "/api/blockchains/zones"
      self._params = ""
      self._data = zoneInfo
      self._endpointName = "create_zone"
      return self._send()

   def getZone(self, zoneId):
      '''
      Get a single zone.
      '''
      self._subPath = "/api/blockchains/zones/{}".format(zoneId)
      self._params = ""
      self._data = None
      self._endpointName = "get_zone"
      return self._send()

   def getZoneDependencies(self, zoneId):
      '''
      Get a single zone's dependencies.
      '''
      self._subPath = "/api/blockchains/zones/dependencies/{}".format(zoneId)
      self._params = ""
      self._data = None
      self._endpointName = "get_zone_dependencies"
      return self._send()

   def deleteZone(self, zoneId):
      '''
      Delete a zone.
      '''
      self._subPath = "/api/blockchains/zones/{}".format(zoneId)
      self._params = ""
      self._data = None
      self._endpointName = "delete_zone"
      return self._send(verb="DELETE")

   def patchZone(self, zoneId, zoneInfo):
      self._subPath = "/api/blockchains/zones/{}".format(zoneId)
      self._params = ""
      self._data = zoneInfo
      self._endpointName = "patch_zone"
      return self._send(verb="PATCH")

   def getZones(self):
      '''
      Get all of the zones.
      '''
      self._subPath = "/api/blockchains/zones"
      self._params = ""
      self._data = None
      self._endpointName = "get_zones"
      return self._send()


   def addUserConfigZones(self, zones, blockchainLocation):
      '''
      Add the zones in user_config.json.
      '''
      ids_added = []

      # Helen only supports adding onprem zones.
      if blockchainLocation != helper.LOCATION_ONPREM:
         raise Exception("The Helen API only supports creation of on prem zones, not '{}'" \
                         .format(blockchainLocation))

      # We need the docker information.  Currently, we put this in the Persephone properties
      # file when we are testing.  Let's keep that as the source of truth for now.
      with open(util.product.persephoneConfigFile, "r") as f:
         for line in f.readlines():
            if line.startswith("provisioning.container.registry.address="):
               containerAddress = line.split("=")[1].strip()
            elif line.startswith("provisioning.container.registry.username="):
               containerUser = line.split("=")[1].strip()
            elif line.startswith("provisioning.container.registry.password="):
               containerPwd = line.split("=")[1].strip()

      if containerPwd.startswith("<") and containerPwd.endswith(">"):
         raise Exception("Ensure that usernames, passwords, etc... are valid in " \
                         "{}".format(util.product.persephoneConfigFile))

      for zone in zones:
         if zone["api"]["credential"]["passwordCredential"]["password"].startswith("<") and \
            zone["api"]["credential"]["passwordCredential"]["password"].endswith(">"):
            raise Exception("Ensure that usernames, passwords, etc... are valid for the " \
                            "zones in zone_config.json.")

         zoneInfo = migration.build_onprem_body(zone)
         # At this point, zoneInfo is set up for the SQL migration used for Persephone testing.
         # Tweak for the Helen API.
         zoneInfo.update({
            "type": helper.LOCATION_TO_ZONE_TYPES[blockchainLocation],
            "container_repo": {
               "url": containerAddress,
               "username": containerUser,
               "password": containerPwd
            }
         })

         # Workaround for https://jira.eng.vmware.com/browse/BC-2156
         # Can't specify bearer token for on prem zones in the Helen
         # API.  Therefore, we must use Log Insight.
         zoneInfo["log_managements"] = [{
            "destination": self._userConfig["logInsight"]["destinationName"],
            "address": self._userConfig["logInsight"]["address"],
            "port": self._userConfig["logInsight"]["port"],
            "username": self._userConfig["logInsight"]["username"],
            "password": self._userConfig["logInsight"]["password"]
         }]

         createResponse = self.createZone(zoneInfo)

         if "id" in createResponse:
            ids_added.append(createResponse["id"])
         else:
            zoneInfo = pprint.pprint(zoneInfo, indent=4)
            raise Exception("Failed to add zone to helen: {}, response: {}".format(zoneInfo, createResponse))

      return ids_added


   def deregisterBlockchain(self, blockchainId):
      '''
      Deregister the passed in blockchain.
      '''
      self._subPath = "/api/blockchains/deregister/{}".format(blockchainId)
      self._params = ""
      self._data = None
      self._endpointName = "deregister_blockchains"
      return self._send(verb="POST")
      
      
   '''
   =================================================================
   =================================================================
   THESE ARE TEMPORARY FUNCTIONS TO BE REMOVED WHEN CSP INTEGRATION
   IS COMPLETE.
   =================================================================
   =================================================================
   '''
   def createOrg(self, orgName):
      '''
      Creates an organization and returns a structure containing the
      organiztion_id and organization_name.
      '''
      self._subPath = "/api/organizations/"
      self._params = ""
      self._endpointName = "organizations"
      self._data = {"organization_name": orgName}
      return self._send()


   def getNodeSizeTemplate(self):
      '''
      Get the node sizing template for the system.
      '''
      self._subPath = "/api/blockchains/nodesizetemplate"
      self._endpointName = "nodesizetemplate"

      return self._send()
