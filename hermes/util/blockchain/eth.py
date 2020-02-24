#########################################################################
# Copyright 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# This file contains functions and values related to interacting with
# a blockchain.
#########################################################################
import json
import logging
import os
import random
import subprocess
import time

import util
import util.numbers_strings

log = logging.getLogger("main")
docker_env_file = ".env"

# The number if blocks/transactions in a page when invoking the
# concord/blocks or concord/transactions calls.
defaultBlocksInAPage = 10
defaultTxInAPage = 10

# These can be generated with https://emn178.github.io/online-tools/keccak_256.html
# hello()
helloFunction = "0x19ff1d21"
# howdy()
howdyFunction = "0x362810fd"

# These can be generated with https://www.rapidtables.com/convert/number/ascii-to-hex.html
# "Hello, World!"
helloHex = "48656c6c6f2c20576f726c6421"
# "Howdy, World!"
howdyHex = "486f7764792c20576f726c6421"

highGas = "0x47e7c4"
itApprovedPort = 443

def getEthrpcNodes(request, blockchainId=None):
   '''
   request: Request to interact with Helen's REST APIs.
   blockchainId: Optional blockchain id, in case one needs to be specified.
   Uses Helen's /blockchain API to get the blockchain members.
   Uses /api/blockchains/{blockchainId} if /concord/members does not work.
   '''
   members = []

   if not blockchainId:
      blockchains = request.getBlockchains()
      blockchainId = blockchains[0]["id"]

   result = request.getBlockchainDetails(blockchainId)

   if not result["node_list"]:
      raise Exception("No ethrpc nodes were returned by Helen.")

   for m in result["node_list"]:
      if m["url"]:
         members.append(m)
      else:
         raise Exception("Helen returned a node with an empty url.")

   return members


def upload_hello_contract(blockchainId, request):
   '''
   Uploads the HelloWorld contract with general settings. This is
   handy when you need blocks with contracts but don't really care
   about the fine details.
   Returns the contract ID and version.  Throws an exception if
   it fails.
   '''
   contractId = util.numbers_strings.random_string_generator()
   contractVersion = util.numbers_strings.random_string_generator()
   result = upload_contract(blockchainId, request,
                            "resources/contracts/HelloWorld.sol",
                            "HelloWorld",
                            contractId = contractId,
                            contractVersion = contractVersion,
                            generateDefaults=True)
   if "url" in result:
      return (contractId, contractVersion)
   else:
      raise Exception("Contract upload failed with error '{}'".format(result["error"]))


def upload_contract(blockchainId, request, sourceFile, contractName,
                    contractId=None,
                    contractVersion=None,
                    fromAddr=None,
                    compilerVersion=None,
                    ctorParams=None,
                    optimize=None,
                    runs=None,
                    generateDefaults=True):
   '''
   Uploads a contract.
   request: A Hermes REST request object.
   sourceFile: Path to the source code.  e.g. "resources/contracts/Counter.sol"
   contractName: The name of the contract, located in sourceFile.
   contractId/Version: Id and version the contract will have in Helen.
   fromAddr: The contract owner.
   compilerVersion: Not sure where available strings come from.
   ctorParams: Contract constructor parameters
   isOptimize: Whether the compiler service should optimize
   runs: Helen passes this to the compiler service when optimizing. I don't know
      what it does or what valid values are.  Helen defaults to 200 if it is
      missing.
   generateDefaults: Whether to generate default values.  Set to False if, for
      example, you want to test sending a request with fields missing.
   '''
   data = {}

   if generateDefaults:
      contractId = contractId or util.numbers_strings.random_string_generator()
      contractVersion = contractVersion or util.numbers_strings.random_string_generator()
      fromAddr = fromAddr or "0x1111111111111111111111111111111111111111"
      compilerVersion = compilerVersion or "v0.5.2+commit.1df8f40c"
      ctorParams = ctorParams or None
      optimize = optimize or False
      runs = runs or "200"

   if fromAddr != None:
      data["from"] = fromAddr
   if contractId != None:
      data["contract_id"] = contractId
   if contractVersion != None:
      data["version"] = contractVersion
   if contractName != None:
      data["contract_name"] = contractName
   if  ctorParams != None:
      data["constructor_params"] = ctorParams
   if compilerVersion != None:
      data["compiler_version"] = compilerVersion
   if optimize != None:
      data["is_optimize"] = optimize
   if runs != None:
      data["runs"] = runs

   if sourceFile:
      with open(sourceFile, 'r') as f:
         data["sourcecode"] = f.read()

   result = request.uploadContract(blockchainId, data)
   return result


def addBlocks(request, rpc, blockchainId, numIterations, invokeContracts=False):
   '''
   Adds blocks to the given blockchain.
   numIterations: How many times to loop.  How many blocks you get depends on the value
      of invokeContracts. (n or 2n)
   invokeContracts: If False, only blocks containing contracts will be added (one block
      per iteration). If True, contracts will be added and invoked (two blocks per
      iteration).
   Returns a list of transaction receipts, each with an extra field called "apiCallTime"
   for testing.
   '''
   txHashes = []
   txReceipts = []

   for i in range(numIterations):
      contractId, contractVersion = upload_hello_contract(blockchainId, request)
      contractResult = request.callContractAPI('/api/concord/contracts/' + contractId
                                               + '/versions/' + contractVersion, "")
      # Getting the latest block is a workaround for VB-812, "No way to get a transaction
      # receipt for submitting a contract via contract API."
      block = getLatestBlock(request, blockchainId)
      # Time service may have added a block after the contract
      # creation, so look for a block with a transaction in it
      while not block["transactions"]:
         block = request.getBlockByNumber(blockchainId, block["number"]-1)
      txHashes.append(block["transactions"][0]["hash"])

      if invokeContracts:
         txHashes.append(rpc.sendTransaction("0x1111111111111111111111111111111111111111",
                                             helloFunction,
                                             to=contractResult["address"]))

   for txHash in txHashes:
      txReceipt = rpc._getTransactionReceipt(txHash)
      txReceipt["apiCallTime"] = int(time.time())
      txReceipts.append(txReceipt)

   return txReceipts


def getLatestBlock(request, blockchainId):
   '''
   Returns the latest block on the given blockchain.
   '''
   blockNumber = getLatestBlockNumber(request, blockchainId)
   return request.getBlockByNumber(blockchainId, blockNumber)


def getLatestBlockNumber(request, blockchainId):
   '''
   Returns the newest block's number for the passed in blockchain.
   '''
   blockList = request.getBlockList(blockchainId)
   return blockList["blocks"][0]["number"]


def ensureEnoughBlocksForPaging(fxConnection, blockchainId):
   '''
   Make sure the passed in blockchain has enough blocks to page multiple
   times with the default page size.
   '''
   latestBlockNumber = getLatestBlockNumber(fxConnection.request, blockchainId)
   numBlocks = latestBlockNumber + 1
   minBlocks = defaultBlocksInAPage * 3
   blocksToAdd = minBlocks - numBlocks

   if blocksToAdd > 0:
      log.info("ensureEnoughBocksForPaging is adding {} blocks.".format(blocksToAdd))

      for i in range(blocksToAdd):
         mock_transaction(fxConnection.rpc, data=util.numbers_strings.decToEvenHex(i))

      latestBlockNumber = getLatestBlockNumber(fxConnection.request, blockchainId)
      assert latestBlockNumber + 1 >= minBlocks, "Failed to add enough blocks for paging."


def mock_transaction(rpc, data = "0x00"):
   # Do a transaction so that we have some blocks
   caller = "0x1111111111111111111111111111111111111111"
   to = "0x2222222222222222222222222222222222222222"
   response = rpc.sendTransaction(caller, data, "0xffff", to, "0x01")
   response = rpc._getTransactionReceipt(response)
   return response;


def loadGenesisJson():
   '''
   Loads the genesis.json file that is used when launching via Docker.
   Won't work when launching in a real environment.
   '''
   hermes = os.getcwd()
   genFile = os.path.join(hermes, "..", "docker", "config-public", "genesis.json")
   genObject = None

   with open(genFile, "r") as f:
      genObject = json.loads(f.read())

   return genObject


def get_concord_container_name(replicaId):
   command = 'docker ps --format "{0}" | grep concord{1}'.format("{{ .Names }}", replicaId)
   output = subprocess.Popen(command,stderr=subprocess.PIPE, shell=True, stdout=subprocess.PIPE).stdout.read().decode().replace(os.linesep,"")
   return output

def get_all_concord_container_names():

   # Note the logic in this function tries to avoid assuming whether Concord
   # container names are 0 or 1 indexed.
   names = list()
   i = 0
   name = get_concord_container_name(i)
   while ((len(name) > 0) or (i < 1)):
      if (len(name) > 0):
         names.append(name)
      i = i + 1
      name = get_concord_container_name(i)
   return names

def exec_in_concord_container(containerName, args):
   command = "docker exec {0} {1}".format(containerName, args)
   output = subprocess.Popen(command,stderr=subprocess.PIPE, shell=True, stdout=subprocess.PIPE).stdout.read().decode("UTF-8")
   return output


def addCodePrefix(code):
   '''
   When creating a contract with some code, that code will run the first time
   only.  If we want to invoke code in a contract after running it, the code
   needs to return the code to run.  For example, say we want to test code
   which adds 5 to storage every time it is invoked:

   PUSH1 00
   SLOAD
   PUSH1 05
   ADD
   PUSH1 00
   SSTORE

   600054600501600055

   As-is, 5 will be put in storage on contract creation, and further
   invocations of the contract will do nothing.  To invoke the code in
   subsequent calls, add code to return that code in front:

   Prefix                   Code we want to test
   600980600c6000396000f300 600054600501600055

   The prefix instructions are:
   60: PUSH1 (Could be PUSH2, PUSH3, etc... depending on the next nunber.
   09: Hex number of bytes of the code we want to test.
   80: DUP1 because we're going to use the above number once for CODECOPY,
       then again for RETURN.
   60: PUSH
   0c: Hex number of bytes offset where the code we want to test starts.
       (In other words, the length of this prefix so that it is skipped.)
   60: PUSH
   00: Destination offset for the upcoming CODECOPY.  It's zero because we're
       going to start writing at the beginning of memory.
   39: CODECOPY (destOffset, offset, length).  These are three of the numbers
       we put on the stack with the above code.
   60: PUSH
   00: Offset for the upcoming RETURN. (We want to return all of the code
       copied to memory by the CODECOPY.)
   f3: RETURN (offset, length).  The length param is the first PUSH.
   00: STOP
   '''
   code = util.numbers_strings.trimHexIndicator(code)

   # Tests may have more than 0xff bytes (the byte1 test), so we may end
   # up with three digits, like 0x9b0. We must have an even number of digits
   # in the bytecode, so pad it if necessary.
   numCodeBytes = int(len(code)/2)
   numCodeBytesString = util.numbers_strings.decToEvenHexNo0x(numCodeBytes)
   codeLengthPush = util.bytecode.getPushInstruction(numCodeBytes)
   codeLengthPush = util.numbers_strings.trimHexIndicator(codeLengthPush)

   # Calculate the length of this prefix.  What makes it variable is the
   # length of numCodeBytesString.  The prefix is 11 bytes without it.
   prefixLength = 11 + int(len(numCodeBytesString)/2)
   prefixLength = util.numbers_strings.decToEvenHexNo0x(prefixLength)

   prefix = "0x{}{}8060{}6000396000f300".format(codeLengthPush,
                                                numCodeBytesString,
                                                prefixLength)
   return prefix + code


def getEthrpcApiUrl(request, blockchainId, useITApprovedPort=False, scheme=None):
   '''
   Fetches a random ethRpc node for the given blockchian ID.

   useITApprovedPort: The "vmware" network only allows https traffic
   on port 443, but our ethrpc nodes use other ports.  Pass in whether
   this function should modify the url to use a port approved by IT.

   This function also provides a way to force the scheme, because we do not
   support https on nodes deployed in the SDDC yet.
   '''
   ethrpcNodes = util.blockchain.eth.getEthrpcNodes(request, blockchainId)
   node = random.choice(ethrpcNodes)
   url = node["url"]

   if useITApprovedPort:
      url = util.helper.replaceUrlParts(url, newPort=itApprovedPort)

   if scheme:
      url = util.helper.replaceUrlParts(url, newScheme=scheme)

   return url


def createContract(user, rpc, testExecutionCode, gas, waitForMining=False):
   '''
   Create a contract and return a transaction receipt.
   '''
   txReceipt = None
   txHash = rpc.sendTransaction(user["hash"], testExecutionCode, gas)

   if txHash:
      txReceipt = rpc.getTransactionReceipt(txHash, waitForMining)

   return txReceipt

def createDELEGATECALLBytecode(contractAddress, numBytesOut, arguments):
   '''
   Returns the bytecode to create a new contract which:
   - Invokes DELEGATECALL on the given contract address.
   - Stores the returned result of DELEGATECALL into storage. That way, the
     test framework can retrieve the returned values.
   '''
   arguments = util.numbers_strings.trimHexIndicator(arguments)

   # Define memory in which to place the return bytes.
   lengthRetBytes = numBytesOut
   retBytesPushInstruction = util.bytecode.getPushInstruction(lengthRetBytes)
   retOffset = 0
   retOffsetPushInstruction = util.bytecode.getPushInstruction(retOffset)

   # Start writing args at the next 32-bit boundary after the memory for
   # the return value.
   argsOffset = lengthRetBytes + 32 - lengthRetBytes % 32
   argsOffsetPushInstruction = util.bytecode.getPushInstruction(argsOffset)
   memoryBytecode = ""
   argsLength = 0
   byteOffset = argsOffset

   # Write one byte to memory at a time with MSTORE8 (53).  Test cases have
   # varying numbers of bytes to pass in.
   for i in range(0, len(arguments)-1, 2):
      memoryBytecode += "60" + arguments[i:i+2]
      memoryBytecode += util.numbers_strings.trimHexIndicator(util.bytecode.getPushInstruction(byteOffset))
      memoryBytecode += util.numbers_strings.decToEvenHexNo0x(byteOffset)
      memoryBytecode += "53"
      byteOffset += 1
      argsLength += 1

   argsLenPushInstruction = util.numbers_strings.trimHexIndicator(util.bytecode.getPushInstruction(argsLength))

   # Define the rest of the fields for DELEGATECALL.
   gas = util.numbers_strings.trimHexIndicator(highGas)

   if not len(gas) % 2 == 0:
      gas = "0" + gas

   gasPushInstruction = util.bytecode.getPushInstruction(int(gas, 16))
   invokeCallBytecode = "0x" + memoryBytecode
   invokeCallBytecode += "{}{}{}{}{}{}{}{}73{}{}{}f4". \
                         format(util.numbers_strings.trimHexIndicator(retBytesPushInstruction),
                                util.numbers_strings.decToEvenHexNo0x(lengthRetBytes),
                                util.numbers_strings.trimHexIndicator(retOffsetPushInstruction),
                                util.numbers_strings.decToEvenHexNo0x(retOffset),
                                util.numbers_strings.trimHexIndicator(argsLenPushInstruction),
                                util.numbers_strings.decToEvenHexNo0x(argsLength),
                                util.numbers_strings.trimHexIndicator(argsOffsetPushInstruction),
                                util.numbers_strings.decToEvenHexNo0x(argsOffset),
                                util.numbers_strings.trimHexIndicator(contractAddress),
                                util.numbers_strings.trimHexIndicator(gasPushInstruction),
                                gas)

   # After the DELEGATECALL has occurred, copy return memory to
   # storage so it can be retrieved by the testing framework for
   # test verification.
   storageSlots = countStorageSlotsForBytes(lengthRetBytes)

   storageStart = delegateStorageKey()
   for storageSlot in range(0, storageSlots):
      memoryOffsetDec = storageSlot * 32
      memoryOffsetHex = util.numbers_strings.decToEvenHexNo0x(memoryOffsetDec)
      storageSlotHex = storageStart + storageSlot
      storageSlotPushInstruction = util.bytecode.getPushInstruction(storageSlotHex)

      # Even if we were given 8 bytes as an expected return value, MLOAD
      # only reads 32 bytes, so the 8 byte value will be stored in 32 bytes
      # of storage.
      # Instructions:
      #   PUSH1  {memory offset}
      #   MLOAD
      #   PUSH1  {storage slot}
      #   SSTORE
      mloadStep = "60{}51{}{}55". \
                  format(memoryOffsetHex,
                         util.numbers_strings.trimHexIndicator(storageSlotPushInstruction),
                         util.numbers_strings.decToEvenHexNo0x(storageSlotHex))
      invokeCallBytecode += mloadStep

   return invokeCallBytecode

def countStorageSlotsForBytes(numBytes):
   storageSlots = int(numBytes/32)

   if numBytes % 32 > 0:
      storageSlots += 1

   return storageSlots

def delegateStorageKey():
   '''
   Returns the key that the bytecode generated by
   _createDELEGATECALLBytecode will use to store the return value
   of the called contract.
   '''
   return 0x6865726D6573 # "hermes" in ASCII
