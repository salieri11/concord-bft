#!/usr/bin/python3

#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import argparse
import datetime
import logging
import os
from tempfile import mkdtemp
from time import strftime, localtime

from rpc.rpc_call import RPC
from suites import test_suite
from util import json_helper

log = None
command = os.path.splitext(os.path.basename(__file__))[0]

def main():
   apiServerUrl = None
   product = None

   parser = argparse.ArgumentParser()
   parser.add_argument("bytecode",
                       help="Bytecode of the contract.")
   parser.add_argument("--addPrefix",
                       help="Add a prefix for the bytecode to be runnable. " \
                            "If you don't use this, and your own bytecode " \
                            "does not have some prefix code, your bytecode " \
                            "will be run once, when the contract is created, " \
                            "and cannot be called again.",
                       default=False,
                       action="store_true")
   parser.add_argument("--callIt",
                       help="Create a second contract from which to call " \
                            "the contract containing your bytecode, and " \
                            "call it. Not implemented: \n" \
                            "Passing in a value for 'value'. \n" \
                            "NOTE: Contracts are always executed once " \
                            "when intially created. This argument is an " \
                            "additional call in order to get the return " \
                            "value and pass in data with --callData.",
                       default=False,
                       action="store_true")
   parser.add_argument("--callData",
                       help="Data to pass to the contract when using " \
                            "--callIt. In the callee, this data is available " \
                            "as msg.data or CALLDATALOAD (instruction 35).",
                       default="0x")
   parser.add_argument("--showStorage",
                       help="Display storage after execution. If --callIt " \
                            "is used, then storage will be displayed " \
                            "twice: Once after initial contract creation, and " \
                            "again after the invocation.",
                       default=False,
                       action="store_true")
   parser.add_argument("--storageIndices",
                       help="If using --showStorage, the number of storage " \
                            "locations to display.",
                       default=1)
   parser.add_argument("--returnIndices",
                       help="If using --callIt, the number of 32-byte " \
                            "chunks of return data to display.",
                       default=1)
   parser.add_argument("--ethereumMode",
                       help="Run against Ethereum instead of the product. " \
                            "Ethereum should already be running and mining.",
                       default=False,
                       action="store_true")
   parser.add_argument("--logLevel",
                       help="Set the log level.  Valid values:"
                       "'DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL'",
                       default="INFO")

   args = parser.parse_args()
   setUpLogging(args)
   args.resultsDir = createResultsDir(command)
   log.info("Results directory: {}".format(args.resultsDir))
   creator = BytecodeContractCreator(args)
   creator.run()

def createResultsDir(suiteName):
   prefix = suiteName + "_" + strftime("%Y%m%d_%H%M_", localtime())

   return mkdtemp(prefix=prefix)

def setUpLogging(args):
   '''
   Checks the log level passed in by the user and sets up logging.
   After running, get a logger and use it like this:

   log = logging.getLogger(__name__)
   log.info("Two things: {} {}".format("pen", "pencil"))
   '''
   stringLevel = args.logLevel.upper()

   try:
      intLevel = getattr(logging, stringLevel)
      logging.basicConfig(level=intLevel, format='%(message)s')
      global log
      log = logging.getLogger(__name__)
   except AttributeError:
      print("Log level", args.logLevel, "not valid.  See the help for valid",
            "values. Exiting")
      exit(1)


class BytecodeContractCreator(test_suite.TestSuite):
   def __init__(self, passedArgs):
      super(BytecodeContractCreator, self).__init__(passedArgs)

   def getName(self):
      return "CreateBytecodeContract"

   def run(self):
      '''
      Creates a contract containing some bytecode which can be invoked
      again later.
      '''
      try:
         self.launchProduct(self._args,
                            self._apiServerUrl,
                            self._userConfig)
      except Exception as e:
         log.error(traceback.format_exc())
         return self._resultFile

      self._createBytecodeContract()

   def _createBytecodeContract(self):
      user = self._getAUser()
      rpc = RPC(self._testLogDir,
                "CreateBytecodeContract",
                self._apiServerUrl,
                self._userConfig)
      gas = self._getGas()
      self._unlockUser(rpc, user)

      if self._args.addPrefix:
         log.info("Creating initial contract with wrapped bytecode.")
         bytecode = self._addCodePrefix(self._args.bytecode)
      else:
         log.info("Creating initial contract with unwrapped bytecode.")
         bytecode = self._args.bytecode

      txReceipt = self._createContract(user, rpc, bytecode, gas)

      if txReceipt:
         log.debug("Tx receipt: '{}'".format(txReceipt))
         contractAddress = RPC.searchResponse(txReceipt, ["contractAddress"])
         if contractAddress:
            log.info("Contract address: '{}'".format(contractAddress))

            if self._args.showStorage:
               log.info("Storage at contract '{}' after contract creation:".\
                        format(contractAddress))
               self._displayStorage(rpc, contractAddress)

            if self._args.callIt:
               log.info("Creating the contract which will call the contract " \
                        "created earlier.")
               numReturnBytes = 32*int(self._args.returnIndices)
               invokerCallBytecode = self.\
                             _createCALLBytecode(contractAddress, numReturnBytes,
                                                 self._args.callData)

               invokerCallBytecode = self._addCodePrefix(invokerCallBytecode)

               invokerTxReceipt = self._createContract(user, rpc,
                                                       invokerCallBytecode, gas)
               invokerContractAddress = RPC.searchResponse(invokerTxReceipt,
                                                           ["contractAddress"])
               log.info("Calling the contract which will call the contract " \
                        "created earlier.")
               txReceipt = self._invokeContract(user, rpc,
                                                invokerContractAddress,
                                                None, gas)

               if self._args.showStorage:
                  log.info("Storage at contract '{}' after contract " \
                           "invocation:".format(contractAddress))
                  self._displayStorage(rpc, contractAddress)

               log.info("Data returned by the contract (32 bytes at a time):")

               for storageLoc in range(int(self._args.returnIndices)):
                  returnStorage = rpc.getStorageAt(invokerContractAddress,
                                                   hex(storageLoc))
                  log.info("   {}: {}".format(storageLoc, returnStorage))
         else:
            log.error("No contract address when creating the contract.")
      else:
         log.error("No transaction receipt when creating the contract.")

   def _displayStorage(self, rpc, contractAddress):
      '''
      Display storage for the given contract.
      '''
      for storageLoc in range(int(self._args.storageIndices)):
         val = rpc.getStorageAt(contractAddress, hex(storageLoc))
         log.info("   {}: {}".format(storageLoc, val))

main()
