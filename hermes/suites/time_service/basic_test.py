#########################################################################
# Copyright 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# "Basic" Time Service tests (i.e. the happy-path, non-evil ones)
#########################################################################
import collections
from base64 import b64decode
import dateutil.parser
import difflib
import inspect
import json
import logging
import os
import pytest
import queue
import re
import sys
import time
from urllib.parse import urlparse
from uuid import UUID

from fixtures.common_fixtures import fxBlockchain, fxConnection, fxHermesRunSettings
from suites import test_suite
from rest.request import Request
from rpc.rpc_call import RPC
import util.blockchain.eth
import util.numbers_strings

log = logging.getLogger(__name__)

def run_conc_time(concordContainer=None, args=""):
   '''
   Run the `conc_time` utility in a concord container. Appends the
   string `args` to the command. Returns the text output.

   '''
   if not concordContainer:
      concordContainer = util.blockchain.eth.get_concord_container_name(1)

   return util.blockchain.eth.exec_in_concord_container(concordContainer,
                                                        "./conc_time -o json {}".format(args))


def time_service_is_disabled():
   '''
   If we receive an error saying the time service is disabled, return
   true. Useful for skipping tests that need the time service to be
   enabled.
   '''
   output = run_conc_time()

   # text format uses snake_case, json format uses camelCase
   if re.findall("error_response", output) or re.findall("errorResponse", output):
      return bool(re.findall("Time service is disabled", output))


pytestmark = pytest.mark.skipif(time_service_is_disabled(), reason="Time service is disabled")

# What we expect time_pusher_period_ms to be, but in seconds.
expectedUpdatePeriodSec = 1

def iso_timestamp_to_seconds(timestamp):
   '''
   Convert ISO-8601 string to seconds.
   '''
   datetimeObj = dateutil.parser.isoparse(timestamp)
   return int(datetimeObj.timestamp())

def extract_samples_from_response(output):
   '''
   Dig through conc_time output to find a TimeResponse message, and
   extract samples from it. The conc_time script should have been
   exected with `-l` and `-o json` flags.
   '''
   responseText = re.findall("Received response: (.*)", output)[0]
   responseJson = json.loads(responseText)
   sampleList = responseJson["timeResponse"]["sample"]

   sampleMap = {}
   for sample in sampleList:
      text_source = b64decode(sample["source"]).decode("utf-8")
      sampleMap[text_source] = iso_timestamp_to_seconds(sample["time"])

   return sampleMap


def get_samples(concordContainer=None):
   '''
   Use the `conc_time` tool to read the latest state of the time contract.
   '''
   output = run_conc_time(concordContainer, "-l")
   return extract_samples_from_response(output)


def extract_time_summary_response(output):
   '''
   Dig through conc_time output to find a TimeResponse message, and
   extract the summary from it. The conc_time script should have been
   exected with `-g` and `-o json` flags.
   '''
   responseText = re.findall("Received response: (.*)", output)[0]
   responseJson = json.loads(responseText)
   return iso_timestamp_to_seconds(responseJson["timeResponse"]["summary"])


def get_summary(concordContainer=None):
   '''
   Use the `conc_time` tool to read the latest state of the time contract.
   '''
   output = run_conc_time(concordContainer, "-g")
   return extract_time_summary_response(output)


@pytest.mark.smoke
def test_cli_get():
   '''
   Attempt to use conc_time to get the current blockchain
   time. Passes if there is no error_response, and there is a
   time_response that contains a summary field filled with digits.
   '''
   concordContainer = util.blockchain.eth.get_concord_container_name(1)
   output = util.blockchain.eth.exec_in_concord_container(concordContainer, "./conc_time -g")

   assert re.findall("time_response", output), "No time response found: {}".format(output)
   assert re.findall("summary: ([:digit:]*)", output), "Summary with time not found: {}".format(output)


def test_low_load_updates():
   '''
   Try to observe time updates happening with no input being
   generated. It is assumed that all sources are configured to publish
   regularly, without transactions being sent to the system
   (i.e. `time_pusher_period_ms` is greater than zero for all
   sources). Note: if anything else is submitting transactions to the
   system, what is really being tested is that updates to time service
   happen at all, and not necessarily that the low-load system is
   functioning.
   '''
   concordContainer = util.blockchain.eth.get_concord_container_name(1)
   startTimes = get_samples(concordContainer)
   assert startTimes.keys(), "No sources found"

   # How many times we're going to query before giving up. This
   # shouldn't need to be larger than 2, if we sleep for
   # expectedUpdatePeriodSec between attempts, but using 3 for now for
   # extra stability.
   maxAttempts = 3

   # How often we want to remind the log that we're waiting, in
   # attempts. This is really only applicable if maxAttempts >
   # logPeriod, but including it now even with a smaller maxAttempts,
   # to fight spew before debugging beings.
   logPeriod = 5

   for attempt in range(1,maxAttempts+1):
      newTimes = get_samples(concordContainer)
      for k,v in newTimes.items():
         if startTimes[k] == newTimes[k]:
            if (attempt % logPeriod) == 1:
               log.info("Waiting for time samples to update... (attempt {})".format(attempt))
            time.sleep(expectedUpdatePeriodSec)
            break
      else:
         # if the loop exited without break-ing, we don't need to read again
         break

   assert len(newTimes.keys()) == len(startTimes.keys()), "All sources should be present in the update"
   for k,v in newTimes.items():
      assert startTimes[k] != newTimes[k], "All sources should have updated"


def test_time_is_recent():
   '''
   Test that the latest time from the time service is "recent". We'll
   defined recent as within 2x low-load update period. If the
   time-pusher threads are running, no sample should be much more than
   1 update period out of date. If 1/2 or more sources have just
   updated, time should be between now and 1 update period ago. If 1/2
   or more sources are just about to update, time should be between 1
   update period ago and 2 update periods ago. If the time-pusher
   threads can't get updates through within one period, we have a bug
   (performance or otherwise) that should be investigated.
   '''
   concordContainer = util.blockchain.eth.get_concord_container_name(1)
   currentHermesTimeBefore = int(time.time())
   output = util.blockchain.eth.exec_in_concord_container(concordContainer,
                                                          "./conc_time -g -o json")
   currentHermesTimeAfter = int(time.time())

   # Doing the extraction after the current time check, in an effort
   # to make the bounds as tight as possible. Since times are compared
   # in seconds, there is likely too much slop for this to be
   # necessary, but it's good practice anyway.
   currentServiceTime = extract_time_summary_response(output)

   # These comparisons are not safe if hermes and concord are not
   # running on the same hardware. If/when we run in that environment,
   # we'll need to add an acceptable skew value to the tests.
   assert currentServiceTime <= currentHermesTimeAfter, \
      "Time service cannot run ahead of the system clock"
   assert (currentServiceTime - currentHermesTimeBefore) <= 2 * expectedUpdatePeriodSec, \
      "Time service should be within 2x update period of system clock"


def test_time_service_in_ethereum_block(fxConnection):
   '''
   Test that the value stored as the timestamp in an ethereum block
   is [likely] sourced from the time service.
   '''
   concordContainer = util.blockchain.eth.get_concord_container_name(1)

   # Since low-load time updates may move the time service forward at
   # any moment, we check to make sure that a block's timestamp is
   # between the time service's state before and its state after the
   # block was created.
   preTxTime = get_summary(concordContainer)
   caller = "0x1111111111111111111111111111111111111111" # fake address

   # Create a contract that does nothing but STOP (opcode 00) when called.
   data = util.blockchain.eth.addCodePrefix("00")
   receipt = fxConnection.rpc.getTransactionReceipt(
      fxConnection.rpc.sendTransaction(caller, data))
   postTxTime = get_summary(concordContainer)

   block = fxConnection.rpc.getBlockByHash(receipt["blockHash"])
   blockTime = int(block["timestamp"], 16)

   assert preTxTime <= blockTime, "Block timestamp should be no earlier than pre-tx check"
   assert blockTime <= postTxTime, "Block timestamp should be no later than post-tx check"


def test_time_service_in_ethereum_code(fxConnection):
   '''
   Test that the time returned from executing a TIMESTAMP ethereum
   opcode is [likely] sourced from the time service.
   '''
   concordContainer = util.blockchain.eth.get_concord_container_name(1)

   # Since low-load time updates may move the time service forward at
   # any moment, we check to make sure that an execution's timestamp is
   # between the time service's state before and its state after the
   # call was executed.
   preTxTime = get_summary(concordContainer)

   # This contract just returns the ethereum timestamp when called.
   bytecode = "4260005260206000f3"
   #           ^ ^   ^ ^   ^   ^-- RETURN
   #           | |   | |   +-- PUSH1 0x00 (where the timestamp is stored)
   #           | |   | +-- PUSH1 0x20 (length of the timestamp)
   #           | |   +-- MSTORE (save the timestamp in memory)
   #           | +-- PUSH1 0x00 (where to store the timestamp)
   #           +-- TIMESTAMP
   caller = "0x1111111111111111111111111111111111111111" # fake address
   receipt = fxConnection.rpc.getTransactionReceipt(
      fxConnection.rpc.sendTransaction(caller, util.blockchain.eth.addCodePrefix(bytecode)))
   address = receipt["contractAddress"]

   contractTime = int(fxConnection.rpc.callContract(address), 16)
   postTxTime = get_summary(concordContainer)

   assert preTxTime <= contractTime, "Opcode time should be no earlier than pre-call check"
   assert contractTime <= postTxTime, "Opcode time should be no later than post-call check"


def ensureEnoughBlocksToTest(fxConnection, minBlocksToTest):
   '''
   Submit transactions, if necessary, to cause blocks to be created
   if there are not already minBlocksToTest available.
   '''
   latestBlockNumber = int(fxConnection.rpc.getBlockNumber(), 16)
   blocksToAdd = minBlocksToTest - latestBlockNumber
   if blocksToAdd > 0:
      log.info("Adding {} additional blocks for testing".format(blocksToAdd))
      for i in range(0, blocksToAdd):
         caller = "0x1111111111111111111111111111111111111111" # fake address
         data = util.blockchain.eth.addCodePrefix("f3")
         fxConnection.rpc.sendTransaction(caller, data)
      latestBlockNumber = int(fxConnection.rpc.getBlockNumber(), 16)
      assert latestBlockNumber >= minBlocksToTest
   return latestBlockNumber


def test_ethereum_time_does_not_reverse(fxConnection):
   '''
   Test that the timestamp in block N is always later than or equal
   to the timestamp in block N-1 (i.e. time never goes backward.
   '''
   minBlocksToTest = 100
   latestBlockNumber = ensureEnoughBlocksToTest(fxConnection, minBlocksToTest)

   blockNm1Time = int(fxConnection.rpc.getBlockByNumber(0)["timestamp"], 16)
   firstTrueNonZeroTimeBlock = 0 if blockNm1Time > 0 else -1
   firstTrueNonZeroTime = max(0, blockNm1Time)

   # let's not accidentally read the whole blockchain, if earlier
   # testing has added hundreds more blocks.
   maxBlocksToTest = min(minBlocksToTest * 2, latestBlockNumber)

   for n in range(1, maxBlocksToTest + 1):
      blockNTime = int(fxConnection.rpc.getBlockByNumber(n)["timestamp"], 16)
      assert blockNTime >= blockNm1Time, "Block N's timestamp should be >= block N-1's"
      blockNm1Time = blockNTime

      # Until the time service has received 1/2 of the sources'
      # samples, its value will be zero. Right at 1/2, on an even
      # number of sources, the time will be "now" / 2 (as the
      # definition of "median"). We really want the first block that
      # has a "real" time. We use the fact that it's not possible for
      # this test to run for 0.01 * (seconds since the UNIX epoch) to
      # weed out the ramp-up time.
      if firstTrueNonZeroTime == 0 and blockNTime >= 0.99 * time.time():
         firstTrueNonZeroTimeBlock = n
         firstTrueNonZeroTime = blockNTime

   log.info("Tested {}/{} blocks covering {} seconds".format(maxBlocksToTest-firstTrueNonZeroTimeBlock, maxBlocksToTest, blockNTime-firstTrueNonZeroTime))

   # If we're not testing that much, log a warning to encourage us to
   # expand this test.
   if latestBlockNumber - firstTrueNonZeroTimeBlock < minBlocksToTest:
      log.warn("Only {} blocks we really tested here.".format(latestBlockNumber - firstTrueNonZeroTimeBlock))

   # If all blocks are happening within one time-pusher window, log a
   # warning to encourage us to expand the test to cover more time.
   if blockNTime - firstTrueNonZeroTime < 2 * expectedUpdatePeriodSec:
      log.warn("Only {} seconds passed between first and last timestamped block".format(blockNTime - firstTrueNonZeroTime))
