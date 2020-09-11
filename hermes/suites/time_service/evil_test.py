#########################################################################
# Copyright 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# "Evil" Time Service tests (i.e. the ones where things go wrong
# intentionally). Some of these duplicate checks done in Concord's
# TimeContractTests unit tests, but it's good to check that the
# behavior of the state machine works with concord-bft's expectations
# as well.
#########################################################################
import collections
import difflib
import inspect
import json
import logging
import os
import pytest
import queue
import random
import re
import sys
import time
from urllib.parse import urlparse
from uuid import UUID

from fixtures.common_fixtures import fxBlockchain, fxConnection, fxProduct
from suites import test_suite
from suites.case import describe, passed, failed
from rest.request import Request
from rpc.rpc_call import RPC
import util.blockchain.eth
import util.numbers_strings

from suites.time_service.basic_test import time_service_is_disabled, run_conc_time, get_samples, extract_samples_from_response, extract_time_summary_response

import util.hermes_logging
log = util.hermes_logging.getMainLogger()

pytestmark = pytest.mark.skipif(time_service_is_disabled(), reason="Time service is disabled")

# What we expect time_pusher_period_ms to be, but in seconds.
expectedUpdatePeriodSec = 1

def max_faulty_clocks():
   '''
   Find out how many sources we expect to be able to tolerate faults
   in. For the implementation based on median, this is less than 1/2.
   '''
   sampleCount = len(get_samples())
   return sampleCount // 2 - (1 if (sampleCount / 2 == sampleCount // 2) else 0)

def get_summary_and_samples():
   '''
   Read both the summary time and all samples.
   '''
   output = run_conc_time(args="-g -l")
   return extract_time_summary_response(output), extract_samples_from_response(output)

def find_faulty_clocks():
   '''
   Look for clocks that are far in the future, or stuck in the past.
   '''
   start_summary, start_samples = get_summary_and_samples()

   # We are guaranteed to see enough updates from non-faulty nodes in
   # three update periods.
   time.sleep(expectedUpdatePeriodSec*3)

   end_summary, end_samples = get_summary_and_samples()

   slow_sources = []
   healthy_sources = []
   fast_sources = []

   for source, end_time in end_samples.items():
      if end_time < start_summary:
         slow_sources.append(source)
      elif start_samples.get(source) > end_summary:
         fast_sources.append(source)
      else:
         healthy_sources.append(source)

   return slow_sources, healthy_sources, fast_sources


@describe()
def test_publish_as_other(fxBlockchain):
   '''
   Make one node attempt to publish time as another, and make sure
   that the sample is rejected.
   '''
   samples = get_samples()

   target = random.choice(list(samples.keys()))
   original_target_value = samples.get(target)

   # Attempt to move the target a minute into the future. This test
   # should take far less time, so the low-load update thread will not
   # beat us. Value is truncated to integer here, because conc_time
   # only accepts an integer sample value.
   test_target_value = int(original_target_value) + 60

   log.info("Attempting to move {} to {} from {}".format(target, test_target_value, original_target_value))

   # Figuring out which container publishes which source is difficult,
   # so what we're going to do is attempt to publish as the target
   # source from each container, and later assert that only one of
   # them worked.
   successful_mod_count = 0
   unsuccessful_mod_count = 0
   for n in range(1, len(samples)+1):
      concord = util.blockchain.eth.get_concord_container_name(n)
      output = run_conc_time(concord, "-l --app /concord/config-local/application.config \
                                          --depl /concord/config-local/deployment.config \
                                          --secr /concord/config-local/secrets.config \
                                       --source {} -t {}".format(target, test_target_value))

      new_samples = extract_samples_from_response(output)
      if new_samples.get(target) == test_target_value:
         successful_mod_count += 1

         # make sure that we can detect further modifications
         test_target_value += 1
      else:
         assert new_samples.get(target) < test_target_value, "Time moved beyond our test bounds"
         unsuccessful_mod_count += 1

   assert successful_mod_count == 1, "Exactly one concord node should be able to publish as a given source"
   assert unsuccessful_mod_count > 1, "Nodes should not be able to publish as each other"


def find_source_container(source):
   '''
   Figure out which container is going to publish as the given time
   source. This is done by using conc_time to send a zero-time update,
   and then looking for which source it was going to use.
   '''
   containerIndex = 1
   container = util.blockchain.eth.get_concord_container_name(containerIndex)
   while container:
      output = run_conc_time(container, "--app /concord/config-local/application.config \
                                         --depl /concord/config-local/deployment.config \
                                         --secr /concord/config-local/secrets.config \
                                         -t 0 -n")
      if output.find(source) > 0:
         log.info("Found time source '{}' in container '{}'".format(source, container))
         return container
      else:
         containerIndex += 1
         container = util.blockchain.eth.get_concord_container_name(containerIndex)

   log.warning("Did not find source '{}' in any container".format(source))

def run_faulty_clock(faulty_time):
   '''
   Verify that corrupting one time source does not cause the time
   service's summary to drift from hermes system time. The value used
   for corrupting the time source is given by the parameter
   `faulty_time`, which should be a function that takes the source's
   current published time as an argument, and returns the value to
   publish for that source next.
   '''
   # If other tests already ran, one or more clocks may already be
   # fast or slow. We don't have a "reset" function yet, so let's work
   # with what we have, by re-corrupting already corrupt sources, if
   # necessary.
   slow_sources, healthy_sources, fast_sources = find_faulty_clocks()
   allowed_faults = max_faulty_clocks()

   if allowed_faults < 1:
      pytest.skip("More sources are required to test resilience.")

   if allowed_faults < len(slow_sources) + len(fast_sources):
      pytest.skip("Too many sources are already faulty")

   test_target_source = None
   if len(slow_sources) + len(fast_sources) < allowed_faults:
      # we can risk introducing an additional fault
      test_target_source = random.choice(slow_sources + healthy_sources + fast_sources)
   else:
      # we're already at our fault limit, so just recorrupt an already faulty source
      test_target_source = random.choice(slow_sources + fast_sources)

   if test_target_source in slow_sources:
      log.info("Re-corrupting a slow source: {}".format(test_target_source))
   elif test_target_source in fast_sources:
      log.info("Re-corrupting a fast source: {}".format(test_target_source))
   else:
      log.info("Corrupting a healthy source: {}".format(test_target_source))

   concordContainer = find_source_container(test_target_source)

   # How close do we expect the time summary to be to hermes' system
   # clock? If all sources are updating, then the difference should be
   # within one update period. We allow three update periods, to allow
   # for updates happening right around the maximal 2-period limit.
   differenceBound = 3 * expectedUpdatePeriodSec

   summary, samples = get_summary_and_samples()
   assert abs(summary - time.time()) <= differenceBound, \
      "Summary and system time are not close before corruption"

   # how many times to publish and check
   attempts = 10

   for attempt in range(1, attempts+1):
      test_target_time = faulty_time(samples.get(test_target_source))

      # If our update is far enough in the future, we expect that it
      # will be the value that we see in return.
      expectUpdate = test_target_time > samples.get(test_target_source) + expectedUpdatePeriodSec

      # If our update is in the past, we expect it will be ignored,
      # and we will not see it in the return.
      expectIgnore = test_target_time < samples.get(test_target_source)

      # Publish our skewed time. The '-s' parameter is not needed
      # here, but is a nice way to make sure that
      # find_source_container returned the correct response.
      output = run_conc_time(concordContainer, args="-g -l --app /concord/config-local/application.config \
                                                           --depl /concord/config-local/deployment.config \
                                                           --secr /concord/config-local/secrets.config \
                                                     --source {} -t {}".format(test_target_source, test_target_time))
      summary = extract_time_summary_response(output)
      samples = extract_samples_from_response(output)

      # Verify that our corruption actually happened, if we expected it to.
      assert (not expectUpdate) or samples.get(test_target_source) == test_target_time, \
            "Target source was not updated with test value."

      # Verify that our corruption was ignored, if we expected it to be.
      assert (not expectIgnore) or samples.get(test_target_source) != test_target_time, \
            "Target source was updated, but should not have been"

      # This is the core of this test: summary time should not differ
      # from the local wall clock if < 1/2 of the sources have been
      # corrupted.
      assert abs(summary - time.time()) <= differenceBound, \
         "Summary has moved away from hermes time."

      time.sleep(expectedUpdatePeriodSec / 2)


@describe()
def test_fast_clock(fxBlockchain):
   '''
   Publish time intentionally ahead of "now", and make sure that it
   doesn't affect block time.
   '''
   # start with a skew that is expected to be at least 10 low-load updates in the future
   forward_skew = 10 * expectedUpdatePeriodSec
   def skew_function(current):
      # If the test is publishing updates faster than our skew (which
      # it should), this should move the corrupted source into the
      # future faster and faster.
      return int(max(current, time.time())) + forward_skew

   return run_faulty_clock(skew_function)


@describe()
def test_random_clock(fxBlockchain):
   '''
   Publish intentionally random time, and make sure that it doesn't
   affect block time.
   '''
   # start with a narrow random window
   rand_window = expectedUpdatePeriodSec
   def skew_function(current):
      nonlocal rand_window
      lowerBound = int(max(current, time.time())) - rand_window
      upperBound = int(max(current, time.time())) + rand_window
      # double the size of the window every sample
      rand_window *= 2
      return random.randint(lowerBound, upperBound)

   return run_faulty_clock(skew_function)
