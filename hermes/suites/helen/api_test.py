import inspect
import logging
import os
import pickle
import pytest
import sys
import time
from uuid import UUID

from suites import test_suite
from rest.request import Request
from rpc.rpc_call import RPC

log = logging.getLogger(__name__)

# The HelenAPITests object.
suiteObject = None

# Holds the results of the _getTests() method.
apiTests = []

# So the pytest fixture will display sensible names.
apiTestNames = []

# Ideally this would be a fixture.  However, fixtures and
# parametrize decorations don't play well together.  (There
# are threads about it.) So just brute force run this code
# and shove the info into global variables.  :(
with open("pickled_helen_api_tests", "rb") as f:
   suiteObject = pickle.load(f)
   apiTests = suiteObject._getTests()
   for test in apiTests:
      apiTestNames.append(test[0])


@pytest.fixture(scope="module")
def restRequest(request):
   '''
   This returns a Request object.  The accepted parameter, "request",
   is an internal PyTest name and must be that.
   The name of the function is what we get in a test case.
   '''
   # Format: suites/helen/api_test.py::test_blockchains_fields (setup)
   longName = os.environ.get('PYTEST_CURRENT_TEST')
   shortName = longName[longName.rindex(":")+1:longName.rindex(" ")]
   testLogDir = os.path.join(suiteObject._testLogDir, shortName)
   actualRequest = Request(testLogDir,
                           shortName,
                           suiteObject.reverseProxyApiBaseUrl,
                           suiteObject._userConfig)
   return actualRequest


# Runs all of the tests from helen_api_tests.py.
@pytest.mark.parametrize("testName", apiTestNames)
def test_existing(testName):
   testLogDir = os.path.join(suiteObject._testLogDir, testName)
   request = Request(testLogDir,
                     testName,
                     suiteObject.reverseProxyApiBaseUrl,
                     suiteObject._userConfig)
   testFn = None
   for apiTest in apiTests:
      if testName in apiTest:
         testFn = apiTest[1]
   assert testFn, "Test named {} not found.".format(testName)
   suiteObject.setEthrpcNode()
   result, info = testFn(request)
   assert result, info


def test_blockchains_fields(restRequest):
   blockchains = restRequest.getBlockchains()
   idValid = False
   consortiumIdValid = False

   for b in blockchains:
      blockchainId = UUID(b["id"])
      consortiumId = UUID(b["consortium_id"])
      assert blockchainId, "'id' field is not a valid UUID."
      assert consortiumId, "'consortium_id' field is not a valid UUID."


def test_members_fields(restRequest):
   blockchains = restRequest.getBlockchains()
   result = restRequest.getMemberList(blockchains[0]["id"])

   assert type(result) is list, "Response was not a list"
   assert len(result) >= 1, "No members returned"

   for m in result:
      (present, missing) = suiteObject.requireFields(m, ["hostname", "status", "address",
                                                         "millis_since_last_message",
                                                         "millis_since_last_message_threshold",
                                                         "rpc_url" ])
      assert present, "No '{}' field in member entry.".format(missing)
      assert isinstance(m["hostname"], str), "'hostname' field in member entry is not a string"
      assert isinstance(m["status"], str), "'status' field in member entry is not a string"
      assert isinstance(m["address"], str), "'address' field in member entry is not a string"
      assert isinstance(m["millis_since_last_message"], int), \
         "'millis_since_last_message' field in member entry is not a string"
      assert isinstance(m["millis_since_last_message_threshold"], int), \
         "'millis_since_last_message_threshold field in member entry is not a string"
      assert isinstance(m["rpc_url"], str), "'rpc_url' field in member entry is not a string"
      assert m["rpc_url"] != "", "'rpc_url' field in member entry is empty string"
      assert not "rpc_cert" in m, "'rpc_cert' field should not be included if certs=true is not passed"


def test_members_rpc_url(restRequest):
   '''
   Test that the returned value for "rpc_url" is an ethrpc node.
   We'll do that by invoking the API. At the moment, Helen still
   supports the API (it is planned to be removed), so also verify
   that we aren't getting Helen's address back by ensuring a
   Helen-only API call fails.
   '''
   blockchains = restRequest.getBlockchains()
   result = restRequest.getMemberList(blockchains[0]["id"])
   ethrpcUrl = None

   for member in result:
      ethrpcUrl = member["rpc_url"]

      # Ensure we have a node that responds to our API.
      # Will throw an exception if not.
      rpc = RPC(restRequest.logDir,
                restRequest.testName,
                ethrpcUrl,
                suiteObject._userConfig)
      rpc.mining()

      # Ensure that the rpc_url isn't Helen.  This will give a 404
      # and throw an exception.
      invalidRequest = Request(restRequest.logDir,
                               restRequest.testName,
                               ethrpcUrl + "blockchains/local",
                               suiteObject._userConfig)
      try:
         result = invalidRequest.getBlockList()
         assert False, "An exception should have been thrown when asking an ethrpc node for blocks."
      except Exception as e:
         # There are of course various reasons a 404 could be returned.  But let's at least
         # be sure we got back 404 for the given path, indicating this call is not available.
         assert "Not Found" in str(e) and "/blockchains/local/api/concord/blocks" in str(e), \
            "Expected a 404 error about calling 'blocks'."


def test_members_hostname(restRequest):
   '''
   Verify the "hostname" fields are "replica1", "replica2", ...
   '''
   blockchains = restRequest.getBlockchains()
   result = restRequest.getMemberList(blockchains[0]["id"])
   nodeCount = len(result)
   hostNames = []

   for nodeData in result:
      hostNames.append(nodeData["hostname"])

   for i in range(0, nodeCount):
      findMe = "replica" + str(i)
      assert findMe in hostNames, "Could not find host {} in the response.".format(findMe)
      hostNames.remove(findMe)

   assert len(hostNames) == 0, "Hosts not returned in the response: {}".format(hostNames)


def test_members_millis_since_last_message(restRequest):
   '''
   Pause a node, get sleep time millis, and make sure it is at least as long as we slept.
   Unpause it, and make sure it decreased.
   The numbers are not exact, but we're not testing concord.  We're just
   testing that Helen is receiving/communicating new values, not always
   showing a default, etc...
   '''
   blockchains = restRequest.getBlockchains()
   blockchainId = blockchains[0]["id"]
   allMembers = restRequest.getMemberList(blockchainId)
   nodeData = allMembers[0] # Any will do.
   hostName = nodeData["hostname"]
   concordIndex = int(hostName[len("replica"):]) + 1 # replica0 == concord1
   testTime = 0
   sleepTime = 5
   expectedMinimum = sleepTime * 1000

   try:
      suiteObject._resumeMembers(allMembers)

      log.info("Pausing concord{}".format(concordIndex))
      paused = suiteObject.product.pause_concord_replica(str(concordIndex))
      assert paused, "Unable to pause the container.  Hostname: {}, concord #: {}". \
         format(hostName, concordIndex)
      time.sleep(sleepTime)

      result = restRequest.getMemberList(blockchainId)
      for nodeData in result:
         if nodeData["hostname"] == hostName:
            testTime = int(nodeData["millis_since_last_message"])
            break

      assert testTime > expectedMinimum, "Expected millis_since_last_message of " \
         "at least {}, got {}.".format(expectedMinimum, testTime)

      log.info("Resuming concord{}".format(concordIndex))
      resumed = suiteObject.product.resume_concord_replica(str(concordIndex))
      assert resumed, "Unable to resume the container.  Hostname: {}, concord #: {}". \
         format(hostName, concordIndex)

      result = restRequest.getMemberList(blockchainId)
      assert len(result) > 0, "No members returned"

      for nodeData in result:
         if nodeData["hostname"] == hostName:
            testTimeResumed = int(nodeData["millis_since_last_message"])
            assert testTimeResumed < testTime, "Expected millis_since_last_message " \
               "to be less than {}, received {}.".format(reportedSilentTime, finalTime)
   finally:
      suiteObject._resumeMembers(allMembers)
