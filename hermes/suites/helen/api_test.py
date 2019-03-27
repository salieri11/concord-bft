import inspect
import os
import pickle
import pytest

from suites import test_suite
from rest.request import Request

# The HelenAPITests object.
testObject = None

# Holds the results of the _getTests() method.
apiTests = []

# So the pytest fixture will display sensible names.
apiTestNames = []

# Ideally this would be a fixture.  However, fixtures and
# parametrize decorations don't play well together.  (There
# are threads about it.) So just brute force run this code
# and shove the info into global variables.  :(
with open("pickled_helen_api_tests", "rb") as f:
   testObject = pickle.load(f)
   apiTests = testObject._getTests()
   for test in apiTests:
      apiTestNames.append(test[0])

# Runs all of the tests from helen_api_tests.py.
@pytest.mark.parametrize("testName", apiTestNames)
def test_existing(testName):
   testLogDir = os.path.join(testObject._testLogDir, testName)
   request = Request(testLogDir,
                     testName,
                     testObject.reverseProxyApiBaseUrl,
                     testObject._userConfig)
   testFn = None
   for apiTest in apiTests:
      if testName in apiTest:
         testFn = apiTest[1]
   assert testFn, "Test named {} not found.".format(testName)
   testObject.setEthrpcNode()
   result, info = testFn(request)
   assert result, info
