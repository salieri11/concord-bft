#################################################################################
# Copyright 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#################################################################################
import pytest
from suites.case import describe


@pytest.fixture
@describe("fixture; sample")
def sampleFixture(request):
   '''
   A fixture is a resource that can be created per test suite,
   per module, etc...  The "request" item passed in is defined
   by PyTest and contains information about the test environment
   and test case.
   The default scope is per test case.  To change it, say to per
   module, define that in the decorator. e.g.

      @pytest.fixture(scope="module")
   '''
   return {"importantNumber": 123}

@describe("Should pass with expreesion; assert 1 + 1 = 2")
def test_example():
   '''
   This is a basic PyTest test which just does a Python assert:
   assert thing_to_check, "Error message when thing_to_check is False or None"
   Note that assert is a keyword, not a function, so don't write assert(foo, "bar").
   '''
   assert 1 + 1 == 2, "Expected 1 + 1 to equal 2"

@describe("testValue 123 should match the importantNumber value 123 in sampleFixture")
def test_example_with_fixture(sampleFixture):
   '''
   This is a PyTest test with a fixture.
   By providing "sampleFixture" as a parameter, PyTest will provide
   the result of running sampleFixture (above) for this test.  We can access
   a field as sampleFixture["importantNumber"].
   '''
   testValue = 123
   assert testValue == sampleFixture["importantNumber"], \
      "Expected the test value to be {}".format(sampleFixture["importantNumber"])
