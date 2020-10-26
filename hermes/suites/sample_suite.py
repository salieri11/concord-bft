#################################################################################
# Copyright 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#################################################################################
import pytest
from suites.case import describe
import util.hermes_logging
log = util.hermes_logging.getMainLogger()


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
        "Expected the test value to be {}".format(
            sampleFixture["importantNumber"])


def stop_primary_node():
    log.info("Demo... stop_primary_node")


def stop_non_primary_node(f=2):
    log.info("Demo.. stop_non_primary_node({})".format(f))


def do_transactions(msg):
    log.info("Push transactions...{}!!".format(msg))


def crash_primary():
    log.info("Demo... crash_primary")


def crash_non_primary():
    log.info("Demo.... crash non primary")


@pytest.mark.parametrize("stop_node_1, stop_node_2", [(stop_primary_node, stop_non_primary_node),
                                                      (stop_non_primary_node,
                                                       stop_primary_node),
                                                      (crash_primary,
                                                       crash_non_primary),
                                                      (crash_non_primary, crash_primary)])
@describe("Parameterization of test function")
def test_parameterize_example(stop_node_1, stop_node_2):
    '''
    This is a sample test case to demonstrate 'parametrize'.
    '''
    first_stop_node = stop_node_1
    execute_transactions = do_transactions
    next_stop_node = stop_node_2

    first_stop_node()
    execute_transactions("now")
    next_stop_node()
    execute_transactions("again")
    assert True, "Expected to  execute functions"
