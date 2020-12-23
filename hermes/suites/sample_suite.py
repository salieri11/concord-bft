#################################################################################
# Copyright 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#################################################################################
import pytest
from suites.case import describe
import util.hermes_logging
import random
from functools import lru_cache
from retry import  retry
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

@pytest.mark.jira("BC-4532", run=True)
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

@lru_cache(maxsize=10)
@retry(Exception, tries=4, delay=2, backoff=2)
def method_requiring_retries(msg):
    '''
    demonstrate use of retry and lru_cache
    retry - retries 5 times with delay of 1 sec between each retry
    retries are done on getting exception
    lru_cache returns already cached results
    '''
    try:
        roll_dice = random.randint(1, 5)
        log.info("Rolling dice, will try 5 times and \
            will succeed on 1 and 5 -  Dice Number: {}".format(roll_dice))
        if roll_dice > 1 and roll_dice < 5:
            e = "Exception Raised: Some random exception in Sample Suite - Ignore"
            raise Exception("Exception Raised: Some random exception in Sample Suite - Ignore")

    except Exception as e:
            log.info("Exception Raised: Some random exception in Sample Suite - Ignore")
            raise

    return(msg)


@describe("Simple Sample to demo retry")
@pytest.mark.parametrize("msg", ["Hi There", "Hello", "Welcome", "Hi There", "Welcome"])
def test_retry_example(msg):
    '''
    Demonstrates usage and use of retry
    '''
    try:
        msg = method_requiring_retries(msg)
        log.info("LRU Cache State: {}".format(method_requiring_retries.cache_info()))
        assert True, "Expected function to send a message"
    except Exception as e:
        assert True, "Exception...do not fail in sample "     