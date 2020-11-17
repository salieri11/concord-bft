#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Beware of inconsistencies; use these constants!
# e.g.
#   "page-size" for requests, "PageSize" in responses.
#   Both "testID" and "test-id" being used in responses.
#########################################################################
class TestSetStates:
    '''
    States of an ALM test set
    '''
    ABORTED = "Aborted"
    COMPLETED = "Completed"
    RUNNING = "Running"
    TRIAGING = "Triaging"
    WAITING = "Waiting"


class TestRunRecordStates:
    '''
    States of a record of running a test instance.
    (AKA possible test result values.)
    '''
    BLOCKED = "Blocked"
    FAILED = "Failed"
    NOT_COMPLETED = "Not Completed"
    PASSED = "Passed"
    RUNNING = "Running"


class BuildTypes:
    '''
    Values for a build passed to ALM
    For builds not in buildweb, use "Other".
    '''
    OFFICIAL = "Official"
    OTHER = "Other"
    SANDBOX = "SandBox"


class AlmApiRequestFields:
    '''
    Names of parameters passed to ALM API calls.
    '''
    BUILD = "build"
    BUILD_TYPE = "buildType"
    FILTER = "filter"
    PAGE_NUMBER = "page-number"
    PAGE_SIZE = "page-size"
    STATUS = "status"
    TESTER = "tester"
    TEST_INSTANCE_ID = "testInstanceID"


class AlmApiResponseFields:
    '''
    Names of fields received in ALM API reponses.
    '''
    ENTITY = "Entity"
    FIELD = "Field"
    ID = "ID"
    NAME = "Name"
    PAGE_NUMBER = "PageNumber"
    PAGE_SIZE = "PageSize"
    TEST_ID = "testID"
    TEST_ID_2 = "test-id"
    TEST_LAB_PATH = "TestLabPath"
    TEST_SET_ID = "testSetID"
    TOTAL_RESULTS = "TotalResults"
    VALUE = "Value"
