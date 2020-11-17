#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
from util.alm.alm_req_base import AlmRequest
from util.alm.alm_constants import TestRunRecordStates, BuildTypes, AlmApiRequestFields, AlmApiResponseFields

class AlmTestRunRecord(AlmRequest):
    def __init__(self, test_instance_id, build, status=TestRunRecordStates.RUNNING, ldap_user=None,
                 build_type=BuildTypes.OTHER, bug_ids=None, run_name=None, logs_location=None, custom_fields=None):
        '''
        A test run record is a record for running a test instance.  Context:
        - Test case: Abstract.  Contains steps to run, etc..., but one cannot do anything with it.
        - Test instance: When a test case is added to a test set, actually a "test instance" of the test case is
        created and added to the test set.  A test instance is a concrete thing which can be run.
        - Test run record: When a test instance is run, information such as pass/fail, build, etc... is stored in a
        test run record.  A test run record holds information for running a SINGLE test instance ONE time.

        This constructor accepts everything that ALM does.  In reality, we only use a subset of the fields.  All
        fields are included for reference and future-proofing.
        :param test_instance_id: The ID of the instance of a test case in the test set. (NOT the test case ID)
        :param status: The status of the running of this test.  This is one of alm_constants.TestRunRecordStates.
        :param ldap_user: Who is running it. Will default to an ldap service account if necessary.
        :param build: Build number
        :param build_type: One of alm_constants.BuildTypes.  Will be "Other" because we do not use Buildweb.
        :param bug_ids: From Bugzilla, N/A for us because we use Jira.
        :param run_name: The name of the run. Here, maybe we could use Jenkins run + Jenkins job #.
        :param logs_location: We are not using this.  ALM will create a logs directory if we do not provide one
        :param custom_fields: We are not using this.
        Creates a test run record set to "Running".
        '''
        super().__init__()
        self._tester = ldap_user
        self._test_instance_id = test_instance_id
        self._build = build
        self._build_type = build_type
        self._status = status
        self._tester = ldap_user

        params = {
            AlmApiRequestFields.TESTER: self._tester,
            AlmApiRequestFields.TEST_INSTANCE_ID: self._test_instance_id,
            AlmApiRequestFields.STATUS: self._status,
            AlmApiRequestFields.BUILD: self._build,
            AlmApiRequestFields.BUILD_TYPE: BuildTypes.OTHER
        }

        # Maybe we'll use this.
        # if run_name:
        #     params["runName"] = run_name

        root = self.send_request("post", "run", params, api_ver=1)

        # Pick up new pieces of info we do not already know.  We could have passed some of these in, but they are
        # conveniently here now.
        for element in root.iter():
            if element.tag == AlmApiResponseFields.ID:
                self._test_run_record_id = element.text
            elif element.tag == AlmApiResponseFields.TEST_ID:
                self._test_id = element.text
            elif element.tag == AlmApiResponseFields.TEST_SET_ID:
                self._test_set_id = element.text


    def update_run(self, status):
        '''
        The PutUpdateRun API call can accept the fields passed in when creating a test run record, but for clarity
        we are first creating one as "Running", then updating its status (Passed, Failed, etc...) with PutUpdateRun
        when the test has finished.
        :param status: The new status, one of TestRunRecordStates.
        :return: Nothing
        '''
        params = {
            AlmApiRequestFields.TESTER: self._tester,
            AlmApiRequestFields.STATUS: status
        }

        sub_path = "run/{}".format(self._test_run_record_id)
        self.send_request("put", sub_path, params, api_ver=1)
