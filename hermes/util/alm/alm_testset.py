#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
from util.alm.alm_req_base import AlmRequest
from util.alm.alm_constants import AlmApiResponseFields, AlmApiRequestFields

class AlmTestSet(AlmRequest):
    def __init__(self, product_version_folder, suite):
        super().__init__()
        self._product_version_folder = product_version_folder
        self._suite = suite
        self._test_set_id = self._get_test_set()
        self._test_instances = None


    def _get_test_set(self):
        '''
        A "test set" is a test suite (from testSuites.yml).
        :param product_version: The product version
        :param suite: The test suite
        :return: The test set ID
        '''
        leaf = self._product_version_folder.split("\\")[-1]
        folders = self.do_entity_get("test-set-folders", "{{name[{}]}}".format(leaf))
        folder_id = None
        for k in folders:
            if folders[k]["test_lab_path"] == self._product_version_folder:
                folder_id = k
        assert folder_id, "Failed to find a folder named {}".format(self._product_version_folder)

        test_set = self.do_entity_get(
            "test-sets",
            "{{name['{}'];parent-id[{}]}}".format(self._suite, folder_id))
        test_set_id = list(test_set.keys())[0]
        return test_set_id


    def get_test_instances(self):
        '''
        :return: Dict of test instances for this test set.
        '''
        if not self._test_instances:
            self._test_instances = self.do_entity_get("test-instances", "{{cycle-id[{}]}}".format(self._test_set_id))

        return self._test_instances


    def verify_required_tests_present(self, required_test_ids):
        '''
        :param test_ids: List of test IDs (not instances) we expect to be present.
        :return: Whether the tests we plan to run are all present.
        Does NOT check that all tests in the test set are part of test_ids.  What
        matters is whether the test automation framework will be able to update
        results for tests it runs.  We do not mind if some tests in a test set
        are being skipped.
        '''
        self.get_test_instances()

        # Quick check:
        if len(required_test_ids) > len(self._test_instances):
            return False

        for required_test_id in required_test_ids:
            found = False

            for test_instance in self._test_instances:
                if self._test_instances[test_instance][AlmApiResponseFields.TEST_ID_2] == required_test_id:
                    found = True
                    break

            if not found:
                return False

        return True


    def update_test_set_status(self, status):
        '''
        Updates the status of the test set.
        :param status: One of the values in TestSetStates
        '''
        params = {
            AlmApiRequestFields.STATUS: status
        }
        sub_path = "test-set/{}".format(self._test_set_id)
        response = self.send_request("put", sub_path, params, api_ver=1)
