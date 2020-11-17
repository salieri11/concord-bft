#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import certifi
import os
import requests
import traceback
import urllib

from util.alm.alm_constants import AlmApiRequestFields, AlmApiResponseFields

from lxml import etree

ALM_CERT_FILE = os.path.join(os.getcwd(), "alm_cert_bundle")
ALM_API_KEY = "not checking in"
VMBC_BASE_URL_V1 = "https://quality-api.eng.vmware.com:8443/QCIntgrt/rest/solutions/blockchain/"
VMBC_BASE_URL_V2 = "https://quality-api.eng.vmware.com/QCIntgr2/rest/rest.php/domains/solutions/projects/blockchain/"

class AlmRequest():
    def __init__(self):
        self._headers = {
            "Accept": "application/xml",
            "APIKey": ALM_API_KEY
        }

        self._cert_file = self._get_cert_file()


    def _get_cert_file(self):
        '''
        The intermediate Digicert certificate we need to for successful https connections
        with VMware's ALM server is not present in Ubuntu or downloaded by the Python
        "certifi" package (as of v2020.11.8).  So fetch it, create a new  file of certificates,
        and return a path to the new file.
        '''
        if not os.path.exists(ALM_CERT_FILE):
            with open(certifi.where(), "r") as f:
                cert_contents = f.read()

            with open(ALM_CERT_FILE, "w") as f:
                f.write(cert_contents)
                f.write(self._get_intermediate_cert())

        return ALM_CERT_FILE


    def _get_intermediate_cert(self):
        '''
        Returns the Digicert intermediate certificate which is needed to validate the certificate
        used by VMware's ALM server.  This was determined by looking at the certificate itself.
        Full list: https://www.digicert.com/kb/digicert-root-certificates.htm
        This is an external service.  If it becomes flaky, then keep it in source control. But it's
        better to always retrieve it in case Digicert revokes/recreates it.
        '''
        digicert_high_assurance_server_ca_url = "https://cacerts.digicert.com/DigiCertSHA2HighAssuranceServerCA.crt.pem"
        response = requests.get(digicert_high_assurance_server_ca_url)
        return response.content.decode("UTF-8")


    def _get_base_url(self, api_ver=2):
        '''
        :param api_ver: The version of the api
        :return: The appropriate base url
        '''
        if api_ver == 1:
            return VMBC_BASE_URL_V1
        elif api_ver == 2:
            return VMBC_BASE_URL_V2
        else:
            raise Exception("Unsupported ALM API version: '{}'".format(api_ver))


    def do_entity_get(self, sub_path, filter_exp=None, api_ver=2):
        '''
        Used for getting lists of entities, such as a list of test sets.
        Parses the result and returns a Python dict in the format (assuming
        we are fetching test sets for the sake of example):
        {
            'Test Set 1': {   'id': '301',
                              'parent-id': '3',
                              'status': 'Open',
                              'test_lab_path': '\\_Training_'},
           'Test set 2': {   'id': '402',
                             'parent-id': '3',
                             'status': 'Open',
                             'test_lab_path': '\\_Training_'},
        }
        :param sub_path: Everything needed after "/blockchain/" in the URL.
        :param filter_exp: An expression that can be used to filter the results.  See the API doc:
        https://confluence.eng.vmware.com/display/ALM/ALM+API+Version2#ALMAPIVersion2-GetTestInstances
        '''
        page_size = 100
        page_number = 0
        total_results = 1
        all_retrieved_items = {}

        while page_size * page_number < total_results:
            params = {
                AlmApiRequestFields.PAGE_SIZE: page_size,
                AlmApiRequestFields.PAGE_NUMBER: page_number + 1
            }

            if filter_exp:
                params[AlmApiRequestFields.FILTER] = filter_exp

            response_tree = self.send_request("get", sub_path, params, api_ver=api_ver)
            total_results = int(response_tree.get(AlmApiResponseFields.TOTAL_RESULTS))

            if total_results == 0:
                break

            page_number = int(response_tree.get(AlmApiResponseFields.PAGE_NUMBER))

            for elem in response_tree.iter(AlmApiResponseFields.ENTITY):
                fields = elem.findall(".//{}".format(AlmApiResponseFields.FIELD))
                key = None
                properties = {}

                for field in fields:
                    if field.get(AlmApiResponseFields.NAME) == "id":
                        key = field.find(AlmApiResponseFields.VALUE).text
                    else:
                        properties[field.get(AlmApiResponseFields.NAME)] = field.find(AlmApiResponseFields.VALUE).text

                test_lab_path_elem = elem.find(".//{}".format(AlmApiResponseFields.TEST_LAB_PATH))
                if test_lab_path_elem:
                    path = test_lab_path_elem.find(AlmApiResponseFields.VALUE).text
                    properties["test_lab_path"] = path

                all_retrieved_items[key] = properties

        return all_retrieved_items


    def send_request(self, action, sub_path, params, api_ver=2):
        '''
        Handle the mechanics of sending the request and analyzing a response.
        e.g. Response code check, ALM exception parsing, retries, etc...
        :param action: The request action, one of "get", "post", or "put"
        :param sub_path: Everything that you need after "/blockchain/" in the URL.
        :param params: Dict of request parameters, will be added to the URL or body per the action.
        :return: An XML etree of the response content.
        '''
        full_url = self._get_base_url(api_ver) + sub_path
        action = action.lower()
        response = None

        if action in ["get", "put"]:
            if params:
                # ALM API does not support a body for put.
                params = urllib.parse.urlencode(params)
                full_url += "?{}".format(params)

        log_msg = "send_request:\n  url:{}\n  params: {}".format(full_url, params)
        print(log_msg)
        error = None

        if action == "get":
            response = requests.get(full_url, verify=self._cert_file, headers=self._headers)
        elif action == "put":
            response = requests.put(full_url, verify=self._cert_file, headers=self._headers)
        elif action == "post":
            response = requests.post(full_url, params, verify=self._cert_file, headers=self._headers)
        else:
            error = "Invalid request action: '{}'".format(action)

        # Response evaluates to False if it is a 404, so explicitly compare to None.
        if not response == None and response.status_code > 399:
            error = "Received error response '{}', content: '{}'".format(response, response.content)

        if error:
            msg = "Error: '{}' when calling {}".format(error, log_msg)
            raise Exception(msg)

        return etree.XML(response.content)