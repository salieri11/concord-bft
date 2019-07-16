#########################################################################
# Copyright 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import requests
import logging
import traceback

log = logging.getLogger(__name__)

# For running tests (currently the admin user).  All tests are currently run
# as an admin.  That obviously won't be the case when testing roles and such.
DEV_ADMIN_TOKEN = "bMvxttSj3Q4vgpclari3QOSZn15zMnFhf3zl3Q7l3KbLFEcJ3zZ9AMduN0bI14Ge"

# For starting up the product and any other non-testing things.
HERMES_UTIL_TOKEN = "EukbnBJ5tn46syIVDKk0okV1R21agJHqqyo2OYWE4Ew8P92UW5vFVtMI1OaulS24"

TESTING_TOKEN = "testing"
UTILITY_TOKEN = "utility"
refresh_tokens = {
   TESTING_TOKEN: DEV_ADMIN_TOKEN,
   UTILITY_TOKEN: HERMES_UTIL_TOKEN
}

def getAccessToken(token_type=TESTING_TOKEN):
   '''
   Accepts a token_type of "tests" or "administrative".  The "tests" type is for running
   test cases.  The "administrative" type is for test framework administrative work.
   This list will grow.
   Returns the access token from CSP.
   '''
   if not token_type in refresh_tokens.keys():
      raise Exception("getAccessToken: Invalid token_type {}.  Choose one of {}" \
                      .format(token_type, refresh_tokens.keys()))
   token = refresh_tokens[token_type]
   stage_url = "https://console-stg.cloud.vmware.com/csp/gateway/am/api/auth/api-tokens/authorize"
   csp_auth_data = requests.post(stage_url, data={"refresh_token": token}).json()
   log.debug('CSP Authorize API Response: {0}'.format(str(csp_auth_data)))
   return csp_auth_data['access_token']
