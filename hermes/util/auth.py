#########################################################################
# Copyright 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import json
import logging
import requests
import traceback
from datetime import datetime
from urllib3.util.retry import Retry

log = logging.getLogger(__name__)

CSP_STAGE_URL = "https://console-stg.cloud.vmware.com/csp/gateway/am/api/auth/api-tokens/authorize"

# For running tests (currently the admin user).  All tests are currently run
# as an admin.  That obviously won't be the case when testing roles and such.
DEV_ADMIN_TOKEN = "bMvxttSj3Q4vgpclari3QOSZn15zMnFhf3zl3Q7l3KbLFEcJ3zZ9AMduN0bI14Ge"

# For starting up the product and any other non-testing things.
HERMES_UTIL_TOKEN = "x2rCV21CJGglt3cULQH6d19T1rXZ6bHQAUSmWXgyghSRMei4qDd9XE4qi0y2Wl4r"

# The "tests" type is for running test cases.
# The "administrative" type is for test framework administrative work.
# This list will grow to include things like "consortium_user", "org_developer", etc...
TESTING_TOKEN = "testing"
UTILITY_TOKEN = "utility"
tokens = {
   TESTING_TOKEN: {
      "api_key": DEV_ADMIN_TOKEN,
      "access_token": None,
      "last_retrieved": 0
   },
   UTILITY_TOKEN: {
      "api_key": HERMES_UTIL_TOKEN,
      "access_token": None,
      "last_retrieved": 0
   }
}

# How long a VMC access token is valid, in seconds, which is 24 minutes as of July 2019.
# In seconds.
ACCESS_TOKEN_LIFESPAN = 24 * 60

# Once we provide an access token, it needs to last long enough for a test to use it. Try 10 min.
# The UI unit tests can take a while.
# In seconds.
TEST_BUFFER = 10 * 60

def getAccessToken(token_type=TESTING_TOKEN):
   '''
   Accepts a key from the tokens table.
   Returns an access token which will work for at least TEST_BUFFER seconds, retrieving
   a new one from CSP if necessary.
   '''
   if not token_type in tokens.keys():
      raise Exception("getAccessToken: Invalid token_type {}.  Choose one of {}" \
                      .format(token_type, tokens.keys()))

   if needNewAccessToken(token_type):
      log.info("getAccessToken() requesting a new access token.")
      new_token = retrieveNewAccessToken(token_type)
      tokens[token_type]["last_retrieved"] = datetime.now().timestamp()
      tokens[token_type]["access_token"] = new_token
   else:
      log.info("getAccessToken() using existing access token.")

   return tokens[token_type]["access_token"]


def needNewAccessToken(token_type):
   '''
   Accepts a key from the global tokens table.
   Returns whether enough time has passed that we should request a new access token from CSP.

   |---------------------------------------------------------------------------------|
                              Access token lifespan
                                                             Test case max run time
                                                           |-------------------------|
                                                           ^
                                      Token will be refreshed when now() == this time.
   '''
   if tokens[token_type]["last_retrieved"]:
      expiration = tokens[token_type]["last_retrieved"]
      expiration += ACCESS_TOKEN_LIFESPAN - TEST_BUFFER
      now = datetime.now().timestamp()
      log.debug("expiration: {}, now: {}".format(expiration, now))
      return expiration < now
   else:
      return True


def retrieveNewAccessToken(token_type):
   '''
   Given a token type, retrieves an access token from CSP.

   Retries for various error codes.  e.g. 104 ("Connection reset by peer") and various 500's.

   Reference:
   https://urllib3.readthedocs.io/en/latest/reference/urllib3.util.html#module-urllib3.util.retry

   Wait time = {backoff factor} * (2 ** ({number of attempts} - 1)),
   up to Retry.BACKOFF_MAX, which is 120 at this time.

   7 tries with backoff_factor of 0.5 = 63.5 seconds of retrying.
   '''
   api_key = tokens[token_type]["api_key"]
   session = requests.Session()
   num_retries = 7
   retries = Retry(total=num_retries,
                   backoff_factor=0.5,
                   status_forcelist=[104, 500, 502, 503, 504],
                   method_whitelist=False)
   session.mount('https://', requests.adapters.HTTPAdapter(max_retries=retries))

   try:
      response = session.post(CSP_STAGE_URL, data={"refresh_token": api_key})
   except Exception as e:
      log.error("***** Error communication with CSP. {} attempt(s) were made. Failing. *****\n".format(num_retries))
      raise(e)

   content = response.content.decode("UTF-8")
   csp_auth_data = json.loads(content)
   log.debug('CSP Authorize API Response: {0}'.format(str(csp_auth_data)))
   return csp_auth_data['access_token']
