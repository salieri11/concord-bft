#########################################################################
# Copyright 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import json
import logging
import requests
import time
import traceback
from datetime import datetime
from urllib3.util.retry import Retry

log = logging.getLogger(__name__)

CSP_STAGE_URL = "https://console-stg.cloud.vmware.com/csp/gateway/am/api/auth/api-tokens/authorize"

# This is a table of API keys used used for authentication with CSP.
# Format:
#
# tokens = {
#   "orgName": {
#     "userName": {
#       "roleDescription": {
#         "api_key": "The api key from CSP"
#         "access_token": Always None in code, updated with the access token at run time.
#         "last_retrieved": Always 0 in code, updated with the timestamp for access_token.
#     }
#   }
# }

tokens = {
   "blockchain_service_dev": {
      "admin-blockchain-dev": {
         "all_roles": {
            "api_key": "iRE68902qI9794DgIGgZpBwFWFdrZj48RGtLo0TgtB8IIRvKm724vCuFYVs7Pprd",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "HermesUtility": {
         "all_roles": {
            "api_key": "x2rCV21CJGglt3cULQH6d19T1rXZ6bHQAUSmWXgyghSRMei4qDd9XE4qi0y2Wl4r",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_con_admin": {
         "consortium_admin": {
            "api_key": "uSE3ZmTwUlOW8WzVr1OwFwr6JBVTKTvUE8FT4uQMMxZ0XsBXKJ57ds0ZcGUuLGvd",
            "access_token": None,
            "last_retrieved": 0
         },
         "consortium_admin_and_org_user": {
            "api_key": "NNQSY7jrz8DZIx0LG4Xm0nfaijD4gD7HJ5XupwDD7K5Y4h50hKGQOSzO2IWyeVqw",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_con_operator": {
         "consortium_operator": {
            "api_key": "6fuqGD8rSavtTWKjeqvu5evK7eq5PEaxuUavuAD62X1QYLjQoNuW5n514OVbv1pO",
            "access_token": None,
            "last_retrieved": 0
         },
         "consortium_operator_and_org_user": {
            "api_key": "EvhVlvU5iC3Qfv4xdCcHdHHfkiPk3WQ4yef3C4qt9258YDnc5CZ9SUf6QYrzpzHZ",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_con_participant": {
         "consortium_participant": {
            "api_key": "xGUOuiLC3pWpAlj19Fu5jo7i2k3Jx8SGpSKBAvQFTc0D43xMLhfLjQ2B5G3kmYr5",
            "access_token": None,
            "last_retrieved": 0
         },
         "consortium_participant_and_org_user": {
            "api_key": "5lNtnJaRPdq340q3x8amWhhu00B7O23FXqliE56WONUpTblYrhajS5R39out2bqw",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_org_admin": {
         "org_admin": {
            "api_key": "36qfWgfDp5BVJL5giRDsrR3FWi3jDO7Pt2Xon4to8vgbFPV48TUTQ6PNzvedKlPy",
            "access_token": None,
            "last_retrieved": 0
         },
         "org_admin_and_org_user": {
            "api_key": "v4BlKP8H7b9pmgt86N0R29ck16fZweLgjCOqcntBavjs5sja5AkVhRh9eD8VB2Dy",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_org_dev": {
         "org_dev": {
            "api_key": "2ix3PPyva43dP0365lMO6qcTCOCMmZdxgmCMb9bLDG0VBZu6IRHI2Rqitz7EljHS",
            "access_token": None,
            "last_retrieved": 0
         },
         "org_dev_and_org_user": {
            "api_key": "msoDqLwKet6WzX8K0oQuyCTGt6JXg9S9LvUWUG8fGq3t2wNeQn2TdgxT6pFcwsa6",
            "access_token": None,
            "last_retrieved": 0
         },
      },
      "vmbc_test_org_user": {
         "org_user": {
            "api_key": "F2jLad61RFMOurRlN2PN3MHvDOZTB424Ks8u95Ahf2Vt92KeNGL18LJQ9Lhq78Nv",
            "access_token": None,
            "last_retrieved": 0
         },
     },
      "vmbc_test_no_roles": { # CSP org member, but no VMware Blockchain service roles.
         "no_roles": {
            "api_key": "GVZXduPCUfU6MeG4BP5EEiL5ycDQUmxWc1ij65fQWUktK45xevvSrWtIB7XU64pq",
            "access_token": None,
            "last_retrieved": 0
         },
     },
   },
   "hermes_org0": {
      "admin-blockchain-dev": {
         "all_roles": {
            "api_key": "8uA3ZxIBG0U1IysRu50F1XRiqUXHhjPZqiT7jQNKelQQASfQmFAyexrpqvH1X152",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_con_admin": {
         "consortium_admin": {
            "api_key": "l3hV2IGdkqRF28CrKgujx32zjc9xA1RU98MwNlF1GiaAYtTwTaYJrYvFBpjhNd8Q",
            "access_token": None,
            "last_retrieved": 0
         },
         "consortium_admin_and_org_user": {
            "api_key": "OYsKWiY7equsTyN1N1HL4xFpVuQbKRkXVXaLp6Q8pDwFisJtYoLZEppvTFRA5btD",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_con_operator": {
         "consortium_operator": {
            "api_key": "WhEWpiqFi36E5Qy855s6BwL13GgVqC1EMXini2xkofAvR6mJE6tRQMHNW1HOukOW",
            "access_token": None,
            "last_retrieved": 0
         },
         "consortium_operator_and_org_user": {
            "api_key": "jVkh9X6HYYbxERd1I01DEy2glgwKJj4zSyH8x2FyV5DtM4v951LjFxCoDAzyaSPC",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_con_participant": {
         "consortium_participant": {
            "api_key": "nSivBW33y4rJINGEEd2P9AbaHt2FLHXEmEl8I1wXjuHSjWzr8DWrnqJleq8nFDf4",
            "access_token": None,
            "last_retrieved": 0
         },
         "consortium_participant_and_org_user": {
            "api_key": "70Yin0x3pJfl9Jxla0Ou0DyYECtTUSu62cr6kcLCjRkZftvGjQlFyPvsD3KFtmTH",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_org_admin": {
         "org_admin": {
            "api_key": "qcF6usiw9OeS6mEgUj8BWZUAp4hN6VUjuBrrTzM1Kq4v3Wk9FIHD5DKf8zMa2uZO",
            "access_token": None,
            "last_retrieved": 0
         },
         "org_admin_and_org_user": {
            "api_key": "ATj9NhPyBOGEzHid16G1kv1KRsyV9f74MJeoO807rHq9b0bHrREhbW30F9LgpLKU",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_org_dev": {
         "org_dev": {
            "api_key": "8X1A5bbLknIlm1ankT61mQN7wDbA2XcblJCopnMB6gC8vDOEZxtxswwbomxT02zc",
            "access_token": None,
            "last_retrieved": 0
         },
         "org_dev_and_org_user": {
            "api_key": "keZFJNRnaGLcZu4U6XJUFVXUpjYBciLfzJNrzohqRZg631iPbr9WXe1FWg5O1b5C",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_org_user": {
         "org_user": {
            "api_key": "yUuHpPQ1LPKwEOAmRPu1Y4kD3a5G0882kIWeJsGmOM2dTtEkQ5Qo8K4LR5doIwFa",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_no_roles": { # CSP org member, but no VMware Blockchain service roles.
         "no_roles": {
            "api_key": "MVXk3rTP25PjT5CZBC4dg4yGumW63QtyYAcAl81Ui68lO52S6gi7SH1imx8FtXfs",
            "access_token": None,
            "last_retrieved": 0
         }
     }
   },
   "hermes_org1": {
      "vmbc_test_con_admin": {
         "consortium_admin": {
            "api_key": "pZkY7Ci2dfWTX0utmcDHnJHrg3lb2kNcDQMqE0xy8Sa5NESMK7bVadEiiuHQxM6s",
            "access_token": None,
            "last_retrieved": 0
         },
         "consortium_admin_and_org_user": {
            "api_key": "Bc4JYwmNPLZcK8Gc2EV9d0zK5ehKfBCoID92XRN5EyZPrnEpKNa8Rvv54N1tuspG",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_con_operator": {
         "consortium_operator": {
            "api_key": "aBW9WLZezqn1aU5LVaUJoM6MsnW2hXRhKrDC3p6bdFfDmV1NivfQ5sB79mYfUc2c",
            "access_token": None,
            "last_retrieved": 0
         },
         "consortium_operator_and_org_user": {
            "api_key": "eva0j7MfD9AxBuwVLe5JGOj27aIPHL8Bnuf4CEZ07ICzwEWCH4MXVwCZeBgSVuSo",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_con_participant": {
         "consortium_participant": {
            "api_key": "X7kdYGKP6nZjsFiU7qx48t8kiZzR1oYEX4p10PYZIPsKIovmJ49586K0d7BRB3Qp",
            "access_token": None,
            "last_retrieved": 0
         },
         "consortium_participant_and_org_user": {
            "api_key": "SMEIJxSw6nr2f4ZKWMiSfDW8h8uIc1KW7q4dn8t76rIYLCakN5Mg2pdLYSB1iq9c",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_org_admin": {
         "org_admin": {
            "api_key": "YFpBVt4obvlFIsyUS5sJcnoqbCwImz1LVAV2zVx7Lx84N0aa8QWXJoW2Lh5ryAIF",
            "access_token": None,
            "last_retrieved": 0
         },
         "org_admin_and_org_user": {
            "api_key": "Z3sOUaT2FWVepL2Lm9W6M3eshLfLo5TwrlhJyVA1uB7g86sBcBKJbxHVera0ODll",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_org_dev": {
         "org_dev": {
            "api_key": "y9eyEUNvFl7JJBCTzaoyM2bjxfsOJ887UDHwag94hDzAi08gJ4PNCXdV7PNl7kgf",
            "access_token": None,
            "last_retrieved": 0
         },
         "org_dev_and_org_user": {
            "api_key": "sRGX8uzmxPZcBCrXZSoxQ2ro7frAP9Y6fddGITl2ZQXOjxuvcCxvUavRmMg3tepn",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_org_user": {
         "org_user": {
            "api_key": "ZBkuNJc44D5vAYDdgfa5tTc6e9o64TtpvMTmlFZZqpQAz2lrZoYVRrMoN2bupNZr",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_no_roles": { # CSP org member, but no VMware Blockchain service roles.
         "no_roles": {
            "api_key": "MuX6xzo4XIfaX6WL8VC1wjSjDK9xPxWplCV4Q9xYZCf0b27h01QIcWdnLQmn98KC",
            "access_token": None,
            "last_retrieved": 0
         }
     }
   }
}

orgs = {
   "blockchain_dev_service_org": "ab29b4a8-40c8-4173-ba74-15e76f8f35e0",
   "hermes_org0": "7e162c74-ae9f-40b4-98ab-89e13f8a2b78",
   "hermes_org1": "923a2597-ba3a-4ef8-a41c-a22406379eac",
   "hermes_org2": "33878dd7-9a3f-447e-b0eb-17d5ea7d3991"
}

# How long a VMC access token is valid, in seconds, which is 24 minutes as of July 2019.
# In seconds.
ACCESS_TOKEN_LIFESPAN = 24 * 60

# Once we provide an access token, it needs to last long enough for a test to use it. Try 10 min.
# The UI unit tests can take a while.
# In seconds.
TEST_BUFFER = 10 * 60

def getAccessToken(token_descriptor=None, force_refresh=False):
   '''
   Given a token descriptor, returns an access token which will work for at
   least TEST_BUFFER seconds, retrieving a new one from CSP if necessary.

   force_refresh forces retrieval of a new one.  This is the equivalent
   of a user logging out and logging back in so that changes can take
   effect.

   This is the token descriptor format.  The values come from the tokens
   table in this file.
   {
      "org": "blockchain_service_dev",
      "user": "HermesUtility",
      "role": "all_roles"
   }
   '''

   if token_descriptor:
      log.debug("Using token descriptor {}".format(token_descriptor))
   else:
      log.debug("Using the default API access for CSP.")
      token_descriptor = {
         "org": "blockchain_service_dev",
         "user": "admin-blockchain-dev",
         "role": "all_roles"
      }

   user = token_descriptor["user"]
   org = token_descriptor["org"]
   role = token_descriptor["role"]

   if not org in tokens:
      raise Exception("getAccessToken: Invalid org '{}'.  Choose one of {}" \
                      .format(org, list(tokens.keys())))

   if not user in tokens[org]:
      raise Exception("getAccessToken: Invalid user '{}' for org '{}'.  Choose one of {}" \
                      .format(user, org, list(tokens[org].keys())))

   if not role in tokens[org][user]:
      raise Exception("getAccessToken: Invalid role '{}' for user '{}' in org '{}'.  Choose one of {}" \
                      .format(role, user, org, list(tokens[org][user].keys())))

   token_object = tokens[org][user][role]

   if needNewAccessToken(org, user, role, force_refresh):
      log.debug("getAccessToken() requesting a new access token.")
      new_token = retrieveNewAccessToken(org, user, role)
      tokens[org][user][role]["last_retrieved"] = datetime.now().timestamp()
      tokens[org][user][role]["access_token"] = new_token
   else:
      log.debug("getAccessToken() using existing access token.")

   return tokens[org][user][role]["access_token"]


def needNewAccessToken(org, user, role, force_refresh):
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
   if force_refresh:
      return True
   elif tokens[org][user][role]["last_retrieved"]:
      expiration = tokens[org][user][role]["last_retrieved"]
      expiration += ACCESS_TOKEN_LIFESPAN - TEST_BUFFER
      now = datetime.now().timestamp()
      log.debug("expiration: {}, now: {}".format(expiration, now))
      return expiration < now
   else:
      return True


def retrieveNewAccessToken(org, user, role):
   '''
   Given org, user, and role, retrieves an access token from CSP.

   Retries for various error codes.  e.g. 104 ("Connection reset by peer") and various 500's.

   Reference:
   https://urllib3.readthedocs.io/en/latest/reference/urllib3.util.html#module-urllib3.util.retry

   Wait time = {backoff factor} * (2 ** ({number of attempts} - 1)),
   up to Retry.BACKOFF_MAX, which is 120 at this time.

   7 tries with backoff_factor of 0.5 = 63.5 seconds of retrying.
   '''

   api_key = tokens[org][user][role]["api_key"]
   # session = requests.Session()
   # num_retries = 7
   # retries = Retry(total=num_retries,
   #                 backoff_factor=0.5,
   #                 status_forcelist=[104, 500, 502, 503, 504],
   #                 method_whitelist=False)
   # session.mount('https://', requests.adapters.HTTPAdapter(max_retries=retries))

   # try:
   #    log.debug("In auth.py, retrieveNewAccessToken() sending request to CSP. Session: {}, " \
   #              "URL: {}, API key: {}".format(session, CSP_STAGE_URL, api_key))
   #    response = session.post(CSP_STAGE_URL, data={"refresh_token": api_key})
   #    log.debug("In auth.py, received response from CSP: {}".format(response))
   # except Exception as e:
   #    log.error("***** Error communication with CSP. {} attempt(s) were made. Failing. *****\n".format(num_retries))
   #    raise(e)

   max_attempts = 5
   sleep_time = 5
   attempts = 0
   success = False

   while not success and attempts < max_attempts:
      attempts += 1
      response = requests.post(CSP_STAGE_URL, data={"refresh_token": api_key})

      if response.status_code != 200:
         log.debug("Response code received from CSP: {}.".format(response.status_code))

         if attempts == max_attempts:
            raise Exception("Never received a good response from CSP.")
         else:
            log.debug("Retrying in {} seconds".format(sleep_time))
            time.sleep(sleep_time)
      else:
         success = True

   content = response.content.decode("UTF-8")
   csp_auth_data = json.loads(content)
   log.debug('CSP Authorize API Response: {0}'.format(str(csp_auth_data)))
   return csp_auth_data['access_token']
