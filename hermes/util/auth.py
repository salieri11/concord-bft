#########################################################################
# Copyright 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import json
import logging
import requests
import time
import traceback
import util
from datetime import datetime
from urllib3.util.retry import Retry

import util.hermes_logging
log = util.hermes_logging.getMainLogger()
CSP_STAGE_URL = "https://console-stg.cloud.vmware.com/csp/gateway/am/api/auth/api-tokens/authorize"
CUSTOM_ORG = "custom_org"
CUSTOM_BLOCKCHAIN = "custom_blockchain"

# CSP roles
ROLE_ALL = "all_roles"
ROLE_NONE = "no_roles"
ROLE_CON_ADMIN = "consortium_admin"
ROLE_CON_OPERATOR = "consortium_operator"
ROLE_CON_PARTICIPANT = "consortium_participant"
ROLE_ORG_ADMIN = "org_admin"
ROLE_ORG_DEVELOPER = "org_dev"
ROLE_ORG_USER = "org_user"

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

# This is what Hermes currently uses for everything.  We're moving
# away from this.
internal_admin = {
   "org": "blockchain_service_dev",
   "user": "admin-blockchain-dev",
   "role": ROLE_ALL
}

# These new defaults are what Hermes tests are moving towards.
default_con_admin = {
   "org": "hermes_org0",
   "user": "vmbc_test_con_admin",
   "role": ROLE_CON_ADMIN
}

# These are all of the users we may use.
# Note that roles include the listed role and the org user (except the "no roles" one).
# To log into CSP to get a new refresh token, use the usernames below + "@csp.local".
# URL: https://console-stg.cloud.vmware.com
tokens = {
   CUSTOM_ORG: {}, # Populated when reading values from the user config file.
   "blockchain_service_dev": {
      "admin-blockchain-dev": {
         ROLE_ALL: {
            # Name in console-stg: Hermes Test Service 8 All Org and Service Roles Dec 2019
            # Never expires.
            "api_key": "e0HWXg5LHxJnvZxgBuHM6rXyq5rNwCY5Ppohyz1lZZiqygt1eXtS9HwVn7nPs3Vv",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "HermesUtility": {
         ROLE_ALL: {
            # Name in console-stg: Automation Tasks Dec 2019
            # Never expires.
            "api_key": "w80krrkPe49VfLaXYA0v0q9kC8tMHP2DTqf8S3hbCq6oohxwAN87S9xhu6wiFobY",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_con_admin": {
         ROLE_CON_ADMIN: {
            # Name in console-stg: Hermes Test Service 8 Con Admin Org User Dec 2019
            # Never expires.
            "api_key": "PIxfMlDc08murFMRd0Qn77Xe8vfDLa3q0JWN4b4mcfMFPndbSQXpnmcs1unz9jJC",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_con_operator": {
         ROLE_CON_OPERATOR: {
            # Name in console-stg: Hermes Test Service 8 Con Operator Org User Dec 2019
            # Never expires
            "api_key": "gLy7GtyqNuVSJbjFcZZDrgf9iEKUiBHCKu9hw4XLYMT5ZTAYhuH2wSeJKShX4l1v",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_con_participant": {
         ROLE_CON_PARTICIPANT: {
            # Name in console-stg: Hermes Test Service 8 Con Participant Org User Dec 2019
            # Never expires.
            "api_key": "k6FVZGiuJM53AeNUfiY47NCmtmjVDSSfk21rMiDedETB5Z0UCP1TrRKxMWl3hf1U",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_org_admin": {
         ROLE_ORG_ADMIN: {
            # Name in console-stg: Hermes Test Service 8 Org Admin Org User Dec 2019
            # Never expires.
            "api_key": "KA1zmDXyEsWjW29AlIZsx7m1ed2MucEXET0mzinke3xW66AMYPb2OduH4GlE3jCD",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_org_dev": {
         ROLE_ORG_DEVELOPER: {
            # Name in console-stg: Hermes Test Service 8 Org Dev Org User Dec 2019
            # Never expires.
            "api_key": "cZBXoBAZTjn2L4A6QRzfUzOMhmACsqd8132w6qxk3AMZ5SfMx11BzB7910dM947T",
            "access_token": None,
            "last_retrieved": 0
         },
      },
      "vmbc_test_org_user": {
         ROLE_ORG_USER: {
            # Name in console-stg: Hermes Test Service 8 Org User Dec 2019
            # Never expires.
            "api_key": "sWAnSCHEX56yU4YV0H2vqjAWHttBfy8Qo0kLroi51aAZxaP88Mwx6U7KlTkbUn6i",
            "access_token": None,
            "last_retrieved": 0
         },
     },
      "vmbc_test_no_roles": { # CSP org member, but no VMware Blockchain service roles.
         ROLE_NONE: {
            # Name in console-stg: Org Member No Service Roles Dec 2019
            # Never expires.
            "api_key": "xh3CeHTZuJgQUWpY9qx9o9BW5vV2Gb5gsfJQxdidykMHHjc2BPDfN0g2fOqJ51KF",
            "access_token": None,
            "last_retrieved": 0
         },
     },
   },
   "hermes_org0": {
      # Never used.  Allow expiration.
      # "admin-blockchain-dev": {
      #    ROLE_ALL: {
      #       "api_key": "8uA3ZxIBG0U1IysRu50F1XRiqUXHhjPZqiT7jQNKelQQASfQmFAyexrpqvH1X152",
      #       "access_token": None,
      #       "last_retrieved": 0
      #    }
      # },
      "vmbc_test_con_admin": {
         ROLE_CON_ADMIN: {
            # Name in console-stg: Hermes Test Service 8 Con Admin Org User Dec 2019
            # Never expires.
            "api_key": "DaI2xKybPAG73qD4nAsGf4DX1a0XTOeKafeTxUlxO2gqHMdyHQcXR0TBfcD77d25",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_con_operator": {
         ROLE_CON_OPERATOR: {
            # Name in console-stg: Hermes Test Service 8 Con Operator Org User Dec 2019
            # Never expires.
            "api_key": "TzkwJHIaxSMcZ5e9q8zAMmHFAG90js2fQrxOzaz89IYnT1iQGjK3GRZMJCUMbLlp",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_con_participant": {
         ROLE_CON_PARTICIPANT: {
            # Name in console-stg: Hermes Test Service 8 Con Participant Org User Dec 2019
            # Never expires.
            "api_key": "9G3JffeHuyo2JdGVsH79tyDSXu0rKricVElAk4LiCO5pGZc84Jx6pO3aAqxVCOXY",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_org_admin": {
         ROLE_ORG_ADMIN: {
            # Name in console-stg: Hermes Test Service 8 Org Admin Org User Dec 2019
            # Never expires.
            "api_key": "52t2yK7TUIo13y73PKmgH3pWqsV73KVguwrOyECkO0MpMhAWaYy3MCXFqbAvuOcL",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_org_dev": {
         ROLE_ORG_DEVELOPER: {
            # Name in console-stg: Hermes Test Service 8 Org Dev Org User Dec 2019
            # Never expires.
            "api_key": "35O0wbC1wPo7cLhjMvcNqu0q3xHxHLVQ4B3xsU66Vnbvkosz4lfVWrmR8G9S9M5S",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_org_user": {
         ROLE_ORG_USER: {
            # Name in console-stg: Hermes Test Service 8 Org User Dec 2019
            # Never expires.
            "api_key": "Gw1TSRuGwO5DalkU2837NQi3tt7XhEHxkcrAqaRCQe9k22h6JcE1e7EVyABUZoLC",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_no_roles": { # CSP org member, but no VMware Blockchain service roles.
         ROLE_NONE: {
            # Name in console-stg: Org Member No Service Roles Dec 2019
            # Never expires.
            "api_key": "84DPf07PrtliL2n9h5oT06YXIDA7TjLXKLSdg7gqX4kr59jPiuReeZ0F8DtaEvEe",
            "access_token": None,
            "last_retrieved": 0
         }
     }
   },
   "hermes_org1": {
      "vmbc_test_con_admin": {
         ROLE_CON_ADMIN: {
            # Name in console-stg: Hermes Test Service 8 Con Admin Org User Dec 2019
            # Never expires.
            "api_key": "HLBXljv63RdMyjLvZwZGEafr8abl9CKJaCCXG6eveAkCGR9qFPylV1AhDzQRqN5S",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_con_operator": {
         ROLE_CON_OPERATOR: {
            # Name in console-stg: Hermes Test Service 8 Con Operator Org User Dec 2019
            # Never expires.
            "api_key": "blGX9p0ONAWpGXGNCP5TRztFOyJd0EKzozJ6GxIjegc63Qo9nTYQjbnveBkXswDj",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_con_participant": {
         ROLE_CON_PARTICIPANT: {
            # Name in console-stg: Hermes Test Service 8 Con Participant Org User Dec 2019
            # Never expires.
            "api_key": "Y2bzjzmHMAa9cWgL2qu30BQq2y42eDjhuP4PG4gBNxXBLKXX6UurM5WMtWROVYBb",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_org_admin": {
         ROLE_ORG_ADMIN: {
            # Name in console-stg: Hermes Test Service 8 Org Admin Org User Dec 2019
            # Never expires.
            "api_key": "Qar716qOXFg44q7Z5G4H0Z3kgFuAVTFzzIld2nVc6vD5Gvvzyr571skOOgPqgCeq",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_org_dev": {
         ROLE_ORG_DEVELOPER: {
            # Name in console-stg: Hermes Test Service 8 Org Dev Org User Dec 2019
            # Never expires.
            "api_key": "nApqcLUw93vaTmjBW3rE0XisOA35s0i1AWf18MkXFE6ebKHQWTC49oXpGNIZYE45",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_org_user": {
         ROLE_ORG_USER: {
            # Name in console-stg: Hermes Test Service 8 Org User Dec 2019
            # Never expires.
            "api_key": "Jkf04CwGcB9AwQ5TET68mMZg1uVudKAh55z8MmW02UN26Q3LRNw7RgrBDzZ2fp2I",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_no_roles": { # CSP org member, but no VMware Blockchain service roles.
         ROLE_NONE: {
            # Name in console-stg: Org Member No Service Roles Dec 2019
            # Never expires.
            "api_key": "qAjc75BI1V3XxXy9z4zrwTBQLCSNATEUtDdE4w7UjQgAJm45VEr3MeZsYhwuAhZT",
            "access_token": None,
            "last_retrieved": 0
         }
     }
   },
   "hermes_api_test_org": {
         "vmbc_test_org_admin": {
            ROLE_ORG_ADMIN: {
               # Name in console-stg: hermes-test-org-token
               # Last updated: Mar. 26, 2020.
               # Expires Sep 22, 2020.
               "api_key": "6nwaP6VuLdezUKX0l0iCufO9GdDmCg50ifrMCzE1F6ovpogjZGY61KjYdjBV7UXo",
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
   "hermes_org2": "33878dd7-9a3f-447e-b0eb-17d5ea7d3991",
   "hermes_api_test_org": "adc86e8a-b257-49a8-9a7f-d237570d2a4f",
   "custom_org": ""
}

# How long a VMC access token is valid, in seconds, which is 24 minutes as of July 2019.
# In seconds.
ACCESS_TOKEN_LIFESPAN = 24 * 60

# Once we provide an access token, it needs to last long enough for a test to use it. Try 10 min.
# The UI unit tests can take a while.
# In seconds.
TEST_BUFFER = 10 * 60

def readUsersFromConfig(userConfig):
   '''
   If there are valid users in the json config file, add them to the tokens
   table in their own new org.
   '''
   new_org = {}

   if CUSTOM_ORG in userConfig["product"] and \
      "id" in userConfig["product"][CUSTOM_ORG] and \
      userConfig["product"][CUSTOM_ORG]["id"]:

      global orgs
      orgs[CUSTOM_ORG] = userConfig["product"][CUSTOM_ORG]["id"]
      del(userConfig["product"][CUSTOM_ORG]["id"])

      for user in userConfig["product"][CUSTOM_ORG]:
         user_data = userConfig["product"][CUSTOM_ORG][user]
         for role in user_data:
            if "api_key" in user_data[role] and user_data[role]["api_key"]:
               new_org[user] = {
                  role: {
                     "api_key": user_data[role]["api_key"],
                     "access_token": None,
                     "last_retrieved": 0
                  }
               }

      if new_org:
         global tokens
         log.info("Found an org in the Hermes config json file: {}".format(json.dumps(new_org, indent=2)))
         util.helper.mergeDictionaries(tokens, {CUSTOM_ORG: new_org})


def getOrgId(org_name):
   global orgs
   return orgs[org_name]


def getOrgName(org_id):
   global orgs
   for org_name in orgs:
      if orgs[org_name] == org_id:
         return org_name


def getTokenDescriptor(role, useConfigFallback=True, defaultToken=None):
   '''
   Returns a token descriptor for the given role, searching in this order:
   1. The exact role match from the Hermes config json file.
   2. If the given role is not found and useConfigFallback is True, the "all_roles"
      entry from the Hermes config json file.
   3. The default passed in.

   Returns None if nothing is found.
   '''
   global tokens
   tokenDescriptor = None

   if CUSTOM_ORG in tokens and \
      tokens[CUSTOM_ORG]:
      for user in tokens[CUSTOM_ORG]:
         if role in tokens[CUSTOM_ORG][user]:
            tokenDescriptor = {
               "org": CUSTOM_ORG,
               "user": user,
               "role": role
            }

      if useConfigFallback and not tokenDescriptor:
         for user in tokens[CUSTOM_ORG]:
            if ROLE_ALL in tokens[CUSTOM_ORG][user]:
               tokenDescriptor = {
                  "org": CUSTOM_ORG,
                  "user": user,
                  "role": ROLE_ALL
               }

   return tokenDescriptor if tokenDescriptor else defaultToken


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
      default_descriptor = {
         "org": "blockchain_service_dev",
         "user": "admin-blockchain-dev",
         "role": ROLE_ALL
      }
      token_descriptor = getTokenDescriptor(ROLE_ALL,
                                            True,
                                            default_descriptor)

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


def get_dockerhub_auth_token(username, password):
   response = requests.post("https://hub.docker.com/v2/users/login/", data={"username": username, "password": password})
   response_obj = json.loads(response.text)
   log.debug("docker hub login response: {}".format(response_obj))
   return response_obj["token"]
