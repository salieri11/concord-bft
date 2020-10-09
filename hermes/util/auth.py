#########################################################################
# Copyright 2019-2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import json
import logging
import requests
import time
import traceback
import util
from datetime import datetime
from urllib3.util.retry import Retry
from util import hermes_logging
log = hermes_logging.getMainLogger()
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
ROLE_CON_ORG_ADMIN = "consortium_org_admin"

SERVICE_DEFAULT = "https://localhost/blockchains/local"
SERVICE_STAGING = "https://vmbc.us-west-2.vdp-stg.vmware.com"

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
#
# Here is a string of users to help with CSP manual user management:
# HermesUtility@csp.local,vmbc_test_con_admin@csp.local,vmbc_test_con_operator@csp.local,
# vmbc_test_con_participant@csp.local,vmbc_test_org_admin@csp.local,vmbc_test_org_dev@csp.local,
# vmbc_test_org_user@csp.local

# These tokens use the "Hermes test service 8" service.
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
            # Never expires.
            "api_key": "TCkMl5Eby6IUQcazQgZydk0fxK9QRxyWZ3rPCcBgeES7PXD8bq4RZ8SoIcCn2pC8",
            "access_token": None,
            "last_retrieved": 0
         }
      }
   },
   "system_test_master": {
      "admin-blockchain-dev": {
         ROLE_ALL: {
            # API key name in console-stg: Hermes all roles
            # Never expires.
            "api_key": "QUFN8ogb32YXPPKy8qp6qtiLCMSFq9oo9sIg1qqG36roEHECSt2lcPekEYxwPz2O",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "HermesUtility": {
         ROLE_ALL: {
            # API key name in console-stg: Hermes all roles
            # Never expires.
            "api_key": "Pj1jUOW5Ef51radoBoFOeU4OosgEiUCgwgziqPIqKb0BCeV64him96u7aGadUKqr",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_con_admin": {
         ROLE_CON_ADMIN: {
            # API key name in console-stg: Hermes consortium admin
            # Never expires.
            "api_key": "LXszJht3d4Xf74f6DMaXUpbCfB9XJ36EDXO7yXYPN3wpfrU7tPnObprsum2YGS75",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_con_operator": {
         ROLE_CON_OPERATOR: {
            # API key name in console-stg: Hermes consortium operator
            # Never expires
            "api_key": "WFUQe5NM31uCviKrjRl2O33Eq4irs7W9VI4oYH3V32e0oS3Hy5UxMUuzY8gNtgTL",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_con_participant": {
         ROLE_CON_PARTICIPANT: {
            # API key name in console-stg: Hermes consortium participant
            # Never expires.
            "api_key": "MXfs4LHZwZR7ZY8ySQ1Y7jwtr2SyoFf42jGHp9bjfXG2UuSgn4CJI2l6aZpzhpbI",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_org_admin": {
         ROLE_ORG_ADMIN: {
            # API key name in console-stg: Hermes org admin
            # Never expires.
            "api_key": "UWN4HKrifPVl51RSWggIvFZXOYhPDwCWcbBEMgHJ1fdmfrTBzo2JvqrNQu3YeHiv",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_org_dev": {
         ROLE_ORG_DEVELOPER: {
            # API key name in console-stg: Hermes org dev
            # Never expires.
            "api_key": "h6wxPVwTTggd4S9UJPhhTdXhttplOiSDW0383VNrjprsGEIQ8icCky1CLeHhblpT",
            "access_token": None,
            "last_retrieved": 0
         },
      },
      "vmbc_test_org_user": {
         ROLE_ORG_USER: {
            # API key name in console-stg: Hermes org user
            # Never expires.
            "api_key": "2D5TJFkEFS1cmeSoObEP8o7Pw5E0t27scFQOLSKqsh7VhETpayhmMmcRjP8Mgal6",
            "access_token": None,
            "last_retrieved": 0
         }
      }
   },
   "system_test_release_1": {
      "admin-blockchain-dev": {
         ROLE_ALL: {
            # API key name in console-stg: Hermes all roles
            # Never expires.
            "api_key": "bX61KeUOwzbtHbOQQUfxhqSoP9VW3idOZXluJ6qTvWnhqyHc2pNckTwUxW3RwFWG",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "HermesUtility": {
         ROLE_ALL: {
            # API key name in console-stg: Hermes all roles
            # Never expires.
            "api_key": "YMiJwligHCZGND6QASBOskjqV5CwrJV7A67CB2VyfOASKq7ycY19J1O6vTAKcRSY",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_con_admin": {
         ROLE_CON_ADMIN: {
            # API key name in console-stg: Hermes consortium admin
            # Never expires.
            "api_key": "W8m7EeRz3BDFhMPO4vEBsiGf2aw1tDkY7pKiwvh3eag9hzW8pKuWK2m2Qmm8hsYT",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_con_operator": {
         ROLE_CON_OPERATOR: {
            # API key name in console-stg: Hermes consortium operator
            # Never expires
            "api_key": "2MaQhfPbS5eITHzhde6kkxVY82r3P5cF1NNyE58OW6Njk2H4NFPb2AVmXvVBYxXZ",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_con_participant": {
         ROLE_CON_PARTICIPANT: {
            # API key name in console-stg: Hermes Hermes consortium participant
            # Never expires.
            "api_key": "bGx1NZaJZXW8LXKxVztVQMTWa2bP3elLLa3IIkZkQzrN19L9e5TycEf2I1odpmdL",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_org_admin": {
         ROLE_ORG_ADMIN: {
            # API key name in console-stg: Hermes org admin
            # Never expires.
            "api_key": "4E5JwatuKl0iCk7uASlW4IAz4tGs0eIm4vfpu7wRZMpvpgVQ6bbdyuUR51YB1S9W",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_org_dev": {
         ROLE_ORG_DEVELOPER: {
            # API key name in console-stg: Hermes org dev
            # Never expires.
            "api_key": "Os8edMLJZPtW6W4QRtVau0s5Zr7ZCIRfzYcxX5Ge7JvP0tLvtvtxtbmIRox2xgT3",
            "access_token": None,
            "last_retrieved": 0
         },
      },
      "vmbc_test_org_user": {
         ROLE_ORG_USER: {
            # API key name in console-stg: Hermes org user
            # Never expires.
            "api_key": "fwGp3Q2y45wGa552MPYkm29uXDnZeLEmEhcl3EWvhRilvEQz6lm6hb1TZxC7i6iW",
            "access_token": None,
            "last_retrieved": 0
         }
      }
   },
   "system_test_release_2": {
      "admin-blockchain-dev": {
         ROLE_ALL: {
            # API key name in console-stg: Hermes all roles
            # Never expires.
            "api_key": "vgXkmEtwQwbyzOprRmoWx4eZcH7vOPVleA8HBAbJNQovdRdiK6s2IZ0iadNR8P2J",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "HermesUtility": {
         ROLE_ALL: {
            # API key name in console-stg: Hermes all roles
            # Never expires.
            "api_key": "KEU2ng6aujwoQPYHYYavkiBkCjCjm1DIjs3S12TDQhSrgZvqca3LiR42O4lddfUK",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_con_admin": {
         ROLE_CON_ADMIN: {
            # API key name in console-stg: Hermes consortium admin
            # Never expires.
            "api_key": "WOpdi2RaOLbibptitUA56z44Jw1G4UND1Tc6NYElFxOQ9lX09eHDQw4xGlHMCCck",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_con_operator": {
         ROLE_CON_OPERATOR: {
            # API key name in console-stg: Hermes consortium operator
            # Never expires
            "api_key": "UPBVO3WrRHPMBrgGN8kMOMop1Em1q9lhAhhBFpzmbx8w6tHCCimJEhmD0LD3UbPo",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_con_participant": {
         ROLE_CON_PARTICIPANT: {
            # API key name in console-stg: Hermes consortium participant
            # Never expires.
            "api_key": "zFcdYwvOl8GX08rjPN1IOcITN5SFPdmpgBItj9dk4N7yU3ty8LVRRdYKBTYZlVv2",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_org_admin": {
         ROLE_ORG_ADMIN: {
            # API key name in console-stg: Hermes org admin
            # Never expires.
            "api_key": "e5Rxc3mJm0PrjbniksTl0Kk17O59SqeLDEXL8tkVS0gimXN01NtPjIV550fKiSNE",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_org_dev": {
         ROLE_ORG_DEVELOPER: {
            # API key name in console-stg: Hermes org dev
            # Never expires.
            "api_key": "P0cTC2P8p8GeuSdewmCxVLmnNOTbXkXlKfqwNtz6iimRou215bkUuwtpxjZ9MkwP",
            "access_token": None,
            "last_retrieved": 0
         },
      },
      "vmbc_test_org_user": {
         ROLE_ORG_USER: {
            # API key name in console-stg: Hermes org user
            # Never expires.
            "api_key": "WDQ4n0Q1x68G0eeMtz4xPA64nT5n7vwKKQbaa5mDqGHiZPLQpMs31HO2eTICD5Zn",
            "access_token": None,
            "last_retrieved": 0
         }
      }
   }
}


# These tokens use the "VMware Blockchain Service - Staging" service.
# Perhaps the service should be another dict level in the above.
staging_tokens = {
   "system_test_master": {
      "admin-blockchain-dev": {
         ROLE_CON_ORG_ADMIN: {
            # Name in console-stg: Admin blockchain dev staging
            # Never expires.
            "api_key": "h6wN0gl7PEV1G3iR8O5zc494TGbDhGNOEAQgi74VFZerNcAMryhemuKzB5ULsxH7",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_con_admin": {
         ROLE_CON_ADMIN: {
            # API key name in console-stg: Hermes consortium admin staging
            # Never expires.
            "api_key": "A839L4B1MrHYGIQFiYO8D4CkxfxF6zpMouAjGiN5AukIrCEIcX9gXTOQxpRMYPwu",
            "access_token": None,
            "last_retrieved": 0
         }
      }
   },
   "system_test_release_1": {
      "admin-blockchain-dev": {
         ROLE_CON_ORG_ADMIN: {
            # Name in console-stg: Admin blockchain dev staging
            # Never expires.
            "api_key": "59L3FFiuy23yND6LJRLdYKq19YUI2Ysz3zD017Axjx4hyRlYqUgJgiGwmY7jxJ9k",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_con_admin": {
         ROLE_CON_ADMIN: {
            # API key name in console-stg: Hermes consortium admin staging
            # Never expires.
            "api_key": "V6L7hIJEBPNjU7piL7oHg5G5EQtqz08uUoPgLEobgaUjXwZ1NpNZJJksqw7aNWx4",
            "access_token": None,
            "last_retrieved": 0
         }
      }
   },
   "system_test_release_2": {
      "admin-blockchain-dev": {
         ROLE_CON_ORG_ADMIN: {
            # Name in console-stg: Admin blockchain dev staging
            # Never expires.
            "api_key": "r5edE3f9KnOTgZfXHfZi0QPzsR3NXtMYjtofW7VvLVlkAZeZ2XHAUDi0VjfII0NF",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_con_admin": {
         ROLE_CON_ADMIN: {
            # API key name in console-stg: Hermes consortium admin staging
            # Never expires.
            "api_key": "gZyxAEbxWtz17JTqumSpZ2q2WbgM849EU5QAGyuDP9M7HNyGVIgcU4CeQEQg6S8R",
            "access_token": None,
            "last_retrieved": 0
         }
      }
   },
   "system_test_1": {
      "admin-blockchain-dev": {
         ROLE_CON_ORG_ADMIN: {
            # Never expires.
            "api_key": "epDrinl6QeMKOk3xMo4swUuhMBuU2LempQB5XNDDFpAtS476FPnCfsRhzmDNr4Nh",
            "access_token": None,
            "last_retrieved": 0
         }
      }
   },
   "system_test_2": {
      "admin-blockchain-dev": {
         ROLE_CON_ORG_ADMIN: {
            # Never expires.
            "api_key": "ny8H6egCh922NWAdgCMz1cEA4VjmsfEBgj03UwgP3MYCnd4vY8PpqMmzmczAPVAH",
            "access_token": None,
            "last_retrieved": 0
         }
      }
   },
   "system_test_3": {
      "admin-blockchain-dev": {
         ROLE_CON_ORG_ADMIN: {
            # Never expires.
            "api_key": "QAgS4i9FahlVDghvo2ZHou5sGDkRgHbIVi68XsTy9sK0L6clO8laD07MS5ZzdPKT",
            "access_token": None,
            "last_retrieved": 0
         }
      }
   },
   "system_test_4": {
      "admin-blockchain-dev": {
         ROLE_CON_ORG_ADMIN: {
            # Never expires.
            "api_key": "NYn0N3YXdut832DHeztdDKOLD2sfYvqGjJRQTft4QO7u5zSLuMjTRNGaPu3r3SXI",
            "access_token": None,
            "last_retrieved": 0
         }
      },
      "vmbc_test_con_admin": {
         ROLE_CON_ADMIN: {
            # Never expires.
            "api_key": "PxsXtehT3P9b6GAXnL6ELitZx24VPu3FiUzAhNKmtLEJO7FFJHeT0c3CZa7mvojb",
            "access_token": None,
            "last_retrieved": 0
         }
      }
   },
   "system_test_5": {
      "admin-blockchain-dev": {
         ROLE_CON_ORG_ADMIN: {
            # Never expires.
            "api_key": "1g2DqKGVYGf1xT5z69l3l6gO8Sxn396CXNm60188Jgyp4amPNvIvIa1OTcVT5PaY",
            "access_token": None,
            "last_retrieved": 0
         }
      }
   }
}


orgs = {
   "blockchain_dev_service_org": "ab29b4a8-40c8-4173-ba74-15e76f8f35e0",
   "custom_org": "",
   "hermes_api_test_org": "adc86e8a-b257-49a8-9a7f-d237570d2a4f",
   "hermes_org0": "7e162c74-ae9f-40b4-98ab-89e13f8a2b78",
   "hermes_org1": "923a2597-ba3a-4ef8-a41c-a22406379eac",
   "hermes_org2": "33878dd7-9a3f-447e-b0eb-17d5ea7d3991",
   "system_test_master": "29717ffe-12dd-43ab-a86a-0003611a319d",
   "system_test_release_1": "8ccd96cc-db11-442b-a9de-2e73d2dbfb9d",
   "system_test_release_2": "5e55804a-07dc-4c07-bc4c-483dab274df9",
   "rvollmar": "25df2f25-bac1-4af0-aa11-edc7deaef569",
   "rsg": "440d4001-6467-4dc0-8eaa-b57a1885fa8a",
   "system_test_1": "bf1316aa-6e44-4947-b8a4-e57421ec5941",
   "system_test_2": "15d0a7d9-6b5c-49c6-8b92-020adedc96c6",
   "system_test_3": "0dfbceb7-cfa8-4c6b-a304-18f0c6f91a2a",
   "system_test_4": "e6fe1ca6-13f5-48e2-8461-18bfbe5029e2",
   "system_test_5": "f9c8979b-3ba8-4acb-a91a-a4914ffd85d9",
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


def getTokenGroup(service=SERVICE_DEFAULT):
   '''
   Depending on the service we are using, there are different API keys.
   '''
   if service == SERVICE_STAGING:
      return staging_tokens
   elif service == SERVICE_DEFAULT:
      return tokens
   else:
      raise Exception("No API keys defined in Hermes for service: '{}'".format(service))


def getTokenDescriptor(role, useConfigFallback=True, defaultToken=None):
   '''
   Returns a token descriptor for the given role, searching in this order:
   1. The exact role match from the Hermes config json file.
   2. If the given role is not found and useConfigFallback is True, the "all_roles"
      entry from the Hermes config json file.
   3. The default passed in.

   Returns None if nothing is found.

   TODO: This was written in simpler times and needs to be updated.
         Accept a service, org, and role, and actually do the lookup. This is currently
         just using the custom org if present, and returning defaultToken if not.
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


def getAccessToken(token_descriptor=None, force_refresh=False, service=None):
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

   RV: There are better ways to do this work.
       TODO: Rework the map at the top of this file and the fuctions which access it.
   '''
   global tokens
   global staging_tokens

   if token_descriptor:
      log.debug("Using token descriptor {}".format(token_descriptor))
   else:
      default_descriptor = {
         "org": "blockchain_service_dev",
         "user": "admin-blockchain-dev",
         "role": ROLE_ALL
      }
      # Pointless?
      token_descriptor = getTokenDescriptor(ROLE_ALL,
                                            True,
                                            default_descriptor)

   user = token_descriptor["user"]
   org = token_descriptor["org"]
   role = token_descriptor["role"]

   if service == SERVICE_STAGING:
      service_tokens = staging_tokens
   else:
      service_tokens = tokens

   if not org in service_tokens:
      raise Exception("getAccessToken: Invalid org '{}'.  Choose one of {}" \
                      .format(org, list(service_tokens.keys())))

   if not user in service_tokens[org]:
      raise Exception("getAccessToken: Invalid user '{}' for org '{}'.  Choose one of {}" \
                      .format(user, org, list(service_tokens[org].keys())))

   if not role in service_tokens[org][user]:
      raise Exception("getAccessToken: Invalid role '{}' for user '{}' in org '{}'.  Choose one of {}" \
                      .format(role, user, org, list(service_tokens[org][user].keys())))

   if needNewAccessToken(service_tokens, org, user, role, force_refresh):
      log.debug("getAccessToken() requesting a new access token.")
      new_token = retrieveNewAccessToken(service_tokens, org, user, role)
      service_tokens[org][user][role]["last_retrieved"] = datetime.now().timestamp()
      service_tokens[org][user][role]["access_token"] = new_token
   else:
      log.debug("getAccessToken() using existing access token.")

   log.debug("*** service_tokens[{}][{}][{}]['api_key']: {}".format(org, user, role, service_tokens[org][user][role]["api_key"]))
   log.debug("*** service_tokens[{}][{}][{}]['access_token']: {}".format(org, user, role, service_tokens[org][user][role]["access_token"]))
   log.debug("*** access_token being used: {}".format(service_tokens[org][user][role]["access_token"]))
   return service_tokens[org][user][role]["access_token"]


def needNewAccessToken(service_tokens, org, user, role, force_refresh):
   '''
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
   elif service_tokens[org][user][role]["last_retrieved"]:
      expiration = service_tokens[org][user][role]["last_retrieved"]
      expiration += ACCESS_TOKEN_LIFESPAN - TEST_BUFFER
      now = datetime.now().timestamp()
      log.debug("expiration: {}, now: {}".format(expiration, now))
      return expiration < now
   else:
      return True


def retrieveNewAccessToken(service_tokens, org, user, role):
   '''
   Given dict of orgs, org, user, and role, retrieves an access token from CSP.

   Retries for various error codes.  e.g. 104 ("Connection reset by peer") and various 500's.

   Reference:
   https://urllib3.readthedocs.io/en/latest/reference/urllib3.util.html#module-urllib3.util.retry

   Wait time = {backoff factor} * (2 ** ({number of attempts} - 1)),
   up to Retry.BACKOFF_MAX, which is 120 at this time.

   7 tries with backoff_factor of 0.5 = 63.5 seconds of retrying.
   '''

   api_key = service_tokens[org][user][role]["api_key"]
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
