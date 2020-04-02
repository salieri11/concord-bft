#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import logging
import pytest
import rest.test_methods
import util.auth

from suites.cases import describe
from fixtures.common_fixtures import fxBlockchain, fxConnection, fxInitializeOrgs, fxHermesRunSettings, fxProduct

import util.hermes_logging
log = util.hermes_logging.getMainLogger()


@describe()
@pytest.mark.roles
def test_roles_consortium_creation(fxConnection):
   '''
   Users who are not consoritum admins should not be able to create a new consortium.
   Note: VMWare SRE roles still need to be defined.
   '''
   org = "hermes_org0"

   for username in util.auth.tokens[org]:
      userdata = util.auth.tokens[org][username]
      role = list(userdata.keys())[0]

      if role != "all_roles":
         defaultDescriptor = {
            "org": org,
            "user": username,
            "role": role
         }
         tokenDescriptor = util.auth.getTokenDescriptor(role,
                                                        True,
                                                        defaultDescriptor)
         suffix = util.numbers_strings.random_string_generator()
         conName = "con_" + suffix
         req = fxConnection.request.newWithToken(tokenDescriptor)
         result = req.createConsortium(conName)

         if tokenDescriptor["role"] == "consortium_admin":
            log.info("Verifying {} could create a consortium.".format(role))
            assert result["consortium_id"], "Consortium creation by a consortium admin failed."
         else:
            log.info("Verifying {} could not create a consortium.".format(role))
            rest.test_methods.validateAccessDeniedResponse(result, "/api/consortiums/")


@describe()
@pytest.mark.roles
def test_role_list_consortiums(fxConnection):
   '''
   Everyone should only be able to use /consortiums (GET) to list the consortiums
   they belong to.
   '''
   conAdminA = {
      "org": "hermes_org0",
      "user": "vmbc_test_con_admin",
      "role": "consortium_admin"
   }

   conAdminB = {
      "org": "hermes_org1",
      "user": "vmbc_test_con_admin",
      "role": "consortium_admin"
   }

   req = fxConnection.request.newWithToken(conAdminA)
   suffix = util.numbers_strings.random_string_generator()
   orgACon = req.createConsortium("con_{}".format(suffix))

   req = fxConnection.request.newWithToken(conAdminB)
   suffix = util.numbers_strings.random_string_generator()
   orgBCon = req.createConsortium("con_{}".format(suffix))

   for org in ["hermes_org0", "hermes_org1"]:
      for username in util.auth.tokens[org]:
         userdata = util.auth.tokens[org][username]
         role = list(userdata.keys())[0]

         if role != "all_roles":
            defaultDescriptor = {
               "org": org,
               "user": username,
               "role": role
            }
            tokenDescriptor = util.auth.getTokenDescriptor(role,
                                                           True,
                                                           defaultDescriptor)

            req = fxConnection.request.newWithToken(tokenDescriptor)
            response = req.getConsortiums()
            conIds = []

            for c in response:
               conIds.append(c["consortium_id"])

            if role == "no_roles":
               # VB-1274: User with no service roles can list consortiums.
               # Should probably be an empty list.
               #assert len(response) == 0, "Expected an empty list for a user with no roles."
               pass
            elif org == "hermes_org0":
               assert orgACon["consortium_id"] in conIds, "Expected consortium not present."
               assert orgBCon["consortium_id"] not in conIds, "Unexpected consortium present."
            else:
               assert orgBCon["consortium_id"] in conIds, "Expected consortium not present."
               assert orgACon["consortium_id"] not in conIds, "Unexpected consortium present."


@describe()
@pytest.mark.roles
def test_role_consortium_get(fxConnection):
   '''
   Everyone should be able to get a specific consortium they are part of.
   (This is similar to the above.  That was to list all consortiums; this is to get
   a specific one.)
   '''
   org0Admin = {
      "org": "hermes_org0",
      "user": "vmbc_test_con_admin",
      "role": "consortium_admin"
   }
   req = fxConnection.request.newWithToken(org0Admin)
   suffix = util.numbers_strings.random_string_generator()
   org0Con = req.createConsortium("con_{}".format(suffix))

   org1Admin = {
      "org": "hermes_org1",
      "user": "vmbc_test_con_admin",
      "role": "consortium_admin"
   }
   req = fxConnection.request.newWithToken(org1Admin)
   suffix = util.numbers_strings.random_string_generator()
   org1Con = req.createConsortium("con_{}".format(suffix))

   for requestorOrg in ["hermes_org0", "hermes_org1"]:
      for username in util.auth.tokens[requestorOrg]:
         userdata = util.auth.tokens[requestorOrg][username]
         roles = list(userdata.keys())

         for role in roles:
            if role.endswith("org_user") and role != "all_roles" or role == "no_roles":
               defaultDescriptor = {
                  "org": requestorOrg,
                  "user": username,
                  "role": role
               }
               tokenDescriptor = util.auth.getTokenDescriptor(role,
                                                              True,
                                                              defaultDescriptor)

               req = fxConnection.request.newWithToken(tokenDescriptor, True)
               org0Response = req.getConsortium(org0Con["consortium_id"])
               org1Response = req.getConsortium(org1Con["consortium_id"])

               if role == "no_roles":
                  # VB-1274
                  # rest.test_methods.validateAccessDeniedResponse(org0Response,
                  #                              "/api/consortiums/{}".format(org0Con["consortium_id"]))
                  # rest.test_methods.validateAccessDeniedResponse(org1Response,
                  #                              "/api/consortiums/{}".format(org1Con["consortium_id"]))
                  pass
               elif requestorOrg == "hermes_org0":
                  log.info("org0Response for org {}, user {}, role {}: {}".format(
                     requestorOrg, username, role, org0Response))
                  assert org0Response["consortium_id"] == org0Con["consortium_id"], \
                     "Expected to be able to fetch consortium in hermes_org0"
                  rest.test_methods.validateAccessDeniedResponse(org1Response,
                                              "/api/consortiums/{}".format(org1Con["consortium_id"]
                                              ))
               else:
                  log.info("org1Response for org {}, user {}, role {}: {}".format(
                     requestorOrg, username, role, org1Response))
                  assert org1Response["consortium_id"] == org1Con["consortium_id"], \
                     "Expected to be able to fetch consortium in hermes_org1"
                  rest.test_methods.validateAccessDeniedResponse(org0Response,
                                              "/api/consortiums/{}".format(org0Con["consortium_id"]
                                              ))


@describe()
@pytest.mark.roles
def test_role_consortium_patch_users_in_org(fxConnection):
   '''
   Only consortium admins should be able to patch a consortium.
   '''
   suffix = util.numbers_strings.random_string_generator()
   conName = "con_" + suffix
   org = "hermes_org0"
   tokenDescriptor = {
      "org": org,
      "user": "vmbc_test_con_admin",
      "role": "consortium_admin"
   }
   req = fxConnection.request.newWithToken(tokenDescriptor)
   createResponse = req.createConsortium(conName)

   for username in util.auth.tokens[org]:
      userdata = util.auth.tokens[org][username]
      roles = list(userdata.keys())
      for role in roles:
         if role.endswith("org_user") and role != "all_roles" or role == "no_roles":
            tokenDescriptor = {
               "org": org,
               "user": username,
               "role": role
            }
            newName = "newName" + util.numbers_strings.random_string_generator()
            req = fxConnection.request.newWithToken(tokenDescriptor, True)
            result = req.patchConsortium(createResponse["consortium_id"],
                                         newName=newName)

            if role == "consortium_admin_and_org_user":
               log.info("Verifying {} could patch a consortium.".format(role))
               assert "consortium_name" in result, "Expected consortium_name in {}".format(result)
               assert result["consortium_name"] == newName, \
                  "Expected to be able to rename the consortium"
            else:
               log.info("Verifying {} could not patch a consortium.".format(role))
               rest.test_methods.validateAccessDeniedResponse(result,
                                           "/api/consortiums/{}".format(
                                              createResponse["consortium_id"]))


@describe()
@pytest.mark.roles
def test_role_consortium_patch_other_admin(fxConnection):
   '''
   Make sure an outside consortium admin cannot change our consortium.
   '''
   suffix = util.numbers_strings.random_string_generator()
   conName = "con_" + suffix
   tokenDescriptor = {
      "org": "hermes_org0",
      "user": "vmbc_test_con_admin",
      "role": "consortium_admin"
   }
   req = fxConnection.request.newWithToken(tokenDescriptor)
   createResponse = req.createConsortium(conName)

   tokenDescriptor = {
      "org": "hermes_org1",
      "user": "vmbc_test_con_admin",
      "role": "consortium_admin"
   }
   req = fxConnection.request.newWithToken(tokenDescriptor)
   newName = "newName" + util.numbers_strings.random_string_generator()
   result = req.patchConsortium(createResponse["consortium_id"],
                                newName=newName)
   rest.test_methods.validateAccessDeniedResponse(result,
                               "/api/consortiums/{}".format(
                                  createResponse["consortium_id"]))


@describe()
@pytest.mark.roles
def test_role_consortium_list_orgs_across_orgs(fxConnection, fxInitializeOrgs):
   '''
   Make sure all roles can only list the orgs in a consortium they are part of.

   consortium0: Has org0 and org1
   consortium1: Has org1

   org0 user: Can see orgs in consortium0, denied for consortium1
   org1 user: Can see orgs in consortium0 and consortium1
   '''
   conName = "con_" + util.numbers_strings.random_string_generator()
   tokenDescriptor = {
      "org": "hermes_org0",
      "user": "vmbc_test_con_admin",
      "role": "consortium_admin"
   }
   req = fxConnection.request.newWithToken(tokenDescriptor)
   consortium0 = req.createConsortium(conName)["consortium_id"]
   req.patchConsortium(consortium0,
                       orgsToAdd=[util.auth.orgs["hermes_org1"]])

   conName = "con_" + util.numbers_strings.random_string_generator()
   tokenDescriptor = {
      "org": "hermes_org1",
      "user": "vmbc_test_con_admin",
      "role": "consortium_admin"
   }
   req = fxConnection.request.newWithToken(tokenDescriptor)
   consortium1 = req.createConsortium(conName)["consortium_id"]
   con0OrgIds = [util.auth.orgs["hermes_org0"], util.auth.orgs["hermes_org1"]]
   con1OrgIds = [util.auth.orgs["hermes_org1"]]

   # Loop through all roles for each org and be sure each can only list the orgs
   # of consortiums they belong to.
   for userOrg in ["hermes_org0", "hermes_org1"]:
      for username in util.auth.tokens[userOrg]:
         userdata = util.auth.tokens[userOrg][username]
         roles = list(userdata.keys())

         for role in roles:
            if role.endswith("org_user") and role != "all_roles" and role != "no_roles":
               tokenDescriptor = {
                  "org": userOrg,
                  "user": username,
                  "role": role
               }
               req = fxConnection.request.newWithToken(tokenDescriptor, True)

               # Users from both orgs can get the orgs in consortium0.
               getConsortiumOrgsResult = req.getConsortiumOrgs(consortium0)
               # log.info("getConsortiumOrgs(consortium0) result for org {}, user {}, role {}: {}".format(
               #    userOrg, username, role, getConsortiumOrgsResult))
               assert len(getConsortiumOrgsResult) == 2, "Expected a list of two items, not {}".format(getConsortiumOrgsResult)
               for o in getConsortiumOrgsResult:
                  assert o["org_id"] in con0OrgIds

               # Only users from org1 can get the orgs in consortium1.
               getConsortiumOrgsResult = req.getConsortiumOrgs(consortium1)
               # log.info("getConsortiumOrgs(consortium1) result for org {}, user {}, role {}: {}".format(
               #    userOrg, username, role, getConsortiumOrgsResult))
               if userOrg == "hermes_org0":
                  rest.test_methods.validateAccessDeniedResponse(getConsortiumOrgsResult,
                                  "/api/consortiums/{}/organizations".format(consortium1))
               elif userOrg == "hermes_org1":
                  assert len(getConsortiumOrgsResult) == 1, "Expected one item, not {}".format(getConsortiumOrgsResult)
                  assert getConsortiumOrgsResult[0]["org_id"] in con1OrgIds, "Expected org ID {}".format(con1OrgIds)


@describe()
@pytest.mark.roles
def test_role_consortium_list_orgs_across_roles(fxConnection, fxInitializeOrgs):
   '''
   Everyone in a consortium should be able to see what orgs are in it.
   '''
   conName = "con_" + util.numbers_strings.random_string_generator()
   mainOrg = "hermes_org0"
   tokenDescriptor = {
      "org": mainOrg,
      "user": "vmbc_test_con_admin",
      "role": "consortium_admin"
   }
   req = fxConnection.request.newWithToken(tokenDescriptor)
   consortium = req.createConsortium(conName)["consortium_id"]
   req.patchConsortium(consortium,
                       orgsToAdd=[util.auth.orgs["hermes_org1"]])
   expectedOrgIds = [util.auth.orgs[mainOrg], util.auth.orgs["hermes_org1"]]
   expectedOrgNames = [mainOrg, "hermes_org1"]

   for username in util.auth.tokens[mainOrg]:
      userdata = util.auth.tokens[mainOrg][username]
      roles = list(userdata.keys())

      for role in roles:
         if role.endswith("org_user") and role != "all_roles" or role == "no_roles":
            tokenDescriptor = {
               "org": mainOrg,
               "user": username,
               "role": role
            }
            req = fxConnection.request.newWithToken(tokenDescriptor, True)
            getConsortiumOrgsResult = req.getConsortiumOrgs(consortium)

            if role == "no_roles":
               # VB-1274
               # rest.test_methods.validateAccessDeniedResponse(getConsortiumOrgsResult,
               #                              "/api/consortiums/{}/organizations".format(consortium))
               pass
            else:
               returnedOrgIds = []

               log.info("getConsortiumOrgs(consortium) result for org {}, user {}, role {}".format(
                  mainOrg, username, role, getConsortiumOrgsResult))

               assert len(getConsortiumOrgsResult) == 2, "Expected two items, not {}".format(getConsortiumOrgsResult)

               # Make sure everything returned is part of the expected list.
               for o in getConsortiumOrgsResult:
                  returnedOrgIds.append(o["org_id"])

                  assert o["org_id"] in expectedOrgIds, "Expected {} in {}".format(o["org_id"], expectedOrgIds)

                  if o["org_id"] == expectedOrgIds[0]:
                     assert o["organization_name"] == expectedOrgNames[0]
                  elif o["org_id"] == expectedOrgIds[1]:
                     assert o["organization_name"] == expectedOrgNames[1]

               # Make sure everything expected is in the returned list.
               for o in expectedOrgIds:
                  assert o in returnedOrgIds


@describe()
@pytest.mark.roles
def test_role_list_blockchains(fxConnection):
   '''
   All users in a consortium should be able to list the blockchains in their consortium,
   and only those blockchains.
   Note: Currently, we only have the built in test blockchain.
   '''
   orgs = ["blockchain_service_dev", "hermes_org0"]

   for org in orgs:
      for username in util.auth.tokens[org]:
         userdata = util.auth.tokens[org][username]
         roles = list(userdata.keys())

         for role in roles:
            if role.endswith("org_user") or role == "no_roles":
               tokenDescriptor = {
                  "org": org,
                  "user": username,
                  "role": role
               }
               req = fxConnection.request.newWithToken(tokenDescriptor)
               getBlockchainsResult = req.getBlockchains()

               if role == "no_roles":
                  rest.test_methods.validateAccessDeniedResponse(getBlockchainsResult,
                                               "/api/blockchains")
               elif org == "blockchain_service_dev":
                  assert len(getBlockchainsResult) > 0, "Expected at least one blockchain."
               else:
                  assert req.getBlockchains() == [], "Expected an empty list"


@describe()
@pytest.mark.roles
def test_role_get_specific_blockchain(fxConnection, fxBlockchain):
   orgs = ["blockchain_service_dev", "hermes_org0"]

   for org in orgs:
      for username in util.auth.tokens[org]:
         userdata = util.auth.tokens[org][username]
         roles = list(userdata.keys())

         for role in roles:
            if role.endswith("org_user"):
               tokenDescriptor = {
                  "org": org,
                  "user": username,
                  "role": role
               }
               req = fxConnection.request.newWithToken(tokenDescriptor)
               getBlockchainResult = req.getBlockchainDetails(fxBlockchain.blockchainId)


               if role == "no_roles":
                  rest.test_methods.validateAccessDeniedResponse(getBlockchainResult,
                                               "/api/blockchains/{}".format(fxBlockchain.blockchainId))
               elif org == "blockchain_service_dev":
                  assert getBlockchainResult["id"] == fxBlockchain.blockchainId, \
                     "Expected to get the requested blockchain"
               else:
                  rest.test_methods.validateAccessDeniedResponse(getBlockchainResult,
                                              "/api/blockchains/{}".format(fxBlockchain.blockchainId))
