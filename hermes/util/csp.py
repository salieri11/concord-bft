#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# This file is for functionality related to interacting with CSP objects
# such as users and organizations.
#########################################################################
import rest.request
import util.hermes_logging
from util import auth

log = util.hermes_logging.getMainLogger()

def patch_org(org_name, action, key, value,
             service=auth.SERVICE_STAGING):
  '''
  Utility method to patch an org, defaulting to using Helen
  on staging to do so.
  org_name: Name of org to patch
  action: "add" or "delete"
  key, value: k/v to add or delete
  service: Service URL
  '''
  token_descriptor = {
    "org": org_name,
    "user": "admin-blockchain-dev",
    "role": util.auth.ROLE_CON_ORG_ADMIN
  }

  req = rest.request.Request("patch_logs",
                             "Patch an org",
                             service,
                             None,
                             tokenDescriptor=token_descriptor,
                             service=service)

  orgId = util.auth.getOrgId(org_name)

  properties = {
    key: value
  }

  log.info("Patching org {}. Before: {}".format(org_name, req.getOrg(orgId)))

  if action == "add":
    resp = req.patchOrg(orgId, addProperties=properties)
  elif action == "delete":
    resp = req.patchOrg(orgId, delProperties=properties)

  print("Org after: {}".format(resp))
