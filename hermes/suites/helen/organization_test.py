#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import pytest
import sys
from uuid import UUID, uuid4

from suites.case import describe, passed, failed

from fixtures.common_fixtures import fxBlockchain, fxConnection, fxHermesRunSettings, fxInitializeOrgs, fxProduct
import util.auth
import util.helen.validators

# For hermes/lib/persephone, used for saving streamed events and deleting them.
sys.path.append('lib')

import util.hermes_logging
log = util.hermes_logging.getMainLogger()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# HELEN ORGANIZATIONS TESTS
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

@describe()
@pytest.mark.smoke
@pytest.mark.organizations
def test_organizations_patch(fxConnection):
    '''
    Patch an organization.
    '''
    descriptor = {
        "org": "hermes_api_test_org",
        "user": "vmbc_test_org_admin",
        "role": "org_admin"
    }
    tokenDescriptor = util.auth.getTokenDescriptor(util.auth.ROLE_ORG_ADMIN,
                                                   True,
                                                   descriptor)

    req = fxConnection.request.newWithToken(tokenDescriptor)

    org_id = util.auth.getOrgId("hermes_api_test_org")

    # We use a UUID for every run to prevent clashes between simultaneous runs.
    # Actually we just make clashes extremely improbable.
    # There is no certainty in this world.
    property_key = str(uuid4())
    property_value = str(uuid4())

    add_properties = {property_key: property_value}
    patched_org = req.patchOrg(org_id, add_properties, {})
    util.helen.validators.validateOrgResponse(patched_org)

    patched_org = req.getOrg(org_id)
    util.helen.validators.validateOrgResponse(patched_org)

    assert patched_org["organization_properties"][property_key] == property_value
    delete_properties = {property_key: ""}
    req.patchOrg(org_id, {}, delete_properties)

    patched_org = req.getOrg(org_id)
    util.helen.validators.validateOrgResponse(patched_org)

    assert not(property_key in patched_org["organization_properties"])