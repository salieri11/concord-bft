#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import pytest
import util.helen.common

from suites.case import describe, passed, failed
from fixtures.common_fixtures import fxBlockchain, fxConnection, fxHermesRunSettings, fxInitializeOrgs, fxProduct

import util.helen.validators

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# HELEN NODE SIZE TEMPLATE API TESTS
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

@describe()
@pytest.mark.smoke
@pytest.mark.replicas
def test_nodeSizeTemplateGet(fxConnection):
    req = util.helen.common.createDefaultConsortiumAdminRequest(fxConnection.request)
    template = req.getNodeSizeTemplate()

    util.helen.validators.validateNodeSizeTemplateResponse(template)
