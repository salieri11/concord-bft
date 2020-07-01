#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import util.auth

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# USEFUL METHODS USED ACROSS ALL TESTS
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

def createDefaultConsortiumAdminRequest(request):
    '''
    Given something like the fixture request, use it as a model
    to create a new request object with the permissions of a
    consortium admin and hermes_org0.
    '''
    descriptor = {
        "org": "hermes_org0",
        "user": "vmbc_test_con_admin",
        "role": "consortium_admin"
    }
    tokenDescriptor = util.auth.getTokenDescriptor(util.auth.ROLE_CON_ADMIN,
                                                   True,
                                                   descriptor)
    return request.newWithToken(tokenDescriptor)