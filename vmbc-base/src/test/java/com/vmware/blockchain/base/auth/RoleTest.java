/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.base.auth;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.fasterxml.jackson.databind.ObjectMapper;

@ExtendWith(SpringExtension.class)
class RoleTest {

    String orgUserStr = "{\"name\":\"vmbc-org:user\",\"displayName\":\"Organization User\","
                        + "\"defaultRole\":true,\"hidden\":false,\"internal\":false,\"authority\":\"vmbc-org:user\"}";

    @Test
    void testSerialize() throws Exception {
        ObjectMapper mapper = new ObjectMapper();
        String str = mapper.writeValueAsString(BaseRoles.ORG_USER);
        Assertions.assertEquals(orgUserStr, str);

        Role r = mapper.readValue(str, Role.class);
        // Note: This shouild be exactly the same object, not just obj.equals
        Assertions.assertTrue(BaseRoles.ORG_USER == r);
    }

}