/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.base.auth;

import java.io.IOException;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;

import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Custom deserializer for Role.
 * Roles are values, so we need to deserialize the data and return the correct value.
 */
public class RoleDeserializer extends JsonDeserializer<Role> {

    @Data
    @NoArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    // We only need the name to deserialize.
    private static class RoleData {
        private String name;

    }

    @Override
    public Role deserialize(JsonParser jsonParser, DeserializationContext deserializationContext)
            throws IOException {
        RoleData rd = jsonParser.readValueAs(RoleData.class);
        // Note:  Anything extended from BaseRoles will add in extra roles using addRole.  This gets entered
        // into the map, and so BaseRoles.get will be able to access it.
        return BaseRoles.get(rd.getName());
    }
}
