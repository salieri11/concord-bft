/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.base.auth;

import org.springframework.security.core.GrantedAuthority;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;

import lombok.Value;

/**
 * Roles used within VMWare Blockchain.
 */
@Value
@JsonDeserialize(using = RoleDeserializer.class)
public class Role implements GrantedAuthority {
    private final String name;
    private final String displayName;
    private final boolean defaultRole;
    private final boolean hidden; // should this show in CSP usermgmt UI.
    // If true - don't register with CSP - this role will be assigned based on other criteria e.g. Org property
    private final boolean internal;


    /**
     * Create a new Role.
     * @param name          Role Name
     * @param displayName   How it's displayed
     * @param cspDefault     True if this is a default role to grant
     * @param hidden      True if the role is hidden
     * @param internal    True if this is not a csp service role, but something used internally
     */
    public Role(String name, String displayName,
              boolean cspDefault, boolean hidden,
              boolean internal) {
        this.name = name;
        this.displayName = displayName;
        this.defaultRole = cspDefault;
        this.hidden = hidden;
        this.internal = internal;
    }

    @Override
    public String toString() {
        return name;
    }

    @Override
    public String getAuthority() {
        return getName();
    }
}
