/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import org.springframework.security.core.GrantedAuthority;

import java.util.HashMap;
import java.util.Map;

/**
 * Enum to define all user roles
 */
public enum Roles implements GrantedAuthority {

    ORG_USER("ORG_USER"), ORG_DEVELOPER("ORG_DEVELOPER"), ORG_ADMIN("ORG_ADMIN"), CONSORTIUM_ADMIN(
            "CONSORTIUM_ADMIN"), SYSTEM_ADMIN("SYSTEM_ADMIN");

    private final String name;

    Roles(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public static boolean contains(String s) {
        for (Roles r : Roles.values()) {
            if (r.toString().equals(s))
                return true;
        }
        return false;
    }

    public static Roles fromString(String s) {
        for (Roles r : Roles.values()) {
            if (r.toString().equals(s))
                return r;
        }
        return null;
    }

    public String toString() {
        return this.name;
    }


    public String getAuthority() {
        return name();
    }

    // Lookup table
    private static final Map<String, Roles> lookup = new HashMap<>();

    // Populate the lookup table on loading time
    static {
        for (Roles role : Roles.values()) {
            lookup.put(role.getName(), role);
        }
    }

    // This method can be used for reverse lookup purpose
    public static Roles get(String name) {
        return lookup.get(name);
    }

}
