/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.HashMap;
import java.util.Map;

import org.springframework.security.core.GrantedAuthority;

/**
 * Enum to define all user roles.
 */
public enum Roles implements GrantedAuthority {

    ORG_USER("ORG_USER"),
    ORG_DEVELOPER("ORG_DEVELOPER"),
    ORG_ADMIN("ORG_ADMIN"),
    CONSORTIUM_ADMIN("CONSORTIUM_ADMIN"),
    SYSTEM_ADMIN("SYSTEM_ADMIN"),
    SYSTEM("SYSTEM"),
    ANONYMOUS("ANONYMOUS");

    private final String name;

    Roles(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    /**
     * True if this is name of one of the roles.
     */
    public static boolean contains(String s) {
        for (Roles r : Roles.values()) {
            if (r.toString().equals(s)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Get Role with this name.
     */
    public static Roles fromString(String s) {
        for (Roles r : Roles.values()) {
            if (r.toString().equals(s)) {
                return r;
            }
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

    // String list of operator role names.
    public static String[] operatorRoles() {
        String[] r = {ORG_ADMIN.name, CONSORTIUM_ADMIN.name, SYSTEM_ADMIN.name};
        return r;
    }

}
