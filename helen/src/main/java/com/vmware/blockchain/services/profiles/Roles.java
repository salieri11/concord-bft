/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.springframework.security.core.GrantedAuthority;

/**
 * Enum to define all user roles.
 */
public enum Roles implements GrantedAuthority {

    ORG_USER("vmbc-org:user", "Organization User", true, false, false),
    ORG_DEVELOPER("vmbc-org:dev", "Organization Developer", false, false, false),
    ORG_ADMIN("vmbc-org:admin", "Organization Admin", false, false, false),
    CONSORTIUM_ADMIN("vmbc-consortium:admin", "Consortium Admin", false, false, false),
    SYSTEM_ADMIN("vmbc-system:admin", "System Admin", false, false, false),
    SYSTEM("SYSTEM", "Internal System User", false, true, true),
    ANONYMOUS("ANONYMOUS", "Temporary ananymous user", false, true, true);

    private final String name;
    private final String displayName;
    private final boolean isDefault;
    private final boolean isHidden; // should this show in CSP usermgmt UI.
    // If true - don't register with CSP - this role will be assigned based on other criteria e.g. Org property
    private final boolean isInternal;


    Roles(String name, String displayName,
          boolean isDefault, boolean isHidden,
          boolean isInternal) {
        this.name = name;
        this.displayName = displayName;
        this.isDefault = isDefault;
        this.isHidden = isHidden;
        this.isInternal = isInternal;
    }

    public String getName() {
        return name;
    }

    public String getDisplayName() {
        return displayName;
    }

    public boolean isDefault() {
        return isDefault;
    }

    public boolean isHidden() {
        return isHidden;
    }

    public boolean isInternal() {
        return isInternal;
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
        return getName();
    }

    // Lookup table
    private static final Map<String, Roles> lookup = new HashMap<>();

    // Populate the lookup table on loading time
    static {
        for (Roles role : Roles.values()) {
            lookup.put(role.getName(), role);
        }
    }

    // get the set of service roles that are visible.
    public static Set<Roles> getServiceRoles() {
        return Arrays.stream(Roles.values()).filter(r -> !r.isInternal).collect(Collectors.toSet());
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
