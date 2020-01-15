/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.base.auth;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Class to define base roles.
 * Services using this can extend this class, but need to make sure the derived class is
 * instantiated once before using, to fill in the rest of the static fields.
 */
public class BaseRoles {
    // Lookup table
    private static final Map<String, Role> roleMap = new HashMap<>();

    public static final Role ORG_USER = addRole("vmbc-org:user", "Organization User", true, false, false);
    public static final Role ORG_ADMIN = addRole("vmbc-org:admin", "Organization Admin", false, false, false);
    public static final Role SYSTEM_ADMIN = addRole("vmbc-system:admin", "VMware SRE Admin", false, true, false);
    public static final Role INFRA_ADMIN =
            addRole("vmbc-system:infra", "External Infrastructure Admin", false, true, false);
    // These are not service roles, but are used internally
    public static final Role CSP_ORG_OWNER = addRole("csp:org_owner", "CSP Org Owner", false, true, true);
    // Spring uses ROLE_permission in many cases
    public static final Role ROLE_SYSTEM_ADMIN =
            addRole("ROLE_vmbc-system:admin", "VMware SRE Admin", true, true, true);

    public static final Role SYSTEM = addRole("SYSTEM", "Internal System User", false, true, true);

    public static final Role ANONYMOUS = addRole("ANONYMOUS", "Temporary ananymous user", false, true, true);

    /**
     * Add a role to the role map.
     */
    public static Role addRole(String name, String displayName, boolean isDefault, boolean isHidden,
                               boolean isInternal) {
        Role role = new Role(name, displayName, isDefault, isHidden, isInternal);
        roleMap.put(name, role);
        return role;
    }


    /**
     * True if this is name of one of the roles.
     */
    public static boolean contains(String s) {
        return roleMap.containsKey(s);
    }

    /**
     * Get Role with this name.
     */
    public static Role fromString(String s) {
        return roleMap.getOrDefault(s, null);
    }

    // get the set of service roles that are visible.
    public static Set<Role> getServiceRoles() {
        return roleMap.values().stream().filter(r -> !r.isInternal()).collect(Collectors.toSet());
    }

    // This method can be used for reverse lookup purpose
    public static Role get(String name) {
        return roleMap.get(name);
    }

    public static String[] systemAdmin() {
        String[] r = {SYSTEM_ADMIN.getName()};
        return r;
    }

    public static String[] infraAdmin() {
        String[] r = {INFRA_ADMIN.getName()};
        return r;
    }

    public static String[] serviceAdmin() {
        String[] r = {INFRA_ADMIN.getName(), SYSTEM_ADMIN.getName()};
        return r;
    }

    public static String[] orgAdmin() {
        String[] r = {INFRA_ADMIN.getName(), SYSTEM_ADMIN.getName(), ORG_ADMIN.getName()};
        return r;
    }

    public static String[] user() {
        String[] r = {INFRA_ADMIN.getName(), SYSTEM_ADMIN.getName(), ORG_USER.getName()};
        return r;
    }

    public static String[] cspOrgOwner() {
        String[] r = {CSP_ORG_OWNER.getName()};
        return r;
    }

}
