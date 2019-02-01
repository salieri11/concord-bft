/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import java.util.Collection;

import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

/**
 * A few helper functions for getting fields of Auth.
 */
@Component
public class AuthHelper {

    /**
     * Get the HelenUserDetails from the security context.
     */
    public HelenUserDetails getDetails() {
        SecurityContext ctx = SecurityContextHolder.getContext();
        if (ctx == null) {
            return null;
        }
        Authentication auth = ctx.getAuthentication();
        return auth == null ? null : (HelenUserDetails) auth.getPrincipal();
    }

    public String getEmail() {
        HelenUserDetails details = getDetails();
        return details == null ? null : details.getUsername();
    }

    public String getConsortiumId() {
        HelenUserDetails details = getDetails();
        return details == null ? null : details.getOrgId();
    }

    public String getAuthToken() {
        HelenUserDetails details = getDetails();
        return details == null ? null : details.getAuthToken();
    }

    public Collection<GrantedAuthority> getAuthorities() {
        HelenUserDetails details = getDetails();
        return details == null ? null : details.getAuthorities();
    }

}
