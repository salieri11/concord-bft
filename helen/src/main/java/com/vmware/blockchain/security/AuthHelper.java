/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import org.springframework.security.access.expression.SecurityExpressionRoot;
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
     * Get the current Authentication.  Return null if none.
     */
    public Authentication getAuthentication() {
        SecurityContext ctx = SecurityContextHolder.getContext();
        if (ctx == null) {
            return null;
        }
        return ctx.getAuthentication();
    }

    /**
     * Get the HelenUserDetails from the security context.
     */
    public HelenUserDetails getDetails() {
        Authentication auth = getAuthentication();
        return auth == null ? null : (HelenUserDetails) auth.getPrincipal();
    }

    public String getEmail() {
        HelenUserDetails details = getDetails();
        return details == null ? null : details.getUsername();
    }

    // Switching to consortiumId, since internally we're still calling it that
    public String getConsortiumId() {
        HelenUserDetails details = getDetails();
        return details == null ? null : details.getOrgId();
    }

    public String getAuthToken() {
        HelenUserDetails details = getDetails();
        return details == null ? null : details.getAuthToken();
    }

    public List<UUID> getPermittedChains() {
        HelenUserDetails details = getDetails();
        return details == null ? Collections.emptyList() : details.getPermittedChains();
    }

    public Collection<GrantedAuthority> getAuthorities() {
        HelenUserDetails details = getDetails();
        return details == null ? null : details.getAuthorities();
    }

    public boolean hasAnyAuthority(String... authorities) {
        SecurityExpressionRoot security = new SecurityExpressionRoot(getAuthentication()) {};
        return security.hasAnyAuthority(authorities);
    }
}
