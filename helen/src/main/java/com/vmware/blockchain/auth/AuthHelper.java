/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.auth;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import org.slf4j.MDC;
import org.springframework.security.access.expression.SecurityExpressionRoot;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import com.vmware.blockchain.security.HelenUserDetails;

/**
 * A few helper functions for getting fields of Auth.
 */
@Component
public class AuthHelper {

    /**
     * Get the current Authentication.  Return null if none.
     * Sigh... In order to support basic auth, we need to handle
     * the case that the Authentication is a UsernamePasswordAuthenticationToken from deep
     * inside Spring.
     */
    private Authentication getAuthentication() {
        SecurityContext ctx = SecurityContextHolder.getContext();
        if (ctx == null) {
            return null;
        }
        return ctx.getAuthentication();
    }

    /**
     * Get the HelenUserDetails from the security context, bassed on whether this
     * is AuthenticationContext or UsernamePasswordAuthenticationToken.
     */
    public HelenUserDetails getDetails() {
        Authentication auth = getAuthentication();
        if (auth == null) {
            return null;
        } else if (auth instanceof AuthenticationContext) {
            return ((AuthenticationContext) auth).getDetails();
        } else if (auth instanceof UsernamePasswordAuthenticationToken) {
            // Well, this looks nasty
            return (HelenUserDetails) ((UsernamePasswordAuthenticationToken) auth).getPrincipal();
        }
        // Don't recognize the Authentication type.
        return null;
    }

    public UUID getUserId() {
        HelenUserDetails details = getDetails();
        return details == null ? null : details.getUserId();
    }

    public String getEmail() {
        HelenUserDetails details = getDetails();
        return details == null ? null : details.getUsername();
    }

    // Switching to consortiumId, since internally we're still calling it that
    public UUID getOrganizationId() {
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

    /**
     * Mostly used for testing.  Set the context.
     * @param authenticationContext Authoriztion context.
     */
    public void setAuthenticationContext(AuthenticationContext authenticationContext) {
        SecurityContextHolder.getContext().setAuthentication(authenticationContext);
        if (authenticationContext != null) {
            MDC.put("userName", authenticationContext.getUserName());
        }
    }

    /**
     * Mostly used for testing.  Clear the context.
     */
    public void clearAuthenticationContext() {
        SecurityContextHolder.clearContext();
        MDC.remove("userName");
    }


}
