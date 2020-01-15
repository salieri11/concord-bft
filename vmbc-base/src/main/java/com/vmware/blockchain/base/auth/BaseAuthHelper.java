/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.base.auth;

import java.util.Collection;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.ThreadContext;
import org.springframework.security.access.expression.SecurityExpressionRoot;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

/**
 * A few helper functions for getting fields of Auth.
 */
@Component
public class BaseAuthHelper {

    private static Logger logger = LogManager.getLogger(BaseAuthHelper.class);

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
    public BaseUserDetails getDetails() {
        Authentication auth = getAuthentication();
        if (auth == null) {
            return null;
        } else if (auth instanceof BaseAuthenticationContext) {
            return ((BaseAuthenticationContext) auth).getDetails();
        } else if (auth instanceof UsernamePasswordAuthenticationToken) {
            return (BaseUserDetails) auth.getPrincipal();
        }
        // Don't recognize the Authentication type.
        return null;
    }

    public UUID getUserId() {
        BaseUserDetails details = getDetails();
        return details == null ? null : details.getUserId();
    }

    public String getEmail() {
        BaseUserDetails details = getDetails();
        return details == null ? null : details.getUsername();
    }

    public UUID getOrganizationId() {
        BaseUserDetails details = getDetails();
        return details == null ? null : details.getOrgId();
    }

    public Collection<GrantedAuthority> getAuthorities() {
        BaseUserDetails details = getDetails();
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
    public void setAuthenticationContext(BaseAuthenticationContext authenticationContext) {
        SecurityContextHolder.getContext().setAuthentication(authenticationContext);
        if (authenticationContext != null) {
            ThreadContext.put("userName", authenticationContext.getUserName());
            ThreadContext.put("orgName", authenticationContext.getOrgId().toString());
        }
    }

    /**
     * Mostly used for testing.  Clear the context.
     */
    public void clearAuthenticationContext() {
        SecurityContextHolder.clearContext();
        ThreadContext.remove("userName");
    }


}
