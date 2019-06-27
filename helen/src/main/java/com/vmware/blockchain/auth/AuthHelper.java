/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.auth;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.ThreadContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.expression.SecurityExpressionRoot;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import com.vmware.blockchain.BaseCacheHelper;
import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.security.HelenUserDetails;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.blockchain.services.profiles.Roles;

/**
 * A few helper functions for getting fields of Auth.
 */
@Component
public class AuthHelper {

    private static Logger logger = LogManager.getLogger(AuthHelper.class);

    // Can't use constructor autowired in the component, due to circular dependencies.
    @Autowired
    private DefaultProfiles defaultProfiles;

    @Autowired
    private BaseCacheHelper baseCacheHelper;

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

    public List<UUID> getConsortiums() {
        HelenUserDetails details = getDetails();
        return details == null ? Collections.emptyList() : details.getConsortiums();
    }

    public Collection<GrantedAuthority> getAuthorities() {
        HelenUserDetails details = getDetails();
        return details == null ? null : details.getAuthorities();
    }

    public boolean hasAnyAuthority(String... authorities) {
        SecurityExpressionRoot security = new SecurityExpressionRoot(getAuthentication()) {};
        return security.hasAnyAuthority(authorities);
    }

    public boolean canAccessChain(UUID id) {
        logger.info("can access chain {}", id);
        return hasAnyAuthority(Roles.systemAdmin()) || getPermittedChains().contains(id);
    }

    /**
     * Back compat with old system.
     */
    public boolean canAccessChain(Optional<UUID> oid) {
        UUID id = oid.orElse(defaultProfiles.getBlockchain().getId());
        return hasAnyAuthority(Roles.systemAdmin()) || getPermittedChains().contains(id);
    }

    /**
     * Can update chain.
     */
    public boolean canUpdateChain(UUID id) {
        logger.debug("can update chain {}", id);
        return hasAnyAuthority(Roles.systemAdmin())
                               || (hasAnyAuthority(Roles.consortiumAdmin()) && getPermittedChains().contains(id));
    }

    /**
     * Back compat with old system.
     */
    public boolean canUpdateChain(Optional<UUID> oid) {
        UUID id = oid.orElse(defaultProfiles.getBlockchain().getId());
        return hasAnyAuthority(Roles.systemAdmin())
               || (hasAnyAuthority(Roles.consortiumAdmin()) && getPermittedChains().contains(id));
    }

    public boolean canAccessConsortium(UUID id) {
        return hasAnyAuthority(Roles.systemAdmin()) || getConsortiums().contains(id);
    }

    public boolean canUpdateConsortium(UUID id) {
        return hasAnyAuthority(Roles.systemAdmin())
               || (hasAnyAuthority(Roles.consortiumAdmin()) && getConsortiums().contains(id));
    }

    public boolean canAccessOrg(UUID id) {
        return hasAnyAuthority(Roles.systemAdmin()) || getOrganizationId().equals(id);
    }

    public boolean canUpdateOrg(UUID id) {
        return hasAnyAuthority(Roles.systemAdmin())
               || (hasAnyAuthority(Roles.orgAdmin()) && getOrganizationId().equals(id));
    }

    public void evictToken() {
        baseCacheHelper.evict(Constants.CSP_TOKEN_CACHE, getAuthToken());
    }

    /**
     * Mostly used for testing.  Set the context.
     * @param authenticationContext Authoriztion context.
     */
    public void setAuthenticationContext(AuthenticationContext authenticationContext) {
        SecurityContextHolder.getContext().setAuthentication(authenticationContext);
        if (authenticationContext != null) {
            ThreadContext.put("userName", authenticationContext.getUserName());
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
