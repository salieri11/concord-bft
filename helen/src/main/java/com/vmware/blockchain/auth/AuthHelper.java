/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.auth;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.ThreadContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.security.access.expression.SecurityExpressionRoot;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import com.vmware.blockchain.BaseCacheHelper;
import com.vmware.blockchain.base.auth.BaseAuthHelper;
import com.vmware.blockchain.base.auth.BaseUserDetails;
import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.security.HelenUserDetails;
import com.vmware.blockchain.services.blockchains.BlockchainService;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.blockchain.services.profiles.Roles;
import com.vmware.blockchain.services.profiles.VmbcRoles;

/**
 * A few helper functions for getting fields of Auth.
 */
@Component
public class AuthHelper extends BaseAuthHelper {

    private static Logger logger = LogManager.getLogger(AuthHelper.class);

    // Can't use constructor autowired in the component, due to circular dependencies.
    @Autowired
    @Lazy
    private DefaultProfiles defaultProfiles;

    @Autowired
    VmbcRoles vmbcRoles;

    @Autowired
    private BaseCacheHelper baseCacheHelper;

    /**
     * BaseAuthHelper will return BaseUserDetails.  All user details going in from this level
     * should already have been HelenUserDetails.
     */
    public HelenUserDetails getDetails() {
        BaseUserDetails details = super.getDetails();
        if (details instanceof HelenUserDetails) {
            return (HelenUserDetails) details;
        }
        // Don't recognize the Authentication type.
        return null;
    }

    public String getAuthToken() {
        HelenUserDetails details = getDetails();
        return details == null ? null : details.getAuthToken();
    }

    public List<UUID> getAccessChains() {
        HelenUserDetails details = getDetails();
        return details == null ? Collections.emptyList() : details.getAccessChains();
    }

    public List<UUID> getUpdateChains() {
        HelenUserDetails details = getDetails();
        return details == null ? Collections.emptyList() : details.getUpdateChains();
    }

    public List<UUID> getAccessConsortiums() {
        HelenUserDetails details = getDetails();
        return details == null ? Collections.emptyList() : details.getAccessConsortiums();
    }

    public List<UUID> getUpdateConsortiums() {
        HelenUserDetails details = getDetails();
        return details == null ? Collections.emptyList() : details.getUpdateConsortiums();
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
     * Back compat with old system.
     */
    public boolean canAccessChain(UUID id) {
        logger.info("can access chain {}", id);
        return hasAnyAuthority(Roles.systemAdmin()) || (getAccessChains().contains(id)
                                                        && hasAnyAuthority(Roles.hasAnyRole()));
    }

    /**
     * Back compat with old system.
     */
    public boolean canAccessChain(Optional<UUID> oid) {
        UUID id = oid.orElse(defaultProfiles.getBlockchain().getId());
        return hasAnyAuthority(Roles.systemAdmin()) || (getAccessChains().contains(id)
                && hasAnyAuthority(Roles.hasAnyRole()));
    }

    /**
     * Is Authenticated, has a role.
     */
    public boolean isAuthenticated() {
        return hasAnyAuthority(Roles.hasAnyRole());
    }

    /**
     * Can update chain.
     */
    public boolean canUpdateChain(UUID id) {
        logger.debug("can update chain {}", id);
        return hasAnyAuthority(vmbcRoles.systemAdmin())
                               || (hasAnyAuthority(vmbcRoles.consortiumAdmin()) && getUpdateChains().contains(id));
    }

    /**
     * Back compat with old system.
     */
    public boolean canUpdateChain(Optional<UUID> oid) {
        UUID id = oid.orElse(defaultProfiles.getBlockchain().getId());
        return hasAnyAuthority(vmbcRoles.systemAdmin())
               || (hasAnyAuthority(vmbcRoles.consortiumAdmin()) && getAccessChains().contains(id));
    }

    public boolean canAccessConsortium(UUID id) {
        return hasAnyAuthority(Roles.systemAdmin())
                || (getAccessConsortiums().contains(id) && hasAnyAuthority(Roles.hasAnyRole()));
    }

    public boolean canUpdateConsortium(UUID id) {
        return hasAnyAuthority(vmbcRoles.systemAdmin())
               || (hasAnyAuthority(vmbcRoles.consortiumAdmin()) && getUpdateConsortiums().contains(id));
    }

    public boolean canAccessOrg(UUID id) {
        return hasAnyAuthority(Roles.systemAdmin()) || (getOrganizationId().equals(id)
                && hasAnyAuthority(Roles.hasAnyRole()));
    }

    public boolean canUpdateOrg(UUID id) {
        return hasAnyAuthority(vmbcRoles.systemAdmin())
               || (hasAnyAuthority(vmbcRoles.orgAdmin()) && getOrganizationId().equals(id));
    }

    public boolean isUser() {
        return hasAnyAuthority(vmbcRoles.user());
    }

    public boolean isUserName(String userId) {
        return hasAnyAuthority(vmbcRoles.systemAdmin())
               || (hasAnyAuthority(vmbcRoles.user()) && getUserId().equals(userId));
    }

    public boolean isConsortiumAdmin() {
        return hasAnyAuthority(vmbcRoles.consortiumAdmin());
    }

    public boolean isConsortiumParticipant() {
        return hasAnyAuthority(vmbcRoles.consortiumParticipant());
    }

    public boolean isDeveloper() {
        return hasAnyAuthority(vmbcRoles.developer());
    }

    public boolean isCspOrgOwner() {
        return hasAnyAuthority(vmbcRoles.cspOrgOwner());
    }

    public boolean isSystemAdmin() {
        return hasAnyAuthority(vmbcRoles.systemAdmin());
    }

    public boolean isServiceAdmin() {
        return hasAnyAuthority(vmbcRoles.serviceAdmin());
    }

    public void evictToken() {
        baseCacheHelper.evict(Constants.CSP_TOKEN_CACHE, getAuthToken());
    }

    /**
     * Is the logged-in user a Consortium admin for the supplied Blockchain?
     * @param bId blockchainId bId
     * @return true if the user is administrator, false otherwise.
     */
    public boolean isUserConsortiumAdminForBlockchain(UUID bId) {
        // How do we populate UserProfile:
        // Blockchain is linked with Consortium.
        // We get orgId for the logged-in user.
        // Pull all the Consortiums for this organization.
        // Pull Blockchain for each Consortium.
        // Finally user details has lists of Consortium and Blockchain Ids that this user has access to.
        // So whether the user has Consortium Admin role, and access to this blockchain is all we need to check.
        return hasAnyAuthority(vmbcRoles.consortiumAdmin()) && getAccessChains().contains(bId);
    }


    /**
     * Mostly used for testing.  Set the context.
     * @param authenticationContext Authoriztion context.
     */
    public void setAuthenticationContext(AuthenticationContext authenticationContext) {
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
