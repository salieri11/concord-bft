/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.auth;

import java.util.Collection;
import java.util.UUID;
import java.util.stream.Collectors;

import org.springframework.security.authentication.AbstractAuthenticationToken;
import org.springframework.security.core.GrantedAuthority;

import com.vmware.blockchain.security.HelenUserDetails;

import lombok.Getter;

/**
 * Custom implementation for {@link org.springframework.security.core.Authentication}.
 * AuthenticationContext contains information about user such as its user name, roles and tenant
 * etc.
 */
public class AuthenticationContext extends AbstractAuthenticationToken {

    private static final long serialVersionUID = 1L;

    @Getter
    private final UUID userId;
    @Getter
    private final String userName;
    @Getter
    private final UUID orgId;
    @Getter
    private final String authToken;
    @Getter
    private final HelenUserDetails details;

    /**
     * Constructor used for an authentication response.
     * @param userId authenticated userId
     * @param orgId for the user
     * @param userName readable username
     * @param authToken user's authentication token
     * @param authorities The granted authorities
     */
    public AuthenticationContext(UUID userId, UUID orgId, String userName, String authToken,
                                 Collection<? extends GrantedAuthority> authorities) {
        super(authorities);
        this.userId = userId;
        this.userName = userName;
        this.orgId = orgId;
        this.authToken = authToken;
        this.details = new HelenUserDetails(userId, orgId, userName, authToken, authorities);
        setAuthenticated(true);
    }

    /**
     * Constructor used for an authentication response filled in from HelenUserDetails.
     * @param details HelenUserDetails for this token
     * @param authorities The granted authorities
     */
    public AuthenticationContext(HelenUserDetails details,
                                 Collection<? extends GrantedAuthority> authorities) {
        super(authorities);
        this.userId = details.getUserId();
        this.userName = details.getUsername();
        this.orgId = details.getOrgId();
        this.authToken = details.getAuthToken();
        this.details = details;
        setAuthenticated(true);
    }

    @Override
    public Object getCredentials() {
        return null;
    }

    @Override
    public Object getPrincipal() {
        return userId;
    }

    @Override
    public String toString() {
        return String.format("AuthContext: userName %s orgId %s orgType %s isImpersonation %s"
                             + " isCrossOrg %s roles [%s]",
                             userName, orgId,
                             getAuthorities() == null ? "[None]" :
                             getAuthorities().stream().map(GrantedAuthority::getAuthority)
                                     .collect(Collectors.joining(",")));
    }

}
