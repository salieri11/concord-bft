/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import java.time.Instant;
import java.util.Collection;
import java.util.List;
import java.util.UUID;

import org.springframework.security.core.GrantedAuthority;

import com.vmware.blockchain.base.auth.BaseUserDetails;

import lombok.Getter;
import lombok.Setter;

/**
 * Helen specific User Details.  Includes the auth token and org id.
 */
public class HelenUserDetails extends BaseUserDetails {

    private static final long serialVersionUID = 1L;
    @Getter
    @Setter
    private String authToken;

    @Getter
    @Setter
    private List<UUID> accessChains;

    @Getter
    @Setter
    private List<UUID> updateChains;

    @Getter
    @Setter
    private List<UUID> accessConsortiums;

    @Getter
    @Setter
    private List<UUID> updateConsortiums;

    @Getter
    @Setter
    private Instant lastLogin;

    /**
     * Create a new instance of HelenUserDetails.
     */
    public HelenUserDetails(UUID userId, String username, String password, boolean enabled, boolean accountNonExpired,
            boolean credentialsNonExpired, boolean accountNonLocked,
            Collection<? extends GrantedAuthority> authorities) {
        super(userId, username, password, enabled, accountNonExpired, credentialsNonExpired, accountNonLocked,
              authorities);
    }

    /**
     * Create a new instance of HelenUserDetails, default all the booleans.
     */
    public HelenUserDetails(UUID userId, String username, String password,
                            Collection<? extends GrantedAuthority> authorities) {
        super(userId, username, password, true, true, true, true, authorities);
    }

    /**
     * Create a new instance of HelenUserDetails, default all the booleans,
     * and set the orgId and authtoken.
     */
    public HelenUserDetails(UUID userId, UUID orgId, String username, String authToken,
                            Collection<? extends GrantedAuthority> authorities) {
        super(userId, username, "", true, true, true, true, authorities);
        setOrgId(orgId);
        this.authToken = authToken;
    }


    /**
     * Create a new instance of HelenUserDetails, default all the booleans and the password.
     */
    public HelenUserDetails(UUID userId, String username, Collection<? extends GrantedAuthority> authorities) {
        super(userId, username, "", true, true, true, true, authorities);
    }
}
