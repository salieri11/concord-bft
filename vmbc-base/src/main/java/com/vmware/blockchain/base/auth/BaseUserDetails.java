/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.base.auth;

import java.util.Collection;
import java.util.UUID;

import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.User;

import lombok.Getter;
import lombok.Setter;

/**
 * Helen specific User Details.  Includes the auth token and org id.
 */
public class BaseUserDetails extends User {

    private static final long serialVersionUID = 1L;

    @Getter
    @Setter
    private UUID orgId;

    @Getter
    @Setter
    private UUID userId;

    @Getter
    @Setter
    private String userName;


    /**
     * Create a new instance of HelenUserDetails.
     */
    public BaseUserDetails(UUID userId, String username, String password, boolean enabled, boolean accountNonExpired,
                           boolean credentialsNonExpired, boolean accountNonLocked,
                           Collection<? extends GrantedAuthority> authorities) {
        super(username, password, enabled, accountNonExpired, credentialsNonExpired, accountNonLocked, authorities);
        this.userId = userId;
        this.userName = username;
    }

    /**
     * Create a new instance of HelenUserDetails, default all the booleans.
     */
    public BaseUserDetails(UUID userId, String username, String password,
                           Collection<? extends GrantedAuthority> authorities) {
        super(username, password, true, true, true, true, authorities);
        this.userId = userId;
        this.userName = username;
    }

    /**
     * Create a new instance of HelenUserDetails, default all the booleans,
     * and set the orgId and authtoken.
     */
    public BaseUserDetails(UUID userId, UUID orgId, String username,
                           Collection<? extends GrantedAuthority> authorities) {
        super(username, "", true, true, true, true, authorities);
        this.userId = userId;
        this.orgId = orgId;
        this.userName = username;
    }

}
