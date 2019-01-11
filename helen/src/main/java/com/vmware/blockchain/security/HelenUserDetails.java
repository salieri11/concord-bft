/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import java.util.Collection;

import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.User;

import lombok.Getter;
import lombok.Setter;

/**
 * Helen specific User Details.  Includes the auth token and org id.
 */
public class HelenUserDetails extends User {

    private static final long serialVersionUID = 1L;
    @Getter
    @Setter
    private String authToken;

    @Getter
    @Setter
    private String orgId;

    /**
     * Create a new instance of HelenUserDetails.
     */
    public HelenUserDetails(String username, String password, boolean enabled, boolean accountNonExpired,
            boolean credentialsNonExpired, boolean accountNonLocked,
            Collection<? extends GrantedAuthority> authorities) {
        super(username, password, enabled, accountNonExpired, credentialsNonExpired, accountNonLocked, authorities);
    }

}
