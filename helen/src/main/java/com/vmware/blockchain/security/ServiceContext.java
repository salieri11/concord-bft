/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import java.util.Collections;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import com.vmware.blockchain.auth.AuthenticationContext;
import com.vmware.blockchain.services.profiles.VmbcRoles;

/**
 * Some calls to set and clear the security context to be the service user. This is needed in a couple of places in the
 * system startup.
 */
@Component
public class ServiceContext {

    private final HelenUserDetails systemDetails;

    // The anonymous user is need for accessing the agreement.  This will go away when we fix agreements.
    private final HelenUserDetails anonymousDetails;

    private static final UUID ANON_ID = UUID.fromString("a1c46d96-e08c-4b74-9e83-42f78db2df80");
    private static final String ANON_USER = "Anonymous User";

    @Autowired
    public ServiceContext(@Value("${vmbc.service.id}") UUID serviceId,
            @Value("${vmbc.service.name}") String serviceName) {
        this.systemDetails = new HelenUserDetails(serviceId, serviceName, Collections.singletonList(VmbcRoles.SYSTEM));
        this.anonymousDetails =
                new HelenUserDetails(ANON_ID, ANON_USER, Collections.singletonList(VmbcRoles.ANONYMOUS));
    }

    /**
     * Set the current security context to the system context.
     */
    public void setSystemContext() {
        SecurityContextHolder.getContext().setAuthentication(
                new AuthenticationContext(systemDetails, systemDetails.getAuthorities()));
    }


    /**
     * Set the current security context to the anonymous user.
     */
    public void setAnonymousContext() {
        SecurityContextHolder.getContext().setAuthentication(
                new AuthenticationContext(anonymousDetails, anonymousDetails.getAuthorities()));
    }


    public void clearServiceContext() {
        SecurityContextHolder.getContext().setAuthentication(null);
    }

}
