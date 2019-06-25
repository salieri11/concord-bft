/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.stereotype.Component;

import com.vmware.blockchain.auth.AuthenticationContext;
import com.vmware.blockchain.common.csp.CspAuthenticationHelper;

/**
 * Handle Basic Auth request.  In this case, password must be a valid api token from csp, matching the
 * username given.
 */
@Component
public class VmbcBasicAuthProver implements AuthenticationProvider {
    private CspAuthenticationHelper cspAuthenticationHelper;
    private TokenAuthenticationProvider tokenAuthenticationProvider;

    @Autowired
    public VmbcBasicAuthProver(TokenAuthenticationProvider tokenAuthenticationProvider,
                               @Value("${csp.url:http://csp}") String cspUrl) {
        this.cspAuthenticationHelper = new CspAuthenticationHelper(cspUrl);
        this.tokenAuthenticationProvider = tokenAuthenticationProvider;

    }

    @Override
    public Authentication authenticate(Authentication authentication) throws AuthenticationException {
        UsernamePasswordAuthenticationToken upt = (UsernamePasswordAuthenticationToken) authentication;
        // The api token will be in the credentials.
        try {
            String token = cspAuthenticationHelper.fetchAuthTokenFromRefreshToken((String) upt.getCredentials());
            AuthenticationContext context = tokenAuthenticationProvider.populateAuthContext(token);
            // make sure the name is the same.
            if (!context.getDetails().getUsername().equals(upt.getPrincipal())) {
                return null;
            }
            return context;
        } catch (Exception e) {
            // if anything went wrong, return no auth.
            return null;
        }
    }

    @Override
    public boolean supports(Class<?> authentication) {
        return authentication.equals(UsernamePasswordAuthenticationToken.class);
    }
}
