/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import javax.servlet.http.HttpServletRequest;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.google.common.collect.ImmutableList;
import com.vmware.blockchain.auth.AuthenticationContext;
import com.vmware.blockchain.auth.TokenValidator;
import com.vmware.blockchain.common.csp.CspConfig;
import com.vmware.blockchain.common.csp.api.client.CspApiClient;
import com.vmware.blockchain.services.profiles.Roles;
import com.vmware.blockchain.utils.ControllerTestConfig;

/**
 * Test class for TokenAuthenticationProvider.
 */
@ExtendWith(SpringExtension.class)
@TestPropertySource(locations = { "classpath:test.properties" })
@ContextConfiguration(classes = { ControllerTestConfig.class })
public class TokenAuthenticationProviderTests {

    private static final UUID USER_ID = UUID.fromString("2daa2ba8-3113-401a-91eb-ed16107330ab");

    private TokenAuthenticationProvider tokenAuthenticationProvider;
    private HttpServletRequest mockRequest;
    private TokenValidator tokenValidator;
    private UUID operatorOrg;
    private CspConfig cspConfig;
    private CspApiClient cspApiClient;
    private Map<String, String> impersonateInfo = new HashMap<>();

    /**
     * Create mocks.
     */
    @BeforeEach
    public void setup() throws Exception {
        mockRequest = mock(HttpServletRequest.class);
        tokenValidator = mock(TokenValidator.class);
        cspConfig = mock(CspConfig.class);
        cspApiClient = mock(CspApiClient.class);
        operatorOrg = UUID.randomUUID();
        tokenAuthenticationProvider = new TokenAuthenticationProvider(cspConfig, cspApiClient,
                tokenValidator, operatorOrg.toString());

    }

    /**
     * Test basic happy path for operator.
     */
    @Test
    public void testSimpleAuthContext() throws Exception {
        HelenUserDetails userInfo = setupUser(operatorOrg, ImmutableList.of(Roles.SYSTEM_ADMIN));

        when(tokenValidator.validateAndGetAuthz("mytoken")).thenReturn(userInfo);

        when(mockRequest.getRequestURI()).thenReturn("/vmc/test/api/operator/orgs");
        AuthenticationContext authContext = tokenAuthenticationProvider.populateAuthContext("mytoken");
        Collection<GrantedAuthority> grantedAuthorities = authContext.getAuthorities();
        Assertions.assertEquals(Roles.SYSTEM_ADMIN.toString(), grantedAuthorities.toArray()[0].toString());
    }

    private HelenUserDetails setupUser(UUID orgId, List<Roles> roles) {
        return new HelenUserDetails(USER_ID, orgId, "user", "", roles);
    }
}
