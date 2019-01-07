/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Base64;
import java.util.UUID;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

import com.vmware.blockchain.services.profiles.Consortium;
import com.vmware.blockchain.services.profiles.Organization;
import com.vmware.blockchain.services.profiles.Roles;
import com.vmware.blockchain.services.profiles.User;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jws;
import io.jsonwebtoken.Jwts;

/**
 * Tests for the JWT Token Provider.
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
public class JwtTokenProviderTest {
    // Just some random UUIDs
    private static final UUID USER_ID = UUID.fromString("f1c1aa4f-4958-4e93-8a51-930d595fb65b");
    private static final UUID ORG_ID = UUID.fromString("82634974-88cf-4944-a99d-6b92664bb765");
    private static final UUID CONSORTIUM_ID = UUID.fromString("5c7cd0e9-57ad-44af-902f-74af2f3dd8fe");


    @Autowired
    public JwtTokenProvider jwtTokenProvider;

    @Mock
    private User user;
    @Mock
    private Organization organization;
    @Mock
    private Consortium consortium;

    @MockBean
    private MyUserDetails myUserDetails;

    String secretKey;

    /**
     * Initialize the mocks.
     */
    @Before
    public void init() {
        // consortium and organization
        MockitoAnnotations.initMocks(this);
        when(consortium.getConsortiumId()).thenReturn(CONSORTIUM_ID);
        when(consortium.getConsortiumName()).thenReturn("Consortium Test");
        when(consortium.getConsortiumType()).thenReturn("Constorium Type");
        when(organization.getOrganizationId()).thenReturn(ORG_ID);
        when(organization.getOrganizationName()).thenReturn("Test Org");
        when(user.getUserId()).thenReturn(USER_ID);
        when(user.getEmail()).thenReturn("user@test.com");
        when(user.getFirstName()).thenReturn("U");
        when(user.getLastName()).thenReturn("Ser");
        when(user.getConsortium()).thenReturn(consortium);
        when(user.getOrganization()).thenReturn(organization);
        when(user.getRole()).thenReturn(Roles.ORG_USER.toString());
        when(user.getRoles()).thenReturn(Arrays.asList(Roles.ORG_USER));
        secretKey = Base64.getEncoder().encodeToString("secret-key".getBytes());
    }

    @Test
    public void testJwt() throws Exception {
        String token = jwtTokenProvider.createRefreshToken(user);
        Jws<Claims> jws = Jwts.parser().setSigningKey(secretKey).parseClaimsJws(token);
        Claims claims = jws.getBody();
        Assert.assertEquals("user@test.com", claims.getSubject());
        Assert.assertEquals(CONSORTIUM_ID.toString(), claims.get("context_name", String.class));
        System.out.println(jws);
    }

    /**
     * Component scan needs to go on configuration.
     */
    @Configuration
    @ComponentScan(basePackageClasses = JwtTokenProvider.class)
    public static class TestConfig {}

}
