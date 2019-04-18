/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import static com.vmware.blockchain.security.SecurityTestUtils.CONSORTIUM_ID;
import static org.mockito.Mockito.when;

import java.util.Base64;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.util.ReflectionTestUtils;

import com.vmware.blockchain.services.blockchains.BlockchainService;
import com.vmware.blockchain.services.profiles.ConsortiumService;
import com.vmware.blockchain.services.profiles.User;
import com.vmware.blockchain.services.profiles.UserService;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jws;
import io.jsonwebtoken.Jwts;

/**
 * Tests for the JWT Token Provider.
 */
@ExtendWith(SpringExtension.class)
@ContextConfiguration
public class JwtTokenProviderTest {

    @Autowired
    public JwtTokenProvider jwtTokenProvider;

    private User user;

    @MockBean
    private HelenUserDetailsService myUserDetails;

    @MockBean
    private UserService userService;

    @MockBean
    private ConsortiumService consortiumService;

    @MockBean
    private BlockchainService blockService;

    @MockBean
    private ServiceContext serviceContext;

    private String secretKey;

    /**
     * Initialize the mocks.
     */
    @BeforeEach
    void init() {
        // consortium and organization
        MockitoAnnotations.initMocks(this);
        user = SecurityTestUtils.getUser();
        when(userService.getDefaultConsortium(user)).thenReturn(SecurityTestUtils.getConsortium());
        // Get the random secret key out of the token provider
        secretKey = (String) ReflectionTestUtils.getField(jwtTokenProvider, "secretKey");
    }

    @Test
    void testJwt() throws Exception {
        String token = jwtTokenProvider.createRefreshToken(user);
        Jws<Claims> jws = Jwts.parser().setSigningKey(secretKey).parseClaimsJws(token);
        Claims claims = jws.getBody();
        Assertions.assertEquals("user@test.com", claims.getSubject());
        Assertions.assertEquals(CONSORTIUM_ID.toString(), claims.get("context_name", String.class));
        // Make sure this isn't our old "secret key"
        Assertions.assertNotEquals(SecurityTestUtils.SECRET_KEY, secretKey);
        byte[] z = new byte[JwtTokenProvider.KEY_LENGTH];
        // also make sure we don't have an array of zeros
        Assertions.assertNotEquals(Base64.getEncoder().encodeToString(z), secretKey);
    }

    /**
     * Component scan needs to go on configuration.
     */
    @Configuration
    @ComponentScan(basePackageClasses = JwtTokenProvider.class)
    public static class TestConfig {}

}
