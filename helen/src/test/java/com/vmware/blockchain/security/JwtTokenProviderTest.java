/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import static com.vmware.blockchain.security.SecurityTestUtils.CONSORTIUM_ID;
import static com.vmware.blockchain.security.SecurityTestUtils.SECRET_KEY;

import org.junit.Test;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.vmware.blockchain.services.profiles.User;

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

    /**
     * Initialize the mocks.
     */
    @BeforeEach
    void init() {
        // consortium and organization
        MockitoAnnotations.initMocks(this);
        user = SecurityTestUtils.createMockUser();
    }

    @Test
    void testJwt() throws Exception {
        String token = jwtTokenProvider.createRefreshToken(user);
        Jws<Claims> jws = Jwts.parser().setSigningKey(SECRET_KEY).parseClaimsJws(token);
        Claims claims = jws.getBody();
        Assertions.assertEquals("user@test.com", claims.getSubject());
        Assertions.assertEquals(CONSORTIUM_ID.toString(), claims.get("context_name", String.class));
        System.out.println(jws);
    }

    /**
     * Component scan needs to go on configuration.
     */
    @Configuration
    @ComponentScan(basePackageClasses = JwtTokenProvider.class)
    public static class TestConfig {}

}
