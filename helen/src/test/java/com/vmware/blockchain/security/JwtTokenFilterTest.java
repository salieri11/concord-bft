/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Arrays;
import java.util.Base64;
import java.util.Optional;

import javax.servlet.Filter;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpHeaders;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

import com.vmware.blockchain.WebSecurityConfig;
import com.vmware.blockchain.services.profiles.Roles;
import com.vmware.blockchain.services.profiles.User;
import com.vmware.blockchain.services.profiles.UserRepository;


/**
 * Test the JwtTokenFilter.  Make sure this filter populates the auth context.
 */
@ExtendWith(SpringExtension.class)
@SpringBootTest(classes = {WebSecurityConfig.class, JwtTestContoller.class, AuthHelper.class})
public class JwtTokenFilterTest {
    @Autowired
    public JwtTokenProvider jwtTokenProvider;

    private User user;

    @MockBean
    RestAuthenticationEntryPoint restAuthenticationEndpoint;

    @MockBean
    private UserRepository userRepository;

    private MockMvc mockMvc;

    @Autowired
    private WebApplicationContext webApplicationContext;

    @Autowired
    private Filter springSecurityFilterChain;

    /*    {
            "sub": "user@test.com",
            "perms": [
              "ORG_USER"
            ],
            "context_name": "5c7cd0e9-57ad-44af-902f-74af2f3dd8fe",
            "iat": 1547145715,
            "exp": 1547147515
          }
    */
    private String expiredToken =
            "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1c2VyQHRlc3QuY29tIiwicGVybXMiOlsiT1JHX1VTRVIiXSwiY29ud"
            + "GV4dF9uYW1lIjoiNWM3Y2QwZTktNTdhZC00NGFmLTkwMmYtNzRhZjJmM2RkOGZlIiwiaWF0IjoxNTQ3MTQ1Nz"
            + "E1LCJleHAiOjE1NDcxNDc1MTV9.wuNQR6ceYPHgBYoIarPaKFEGsYi748cG9ObkOeMD9G4";

    private String token;

    /**
     * Initialize the mockMvc with the security filter chain.
     */
    @BeforeEach
    public void init() {
        user = SecurityTestUtils.createMockUser();
        when(userRepository.findUserByEmail(user.getEmail())).thenReturn(Optional.of(user));
        token = jwtTokenProvider.createToken(user);

        mockMvc = MockMvcBuilders.webAppContextSetup(webApplicationContext)
                .addFilters(springSecurityFilterChain)
                .build();
    }

    @Test
    public void testAccessToRestrictedResourceWithoutToken() throws Exception {
        // No token should give unauthorized
        this.mockMvc.perform(get("/api/users")).andExpect(status().isUnauthorized());
    }

    @Test
    public void testAccessToRestrictedResourceExpiredToken() throws Exception {
        // No token should give unauthorized
        this.mockMvc.perform(get("/api/users").header(HttpHeaders.AUTHORIZATION, "Bearer " + expiredToken))
            .andExpect(status().isUnauthorized());
    }

    @Test
    public void testAccessToRestrictedResourceWithAuthorization() throws Exception {
        MvcResult result = mockMvc.perform(get("/api/concord/eth").header(HttpHeaders.AUTHORIZATION, "Bearer " + token))
            .andExpect(status().isOk()).andReturn();
        Assertions.assertEquals("Tests passed", result.getResponse().getContentAsString());
    }

    @Test
    public void testAccessToRestrictedResourceWithoutAuthorization() throws Exception {
        mockMvc.perform(get("/api/users").header(HttpHeaders.AUTHORIZATION, "Bearer " + token))
            .andExpect(status().isForbidden());
    }

    @Test
    public void testAccessToRestrictedResourceWithNewAuthorization() throws Exception {
        when(user.getRoles()).thenReturn(Arrays.asList(Roles.SYSTEM_ADMIN));
        mockMvc.perform(
                get("/api/users").header(HttpHeaders.AUTHORIZATION, "Bearer " + jwtTokenProvider.createToken(user)))
                .andExpect(status().isOk());
    }

    @Test
    public void testAccessToPermitedResourceWithoutToken() throws Exception {
        this.mockMvc.perform(get("/api/auth/login")).andExpect(status().isOk());
    }

    @Test
    public void testAccessBasicAuth() throws Exception {
        String basicAuth = "Basic " + Base64.getEncoder().encodeToString("user@test.com:1234".getBytes());
        MvcResult result = mockMvc.perform(get("/api/concord/eth").header(HttpHeaders.AUTHORIZATION, basicAuth))
                .andExpect(status().isOk()).andReturn();
        Assertions.assertEquals("Tests passed", result.getResponse().getContentAsString());
    }


}
