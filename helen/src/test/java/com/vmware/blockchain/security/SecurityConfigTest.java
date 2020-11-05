/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;

import javax.servlet.Filter;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.context.web.WebAppConfiguration;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

import com.vmware.blockchain.BaseCacheHelper;
import com.vmware.blockchain.MvcConfig;
import com.vmware.blockchain.services.blockchains.BlockchainService;
import com.vmware.blockchain.services.profiles.ConsortiumService;
import com.vmware.blockchain.services.profiles.OrganizationService;
import com.vmware.blockchain.services.profiles.UserService;

/**
 * SecurityConfigTest -- Make sure the security configuration is loaded, and that
 * the Content-Security-Policy header is set.
 */
@ExtendWith(SpringExtension.class)
@ContextConfiguration(classes = {CspSecurityConfig.class, MvcConfig.class})
@TestPropertySource(locations = "classpath:test.properties")
@EnableWebMvc
@WebAppConfiguration
public class SecurityConfigTest {

    @MockBean
    TokenAuthenticationConfig tokenAuthenticationConfig;

    @MockBean
    TokenAuthenticationProvider tokenAuthenticationProvider;

    @MockBean
    VmbcBasicAuthProvider vmbcBasicAuthProvider;

    @MockBean
    private RestAuthenticationEntryPoint restAuthticationEntryPoint;

    @MockBean
    private TokenRefreshFilter tokenRefreshFilter;

    @MockBean
    private UserService userService;

    @MockBean
    private BlockchainService blockchainService;

    @MockBean
    private ConsortiumService consortiumService;

    @MockBean
    private OrganizationService organizationService;

    @MockBean
    private BaseCacheHelper baseCacheHelper;

    @Autowired
    WebApplicationContext webApplicationContext;

    @Autowired
    private Filter springSecurityFilterChain;

    private MockMvc mockMvc;

    @BeforeEach
    void init() throws Exception {
        mockMvc = MockMvcBuilders
                .webAppContextSetup(webApplicationContext)
                .addFilters(springSecurityFilterChain)
                .build();

    }


    @Test
    // Make sure the Content-Security-Policy header is set.
    void test1() throws Exception {
        MvcResult result = mockMvc.perform(get("/api/users"))
                .andReturn();
        String securityPolicy = result.getResponse().getHeader("Content-Security-Policy");
        Assertions.assertEquals("script-src 'self'", securityPolicy);
    }
}
