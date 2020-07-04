/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.security;

import static com.vmware.blockchain.security.SecurityTestUtils.BC_ID;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.httpBasic;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Arrays;
import java.util.Base64;
import java.util.Collections;
import java.util.UUID;

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

import com.vmware.blockchain.BaseCacheHelper;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.services.blockchains.Blockchain;
import com.vmware.blockchain.services.blockchains.BlockchainService;
import com.vmware.blockchain.services.profiles.Consortium;
import com.vmware.blockchain.services.profiles.ConsortiumService;
import com.vmware.blockchain.services.profiles.Organization;
import com.vmware.blockchain.services.profiles.OrganizationService;
import com.vmware.blockchain.services.profiles.User;
import com.vmware.blockchain.services.profiles.UserService;
import com.vmware.blockchain.services.profiles.VmbcRoles;


/**
 * Test the JwtTokenFilter.  Make sure this filter populates the auth context.
 */
@ExtendWith(SpringExtension.class)
@SpringBootTest(classes = {OldJwtSecurityConfig.class, JwtTestContoller.class, AuthHelper.class, VmbcRoles.class})
public class JwtTokenFilterTest {

    @Autowired
    public JwtTokenProvider jwtTokenProvider;

    private User user;

    @MockBean
    RestAuthenticationEntryPoint restAuthenticationEndpoint;

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
        user = SecurityTestUtils.getUser();
        Consortium c = SecurityTestUtils.getConsortium();
        Organization o = SecurityTestUtils.getOrganization();
        when(userService.getByEmail(user.getEmail())).thenReturn(user);
        when(userService.getDefaultConsortium(user)).thenReturn(c);
        when(userService.getDefaultOrganization(user)).thenReturn(o);
        when(organizationService.get(SecurityTestUtils.ORG_ID)).thenReturn(o);
        when(organizationService.getConsortiums(any(UUID.class)))
                .thenReturn(Collections.singletonList(c));
        when(consortiumService.get(any(UUID.class))).thenReturn(c);
        Blockchain b = mock(Blockchain.class);
        when(b.getId()).thenReturn(BC_ID);
        when(blockchainService.listByConsortium(c)).thenReturn(Collections.singletonList(b));
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
    public void testAccessToRestrictedResourceWithNewAuthorization() throws Exception {
        MvcResult result = mockMvc.perform(get("/api/concord/eth")
                .header(HttpHeaders.AUTHORIZATION, "Bearer " + token))
                .andExpect(status().isOk()).andReturn();
        Assertions.assertEquals("Tests passed", result.getResponse().getContentAsString());
    }

    @Test
    public void testAccessToRestrictedResourceWithoutAuthorization() throws Exception {
        mockMvc.perform(get("/api/users"))
            .andExpect(status().isUnauthorized());
    }

    @Test
    public void testAccessToRestrictedResourceWithBasicAuthorization() throws Exception {
        user.setServiceRoles(Arrays.asList(VmbcRoles.SYSTEM_ADMIN));
        mockMvc.perform(
                get("/api/users").with(httpBasic("user@test.com", "1234")))
                .andExpect(status().isOk());
    }

    @Test
    public void testAccessToRestrictedResourceWithBadBasicAuthorization() throws Exception {
        user.setServiceRoles(Arrays.asList(VmbcRoles.SYSTEM_ADMIN));
        mockMvc.perform(
                get("/api/users").with(httpBasic("user@test.com", "5678")))
                .andExpect(status().isUnauthorized());
    }

    @Test
    public void testAccessToPermitedResourceWithoutToken() throws Exception {
        mockMvc.perform(get("/api/auth/login")).andExpect(status().isOk());
    }

    // Testing hasAnyAutority
    @Test
    void testOperatorAsUser() throws Exception {
        mockMvc.perform(get("/api/operator").header(HttpHeaders.AUTHORIZATION, "Bearer " + token))
            .andExpect(status().isForbidden());
    }

    @Test
    void testOperatorAsOperator() throws Exception {
        user.setServiceRoles(Arrays.asList(VmbcRoles.SYSTEM_ADMIN));
        mockMvc.perform(
                get("/api/operator").header(HttpHeaders.AUTHORIZATION, "Bearer " + jwtTokenProvider.createToken(user)))
                .andExpect(status().isOk());
    }

    // Test access via a blockchain id.
    @Test
    void testBlockchainOk() throws Exception {
        mockMvc.perform(get("/api/blockchain/" + BC_ID.toString()).header(HttpHeaders.AUTHORIZATION, "Bearer " + token))
            .andExpect(status().isOk());
    }

    @Test
    void testBlockchainForbiddent() throws Exception {
        mockMvc.perform(get("/api/blockchain/3181fe0e-0e29-427e-a542-fdb9139aee71")
                .header(HttpHeaders.AUTHORIZATION, "Bearer " + token))
                .andExpect(status().isForbidden());
    }

    @Test
    void testBlockchainOperator() throws Exception {
        user.setServiceRoles(Arrays.asList(VmbcRoles.SYSTEM_ADMIN));
        mockMvc.perform(get("/api/blockchain/3181fe0e-0e29-427e-a542-fdb9139aee71")
                .header(HttpHeaders.AUTHORIZATION, "Bearer " + jwtTokenProvider.createToken(user)))
                .andExpect(status().isOk());
    }

    @Test
    public void testAccessBasicAuth() throws Exception {
        String basicAuth = "Basic " + Base64.getEncoder().encodeToString("user@test.com:1234".getBytes());
        MvcResult result = mockMvc.perform(get("/api/concord/eth").header(HttpHeaders.AUTHORIZATION, basicAuth))
                .andExpect(status().isOk()).andReturn();
        Assertions.assertEquals("Tests passed", result.getResponse().getContentAsString());
    }


}
