/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.invitation;

import static com.vmware.blockchain.security.MvcTestSecurityConfig.createContext;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.authentication;
import static org.springframework.security.test.web.servlet.setup.SecurityMockMvcConfigurers.springSecurity;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.redirectedUrl;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Collections;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;
import org.springframework.mock.web.MockHttpSession;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.util.UriComponentsBuilder;

import com.google.common.collect.ImmutableList;
import com.vmware.blockchain.MvcConfig;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.auth.AuthenticationContext;
import com.vmware.blockchain.common.Constants;
import com.vmware.blockchain.common.HelenExceptionHandler;
import com.vmware.blockchain.common.csp.CspConstants;
import com.vmware.blockchain.security.MvcTestSecurityConfig;
import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.blockchain.services.profiles.VmbcRoles;

@ExtendWith(SpringExtension.class)
@WebMvcTest(controllers = {InvitationController.class })
@TestPropertySource(locations = "classpath:test.properties")
@ContextConfiguration(classes = {MvcTestSecurityConfig.class, MvcConfig.class })
@ComponentScan(basePackageClasses = {InvitationControllerTest.class, HelenExceptionHandler.class })
class InvitationControllerTest {

    private static UUID ORG_ID = UUID.fromString("dcaf9e25-cc1a-4f60-b956-46b67e651314");

    private MockMvc mockMvc;

    @Autowired
    private WebApplicationContext webApplicationContext;

    @Autowired
    private AuthHelper authHelper;

    @Autowired
    private Jackson2ObjectMapperBuilder jacksonBuilder;

    @MockBean
    private DefaultProfiles defaultProfiles;

    @MockBean
    private InvitationService invitationService;

    AuthenticationContext userAuth;
    AuthenticationContext orgAdminAuth;

    String invitationLink = "/csp/slc/api/service-invitations/59954cc9-be6a-4920-8938-497d5702910b";

    String invitationUrl;
    String redirectedUrl;

    @BeforeEach
    void init() throws Exception {
        mockMvc = MockMvcBuilders
            .webAppContextSetup(webApplicationContext)
            .apply(springSecurity())
            .build();
        invitationUrl = UriComponentsBuilder.fromUriString(Constants.AUTH_INVITATION).toUriString();
        String orgLink = String.format("%s/%s", CspConstants.CSP_ORG_API, ORG_ID.toString());
        redirectedUrl = UriComponentsBuilder.fromUriString(Constants.AUTH_LOGIN)
                .queryParam(Constants.CSP_ORG_LINK, orgLink)
                .queryParam(Constants.NEW_USER_PARAM, "new").toUriString();

        userAuth = createContext("user@domain", ORG_ID,
                                ImmutableList.of(VmbcRoles.ORG_USER),
                                Collections.emptyList(), Collections.emptyList(),
                                "atoken");

        orgAdminAuth = createContext("user@domain", ORG_ID,
                                 ImmutableList.of(VmbcRoles.ORG_USER, VmbcRoles.CSP_ORG_OWNER),
                                 Collections.emptyList(), Collections.emptyList(),
                                 "atoken");
    }

    @Test
    void testInvitation() throws Exception {
        ArgumentCaptor<String> captor = ArgumentCaptor.forClass(String.class);
        mockMvc.perform(get(invitationUrl).with(authentication(orgAdminAuth))
                                .sessionAttr(Constants.CSP_INVITATION_LINK, invitationLink))
                .andExpect(redirectedUrl(redirectedUrl));
        verify(invitationService, times(1)).handleServiceInvitation(captor.capture());
        Assertions.assertEquals(invitationLink, captor.getValue());
    }

    @Test
    void testNoSession() throws Exception {
        ArgumentCaptor<String> captor = ArgumentCaptor.forClass(String.class);
        mockMvc.perform(get(invitationUrl).with(authentication(orgAdminAuth)))
                .andExpect(status().isBadRequest());
    }

    @Test
    void testNoInviation() throws Exception {
        ArgumentCaptor<String> captor = ArgumentCaptor.forClass(String.class);
        mockMvc.perform(get(invitationUrl).with(authentication(orgAdminAuth))
                                .session(new MockHttpSession()))
                .andExpect(status().isBadRequest());
    }

    @Test
    void testInvitationBadAuth() throws Exception {
        ArgumentCaptor<String> captor = ArgumentCaptor.forClass(String.class);
        mockMvc.perform(get(invitationUrl).with(authentication(userAuth))
                                .sessionAttr(Constants.CSP_INVITATION_LINK, invitationLink))
                .andExpect(status().isForbidden());
    }

}