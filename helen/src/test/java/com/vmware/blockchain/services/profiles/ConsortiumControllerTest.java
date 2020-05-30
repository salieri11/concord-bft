/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import static com.vmware.blockchain.security.MvcTestSecurityConfig.createContext;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.authentication;
import static org.springframework.security.test.web.servlet.setup.SecurityMockMvcConfigurers.springSecurity;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.patch;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Collections;
import java.util.List;
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
import org.springframework.http.MediaType;
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.ImmutableList;
import com.vmware.blockchain.MvcConfig;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.auth.AuthenticationContext;
import com.vmware.blockchain.common.HelenExceptionHandler;
import com.vmware.blockchain.security.MvcTestSecurityConfig;
import com.vmware.blockchain.services.blockchains.BlockchainService;
import com.vmware.blockchain.services.profiles.ConsortiumController.ConGetResponse;
import com.vmware.blockchain.services.profiles.ConsortiumController.ConPatchResponse;
import com.vmware.blockchain.services.profiles.OrganizationContoller.OrgGetResponse;

/**
 * Tests for the consortium controller.
 */
@ExtendWith(SpringExtension.class)
@WebMvcTest(controllers = {ConsortiumController.class})
@ContextConfiguration(classes = {MvcConfig.class, MvcTestSecurityConfig.class})
@ComponentScan(basePackageClasses = {ConsortiumControllerTest.class, HelenExceptionHandler.class})
public class ConsortiumControllerTest {

    static final UUID C_1 = UUID.fromString("110da5dc-39c1-4148-a911-ec985b9448b0");
    static final UUID O_1 = UUID.fromString("fdd53f14-acaa-4c55-a41c-cad76154cd1f");
    static final UUID O_2 = UUID.fromString("878ddd8a-8ed0-4a90-a72d-5b32b7acb03c");

    @Autowired
    private WebApplicationContext context;

    private MockMvc mockMvc;

    @MockBean
    DefaultProfiles defaultProfiles;

    @Autowired
    Jackson2ObjectMapperBuilder jacksonBuilder;

    @MockBean
    ConsortiumService consortiumService;

    @MockBean
    BlockchainService blockchainService;

    @MockBean
    ProfilesService profilesService;

    @Autowired
    AuthHelper authHelper;

    private ObjectMapper objectMapper;

    private AuthenticationContext adminAuth;
    private AuthenticationContext userAuth;

    @BeforeEach
    void init() throws Exception {
        mockMvc = MockMvcBuilders
                .webAppContextSetup(context)
                .apply(springSecurity())
                .build();
        Organization o1 = new Organization("Test Org");
        o1.setId(O_1);
        Organization o2 = new Organization("Other Test Org");
        o2.setId(O_2);
        Consortium c = new Consortium("Test", "Test", O_1);
        c.setId(C_1);
        when(consortiumService.list()).thenReturn(Collections.singletonList(c));
        when(consortiumService.get(C_1)).thenReturn(c);
        when(consortiumService.put(any(Consortium.class))).thenAnswer(i -> {
            Consortium consortium = i.getArgument(0);
            if (consortium.getId() == null) {
                consortium.setId(UUID.randomUUID());
            }
            return (consortium);
        });
        when(consortiumService.getOrganizations(C_1)).thenReturn(Collections.singletonList(o1));
        objectMapper = jacksonBuilder.build();
        adminAuth = createContext("operator", O_1,
                                  ImmutableList.of(Roles.SYSTEM_ADMIN, Roles.ORG_USER),
                                  ImmutableList.of(C_1),
                                  Collections.emptyList(), "");

        userAuth = createContext("operator", O_1,
                                 ImmutableList.of(Roles.ORG_USER),
                                 ImmutableList.of(C_1),
                                 Collections.emptyList(),
                                 Collections.emptyList(),
                                 Collections.emptyList(), "");

    }

    @Test
    void testList() throws Exception {
        MvcResult result = mockMvc.perform(get("/api/consortiums").with(authentication(userAuth))
                                                   .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        List<ConGetResponse> list = objectMapper.readValue(body, new TypeReference<List<ConGetResponse>>() {
        });
        Assertions.assertEquals(1, list.size());
        Assertions.assertEquals(C_1, list.get(0).consortiumId);
    }

    @Test
    void testGet() throws Exception {
        MvcResult result = mockMvc.perform(get("/api/consortiums/" + C_1.toString()).with(authentication(userAuth))
                                                   .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        ConGetResponse con = objectMapper.readValue(body, ConGetResponse.class);
        Assertions.assertEquals(C_1, con.consortiumId);
        Assertions.assertEquals("Test", con.getConsortiumName());
    }


    @Test
    void testPost() throws Exception {
        String content = String.format("    {"
                                       + "        \"consortium_name\": \"New Con\","
                                       + "        \"consortium_type\": \"Obsolete\","
                                       + "        \"organization\": \"%s\""
                                       + "    }", O_1);
        MvcResult result = mockMvc.perform(post("/api/consortiums").with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(content))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        ConGetResponse response = objectMapper.readValue(body, ConGetResponse.class);
        Assertions.assertEquals("New Con", response.getConsortiumName());
        Assertions.assertEquals(O_1, response.getOrganizationId());
        Assertions.assertNotNull(response.getConsortiumId());
    }

    @Test
    void testPostBlankName() throws Exception {
        String content = String.format("    {"
                                       + "        \"consortium_name\": \"   \","
                                       + "        \"consortium_type\": \"Obsolete\","
                                       + "        \"organization\": \"%s\""
                                       + "    }", O_1);
        mockMvc.perform(post("/api/consortiums").with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(content))
                .andExpect(status().isBadRequest());
    }

    @Test
    void testPostEmptyName() throws Exception {
        String content = String.format("    {"
                                       + "        \"consortium_name\": \"\","
                                       + "        \"consortium_type\": \"Obsolete\","
                                       + "        \"organization\": \"%s\""
                                       + "    }", O_1);
        mockMvc.perform(post("/api/consortiums").with(authentication(adminAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(content))
                .andExpect(status().isBadRequest());
    }

    @Test
    void testPostMissingName() throws Exception {
        String content = String.format("    {"
                                       + "        \"consortium_type\": \"Obsolete\","
                                       + "        \"organization\": \"%s\""
                                       + "    }", O_1);
        mockMvc.perform(post("/api/consortiums").with(authentication(adminAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(content))
                .andExpect(status().isBadRequest());
    }


    @Test
    void testRenameConsortium() throws Exception {
        String content = "    {"
                         + "        \"consortium_name\": \"My New Name\""
                         + "   }";
        MvcResult result = mockMvc.perform(patch("/api/consortiums/" + C_1.toString()).with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(content))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        ConPatchResponse con = objectMapper.readValue(body, ConPatchResponse.class);
        Assertions.assertEquals("My New Name", con.getConsortiumName());
    }

    @Test
    void testAdd() throws Exception {
        String content = "   {"
                         + "        \"orgs_to_add\": [\"" + O_2.toString() + "\"]"
                         + "  }";
        MvcResult result = mockMvc.perform(patch("/api/consortiums/" + C_1.toString()).with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(content))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        ConPatchResponse con = objectMapper.readValue(body, ConPatchResponse.class);
        Assertions.assertEquals(C_1, con.consortiumId);
        Assertions.assertEquals("Test", con.getConsortiumName());
        ArgumentCaptor<UUID> captor = ArgumentCaptor.forClass(UUID.class);
        // mock doesn't really add a member
        Assertions.assertEquals(1, con.getMembers().size());
        verify(consortiumService, times(1)).addOrganization(any(Consortium.class), captor.capture());
        Assertions.assertEquals(O_2, captor.getValue());
        verify(consortiumService, times(0)).removeOrganization(any(Consortium.class), any(UUID.class));
    }

    @Test
    void testRemove() throws Exception {
        String content = "   {"
                         + "        \"orgs_to_remove\": [\"" + O_2.toString() + "\"]"
                         + "  }";
        MvcResult result = mockMvc.perform(patch("/api/consortiums/" + C_1.toString()).with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(content))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        ConPatchResponse con = objectMapper.readValue(body, ConPatchResponse.class);
        Assertions.assertEquals(C_1, con.consortiumId);
        Assertions.assertEquals("Test", con.getConsortiumName());
        Assertions.assertEquals(1, con.getMembers().size());
        ArgumentCaptor<UUID> captor = ArgumentCaptor.forClass(UUID.class);
        verify(consortiumService, times(0)).addOrganization(any(Consortium.class), any(UUID.class));
        verify(consortiumService, times(1)).removeOrganization(any(Consortium.class), captor.capture());
        Assertions.assertEquals(O_2, captor.getValue());
    }

    @Test
    void testRemoveOwner() throws Exception {
        String content = "   {"
                         + "        \"orgs_to_remove\": [\"" + O_1.toString() + "\"]"
                         + "  }";
        mockMvc.perform(patch("/api/consortiums/" + C_1.toString()).with(authentication(adminAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(content))
                .andExpect(status().isBadRequest());
    }

    @Test
    void testRemoveAsUser() throws Exception {
        String content = "   {"
                         + "        \"orgs_to_remove\": [\"" + O_2.toString() + "\"]"
                         + "  }";
        mockMvc.perform(patch("/api/consortiums/" + C_1.toString()).with(authentication(userAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(content))
                .andExpect(status().isForbidden());
    }

    @Test
    void testgetOrgs() throws Exception {
        MvcResult result = mockMvc.perform(get("/api/consortiums/" + C_1.toString() + "/organizations")
                                                   .with(authentication(userAuth))
                                                   .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        List<OrgGetResponse> list = objectMapper.readValue(body, new TypeReference<List<OrgGetResponse>>() {
        });
        Assertions.assertEquals(1, list.size());
        Assertions.assertEquals(O_1, list.get(0).getOrganizationId());
    }


}
