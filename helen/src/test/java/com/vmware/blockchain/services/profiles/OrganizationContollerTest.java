/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.patch;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.http.MediaType;
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.ImmutableList;
import com.vmware.blockchain.MvcConfig;
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.ConcordProperties;
import com.vmware.blockchain.connections.ConcordConnectionPool;
import com.vmware.blockchain.dao.GenericDao;
import com.vmware.blockchain.security.JwtTokenProvider;
import com.vmware.blockchain.security.ServiceContext;
import com.vmware.blockchain.services.blockchains.BlockchainService;
import com.vmware.blockchain.services.profiles.OrganizationContoller.OrgGetResponse;
import com.vmware.blockchain.services.tasks.TaskService;

import io.grpc.ManagedChannel;

/**
 * Test Organization Controller.
 */
@ExtendWith(SpringExtension.class)
@WebMvcTest(secure = false, controllers = { OrganizationContoller.class })
@ContextConfiguration(classes = MvcConfig.class)
@ComponentScan(basePackageClasses = { OrganizationContoller.class })
public class OrganizationContollerTest {
    static final UUID ORG_1 = UUID.fromString("fdd53f14-acaa-4c55-a41c-cad76154cd1f");

    @Autowired
    MockMvc mockMvc;

    @MockBean
    OrganizationService organizationService;

    @MockBean
    private UserService userService;

    @MockBean
    private ProfilesService prm;

    @MockBean
    private PasswordEncoder passwordEncoder;

    @MockBean
    private JwtTokenProvider jwtTokenProvider;

    @MockBean
    private ConcordProperties concordProperties;

    @MockBean
    private ConcordConnectionPool connectionPool;

    @MockBean
    private KeystoreService keystoreService;

    @MockBean
    private DefaultProfiles profiles;

    @MockBean
    private BlockchainService blockchainService;

    @MockBean
    private ConsortiumService consortiumService;

    @MockBean
    AuthHelper authHelper;

    @MockBean
    GenericDao genericDao;

    @MockBean
    ServiceContext serviceContext;

    @MockBean
    TaskService taskService;

    @MockBean
    @Qualifier("provisioningServerChannel")
    ManagedChannel channel;

    @Autowired
    DefaultProfiles defaultProfiles;

    @Autowired
    Jackson2ObjectMapperBuilder jacksonBuilder;


    ObjectMapper objectMapper;

    @BeforeEach
    void init() throws Exception {
        Organization org1 = new Organization("One");
        org1.setId(ORG_1);
        Map<String, String> org1Map = new LinkedHashMap<>();
        org1Map.put("Test", "wow");
        org1.setOrganizationProperties(org1Map);

        Organization org2 = new Organization("Acme");
        Map<String, String> org2Map = new LinkedHashMap<>();
        org2Map.put("Test", "sugoi");
        org2.setOrganizationProperties(org2Map);
        List<Organization> orgList = ImmutableList.of(org1, org2);

        Mockito.when(organizationService.list()).thenReturn(orgList);
        Mockito.when(organizationService.get(ORG_1)).thenReturn(org1);
        when(organizationService.put(any(Organization.class))).thenAnswer(i -> {
            Organization organization = i.getArgument(0);
            if (organization.getId() == null) {
                organization.setId(UUID.randomUUID());
            }
            return (organization);
        });
        objectMapper = jacksonBuilder.build();
    }

    @Test
    void listOrgs() throws Exception {
        MvcResult result = mockMvc.perform(get("/api/organizations").contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        List<OrganizationContoller.OrgGetResponse> res = objectMapper.readValue(body, new TypeReference<>() {});
        // As an operator, we should see both blockchains.
        Assertions.assertEquals(2, res.size());
    }

    @Test
    void getOrg() throws Exception {
        MvcResult result = mockMvc.perform(get("/api/organizations/" + ORG_1.toString())
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        String body = result.getResponse().getContentAsString();

        OrgGetResponse res = objectMapper.readValue(body, OrgGetResponse.class);

        // Assert known details for ORG_1
        Assertions.assertEquals(ORG_1, res.orgId);
        Assertions.assertEquals("One", res.organizationName);
        Map<String, String> properties = new LinkedHashMap<>();
        properties.put("Test", "wow");
        Assertions.assertEquals(properties, res.organizationProperties);
    }

    @Test
    void updateOrg() throws Exception {
        String content = String.format("    {"
                + "        \"organization_properties\": {"
                + "                                         \"hello\" : \"world\""
                + "                                     }"
                + "    }");

        MvcResult result = mockMvc.perform(patch("/api/organizations/" + ORG_1.toString())
                .contentType(MediaType.APPLICATION_JSON).content(content))
                .andExpect(status().isOk()).andReturn();

        String body = result.getResponse().getContentAsString();

        OrgGetResponse res = objectMapper.readValue(body, OrgGetResponse.class);

        Assertions.assertEquals(ORG_1, res.orgId);
        Map<String, String> properties = new LinkedHashMap<>();
        properties.put("hello", "world");
        properties.put("Test", "wow");
        Assertions.assertEquals(properties, res.organizationProperties);
    }

    @Test
    void updateOrgWithNullProperties() throws Exception {
        String content = String.format("    {"
                + "        \"organization_properties\": null"
                + "    }");

        MvcResult result = mockMvc.perform(patch("/api/organizations/" + ORG_1.toString())
                .contentType(MediaType.APPLICATION_JSON).content(content))
                .andExpect(status().isOk()).andReturn();

        String body = result.getResponse().getContentAsString();

        OrgGetResponse res = objectMapper.readValue(body, OrgGetResponse.class);

        Assertions.assertEquals(ORG_1, res.orgId);
        Map<String, String> properties = new LinkedHashMap<>();
        properties.put("Test", "wow");
        Assertions.assertEquals(properties, res.organizationProperties);
    }

    @Test
    void updateOrgWithChangedValue() throws Exception {
        // Current org properties are {Test:wow}
        // Update them to {Test:SUGOI}
        String content = String.format("    {"
                + "        \"organization_properties\": {"
                + "                                         \"Test\" : \"SUGOI\""
                + "                                     }"
                + "    }");

        MvcResult result = mockMvc.perform(patch("/api/organizations/" + ORG_1.toString())
                .contentType(MediaType.APPLICATION_JSON).content(content))
                .andExpect(status().isOk()).andReturn();

        String body = result.getResponse().getContentAsString();

        OrgGetResponse res = objectMapper.readValue(body, OrgGetResponse.class);

        Assertions.assertEquals(ORG_1, res.orgId);
        Map<String, String> properties = new LinkedHashMap<>();
        properties.put("Test", "SUGOI");
        Assertions.assertEquals(properties, res.organizationProperties);
    }

    @Test
    void updateOrgWithManyProperties() throws Exception {
        // Current org properties are {Test:wow}
        // Update them to {Test:SUGOI, Platinum:Disco, Delusion:Express}
        String content = String.format("    {"
                + "        \"organization_properties\": {"
                + "                                         \"Test\" : \"SUGOI\","
                + "                                         \"Platinum\" : \"Disco\","
                + "                                         \"Delusion\" : \"Express\""
                + "                                     }"
                + "    }");

        MvcResult result = mockMvc.perform(patch("/api/organizations/" + ORG_1.toString())
                .contentType(MediaType.APPLICATION_JSON).content(content))
                .andExpect(status().isOk()).andReturn();

        String body = result.getResponse().getContentAsString();

        OrgGetResponse res = objectMapper.readValue(body, OrgGetResponse.class);

        Assertions.assertEquals(ORG_1, res.orgId);
        Map<String, String> properties = new LinkedHashMap<>();
        properties.put("Test", "SUGOI");
        properties.put("Platinum", "Disco");
        properties.put("Delusion", "Express");
        Assertions.assertEquals(properties, res.organizationProperties);
    }
}