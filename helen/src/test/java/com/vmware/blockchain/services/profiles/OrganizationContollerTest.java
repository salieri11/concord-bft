/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.profiles;

import static com.vmware.blockchain.security.MvcTestSecurityConfig.createContext;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.authentication;
import static org.springframework.security.test.web.servlet.setup.SecurityMockMvcConfigurers.springSecurity;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.patch;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.text.MessageFormat;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.http.MediaType;
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.ImmutableList;
import com.vmware.blockchain.MvcConfig;
import com.vmware.blockchain.auth.AuthenticationContext;
import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.common.HelenExceptionHandler;
import com.vmware.blockchain.common.NotFoundException;
import com.vmware.blockchain.dao.GenericDao;
import com.vmware.blockchain.security.MvcTestSecurityConfig;
import com.vmware.blockchain.security.ServiceContext;
import com.vmware.blockchain.services.profiles.OrganizationContoller.OrgGetResponse;
import com.vmware.blockchain.services.tasks.TaskService;

/**
 * Test Organization Controller.
 */
@ExtendWith(SpringExtension.class)
@WebMvcTest(controllers = {OrganizationContoller.class})
@ContextConfiguration(classes = {MvcTestSecurityConfig.class, MvcConfig.class})
@ComponentScan(basePackageClasses = {OrganizationContoller.class, HelenExceptionHandler.class})
@TestPropertySource(properties = "spring.main.allow-bean-definition-overriding=true")
public class OrganizationContollerTest {
    static final UUID ORG_1 = UUID.fromString("fdd53f14-acaa-4c55-a41c-cad76154cd1f");
    static final UUID ORG_NO_PROPERTIES = UUID.fromString("fbb53f14-acaa-4c55-a41c-cad76154cd1f");
    static final UUID MISSING_ORG = UUID.fromString("09ba67b0-f511-4bd2-975a-b87e7cf88631");
    static final UUID NOT_FOUND_ORG = UUID.fromString("09ba67b0-f511-4bd2-975a-b87e7cf88631");
    static final UUID CONSORTIUM = UUID.fromString("7a3a76eb-d9ed-4836-9cca-c28fdb0bde8e");

    @Autowired
    private WebApplicationContext context;

    private MockMvc mockMvc;

    @MockBean
    OrganizationService organizationService;

    @MockBean
    DefaultProfiles defaultProfiles;

    @MockBean
    UserAuthenticator userAuthenticator;

    @MockBean
    ProfilesService profilesService;

    @MockBean(name = "genericDao")
    GenericDao genericDao;

    @MockBean(name = "serviceContext")
    ServiceContext serviceContext;

    @MockBean(name = "taskService")
    TaskService taskService;

    @Autowired
    Jackson2ObjectMapperBuilder jacksonBuilder;


    VmbcRoles vmbcRoles = new VmbcRoles();


    private ObjectMapper objectMapper;

    private AuthenticationContext adminAuth;

    @BeforeEach
    void init() throws Exception {
        mockMvc = MockMvcBuilders
                .webAppContextSetup(context)
                .apply(springSecurity())
                .build();

        Organization org1 = new Organization("One");
        org1.setId(ORG_1);
        Map<String, String> org1Map = new LinkedHashMap<>();
        org1Map.put("Test", "wow");
        org1.setOrganizationProperties(org1Map);

        Organization org2 = new Organization("Acme");
        Map<String, String> org2Map = new LinkedHashMap<>();
        org2Map.put("Test", "sugoi");
        org2.setOrganizationProperties(org2Map);

        Organization orgWithEmptyProperties = new Organization("TrySail");
        orgWithEmptyProperties.setId(ORG_NO_PROPERTIES);

        List<Organization> orgList = ImmutableList.of(org1, org2, orgWithEmptyProperties);

        when(organizationService.list()).thenReturn(orgList);
        when(organizationService.get(ORG_1)).thenReturn(org1);
        when(organizationService.get(MISSING_ORG)).thenReturn(null);
        when(organizationService.get(ORG_NO_PROPERTIES)).thenReturn(orgWithEmptyProperties);
        when(organizationService.put(any(Organization.class))).thenAnswer(i -> {
            Organization organization = i.getArgument(0);
            if (organization.getId() == null) {
                organization.setId(UUID.randomUUID());
            }
            return (organization);
        });
        doThrow(new NotFoundException("...")).when(organizationService).get(NOT_FOUND_ORG);

        adminAuth = createContext("operator", ORG_1,
                ImmutableList.of(vmbcRoles.SYSTEM_ADMIN, vmbcRoles.ORG_USER),
                ImmutableList.of(CONSORTIUM),
                Collections.emptyList(), "");

        objectMapper = jacksonBuilder.build();
    }

    @Test
    void listOrgs() throws Exception {
        MvcResult result = mockMvc.perform(get("/api/organizations")
                .with(authentication(adminAuth))
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        List<OrganizationContoller.OrgGetResponse> res = objectMapper.readValue(body, new TypeReference<>() {});
        // As an operator, we should see both blockchains.
        Assertions.assertEquals(3, res.size());
    }

    @Test
    void getOrg() throws Exception {
        MvcResult result = mockMvc.perform(get("/api/organizations/" + ORG_1.toString())
                .with(authentication(adminAuth))
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        String body = result.getResponse().getContentAsString();

        OrgGetResponse res = objectMapper.readValue(body, OrgGetResponse.class);

        // Assert known details for ORG_1
        Assertions.assertEquals(ORG_1, res.organizationId);
        Assertions.assertEquals("One", res.organizationName);
        Map<String, String> properties = new LinkedHashMap<>();
        properties.put("Test", "wow");
        Assertions.assertEquals(properties, res.organizationProperties);
    }

    @Test
    void getMissingOrg() throws Exception {
        MvcResult result = mockMvc.perform(get("/api/organizations/" + MISSING_ORG.toString())
                .with(authentication(adminAuth))
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound()).andReturn();

        String body = result.getResponse().getContentAsString();

        String message = objectMapper.readValue(body, Map.class).get("error_message").toString();

        Assertions.assertEquals(MessageFormat.format(ErrorCode.ORG_NOT_FOUND, MISSING_ORG.toString()), message);
    }

    @Test
    void getNotFoundOrg() throws Exception {
        MvcResult result = mockMvc.perform(get("/api/organizations/" + NOT_FOUND_ORG.toString())
                .with(authentication(adminAuth))
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound()).andReturn();

        String body = result.getResponse().getContentAsString();

        String message = objectMapper.readValue(body, Map.class).get("error_message").toString();

        Assertions.assertEquals(MessageFormat.format(ErrorCode.ORG_NOT_FOUND, NOT_FOUND_ORG.toString()), message);
    }

    @Test
    void updateOrg() throws Exception {
        String content = String.format("    {"
                + "        \"add_properties\": {"
                + "                                         \"hello\" : \"world\""
                + "                                     }"
                + "    }");

        MvcResult result = mockMvc.perform(patch("/api/organizations/" + ORG_1.toString())
                .with(authentication(adminAuth))
                .contentType(MediaType.APPLICATION_JSON).content(content))
                .andExpect(status().isOk()).andReturn();

        String body = result.getResponse().getContentAsString();

        OrgGetResponse res = objectMapper.readValue(body, OrgGetResponse.class);

        Assertions.assertEquals(ORG_1, res.organizationId);
        Map<String, String> properties = new LinkedHashMap<>();
        properties.put("hello", "world");
        properties.put("Test", "wow");
        Assertions.assertEquals(properties, res.organizationProperties);
    }

    @Test
    void updateOrgWithNoProperties() throws Exception {
        String content = String.format("    {"
                + "        \"add_properties\": {"
                + "                                         \"soda\" : \"pops\""
                + "                             }"
                + "    }");

        MvcResult result = mockMvc.perform(patch("/api/organizations/" + ORG_NO_PROPERTIES.toString())
                .with(authentication(adminAuth))
                .contentType(MediaType.APPLICATION_JSON).content(content))
                .andExpect(status().isOk()).andReturn();

        String body = result.getResponse().getContentAsString();

        OrgGetResponse res = objectMapper.readValue(body, OrgGetResponse.class);

        Assertions.assertEquals(ORG_NO_PROPERTIES, res.organizationId);
        Map<String, String> properties = new LinkedHashMap<>();
        properties.put("soda", "pops");
        Assertions.assertEquals(properties, res.organizationProperties);
    }

    @Test
    void updateOrgDeleteProperties() throws Exception {
        String content = String.format("    {"
                + "        \"delete_properties\": {"
                + "                                         \"Test\" : \"\""
                + "                             }"
                + "    }");

        MvcResult result = mockMvc.perform(patch("/api/organizations/" + ORG_1.toString())
                .with(authentication(adminAuth))
                .contentType(MediaType.APPLICATION_JSON).content(content))
                .andExpect(status().isOk()).andReturn();

        String body = result.getResponse().getContentAsString();

        OrgGetResponse res = objectMapper.readValue(body, OrgGetResponse.class);

        Assertions.assertEquals(ORG_1, res.organizationId);
        Map<String, String> properties = new LinkedHashMap<>();
        Assertions.assertEquals(properties, res.organizationProperties);
    }

    @Test
    void updateOrgAddAndDeleteProperties() throws Exception {
        String content = String.format("    {"
                + "        \"add_properties\": {"
                + "                                         \"Orbital\" : \"Beat\""
                + "                             },"
                + "        \"delete_properties\": {"
                + "                                         \"Test\" : \"\""
                + "                             }"
                + "    }");

        MvcResult result = mockMvc.perform(patch("/api/organizations/" + ORG_1.toString())
                .with(authentication(adminAuth))
                .contentType(MediaType.APPLICATION_JSON).content(content))
                .andExpect(status().isOk()).andReturn();

        String body = result.getResponse().getContentAsString();

        OrgGetResponse res = objectMapper.readValue(body, OrgGetResponse.class);

        Assertions.assertEquals(ORG_1, res.organizationId);
        Map<String, String> properties = new LinkedHashMap<>();
        properties.put("Orbital", "Beat");
        Assertions.assertEquals(properties, res.organizationProperties);
    }

    @Test
    void updateOrgAddAndDeletePropertiesNoExistingProperties() throws Exception {
        String content = String.format("    {"
                + "        \"add_properties\": {"
                + "                                         \"Orbital\" : \"Beat\""
                + "                             },"
                + "        \"delete_properties\": {"
                + "                                         \"Test\" : \"\""
                + "                             }"
                + "    }");

        MvcResult result = mockMvc.perform(patch("/api/organizations/" + ORG_NO_PROPERTIES.toString())
                .with(authentication(adminAuth))
                .contentType(MediaType.APPLICATION_JSON).content(content))
                .andExpect(status().isOk()).andReturn();

        String body = result.getResponse().getContentAsString();

        OrgGetResponse res = objectMapper.readValue(body, OrgGetResponse.class);

        Assertions.assertEquals(ORG_NO_PROPERTIES, res.organizationId);
        Map<String, String> properties = new LinkedHashMap<>();
        properties.put("Orbital", "Beat");
        Assertions.assertEquals(properties, res.organizationProperties);
    }

    @Test
    void updateOrgWithNullProperties() throws Exception {
        String content = String.format("    {"
                + "        \"add_properties\": null"
                + "    }");

        MvcResult result = mockMvc.perform(patch("/api/organizations/" + ORG_1.toString())
                .with(authentication(adminAuth))
                .contentType(MediaType.APPLICATION_JSON).content(content))
                .andExpect(status().isOk()).andReturn();

        String body = result.getResponse().getContentAsString();

        OrgGetResponse res = objectMapper.readValue(body, OrgGetResponse.class);

        Assertions.assertEquals(ORG_1, res.organizationId);
        Map<String, String> properties = new LinkedHashMap<>();
        properties.put("Test", "wow");
        Assertions.assertEquals(properties, res.organizationProperties);
    }

    @Test
    void updateOrgWithChangedValue() throws Exception {
        // Current org properties are {Test:wow}
        // Update them to {Test:SUGOI}
        String content = String.format("    {"
                + "        \"add_properties\": {"
                + "                                         \"Test\" : \"SUGOI\""
                + "                                     }"
                + "    }");

        MvcResult result = mockMvc.perform(patch("/api/organizations/" + ORG_1.toString())
                .with(authentication(adminAuth))
                .contentType(MediaType.APPLICATION_JSON).content(content))
                .andExpect(status().isOk()).andReturn();

        String body = result.getResponse().getContentAsString();

        OrgGetResponse res = objectMapper.readValue(body, OrgGetResponse.class);

        Assertions.assertEquals(ORG_1, res.organizationId);
        Map<String, String> properties = new LinkedHashMap<>();
        properties.put("Test", "SUGOI");
        Assertions.assertEquals(properties, res.organizationProperties);
    }

    @Test
    void updateOrgWithManyProperties() throws Exception {
        // Current org properties are {Test:wow}
        // Update them to {Test:SUGOI, Platinum:Disco, Delusion:Express}
        String content = String.format("    {"
                + "        \"add_properties\": {"
                + "                                         \"Test\" : \"SUGOI\","
                + "                                         \"Platinum\" : \"Disco\","
                + "                                         \"Delusion\" : \"Express\""
                + "                                     }"
                + "    }");

        MvcResult result = mockMvc.perform(patch("/api/organizations/" + ORG_1.toString())
                .with(authentication(adminAuth))
                .contentType(MediaType.APPLICATION_JSON).content(content))
                .andExpect(status().isOk()).andReturn();

        String body = result.getResponse().getContentAsString();

        OrgGetResponse res = objectMapper.readValue(body, OrgGetResponse.class);

        Assertions.assertEquals(ORG_1, res.organizationId);
        Map<String, String> properties = new LinkedHashMap<>();
        properties.put("Test", "SUGOI");
        properties.put("Platinum", "Disco");
        properties.put("Delusion", "Express");
        Assertions.assertEquals(properties, res.organizationProperties);
    }

    @Test
    void updateMissingOrg() throws Exception {
        String content = String.format("    {"
                + "        \"add_properties\": {"
                + "                                         \"Distance\" : \"Hinamatsuri\""
                + "                             },"
                + "        \"delete_properties\": {"
                + "                                         \"Test\" : \"\""
                + "                             }"
                + "    }");

        MvcResult result = mockMvc.perform(patch("/api/organizations/" + MISSING_ORG.toString())
                .with(authentication(adminAuth))
                .contentType(MediaType.APPLICATION_JSON).content(content))
                .andExpect(status().isNotFound()).andReturn();

        String body = result.getResponse().getContentAsString();

        String message = objectMapper.readValue(body, Map.class).get("error_message").toString();

        Assertions.assertEquals(MessageFormat.format(ErrorCode.ORG_NOT_FOUND, MISSING_ORG.toString()), message);
    }

    @Test
    void updateNotFoundOrg() throws Exception {
        String content = String.format("    {"
                + "        \"add_properties\": {"
                + "                                         \"Distance\" : \"Hinamatsuri\""
                + "                             },"
                + "        \"delete_properties\": {"
                + "                                         \"Test\" : \"\""
                + "                             }"
                + "    }");

        MvcResult result = mockMvc.perform(patch("/api/organizations/" + NOT_FOUND_ORG.toString())
                .with(authentication(adminAuth))
                .contentType(MediaType.APPLICATION_JSON).content(content))
                .andExpect(status().isNotFound()).andReturn();

        String body = result.getResponse().getContentAsString();

        String message = objectMapper.readValue(body, Map.class).get("error_message").toString();

        Assertions.assertEquals(MessageFormat.format(ErrorCode.ORG_NOT_FOUND, NOT_FOUND_ORG.toString()), message);
    }
}