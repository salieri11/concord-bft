/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.zones;

import static com.vmware.blockchain.security.MvcTestSecurityConfig.createContext;
import static com.vmware.blockchain.services.blockchains.zones.Zone.Type.ON_PREM;
import static com.vmware.blockchain.services.blockchains.zones.Zone.Type.VMC_AWS;
import static com.vmware.blockchain.services.blockchains.zones.ZoneTestUtils.getOnpremZone;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.authentication;
import static org.springframework.security.test.web.servlet.setup.SecurityMockMvcConfigurers.springSecurity;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Collections;
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
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;
import com.vmware.blockchain.MvcConfig;
import com.vmware.blockchain.auth.AuthenticationContext;
import com.vmware.blockchain.common.BadRequestException;
import com.vmware.blockchain.common.HelenExceptionHandler;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteServiceGrpc.OrchestrationSiteServiceStub;
import com.vmware.blockchain.deployment.v1.ValidateOrchestrationSiteRequest;
import com.vmware.blockchain.deployment.v1.ValidateOrchestrationSiteResponse;
import com.vmware.blockchain.security.MvcTestSecurityConfig;
import com.vmware.blockchain.services.blockchains.BlockchainUtils;
import com.vmware.blockchain.services.blockchains.zones.ZoneController.OnPremGetResponse;
import com.vmware.blockchain.services.blockchains.zones.ZoneController.ZoneListResponse;
import com.vmware.blockchain.services.blockchains.zones.ZoneController.ZoneResponse;
import com.vmware.blockchain.services.profiles.Roles;

import io.grpc.stub.StreamObserver;

/**
 * Tests for the ZoneController.
 */
@ExtendWith({SpringExtension.class})
@WebMvcTest(controllers = { ZoneController.class })
@ContextConfiguration(classes = {MvcTestSecurityConfig.class, MvcConfig.class, ZoneConfig.class})
@ComponentScan(basePackageClasses = {ZoneController.class, HelenExceptionHandler.class})

class ZoneControllerTest {

    private static final UUID SITE_1 = UUID.fromString("84b9a0ed-c162-446a-b8c0-2e45755f3844");
    private static final UUID SITE_2 = UUID.fromString("275638a3-8860-4925-85de-c73d45cb7232");
    private static final UUID OP_SITE = UUID.fromString("2aa01247-dd51-4460-bba0-0a0934f0419e");
    private static final UUID ORG_ID = UUID.fromString("9ecb07bc-482c-48f3-80d0-23c4f9514902");
    private static final UUID OPER_ORG = UUID.fromString("0b93680b-e99d-4f83-bcfd-e2d52c6ea697");

    private static final String POST_ONPREM_BODY = "{\n"
                                                   + "  \"name\": \"OnPrem\",\n"
                                                   + "  \"type\": \"ON_PREM\",\n"
                                                   + "  \"org_id\": \"5e5ff1c8-34b9-4fa3-9924-83eb14354d4c\",\n"
                                                   + "  \"vcenter\": {\n"
                                                   + "    \"url\": \"http://vcenter\",\n"
                                                   + "    \"username\": \"admin\",\n"
                                                   + "    \"password\": \"password\"\n"
                                                   + "  },\n"
                                                   + "  \"resource_pool\": \"pool\",\n"
                                                   + "  \"storage\": \"datastore\",\n"
                                                   + "  \"folder\": \"folder\",\n"
                                                   + "  \"network\": {\n"
                                                   + "    \"name\": \"Network 1\",\n"
                                                   + "    \"ip_pool\": [\n"
                                                   + "      \"10.1.1.16-10.1.1.64\",\n"
                                                   + "      \"10.1.1.100-10.1.1.200\"\n"
                                                   + "    ],\n"
                                                   + "    \"gateway\": \"10.1.1.1\",\n"
                                                   + "    \"subnet\": \"24\",\n"
                                                   + "    \"name_servers\": [\n"
                                                   + "      \"10.1.1.3\"\n"
                                                   + "    ]\n"
                                                   + "  },\n"
                                                   + "  \"container_repo\": {\n"
                                                   + "    \"url\": \"https://docker-repo.com\",\n"
                                                   + "    \"username\": \"user\",\n"
                                                   + "    \"password\": \"docker\"\n"
                                                   + "  },\n"
                                                   + "  \"log_managements\": [{\n"
                                                   + "    \"destination\": \"LOG_INSIGHT\",\n"
                                                   + "    \"address\": \"10.78.20.10:9000\",\n"
                                                   + "    \"username\": \"foo\",\n"
                                                   + "    \"password\": \"bar\"\n"
                                                   + "  }]\n"
                                                   + "}";

    private static final String POST_NO_ORG_BODY = "{\n"
                                                   + "  \"name\": \"OnPrem\",\n"
                                                   + "  \"type\": \"ON_PREM\",\n"
                                                   + "  \"vcenter\": {\n"
                                                   + "    \"url\": \"http://vcenter\",\n"
                                                   + "    \"username\": \"admin\",\n"
                                                   + "    \"password\": \"password\"\n"
                                                   + "  },\n"
                                                   + "  \"resource_pool\": \"pool\",\n"
                                                   + "  \"storage\": \"datastore\",\n"
                                                   + "  \"folder\": \"folder\",\n"
                                                   + "  \"network\": {\n"
                                                   + "    \"name\": \"Network 1\",\n"
                                                   + "    \"ip_pool\": [\n"
                                                   + "      \"10.1.1.16-10.1.1.64\",\n"
                                                   + "      \"10.1.1.100-10.1.1.200\"\n"
                                                   + "    ],\n"
                                                   + "    \"gateway\": \"10.1.1.1\",\n"
                                                   + "    \"subnet\": \"24\",\n"
                                                   + "    \"name_servers\": [\n"
                                                   + "      \"10.1.1.3\"\n"
                                                   + "    ]\n"
                                                   + "  },\n"
                                                   + "  \"container_repo\": {\n"
                                                   + "    \"url\": \"https://docker-repo.com\",\n"
                                                   + "    \"username\": \"user\",\n"
                                                   + "    \"password\": \"docker\"\n"
                                                   + "  }\n"
                                                   + "}";

    private static final String POST_NO_VCENTER_BODY = "{\n"
                                                   + "  \"name\": \"OnPrem\",\n"
                                                   + "  \"type\": \"ON_PREM\",\n"
                                                   + "  \"resource_pool\": \"pool\",\n"
                                                   + "  \"storage\": \"datastore\",\n"
                                                   + "  \"folder\": \"folder\",\n"
                                                   + "  \"network\": {\n"
                                                   + "    \"name\": \"Network 1\",\n"
                                                   + "    \"ip_pool\": [\n"
                                                   + "      \"10.1.1.16-10.1.1.64\",\n"
                                                   + "      \"10.1.1.100-10.1.1.200\"\n"
                                                   + "    ],\n"
                                                   + "    \"gateway\": \"10.1.1.1\",\n"
                                                   + "    \"subnet\": \"24\",\n"
                                                   + "    \"name_servers\": [\n"
                                                   + "      \"10.1.1.3\"\n"
                                                   + "    ]\n"
                                                   + "  },\n"
                                                   + "  \"container_repo\": {\n"
                                                   + "    \"url\": \"https://docker-repo.com\",\n"
                                                   + "    \"username\": \"user\",\n"
                                                   + "    \"password\": \"docker\"\n"
                                                   + "  }\n"
                                                   + "}";

    private static final String POST_NO_NETWORK_BODY = "{\n"
                                                       + "  \"name\": \"OnPrem\",\n"
                                                       + "  \"type\": \"ON_PREM\",\n"
                                                       + "  \"vcenter\": {\n"
                                                       + "    \"url\": \"http://vcenter\",\n"
                                                       + "    \"username\": \"admin\",\n"
                                                       + "    \"password\": \"password\"\n"
                                                       + "  },\n"
                                                       + "  \"resource_pool\": \"pool\",\n"
                                                       + "  \"storage\": \"datastore\",\n"
                                                       + "  \"folder\": \"folder\",\n"
                                                       + "  \"container_repo\": {\n"
                                                       + "    \"url\": \"https://docker-repo.com\",\n"
                                                       + "    \"username\": \"user\",\n"
                                                       + "    \"password\": \"docker\"\n"
                                                       + "  }\n"
                                                       + "}";

    private static final String POST_NO_FOLDER_BODY = "{\n"
                                                   + "  \"name\": \"OnPrem\",\n"
                                                   + "  \"type\": \"ON_PREM\",\n"
                                                   + "  \"vcenter\": {\n"
                                                   + "    \"url\": \"http://vcenter\",\n"
                                                   + "    \"username\": \"admin\",\n"
                                                   + "    \"password\": \"password\"\n"
                                                   + "  },\n"
                                                   + "  \"resource_pool\": \"pool\",\n"
                                                   + "  \"storage\": \"datastore\",\n"
                                                   + "  \"network\": {\n"
                                                   + "    \"name\": \"Network 1\",\n"
                                                   + "    \"ip_pool\": [\n"
                                                   + "      \"10.1.1.16-10.1.1.64\",\n"
                                                   + "      \"10.1.1.100-10.1.1.200\"\n"
                                                   + "    ],\n"
                                                   + "    \"gateway\": \"10.1.1.1\",\n"
                                                   + "    \"subnet\": \"24\",\n"
                                                   + "    \"name_servers\": [\n"
                                                   + "      \"10.1.1.3\"\n"
                                                   + "    ]\n"
                                                   + "  },\n"
                                                   + "  \"container_repo\": {\n"
                                                   + "    \"url\": \"https://docker-repo.com\",\n"
                                                   + "    \"username\": \"user\",\n"
                                                   + "    \"password\": \"docker\"\n"
                                                   + "  }\n"
                                                   + "}";

    private static final String POST_EMPTY_FOLDER_BODY = "{\n"
                                                         + "  \"name\": \"OnPrem\",\n"
                                                         + "  \"type\": \"ON_PREM\",\n"
                                                         + "  \"vcenter\": {\n"
                                                         + "    \"url\": \"http://vcenter\",\n"
                                                         + "    \"username\": \"admin\",\n"
                                                         + "    \"password\": \"password\"\n"
                                                         + "  },\n"
                                                         + "  \"resource_pool\": \"pool\",\n"
                                                         + "  \"storage\": \"datastore\",\n"
                                                         + "  \"folder\": \"  \",\n"
                                                         + "  \"network\": {\n"
                                                         + "    \"name\": \"Network 1\",\n"
                                                         + "    \"ip_pool\": [\n"
                                                         + "      \"10.1.1.16-10.1.1.64\",\n"
                                                         + "      \"10.1.1.100-10.1.1.200\"\n"
                                                         + "    ],\n"
                                                         + "    \"gateway\": \"10.1.1.1\",\n"
                                                         + "    \"subnet\": \"24\",\n"
                                                         + "    \"name_servers\": [\n"
                                                         + "      \"10.1.1.3\"\n"
                                                         + "    ]\n"
                                                         + "  },\n"
                                                         + "  \"container_repo\": {\n"
                                                         + "    \"url\": \"https://docker-repo.com\",\n"
                                                         + "    \"username\": \"user\",\n"
                                                         + "    \"password\": \"docker\"\n"
                                                         + "  }\n"
                                                         + "}";

    private static final String POST_NO_CONTAINER_BODY = "{\n"
                                                   + "  \"name\": \"OnPrem\",\n"
                                                   + "  \"type\": \"ON_PREM\",\n"
                                                   + "  \"vcenter\": {\n"
                                                   + "    \"url\": \"http://vcenter\",\n"
                                                   + "    \"username\": \"admin\",\n"
                                                   + "    \"password\": \"password\"\n"
                                                   + "  },\n"
                                                   + "  \"resource_pool\": \"pool\",\n"
                                                   + "  \"storage\": \"datastore\",\n"
                                                   + "  \"folder\": \"folder\",\n"
                                                   + "  \"network\": {\n"
                                                   + "    \"name\": \"Network 1\",\n"
                                                   + "    \"ip_pool\": [\n"
                                                   + "      \"10.1.1.16-10.1.1.64\",\n"
                                                   + "      \"10.1.1.100-10.1.1.200\"\n"
                                                   + "    ],\n"
                                                   + "    \"gateway\": \"10.1.1.1\",\n"
                                                   + "    \"subnet\": \"24\",\n"
                                                   + "    \"name_servers\": [\n"
                                                   + "      \"10.1.1.3\"\n"
                                                   + "    ]\n"
                                                   + "  }\n"
                                                   + "}";

    private static final String POST_ZONE_BODY = "{\n"
                                                 + "  \"name\": \"OnPrem\",\n"
                                                 + "  \"latitude\": \"45.5946\",\n"
                                                 + "  \"longitude\": \"-121.1787\",\n"
                                                 + "  \"type\": \"VMC_AWS\"\n"
                                                 + "}";

    @Autowired
    WebApplicationContext context;

    private MockMvc mockMvc;

    @Autowired
    Jackson2ObjectMapperBuilder jacksonBuilder;

    private AuthenticationContext userAuth;
    private AuthenticationContext adminAuth;
    private AuthenticationContext systemAuth;

    private ObjectMapper objectMapper;

    @MockBean
    ZoneService zoneService;

    @MockBean
    OrchestrationSiteServiceStub orchestrationClient;

    @BeforeEach
    void setUp() {
        mockMvc = MockMvcBuilders
                .webAppContextSetup(context)
                .apply(springSecurity())
                .build();


        userAuth = createContext("operator", ORG_ID,
                                 ImmutableList.of(Roles.ORG_USER),
                                 Collections.emptyList(),
                                 Collections.emptyList(), "");

        adminAuth = createContext("operator", ORG_ID,
                                 ImmutableList.of(Roles.ORG_USER, Roles.CONSORTIUM_ADMIN),
                                 Collections.emptyList(),
                                 Collections.emptyList(), "");

        systemAuth = createContext("operator", OPER_ORG,
                                   ImmutableList.of(Roles.ORG_USER, Roles.SYSTEM_ADMIN),
                                   Collections.emptyList(),
                                   Collections.emptyList(), "");

        // This creates our default object mapper
        objectMapper = jacksonBuilder.build();

        OnPremZone.LogManagementOnPrem logOnPrem = new OnPremZone.LogManagementOnPrem();
        logOnPrem.destination = Zone.LogDestination.LOG_INSIGHT;
        logOnPrem.address = "10.78.20.10:9000";
        logOnPrem.username = "foo";
        logOnPrem.password = "bar";

        Map<String, String> usWest =
                ImmutableMap.of("name", "US_WEST1", "geo-latitude", "45.5946", "geo-longitude", "-121.1787");
        Map<String, String> usEast =
                ImmutableMap.of("name", "US_EAST1", "geo-latitude", "33.1960", "geo-longitude", "-80.0131");

        OnPremZone ozone = getOnpremZone(OP_SITE, ORG_ID);
        ozone.setLogManagements(Lists.newArrayList(logOnPrem));
        List<Zone> sites =
                ImmutableList.of(new Zone(SITE_1, VMC_AWS, usWest),
                        new Zone(SITE_2, VMC_AWS, usEast),
                        ozone);
        when(zoneService.getAllAuthorized()).thenReturn(sites);
        when(zoneService.getAuthorized(OP_SITE)).thenReturn(ozone);
        when(zoneService.put(any(Zone.class))).thenAnswer(i -> {
            Zone z = i.getArgument(0);
            z.setId(UUID.randomUUID());
            return z;
        });

    }

    @Test
    void testGet() throws Exception {
        MvcResult result =  mockMvc.perform(get("/api/blockchains/zones").with(authentication(userAuth)))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        List<ZoneListResponse> zones =
                objectMapper.readValue(body, new TypeReference<List<ZoneListResponse>>() {});
        Assertions.assertEquals(3, zones.size());
        Assertions.assertEquals("US_WEST1", zones.get(0).getName());
        Assertions.assertEquals("45.5946", zones.get(0).getLatitude());
        Assertions.assertEquals("-121.1787", zones.get(0).getLongitude());
        Assertions.assertEquals(VMC_AWS, zones.get(0).getType());
        Assertions.assertEquals("US_EAST1", zones.get(1).getName());
        Assertions.assertEquals("33.1960", zones.get(1).getLatitude());
        Assertions.assertEquals("-80.0131", zones.get(1).getLongitude());
        Assertions.assertEquals(VMC_AWS, zones.get(1).getType());
        // Make sure the on-prem site returns the right type
        Assertions.assertEquals(ON_PREM, zones.get(2).getType());
    }

    @Test
    void testGetDetails() throws Exception {
        MvcResult result =  mockMvc.perform(get("/api/blockchains/zones/" + OP_SITE).with(authentication(adminAuth)))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        ZoneResponse z = objectMapper.readValue(body, ZoneResponse.class);
        Assertions.assertTrue(z instanceof OnPremGetResponse);
        OnPremGetResponse r = (OnPremGetResponse) z;
        Assertions.assertEquals("On Prem", r.getName());
        Assertions.assertNotNull(r.getContainerRepo());
        Assertions.assertNotNull(r.getVcenter());
        Assertions.assertNotNull(r.getName());
        Assertions.assertEquals("Network 1", r.getNetwork().getName());
        Assertions.assertEquals("admin", r.getVcenter().getUsername());
        Assertions.assertEquals(1, r.logManagements.size());
        Assertions.assertEquals(Zone.LogDestination.LOG_INSIGHT, r.logManagements.get(0).destination);
    }

    @Test
    void testLoadZonesBadAction() throws Exception {
        mockMvc.perform(post("/api/blockchains/zones?action=sleep").with(authentication(adminAuth)))
                .andExpect(status().isBadRequest());

    }

    @Test
    void testPostOnPrem() throws Exception {
        MvcResult result = mockMvc.perform(post("/api/blockchains/zones").with(authentication(adminAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(POST_ONPREM_BODY))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        ZoneResponse zone = objectMapper.readValue(body, OnPremGetResponse.class);
        verify(zoneService, times(1)).put(any(Zone.class));
        Assertions.assertTrue(zone instanceof OnPremGetResponse);
        // This was done as a consortium admin.  The org id should have been changed.
        Assertions.assertEquals(UUID.fromString("9ecb07bc-482c-48f3-80d0-23c4f9514902"),
                                ((OnPremGetResponse) zone).getOrgId());
        Assertions.assertEquals("admin", ((OnPremGetResponse) zone).getVcenter().getUsername());
    }

    @Test
    void testPostOnPremNoOrg() throws Exception {
        MvcResult result = mockMvc.perform(post("/api/blockchains/zones").with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(POST_NO_ORG_BODY))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        ZoneResponse zone = objectMapper.readValue(body, ZoneResponse.class);
        verify(zoneService, times(1)).put(any(Zone.class));
        Assertions.assertTrue(zone instanceof OnPremGetResponse);
        // Org not specified, so it should be filled in
        Assertions.assertEquals(UUID.fromString("9ecb07bc-482c-48f3-80d0-23c4f9514902"),
                                ((OnPremGetResponse) zone).getOrgId());
        Assertions.assertEquals("admin", ((OnPremGetResponse) zone).getVcenter().getUsername());
    }

    @Test
    void testPostOnPremSysadmin() throws Exception {
        MvcResult result = mockMvc.perform(post("/api/blockchains/zones").with(authentication(systemAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(POST_ONPREM_BODY))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        ZoneResponse zone = objectMapper.readValue(body, ZoneResponse.class);
        // This was done as a system admin.  The org_id should not have been changed.
        Assertions.assertEquals(UUID.fromString("5e5ff1c8-34b9-4fa3-9924-83eb14354d4c"),
                                ((OnPremGetResponse) zone).getOrgId());
        Assertions.assertEquals("admin", ((OnPremGetResponse) zone).getVcenter().getUsername());
    }

    @Test
    void testPostOnPremSysadminNoOrg() throws Exception {
        MvcResult result = mockMvc.perform(post("/api/blockchains/zones").with(authentication(systemAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(POST_NO_ORG_BODY))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        ZoneResponse zone = objectMapper.readValue(body, ZoneResponse.class);
        // No org, so it should be filled in from OPER_ORG
        Assertions.assertEquals(OPER_ORG,
                                ((OnPremGetResponse) zone).getOrgId());
        Assertions.assertEquals("admin", ((OnPremGetResponse) zone).getVcenter().getUsername());
    }

    @Test
    void testSiteTest() throws Exception {
        // grpc call has a valid return.
        ValidateOrchestrationSiteResponse r = ValidateOrchestrationSiteResponse.newBuilder().build();
        doAnswer(i -> {
            StreamObserver ob = i.getArgument(1);
            ob.onNext(r);
            ob.onCompleted();
            return null;
        })
                .when(orchestrationClient)
                .validateOrchestrationSite(any(ValidateOrchestrationSiteRequest.class),
                                           any(StreamObserver.class));
        MvcResult result = mockMvc.perform(post("/api/blockchains/zones?action=test")
                                                   .with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(POST_ONPREM_BODY))
                .andExpect(status().isOk()).andReturn();
    }

    @Test
    void testSiteNoContainer() throws Exception {
        // grpc call has a valid return.
        ValidateOrchestrationSiteResponse r = ValidateOrchestrationSiteResponse.newBuilder().build();
        doAnswer(i -> {
            StreamObserver ob = i.getArgument(1);
            ob.onNext(r);
            ob.onCompleted();
            return null;
        })
                .when(orchestrationClient)
                .validateOrchestrationSite(any(ValidateOrchestrationSiteRequest.class),
                                           any(StreamObserver.class));
        MvcResult result = mockMvc.perform(post("/api/blockchains/zones?action=test")
                                                   .with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(POST_NO_CONTAINER_BODY))
                .andExpect(status().isOk()).andReturn();
    }

    @Test
    void testSiteNoVcenter() throws Exception {
        // grpc call has a valid return.
        ValidateOrchestrationSiteResponse r = ValidateOrchestrationSiteResponse.newBuilder().build();
        doAnswer(i -> {
            StreamObserver ob = i.getArgument(1);
            ob.onNext(r);
            ob.onCompleted();
            return null;
        })
                .when(orchestrationClient)
                .validateOrchestrationSite(any(ValidateOrchestrationSiteRequest.class),
                                           any(StreamObserver.class));
        mockMvc.perform(post("/api/blockchains/zones?action=test")
                                                   .with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(POST_NO_VCENTER_BODY))
                .andExpect(status().isBadRequest());
    }

    @Test
    void testSiteNoNetwork() throws Exception {
        // grpc call has a valid return.
        ValidateOrchestrationSiteResponse r = ValidateOrchestrationSiteResponse.newBuilder().build();
        doAnswer(i -> {
            StreamObserver ob = i.getArgument(1);
            ob.onNext(r);
            ob.onCompleted();
            return null;
        })
                .when(orchestrationClient)
                .validateOrchestrationSite(any(ValidateOrchestrationSiteRequest.class),
                                           any(StreamObserver.class));
        mockMvc.perform(post("/api/blockchains/zones?action=test")
                                .with(authentication(adminAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(POST_NO_NETWORK_BODY))
                .andExpect(status().isBadRequest());
    }

    @Test
    void testSiteNoFolder() throws Exception {
        // grpc call has a valid return.
        ValidateOrchestrationSiteResponse r = ValidateOrchestrationSiteResponse.newBuilder().build();
        doAnswer(i -> {
            StreamObserver ob = i.getArgument(1);
            ob.onNext(r);
            ob.onCompleted();
            return null;
        })
                .when(orchestrationClient)
                .validateOrchestrationSite(any(ValidateOrchestrationSiteRequest.class),
                                           any(StreamObserver.class));
        mockMvc.perform(post("/api/blockchains/zones?action=test")
                                .with(authentication(adminAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(POST_NO_FOLDER_BODY))
                .andExpect(status().isBadRequest());
    }

    @Test
    void testSiteEmptyFolder() throws Exception {
        // grpc call has a valid return.
        ValidateOrchestrationSiteResponse r = ValidateOrchestrationSiteResponse.newBuilder().build();
        doAnswer(i -> {
            StreamObserver ob = i.getArgument(1);
            ob.onNext(r);
            ob.onCompleted();
            return null;
        })
                .when(orchestrationClient)
                .validateOrchestrationSite(any(ValidateOrchestrationSiteRequest.class),
                                           any(StreamObserver.class));
        mockMvc.perform(post("/api/blockchains/zones?action=test")
                                .with(authentication(adminAuth))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(POST_EMPTY_FOLDER_BODY))
                .andExpect(status().isBadRequest());
    }


    @Test
    void testSiteTestError() throws Exception {
        // grpc call has a valid return.
        ValidateOrchestrationSiteResponse r = ValidateOrchestrationSiteResponse.newBuilder().build();
        doAnswer(i -> {
            StreamObserver ob = i.getArgument(1);
            ob.onError(new BadRequestException("bad"));
            return null;
        })
                .when(orchestrationClient)
                .validateOrchestrationSite(any(ValidateOrchestrationSiteRequest.class),
                                           any(StreamObserver.class));
        MvcResult result = mockMvc.perform(post("/api/blockchains/zones?action=test")
                                                   .with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(POST_ONPREM_BODY))

                .andExpect(status().is5xxServerError()).andReturn();
    }

    @Test
    void testUtils() throws Exception {
        Zone op = getOnpremZone(OP_SITE, ORG_ID);
        OrchestrationSiteInfo info = BlockchainUtils.toInfo(op);
        System.out.println(info);
    }



}
