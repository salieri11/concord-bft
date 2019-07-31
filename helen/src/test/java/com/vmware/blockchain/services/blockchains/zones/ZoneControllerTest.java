/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.zones;

import static com.vmware.blockchain.security.MvcTestSecurityConfig.createContext;
import static com.vmware.blockchain.security.SecurityTestUtils.ORG_ID;
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
import com.vmware.blockchain.MvcConfig;
import com.vmware.blockchain.auth.AuthenticationContext;
import com.vmware.blockchain.common.HelenExceptionHandler;
import com.vmware.blockchain.deployment.model.OrchestrationSiteInfo.Type;
import com.vmware.blockchain.security.MvcTestSecurityConfig;
import com.vmware.blockchain.services.blockchains.zones.ZoneController.ZoneGetRequest;
import com.vmware.blockchain.services.profiles.Roles;

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

    @Autowired
    WebApplicationContext context;

    private MockMvc mockMvc;

    @Autowired
    Jackson2ObjectMapperBuilder jacksonBuilder;

    private AuthenticationContext userAuth;
    private AuthenticationContext adminAuth;

    private ObjectMapper objectMapper;

    @MockBean
    ZoneService zoneService;

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

        // This creates our default object mapper
        objectMapper = jacksonBuilder.build();

        Map<String, String> usWest =
                ImmutableMap.of("name", "US_WEST1", "geo-latitude", "45.5946", "geo-longitude", "-121.1787");
        Map<String, String> usEast =
                ImmutableMap.of("name", "US_EAST1", "geo-latitude", "33.1960", "geo-longitude", "-80.0131");
        List<Zone> sites =
                ImmutableList.of(new Zone(SITE_1, Type.VMC, usWest), new Zone(SITE_2, Type.VMC, usEast));
        when(zoneService.getZones()).thenReturn(sites);
    }

    @Test
    void testGet() throws Exception {
        MvcResult result =  mockMvc.perform(get("/api/blockchains/zones").with(authentication(userAuth)))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        List<ZoneGetRequest> zones = objectMapper.readValue(body, new TypeReference<List<ZoneGetRequest>>() {});
        Assertions.assertEquals(2, zones.size());
        Assertions.assertEquals("US_WEST1", zones.get(0).getName());
        Assertions.assertEquals("45.5946", zones.get(0).getLatitude());
        Assertions.assertEquals("-121.1787", zones.get(0).getLongitude());
        Assertions.assertEquals("US_EAST1", zones.get(1).getName());
        Assertions.assertEquals("33.1960", zones.get(1).getLatitude());
        Assertions.assertEquals("-80.0131", zones.get(1).getLongitude());
    }

    @Test
    void testLoadZones() throws Exception {
        mockMvc.perform(post("/api/blockchains/zones?action=reload").with(authentication(adminAuth)))
                .andExpect(status().isOk());
        verify(zoneService, times(1)).loadZones();

    }

    @Test
    void testLoadZonesNoAction() throws Exception {
        mockMvc.perform(post("/api/blockchains/zones").with(authentication(adminAuth)))
                .andExpect(status().isBadRequest());

    }

    @Test
    void testLoadZonesBadAction() throws Exception {
        mockMvc.perform(post("/api/blockchains/zones?action=sleep").with(authentication(adminAuth)))
                .andExpect(status().isBadRequest());

    }
}