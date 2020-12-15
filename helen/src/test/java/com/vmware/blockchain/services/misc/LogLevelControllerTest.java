/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.misc;

import static com.vmware.blockchain.security.MvcTestSecurityConfig.createContext;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.authentication;
import static org.springframework.security.test.web.servlet.setup.SecurityMockMvcConfigurers.springSecurity;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

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
import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.auth.AuthenticationContext;
import com.vmware.blockchain.common.HelenExceptionHandler;
import com.vmware.blockchain.operation.OperationContext;
import com.vmware.blockchain.security.MvcTestSecurityConfig;

import com.vmware.blockchain.services.profiles.DefaultProfiles;
import com.vmware.blockchain.services.profiles.VmbcRoles;

/**
 * Tests for the LogLevel controller.
 */
@ExtendWith(SpringExtension.class)
@TestPropertySource(locations = "classpath:lint-test.properties")
@WebMvcTest(controllers = {LogLevelController.class})
@ContextConfiguration(classes = {MvcTestSecurityConfig.class, MvcConfig.class})
@ComponentScan(basePackageClasses = {com.vmware.blockchain.services.misc.LogLevelControllerTest.class,
                                     HelenExceptionHandler.class})
public class LogLevelControllerTest {
    @Autowired
    private WebApplicationContext context;

    private MockMvc mockMvc;

    @MockBean
    DefaultProfiles defaultProfiles;

    @Autowired
    Jackson2ObjectMapperBuilder jacksonBuilder;

    @MockBean
    OperationContext operationContext;

    @Autowired
    AuthHelper authHelper;

    private ObjectMapper objectMapper;

    private AuthenticationContext adminAuth;
    private AuthenticationContext userAuth;

    static final UUID C2_ID = UUID.fromString("04e4f62d-5364-4363-a582-b397075b65a3");
    static final UUID BC_ID = UUID.fromString("437d97b2-76df-4596-b0d8-3d8a9412ff2f");
    static final UUID ORG_ID = UUID.fromString("5c373085-0cd1-47e4-b4f2-66d418f22fdf");

    /**
     * Initialize log4j.
     */
    @BeforeEach
    public void init() {
        mockMvc = MockMvcBuilders
                .webAppContextSetup(context)
                .apply(springSecurity())
                .build();

        // This creates our default object mapper
        objectMapper = jacksonBuilder.build();

        // Create authorizations for the different users.
        adminAuth = createContext("operator", ORG_ID,
                                  ImmutableList.of(VmbcRoles.SYSTEM_ADMIN, VmbcRoles.ORG_USER),
                                  ImmutableList.of(C2_ID),
                                  ImmutableList.of(BC_ID), "");

        userAuth = createContext("operator", ORG_ID,
                                 ImmutableList.of(VmbcRoles.ORG_USER),
                                 ImmutableList.of(C2_ID),
                                 ImmutableList.of(BC_ID), "");

    }

    @Test
    public void testGetLoggers() throws Exception {
        MvcResult result = mockMvc.perform(get("/api/config/logging/logger").with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        String body = result.getResponse().getContentAsString();
        Map<String, List<String>> res = objectMapper.readValue(body, new TypeReference<Map<String, List<String>>>() {});
        // Do we see 3 loggers?
        Assertions.assertEquals(3, res.get("loggers").size());
    }

    @Test
    public void testGetGlobalLogLevel() throws Exception {
        MvcResult result = mockMvc.perform(get("/api/config/logging/log_level")
                                                   .with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
                .andReturn();
        String body = result.getResponse().getContentAsString();
        Map<String, String> res = objectMapper.readValue(body, new TypeReference<Map<String, String>>() {});
        // Do we see a log level?
        Assertions.assertEquals(1, res.size());
    }

    @Test
    public void testGetLogLevelForALogger() throws Exception {
        MvcResult result = mockMvc.perform(get("/api/config/logging/log_level?logger=ethLogger")
                                                   .with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
                .andReturn();
        String body = result.getResponse().getContentAsString();
        Map<String, String> res = objectMapper.readValue(body, new TypeReference<Map<String, String>>() {});
        // Do we see a log level?
        Assertions.assertEquals(2, res.size());
        Assertions.assertEquals("ethLogger", res.get("logger"));
    }

    @Test
    public void testGetLogLevelForAnInvalidLogger() throws Exception {
        MvcResult result = mockMvc.perform(get("/api/config/logging/log_level?logger=badLogger")
                                                   .with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest())
                .andReturn();
        String body = result.getResponse().getContentAsString();
        Map<String, String> res = objectMapper.readValue(body, new TypeReference<Map<String, String>>() {});
        // Do we see a log level?
        Assertions.assertEquals(1, res.size());
        Assertions.assertEquals("Invalid Logger badLogger", res.get("error"));
    }

    @Test
    public void testPutGlobalLogLevel() throws Exception {
        String body = "{\"log_level\": \"DEBUG\"}";
        MvcResult result = mockMvc.perform(put("/api/config/logging/log_level")
                                                   .with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(body))

                .andExpect(status().isOk())
                .andReturn();
        String response = result.getResponse().getContentAsString();
        Map<String, String> res = objectMapper.readValue(response, new TypeReference<Map<String, String>>() {});
        // Do we see a log level?
        Assertions.assertEquals(1, res.size());
        Assertions.assertEquals("DEBUG", res.get("log_level"));
    }

    @Test
    public void testPutLogLevelForALogger() throws Exception {
        String body = "{\"log_level\": \"ERROR\", \"logger\": \"ethLogger\"}";
        MvcResult result = mockMvc.perform(put("/api/config/logging/log_level")
                                                   .with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(body))

                .andExpect(status().isOk())
                .andReturn();
        String response = result.getResponse().getContentAsString();
        Map<String, String> res = objectMapper.readValue(response, new TypeReference<Map<String, String>>() {});
        // Do we see a log level?
        Assertions.assertEquals(2, res.size());
        Assertions.assertEquals("ERROR", res.get("log_level"));
        Assertions.assertEquals("ethLogger", res.get("logger"));
    }

    @Test
    public void testPutLogLevelForAnInvalidLogger() throws Exception {
        String body = "{\"log_level\": \"DEBUG\", \"logger\": \"badLogger\"}";
        MvcResult result = mockMvc.perform(put("/api/config/logging/log_level")
                                                   .with(authentication(adminAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(body))
                .andExpect(status().isBadRequest())
                .andReturn();
        String response = result.getResponse().getContentAsString();
        Map<String, String> res = objectMapper.readValue(response, new TypeReference<Map<String, String>>() {});
        // Do we see a log level?
        Assertions.assertEquals(1, res.size());
        Assertions.assertEquals("Invalid Logger badLogger", res.get("error"));
    }

    @Test
    public void testPutLogLevelForALoggerByInvalidUser() throws Exception {
        String body = "{\"log_level\": \"DEBUG\", \"logger\": \"ethLogger\"}";
        MvcResult result = mockMvc.perform(put("/api/config/logging/log_level")
                                                   .with(authentication(userAuth))
                                                   .contentType(MediaType.APPLICATION_JSON)
                                                   .content(body))

                .andExpect(status().isForbidden())
                .andReturn();
        String response = result.getResponse().getContentAsString();
        Map<String, String> res = objectMapper.readValue(response, new TypeReference<Map<String, String>>() {});
        // Do we see a log level?
        Assertions.assertEquals(5, res.size());
        Assertions.assertEquals("Access is denied", res.get("error_message"));
        Assertions.assertEquals("AccessDeniedException", res.get("error_code"));
    }
}

