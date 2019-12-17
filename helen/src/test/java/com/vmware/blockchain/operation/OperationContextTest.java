/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.operation;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.vmware.blockchain.MvcConfig;
import com.vmware.blockchain.common.HelenExceptionHandler;

/**
 * Test Operation Contex.
 */
@ExtendWith(SpringExtension.class)
@WebMvcTest(controllers = {OperationTestController.class})
@ComponentScan(basePackageClasses = {OperationContext.class, HelenExceptionHandler.class})
@ContextConfiguration(classes = {OperationTestConfig.class, MvcConfig.class})
class OperationContextTest {
    @Autowired
    private OperationContext operationContext;

    @Autowired
    Jackson2ObjectMapperBuilder jacksonBuilder;

    @Autowired
    OperationTestController operationTestController;

    @Autowired
    HelenExceptionHandler helenExceptionHandler;

    private MockMvc mockMvc;
    private ObjectMapper objectMapper;

    @BeforeEach
    void setUp() {
        objectMapper = jacksonBuilder.build();
        MappingJackson2HttpMessageConverter mappingJackson2HttpMessageConverter =
                new MappingJackson2HttpMessageConverter();
        mappingJackson2HttpMessageConverter.setObjectMapper(objectMapper);
        // Need to do a few things to make this mock controller pick up the right controller advice and object
        // mapper
        mockMvc = MockMvcBuilders.standaloneSetup(operationTestController)
                .setControllerAdvice(helenExceptionHandler)
                .setMessageConverters(mappingJackson2HttpMessageConverter)
                .addFilter(new RequestTrackingFilter(operationContext))
                .build();
    }

    @Test
    void testOpid() throws Exception {
        mockMvc.perform(get("/api/authtest")).andExpect(status().isOk());
    }


    @Test
    void testBad() throws Exception {
        MvcResult result = mockMvc.perform(get("/api/bad")).andExpect(status().isBadRequest()).andReturn();
        String body = result.getResponse().getContentAsString();
        Map<String, String> r = objectMapper.readValue(body, Map.class);
        Assertions.assertNotNull(r.get("op_id"));
        Assertions.assertFalse(r.get("op_id").isBlank());
    }
}