/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.exceptions;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.vmware.blockchain.agent.utils.MvcTestSecurityConfig;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * AgentExceptionHandler Test.
 * */
@ExtendWith(SpringExtension.class)
@WebMvcTest(controllers = AgentExceptionTestController.class)
@ComponentScan(basePackageClasses = {AgentExceptionHandlerTest.class, AgentExceptionTestController.class,
                                     AgentExceptionHandler.class})
@ContextConfiguration(classes = {MvcTestSecurityConfig.class})
public class AgentExceptionHandlerTest {

    @Autowired
    MockMvc mvc;

    @Autowired
    Jackson2ObjectMapperBuilder jacksonBuilder;

    private ObjectMapper objectMapper;

    @BeforeEach
    void init() throws Exception {
        objectMapper = jacksonBuilder.build();
    }

    @Test
    public void configurationRetrievalFailureException() throws Exception {
        String path = "/api/config-error/10222e97-ca0b-45b3-a324-af4f1057ebd9";
        MvcResult result = mvc.perform(post("/api/config-error/10222e97-ca0b-45b3-a324-af4f1057ebd9")
                                               .accept(MediaType.APPLICATION_JSON_VALUE)).andReturn();

        ErrorResponse error = objectMapper.readValue(result.getResponse().getContentAsByteArray(), ErrorResponse.class);
        assertEquals(error.getAgentErrorCode(), ErrorCode.CONFIGURATION_RETRIEVAL_FAILURE);
        assertEquals(error.getStatus(), HttpStatus.INTERNAL_SERVER_ERROR);
        assertEquals(error.getPath(), path);
        assertEquals(error.getDetails(), ErrorCode.CONFIGURATION_RETRIEVAL_FAILURE.getMessage() + ": " + "Some");
    }

    @Test
    public void randomException() throws Exception {
        String path = "/api/random";

        MvcResult result = mvc.perform(get(path).accept(MediaType.APPLICATION_JSON_VALUE))
                .andReturn();

        ErrorResponse error = objectMapper.readValue(result.getResponse().getContentAsByteArray(), ErrorResponse.class);
        assertEquals(error.getAgentErrorCode(), ErrorCode.UNKNOWN_ERROR);
        assertEquals(error.getStatus(), HttpStatus.INTERNAL_SERVER_ERROR);
        assertEquals(error.getPath(), path);
        assertEquals(error.getDetails(), ErrorCode.UNKNOWN_ERROR.getMessage() + ": " + "Some");
    }

    @Test
    public void noException() {
        String path = "/api/ok";
        assertDoesNotThrow(() -> mvc.perform(get(path).accept(MediaType.APPLICATION_JSON_VALUE))
                .andReturn());
    }

    @AllArgsConstructor
    @NoArgsConstructor
    @Getter
    @Setter
    private static class ErrorResponse {

        private HttpStatus status;
        private ErrorCode agentErrorCode;
        private String excCause;
        private String details;
        private String path;
    }

}
