/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.ImmutableList;
import com.vmware.blockchain.MvcConfig;
import com.vmware.blockchain.utils.ControllerTestConfig;

import lombok.Data;
import lombok.Value;

/**
 * Test various controller exceptions to be sure we get proper error messages.
 */
@ExtendWith(SpringExtension.class)
@WebMvcTest(controllers = ExceptionHandlerTestController.class)
@AutoConfigureMockMvc(addFilters = false)
@ContextConfiguration(classes = { MvcConfig.class, ControllerTestConfig.class })
@ComponentScan(basePackageClasses = { ExceptionHandlerTest.class, HelenExceptionHandler.class })
@TestPropertySource(properties = "spring.main.allow-bean-definition-overriding=true")
public class ExceptionHandlerTest {

    @Data
    private static class ErrorMessage {
        String errorCode;
        String errorMessage;
        int status;
        String path;
    }

    @Value
    private static class HandlerTest {
        private String uri;
        String errorCode;
        private int status;
    }

    // URIs and return values we expect
    ImmutableList<HandlerTest> helenExceptionTests = new ImmutableList.Builder<HandlerTest>()
            .add(new HandlerTest("/api/badrequest", "BadRequestException", 400))
            .add(new HandlerTest("/api/concordexception", "ConcordConnectionException", 500))
            .add(new HandlerTest("/api/conflict", "ConflictException", 409))
            .add(new HandlerTest("/api/entitymodification", "EntityModificationException", 400))
            .add(new HandlerTest("/api/forbidden", "ForbiddenException", 403))
            .add(new HandlerTest("/api/notfound", "NotFoundException", 404))
            .add(new HandlerTest("/api/serviceunavailable", "ServiceUnavailableException", 503))
            .add(new HandlerTest("/api/unauthorized", "UnauthorizedException", 401))
            .add(new HandlerTest("/api/wallet", "WalletException", 400)).build();


    ImmutableList<HandlerTest> otherExceptionTests = new ImmutableList.Builder<HandlerTest>()
                    .add(new HandlerTest("/api/unsupported", "UnsupportedOperationException", 405))
                    .add(new HandlerTest("/api/illegalarg", "IllegalArgumentException", 400))
                    .add(new HandlerTest("/api/io", "IOException", 500))
                    .add(new HandlerTest("/api/protocol", "InvalidProtocolBufferException", 500))
                    .add(new HandlerTest("/api/denied", "AccessDeniedException", 403)).build();

    @Autowired
    MockMvc mockMvc;

    @Autowired
    Jackson2ObjectMapperBuilder jacksonBuilder;

    private ObjectMapper objectMapper;

    @BeforeEach
    void init() throws Exception {
        objectMapper = jacksonBuilder.build();
    }

    private void doTest(HandlerTest test) throws Exception {
        MvcResult result = mockMvc.perform(get(test.getUri())).andReturn();
        // This should not give us a parse error
        ErrorMessage message = objectMapper.readValue(result.getResponse().getContentAsByteArray(), ErrorMessage.class);
        Assertions.assertEquals(test.getErrorCode(), message.getErrorCode());
        Assertions.assertEquals(test.getUri(), message.getPath());
        Assertions.assertEquals(test.getStatus(), message.getStatus());
        Assertions.assertEquals(test.getStatus(), result.getResponse().getStatus());
    }

    // This one test tests all the HelenExceptions
    @Test
    void helenTests() throws Exception {
        for (HandlerTest test : helenExceptionTests) {
            doTest(test);
        }
    }

    // This one test tests all the other exceptions
    @Test
    void otherTests() throws Exception {
        for (HandlerTest test : otherExceptionTests) {
            doTest(test);
        }
    }

    // Test the argument handling in the error message
    @Test
    void testArguments() throws Exception {
        MvcResult result = mockMvc.perform(get("/api/text?arg1=Hi&arg2=Mom")).andReturn();
        ErrorMessage message = objectMapper.readValue(result.getResponse().getContentAsByteArray(), ErrorMessage.class);
        Assertions.assertEquals("Bad request: Hi, Mom", message.getErrorMessage());
        Assertions.assertEquals("/api/text", message.getPath());
    }

    @Test
    void testRandom() throws Exception {
        // This call generates a divide by zero
        MvcResult result = mockMvc.perform(get("/api/random")).andReturn();
        ErrorMessage message = objectMapper.readValue(result.getResponse().getContentAsByteArray(), ErrorMessage.class);
        Assertions.assertEquals("ArithmeticException", message.getErrorCode());
        Assertions.assertEquals(500, message.getStatus());

    }

    @Test
    void testGoodUuid() throws Exception {
        mockMvc.perform(get("/api/uuid/a67b04e8-178e-42e5-8ceb-adb12222f8d4"))
                .andExpect(status().isOk());
    }

    @Test
    void testBadUuid() throws Exception {
        mockMvc.perform(get("/api/uuid/himom"))
                .andExpect(status().isBadRequest());
    }

}
