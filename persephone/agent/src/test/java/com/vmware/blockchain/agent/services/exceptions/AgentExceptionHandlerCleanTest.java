/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.exceptions;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.lang.reflect.Method;
import java.util.Collections;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.method.annotation.ExceptionHandlerMethodResolver;
import org.springframework.web.servlet.mvc.method.annotation.ExceptionHandlerExceptionResolver;
import org.springframework.web.servlet.mvc.method.annotation.ServletInvocableHandlerMethod;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.vmware.blockchain.agent.utils.MvcTestSecurityConfig;

@ExtendWith(SpringExtension.class)
@WebMvcTest(controllers = AgentExceptionTestController.class)
@ContextConfiguration(classes = {MvcConfig.class, MvcTestSecurityConfig.class})
public class AgentExceptionHandlerCleanTest {

    @InjectMocks
    private AgentExceptionTestController controller;

    @Autowired
    MockMvc mvc;


    @BeforeEach
    public void setup() {

        mvc = MockMvcBuilders.standaloneSetup(controller)
                .setHandlerExceptionResolvers(createExceptionResolver())
                .build();
    }

    /**
     * This is totally not going to build.
     * The test is not working, I am putting it into the MR, only as an illustration
     * of what I do, in order to seek for help with this test.
     *
     * */

    @Test
    public void thatExceptionHappens() throws Exception {

        MvcResult result = mvc.perform(post("/api/gg/10222e97-ca0b-45b3-a324-af4f1057ebd9")).andReturn();
        mvc.perform(post("/api/gg/10222e97-ca0b-45b3-a324-af4f1057ebd9")).andExpect(status().isInternalServerError());

    }

    private ExceptionHandlerExceptionResolver createExceptionResolver() {
        ExceptionHandlerExceptionResolver exceptionResolver = new ExceptionHandlerExceptionResolver() {
            @Override
            protected ServletInvocableHandlerMethod getExceptionHandlerMethod(
                    HandlerMethod handlerMethod, Exception exception) {
                Method method = new ExceptionHandlerMethodResolver(
                        AgentExceptionHandler.class).resolveMethod(exception);
                return new ServletInvocableHandlerMethod(
                        new AgentExceptionHandler(), method);
            }
        };
        exceptionResolver.afterPropertiesSet();
        exceptionResolver.setMessageConverters(
                Collections.singletonList(new MappingJackson2HttpMessageConverter(new ObjectMapper()))
        );
        return exceptionResolver;
    }

}
