/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.node.status;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.setup.SecurityMockMvcConfigurers.springSecurity;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Arrays;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.http.MediaType;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import com.github.dockerjava.api.command.InspectContainerResponse;
import com.vmware.blockchain.agent.services.AgentDockerClient;
import com.vmware.blockchain.agent.services.NodeStartupOrchestrator;
import com.vmware.blockchain.agent.services.configuration.BaseContainerSpec;
import com.vmware.blockchain.agent.utils.AgentTestConfiguration;
import com.vmware.blockchain.agent.utils.MvcTestSecurityConfig;

/**
 * Tests for NodeComponentController.
 */
@ExtendWith(SpringExtension.class)
@WebMvcTest(controllers = { NodeComponentController.class})
@ContextConfiguration(classes = {AgentTestConfiguration.class, MvcTestSecurityConfig.class})
@ComponentScan(basePackageClasses = { NodeComponentControllerTests.class})
public class NodeComponentControllerTests {

    @Autowired
    private WebApplicationContext context;

    private MockMvc mockMvc;
    private String jsonResponse;

    @MockBean
    private AgentDockerClient agentDockerClient;

    @MockBean
    private NodeStartupOrchestrator nodeStartupOrchestrator;

    @MockBean
    private NodeComponentHealthUtil nodeComponentHealthUtil;

    @BeforeEach
    void init() throws JsonProcessingException {
        mockMvc = MockMvcBuilders
                .webAppContextSetup(context)
                .apply(springSecurity())
                .build();
        var containerObj = mock(BaseContainerSpec.class);
        when(nodeStartupOrchestrator.getComponents()).thenReturn(Arrays.asList(containerObj));
        when(nodeComponentHealthUtil.getConcordHealth()).thenReturn(Boolean.TRUE);
        when(nodeComponentHealthUtil.getDamlHealth(any())).thenReturn(Boolean.TRUE);

        ObjectMapper objectMapper = new ObjectMapper();
        var response = NodeStatusResponse.builder().status("HEALTHY").build();
        jsonResponse = objectMapper.writeValueAsString(response);

    }

    @Test
    void startComponents() throws Exception {
        var res = mock(InspectContainerResponse.class);
        doReturn(res).when(agentDockerClient).inspectContainer(any(), any());
        doNothing().when(agentDockerClient).startComponent(any(), any(), any());

        MvcResult result = mockMvc.perform(post("/api/node/start")
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        verify(agentDockerClient, times(1)).inspectContainer(any(), any());
        verify(agentDockerClient, times(1)).startComponent(any(), any(), any());
    }

    @Test
    void stopComponents() throws Exception {
        var res = mock(InspectContainerResponse.class);
        doReturn(res).when(agentDockerClient).inspectContainer(any(), any());
        doNothing().when(agentDockerClient).stopComponent(any(), any(), any());
        MvcResult result = mockMvc.perform(post("/api/node/stop")
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        verify(agentDockerClient, times(1)).inspectContainer(any(), any());
        verify(agentDockerClient, times(1)).stopComponent(any(), any(), any());
    }

    @Test
    void getConcordHealth() throws Exception {
        MvcResult result = mockMvc.perform(get("/api/health/concord")
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(content().json(jsonResponse))
                .andReturn();
        verify(nodeComponentHealthUtil, times(1)).getConcordHealth();
    }

    @Test
    void getDamlHealth() throws Exception {
        MvcResult result = mockMvc.perform(get("/api/health/daml")
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(content().json(jsonResponse))
                .andReturn();
        verify(nodeComponentHealthUtil, times(1)).getDamlHealth(any());
    }
}