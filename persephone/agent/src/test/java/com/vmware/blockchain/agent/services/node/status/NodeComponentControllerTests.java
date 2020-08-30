/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.node.status;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
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
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import com.github.dockerjava.api.command.InspectContainerResponse;
import com.vmware.blockchain.agent.services.AgentDockerClient;
import com.vmware.blockchain.agent.services.NodeStartupOrchestrator;
import com.vmware.blockchain.agent.services.configuration.BaseContainerSpec;
import com.vmware.blockchain.agent.services.node.health.ComponentHealth;
import com.vmware.blockchain.agent.services.node.health.HealthCheckScheduler;
import com.vmware.blockchain.agent.services.node.health.HealthStatusResponse;
import com.vmware.blockchain.agent.services.node.health.NodeComponentHealthFactory;
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
    private String jsonException;
    private final ComponentHealth mockComponentHealth = mock(ComponentHealth.class);

    @MockBean
    private AgentDockerClient agentDockerClient;

    @MockBean
    private NodeStartupOrchestrator nodeStartupOrchestrator;

    @MockBean
    private NodeComponentHealthFactory nodeComponentHealthFactory;

    @MockBean
    private HealthCheckScheduler healthCheckScheduler;

    private final HealthStatusResponse response = HealthStatusResponse.builder()
            .status(HealthStatusResponse.HealthStatus.HEALTHY)
            .build();
    private final Exception exception = new IllegalArgumentException("Exception in unit test");
    private final HealthStatusResponse exceptionResponse = HealthStatusResponse.builder()
            .exception(exception.getLocalizedMessage()).build();

    @BeforeEach
    void init() throws JsonProcessingException {
        mockMvc = MockMvcBuilders
                .webAppContextSetup(context)
                .apply(springSecurity())
                .build();
        var containerObj = mock(BaseContainerSpec.class);
        when(nodeStartupOrchestrator.getComponents()).thenReturn(Arrays.asList(containerObj));

        ObjectMapper objectMapper = new ObjectMapper();
        jsonResponse = objectMapper.writeValueAsString(response);
        jsonException = objectMapper.writeValueAsString(exceptionResponse);
    }

    @Test
    void startComponents() throws Exception {
        var res = mock(InspectContainerResponse.class);
        doReturn(res).when(agentDockerClient).inspectContainer(any(), any());
        doNothing().when(agentDockerClient).startComponent(any(), any(), any());

        mockMvc.perform(post("/api/node/start")
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
        mockMvc.perform(post("/api/node/stop")
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        verify(agentDockerClient, times(1)).inspectContainer(any(), any());
        verify(agentDockerClient, times(1)).stopComponent(any(), any(), any());
    }

    @Test
    void getConcordHealth() throws Exception {
        when(nodeComponentHealthFactory.getHealthComponent(any())).thenReturn(mockComponentHealth);
        when(mockComponentHealth.getHealth()).thenReturn(response);

        mockMvc.perform(get("/api/health/concord")
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(content().json(jsonResponse))
                .andReturn();
        verify(mockComponentHealth, times(1)).getHealth();
    }

    @Test
    void getDamlHealth() throws Exception {
        when(nodeComponentHealthFactory.getHealthComponent(any())).thenReturn(mockComponentHealth);
        when(mockComponentHealth.getHealth()).thenReturn(response);

        mockMvc.perform(get("/api/health/daml")
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(content().json(jsonResponse))
                .andReturn();
        verify(mockComponentHealth, times(1)).getHealth();
    }

    @Test
    void getConcordHealthException() throws Exception {
        when(nodeComponentHealthFactory.getHealthComponent(any())).thenThrow(exception);
        mockMvc.perform(get("/api/health/concord")
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is5xxServerError())
                .andExpect(content().json(jsonException))
                .andReturn();
        verify(nodeComponentHealthFactory, times(1)).getHealthComponent(any());
    }

    @Test
    void getDamlHealthException() throws Exception {
        when(nodeComponentHealthFactory.getHealthComponent(any())).thenThrow(exception);
        mockMvc.perform(get("/api/health/daml")
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is5xxServerError())
                .andExpect(content().json(jsonException))
                .andReturn();
        verify(nodeComponentHealthFactory, times(1)).getHealthComponent(any());
    }

    @Test
    void stopHealthCheck() throws Exception {
        doNothing().when(healthCheckScheduler).stopHealthCheck();

        mockMvc.perform(post("/api/health/stop")
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        verify(healthCheckScheduler, times(1)).stopHealthCheck();
    }

    @Test
    void stopHealthCheckException() throws Exception {
        doThrow(new IllegalStateException("Exception from unit test")).when(healthCheckScheduler).stopHealthCheck();

        mockMvc.perform(post("/api/health/stop")
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is5xxServerError()).andReturn();
        verify(healthCheckScheduler, times(1)).stopHealthCheck();
    }

    @Test
    void startHealthCheck() throws Exception {
        doNothing().when(healthCheckScheduler).startHealthCheck();

        mockMvc.perform(post("/api/health/start")
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        verify(healthCheckScheduler, times(1)).startHealthCheck();
    }

    @Test
    void startHealthCheckException() throws Exception {
        doThrow(new IllegalStateException("Exception from unit test")).when(healthCheckScheduler).startHealthCheck();

        mockMvc.perform(post("/api/health/start")
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is5xxServerError()).andReturn();
        verify(healthCheckScheduler, times(1)).startHealthCheck();
    }
}