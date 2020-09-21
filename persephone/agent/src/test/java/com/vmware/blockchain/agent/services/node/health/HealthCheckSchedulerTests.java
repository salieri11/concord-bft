/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.node.health;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.scheduling.TaskScheduler;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.util.ReflectionTestUtils;

import com.vmware.blockchain.agent.utils.AgentTestConfiguration;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;

import lombok.extern.slf4j.Slf4j;

/**
 * Test class for {@link HealthCheckScheduler}.
 */
@ExtendWith(SpringExtension.class)
@ContextConfiguration(classes = {AgentTestConfiguration.class})
@Slf4j
public class HealthCheckSchedulerTests {

    @Autowired
    ConcordModelSpecification.NodeType nodeType;

    @Autowired
    TaskScheduler taskScheduler;

    @MockBean
    NodeComponentHealthFactory nodeComponentHealthFactory;

    private final ComponentHealth mockComponentHealth = mock(ComponentHealth.class);
    private HealthCheckScheduler healthCheckScheduler;

    @BeforeEach
    void init() {
        when(nodeComponentHealthFactory.getHealthComponent(any())).thenReturn(mockComponentHealth);
        when(mockComponentHealth.getHealth()).thenReturn(HealthStatusResponse.builder()
                .status(HealthStatusResponse.HealthStatus.HEALTHY).build());

        healthCheckScheduler = new HealthCheckScheduler();
        ReflectionTestUtils.setField(healthCheckScheduler, "nodeType", nodeType);
        ReflectionTestUtils.setField(healthCheckScheduler,
                "nodeComponentHealthFactory", nodeComponentHealthFactory);
        ReflectionTestUtils.setField(healthCheckScheduler, "taskScheduler", taskScheduler);

        healthCheckScheduler.startHealthCheck();
    }

    @AfterEach
    void teardown() {
        try {
            healthCheckScheduler.stopHealthCheck();
        } catch (Exception ex) {
            log.warn("health check thread could not be stopped.");
        }
    }

    @Test
    void testAll() throws InterruptedException {
        // Collated to reduce UT run time
        Thread.sleep(70000);
        testStartHealthCheck();
        testRestartWithoutStopThrowsException();
        testStopHealthCheck();
        testStopAgainThrowsException();
        testRestartHealthCheck();
    }

    void testStartHealthCheck() {
        verify(mockComponentHealth, atLeastOnce()).getHealth();
    }

    void testStopHealthCheck() {
        verify(mockComponentHealth, atLeastOnce()).getHealth();
        healthCheckScheduler.stopHealthCheck();
        Assertions.assertTrue(healthCheckScheduler.getScheduledFuture().isCancelled());
    }

    void testRestartWithoutStopThrowsException() {
        Assertions.assertThrows(IllegalStateException.class, () -> healthCheckScheduler.startHealthCheck());
    }

    void testStopAgainThrowsException() {
        Assertions.assertThrows(IllegalStateException.class, () -> healthCheckScheduler.stopHealthCheck());
    }

    void testRestartHealthCheck() throws InterruptedException {
        Assertions.assertTrue(healthCheckScheduler.getScheduledFuture().isCancelled());
        healthCheckScheduler.startHealthCheck();
        Thread.sleep(70000);
        verify(mockComponentHealth, atLeastOnce()).getHealth();
    }

}
