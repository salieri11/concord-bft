/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.node.health.daml;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.vmware.blockchain.agent.services.node.health.HealthStatusResponse;
import com.vmware.blockchain.agent.utils.AgentTestConfiguration;
import com.vmware.blockchain.grpc.health.v1.HealthCheckResponse;

/**
 * Test for {@link DamlHealthServiceInvoker}.
 */
@ExtendWith(SpringExtension.class)
@ContextConfiguration(classes = {AgentTestConfiguration.class})
@ComponentScan(basePackageClasses = { DamlHealthServiceInvokerTest.class})
public class DamlHealthServiceInvokerTest {

    @MockBean
    private DamlHealthServiceInvoker damlHealthServiceInvoker;

    @BeforeEach
    void init() throws Exception {
        when(damlHealthServiceInvoker.check()).thenCallRealMethod();
        when(damlHealthServiceInvoker.getHealthResponse()).thenCallRealMethod();
        when(damlHealthServiceInvoker.getServices()).thenReturn(List.of("unit", "test"));
        when(damlHealthServiceInvoker.getNodeTypeName()).thenReturn("myUnitTest");
        doNothing().when(damlHealthServiceInvoker).logMetrics(isA(Integer.class));
    }

    @Test
    void checkTest() throws Exception {
        when(damlHealthServiceInvoker.getResponse(any())).thenReturn(HealthCheckResponse.newBuilder()
                .setStatus(HealthCheckResponse.ServingStatus.SERVING).build());
        List<HealthCheckResponse.ServingStatus> statusList = damlHealthServiceInvoker.check().stream()
                .map(HealthCheckResponse::getStatus).collect(Collectors.toList());

        Assertions.assertTrue(statusList.contains(HealthCheckResponse.ServingStatus.SERVING));
        Assertions.assertEquals(Collections.frequency(statusList, HealthCheckResponse.ServingStatus.SERVING),
                statusList.size());
    }

    @Test
    void getHealthResponseTest() {
        when(damlHealthServiceInvoker.getResponse(any())).thenReturn(HealthCheckResponse.newBuilder()
                .setStatus(HealthCheckResponse.ServingStatus.SERVING).build());
        HealthStatusResponse response = damlHealthServiceInvoker.getHealthResponse();
        Assertions.assertEquals(HealthStatusResponse.builder()
                .status(HealthStatusResponse.HealthStatus.HEALTHY).build(), response);
        verify(damlHealthServiceInvoker, times(1)).logMetrics(1);
    }

    @Test
    void getHealthResponseNegative() {
        when(damlHealthServiceInvoker.getResponse(any())).thenReturn(HealthCheckResponse.newBuilder()
                .setStatus(HealthCheckResponse.ServingStatus.NOT_SERVING).build());
        HealthStatusResponse response = damlHealthServiceInvoker.getHealthResponse();
        Assertions.assertEquals(HealthStatusResponse.builder()
                .status(HealthStatusResponse.HealthStatus.UNHEALTHY).build(), response);
        verify(damlHealthServiceInvoker, times(1)).logMetrics(0);
    }

    @Test
    void getHealthResponseThrowsException() {
        String message = "unit test exception";
        Exception exception = new IllegalArgumentException(message);
        when(damlHealthServiceInvoker.getResponse(any())).thenThrow(exception);
        HealthStatusResponse response = damlHealthServiceInvoker.getHealthResponse();
        Assertions.assertEquals(HealthStatusResponse.builder()
                .status(HealthStatusResponse.HealthStatus.SERVICE_UNAVAILABLE)
                .exception(new IllegalArgumentException("java.lang.IllegalArgumentException: unit test exception")
                        .getLocalizedMessage()).build(), response);
        verify(damlHealthServiceInvoker, times(1)).logMetrics(-1);
    }
}