/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.node.health.daml;

import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.vmware.blockchain.agent.services.node.health.HealthStatusResponse;

/**
 * Test for {@link DamlHealth}.
 */
public class DamlHealthTest {
    private final DamlHealthServiceInvoker damlHealthServiceInvoker = mock(DamlHealthServiceInvoker.class);

    @Test
    void getHealthResponse() {
        when(damlHealthServiceInvoker.getHealthResponse()).thenReturn(
                HealthStatusResponse.builder().status(HealthStatusResponse.HealthStatus.UNHEALTHY).build());
        DamlHealth damlHealth = new DamlHealth(damlHealthServiceInvoker);
        HealthStatusResponse response = damlHealth.getHealth();
        Assertions.assertEquals(HealthStatusResponse.builder()
                                        .status(HealthStatusResponse.HealthStatus.UNHEALTHY).build(), response);
    }

    @Test
    void getHealthResponseThrowsException() {
        String message = "Exception from unit test";
        doThrow(new IllegalStateException(message)).when(damlHealthServiceInvoker).getHealthResponse();
        DamlHealth damlHealth = new DamlHealth(damlHealthServiceInvoker);
        HealthStatusResponse response = damlHealth.getHealth();
        Assertions.assertEquals(HealthStatusResponse.builder()
                                        .status(HealthStatusResponse.HealthStatus.SERVICE_UNAVAILABLE)
                                        .exception(new IllegalArgumentException("Exception from unit test")
                                         .getLocalizedMessage()).build(), response);
    }
}