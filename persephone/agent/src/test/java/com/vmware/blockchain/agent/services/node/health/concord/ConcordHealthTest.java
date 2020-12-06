/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services.node.health.concord;

import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.vmware.blockchain.agent.services.node.health.HealthStatusResponse;

/**
 * Test for {@link com.vmware.blockchain.agent.services.node.health.concord.ConcordHealth}.
 */
public class ConcordHealthTest {
    private final ConcordHealthServiceInvoker concordHealthServiceInvoker = mock(ConcordHealthServiceInvoker.class);

    @Test
    void getHealthResponse() {
        when(concordHealthServiceInvoker.getConcordHealth()).thenReturn(
                HealthStatusResponse.builder().status(HealthStatusResponse.HealthStatus.UNHEALTHY).build());
        ConcordHealth concordHealth = new ConcordHealth(concordHealthServiceInvoker);
        HealthStatusResponse response = concordHealth.getHealth();
        Assertions.assertEquals(HealthStatusResponse.builder()
                                        .status(HealthStatusResponse.HealthStatus.UNHEALTHY).build(), response);
    }

    @Test
    void getHealthResponseThrowsException() {
        String message = "Exception from unit test";
        doThrow(new IllegalStateException(message)).when(concordHealthServiceInvoker).getConcordHealth();
        ConcordHealth concordHealth = new ConcordHealth(concordHealthServiceInvoker);
        HealthStatusResponse response = concordHealth.getHealth();
        Assertions.assertEquals(HealthStatusResponse.builder()
                                        .status(HealthStatusResponse.HealthStatus.SERVICE_UNAVAILABLE)
                                        .exception(new IllegalArgumentException("Exception from unit test")
                                                           .getLocalizedMessage()).build(), response);
    }
}