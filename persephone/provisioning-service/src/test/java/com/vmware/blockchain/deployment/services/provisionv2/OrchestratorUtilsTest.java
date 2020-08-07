/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.provisionv2;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.Assert;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.vmware.blockchain.deployment.services.orchestration.OrchestratorUtils;
import com.vmware.blockchain.deployment.services.orchestration.ipam.IpamClient;
import com.vmware.blockchain.deployment.v1.Address;

/**
 * Test for OrchestrationUtils class.
 */
public class OrchestratorUtilsTest {

    private static final String IP = "10.0.0.1";
    private static final String NO_IP = "";
    private static final int INT_IP = 167772161;
    private static final String ADDR_NAME = "my-test";

    IpamClient ipamClient = mock(IpamClient.class);

    Address address;

    @BeforeEach
    void setup() {
        address = Address.newBuilder().setName(ADDR_NAME).setValue(INT_IP).build();
        when(ipamClient.allocatedPrivateIp(ADDR_NAME)).thenReturn(address);
    }

    @Test
    void testGetAddressWhenIpGiven() {

        var actual = OrchestratorUtils.getAddress(ipamClient, ADDR_NAME, IP, ADDR_NAME);

        Assert.assertEquals(actual.getKey(), ADDR_NAME);
        Assert.assertEquals(actual.getValue(), IP);
    }

    @Test
    void testGetAddressWhenIpNotGiven() {

        var actual = OrchestratorUtils.getAddress(ipamClient, ADDR_NAME, NO_IP, ADDR_NAME);

        Assert.assertEquals(actual.getKey(), ADDR_NAME);
        Assert.assertEquals(actual.getValue(), IP);
    }
}
