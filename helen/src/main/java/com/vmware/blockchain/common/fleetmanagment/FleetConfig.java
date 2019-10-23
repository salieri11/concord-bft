/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.fleetmanagment;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.vmware.blockchain.deployment.v1.FleetManagementServiceGrpc;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteServiceGrpc;
import com.vmware.blockchain.deployment.v1.ProvisioningServiceGrpc;

import io.grpc.ManagedChannel;

/**
 * Configuration to create beans for FleetManagement calls.
 */
@Configuration
public class FleetConfig {

    private final ManagedChannel provisioningServerChannel;
    private final ManagedChannel fleetManagementServerChannel;

    @Autowired
    public FleetConfig(
            @Qualifier("provisioningServerChannel") ManagedChannel provisioningServerChannel,
            @Qualifier("fleetManagementServerChannel") ManagedChannel fleetManagementServerChannel
    ) {
        this.provisioningServerChannel = provisioningServerChannel;
        this.fleetManagementServerChannel = fleetManagementServerChannel;
    }

    @Bean
    FleetManagementServiceGrpc.FleetManagementServiceStub fleetManagementServiceStub() {
        return FleetManagementServiceGrpc.newStub(fleetManagementServerChannel);
    }

    @Bean
    ProvisioningServiceGrpc.ProvisioningServiceStub provisioningServiceStub() {
        return ProvisioningServiceGrpc.newStub(provisioningServerChannel);
    }

    @Bean
    OrchestrationSiteServiceGrpc.OrchestrationSiteServiceStub orchestrationSiteServiceStub() {
        return OrchestrationSiteServiceGrpc.newStub(provisioningServerChannel);
    }
}
