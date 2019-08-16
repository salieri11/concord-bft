/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.fleetmanagment;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.vmware.blockchain.deployment.v1.FleetManagementServiceStub;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteServiceStub;
import com.vmware.blockchain.deployment.v1.ProvisioningServiceStub;

import io.grpc.CallOptions;
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
    FleetManagementServiceStub fleetManagementServiceStub() {
        return new FleetManagementServiceStub(fleetManagementServerChannel, CallOptions.DEFAULT);
    }

    @Bean
    ProvisioningServiceStub provisioningServiceStub() {
        return new ProvisioningServiceStub(provisioningServerChannel, CallOptions.DEFAULT);
    }

    @Bean
    OrchestrationSiteServiceStub orchestrationSiteServiceStub() {
        return new OrchestrationSiteServiceStub(provisioningServerChannel, CallOptions.DEFAULT);
    }
}
