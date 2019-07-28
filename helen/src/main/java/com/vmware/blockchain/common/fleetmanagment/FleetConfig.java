/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.fleetmanagment;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.vmware.blockchain.deployment.model.FleetManagementServiceStub;
import com.vmware.blockchain.deployment.model.OrchestrationSiteServiceStub;
import com.vmware.blockchain.deployment.model.ProvisioningServiceStub;

import io.grpc.CallOptions;
import io.grpc.ManagedChannel;

/**
 * Configuration to create beans for FleetManagment calls.
 */
@Configuration
public class FleetConfig {

    private ManagedChannel channel;

    @Autowired
    public FleetConfig(ManagedChannel channel) {
        this.channel = channel;
    }

    @Bean
    FleetManagementServiceStub fleetManagementServiceStub() {
        return new FleetManagementServiceStub(channel, CallOptions.DEFAULT);
    }

    @Bean
    ProvisioningServiceStub provisioningServiceStub() {
        return new ProvisioningServiceStub(channel, CallOptions.DEFAULT);
    }

    @Bean
    OrchestrationSiteServiceStub orchestrationSiteServiceStub() {
        return new OrchestrationSiteServiceStub(channel, CallOptions.DEFAULT);
    }
}
