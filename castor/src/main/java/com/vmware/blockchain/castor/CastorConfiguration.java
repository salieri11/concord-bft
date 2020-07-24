/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

import com.vmware.blockchain.castor.configuration.GrpcConfig;
import com.vmware.blockchain.deployment.v1.ProvisioningServiceV2Grpc;

import io.grpc.ManagedChannel;
import lombok.RequiredArgsConstructor;

/**
 * Uber configuration class for Castor application.
 */
@RequiredArgsConstructor
@Configuration
@Import(GrpcConfig.class)
public class CastorConfiguration {

    private final ManagedChannel provisioningServerChannel;

    @Bean
    ProvisioningServiceV2Grpc.ProvisioningServiceV2Stub provisioningServiceStub() {
        return ProvisioningServiceV2Grpc.newStub(provisioningServerChannel);
    }

    @Bean
    ProvisioningServiceV2Grpc.ProvisioningServiceV2BlockingStub provisioningServiceBlockingStub() {
        return ProvisioningServiceV2Grpc.newBlockingStub(provisioningServerChannel);
    }
}
