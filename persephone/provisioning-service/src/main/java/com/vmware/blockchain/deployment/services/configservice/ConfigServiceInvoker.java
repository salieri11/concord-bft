/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.configservice;

import org.springframework.stereotype.Component;

import com.vmware.blockchain.deployment.v1.ConfigurationServiceGrpc;
import com.vmware.blockchain.deployment.v1.Endpoint;
import com.vmware.blockchain.deployment.v1.TransportSecurity;

import io.grpc.ManagedChannel;
import io.grpc.ManagedChannelBuilder;
import lombok.extern.slf4j.Slf4j;

/**
 * Wrapper class which hides the gRPC vs REST invocation of the API.
 */
@Slf4j
@Component
public class ConfigServiceInvoker {

    private final Endpoint endpoint;

    public ConfigServiceInvoker(Endpoint configServiceEndpoint) {
        this.endpoint = configServiceEndpoint;
    }

    /**
     * Generates CS stub.
     * @return stub
     */
    public ConfigurationServiceGrpc.ConfigurationServiceFutureStub generateConfigServiceStub() {
        ManagedChannel channel = null;
        if (endpoint.getTransportSecurity().getType()
            == TransportSecurity.Type.NONE) {
            channel = ManagedChannelBuilder
                    .forTarget(endpoint.getAddress())
                    .usePlaintext()
                    .build();
        } else {
            channel = ManagedChannelBuilder
                    .forTarget(endpoint.getAddress())
                    .build();
        }
        return ConfigurationServiceGrpc.newFutureStub(channel);
    }
}
