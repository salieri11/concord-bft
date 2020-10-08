/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.configservice;

import com.vmware.blockchain.deployment.services.orchestration.OrchestratorUtils;
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
public class ConfigServiceInvoker {

    private final Endpoint endpoint;
    private String path;

    public ConfigServiceInvoker(Endpoint configServiceEndpoint, String pathToCerts) {
        this.endpoint = configServiceEndpoint;
        this.path = pathToCerts;
    }

    /**
     * Generates CS stub.
     * @return stub
     */
    public ConfigurationServiceGrpc.ConfigurationServiceStub generateConfigServiceStub() {
        ManagedChannel channel = null;
        if (endpoint.getTransportSecurity().getType()
            == TransportSecurity.Type.NONE) {
            channel = ManagedChannelBuilder
                    .forTarget(endpoint.getAddress())
                    .usePlaintext()
                    .build();
        } else {
            channel = OrchestratorUtils.getSecureManagedChanel(endpoint.getAddress(), path);
        }
        return ConfigurationServiceGrpc.newStub(channel);
    }

    /**
     * Generates CS Future stub.
     * @return stub
     */
    public ConfigurationServiceGrpc.ConfigurationServiceFutureStub generateConfigServiceFutureStub() {
        ManagedChannel channel = null;
        if (endpoint.getTransportSecurity().getType()
            == TransportSecurity.Type.NONE) {
            channel = ManagedChannelBuilder
                    .forTarget(endpoint.getAddress())
                    .usePlaintext()
                    .build();
        } else {
            channel = OrchestratorUtils.getSecureManagedChanel(endpoint.getAddress(), path);
        }
        return ConfigurationServiceGrpc.newFutureStub(channel);
    }
}
