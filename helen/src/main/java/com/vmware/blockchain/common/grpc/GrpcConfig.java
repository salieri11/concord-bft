/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.grpc;

import java.net.URI;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;

import io.grpc.ManagedChannel;
import io.grpc.ManagedChannelBuilder;

/**
 * {@link Configuration} declaring gRPC-related {@link org.springframework.stereotype.Component}s.
 */
@Configuration
public class GrpcConfig {

    private final Environment environment;

    @Autowired
    public GrpcConfig(Environment environment) {
        this.environment = environment;
    }

    /**
     * gRPC {@link ManagedChannel} to use to communicate with provisioning server.
     *
     * @return
     *   a singleton {@link ManagedChannel} instance.
     */
    @Bean
    @Qualifier("provisioningServerChannel")
    public ManagedChannel provisioningServerChannel() {
        var url = URI.create(
                environment.getProperty(
                        ServerEndpoints.PROVISIONING_SERVER.getKey(),
                        ServerEndpoints.PROVISIONING_SERVER.getDefaultUrl()
                )
        );

        if (ServerEndpoints.SCHEME_GRPC_TLS.equals(url.getScheme())) {
            return ManagedChannelBuilder.forTarget(url.getAuthority()).build();
        } else {
            return ManagedChannelBuilder.forTarget(url.getAuthority()).usePlaintext().build();
        }
    }

    /**
     * gRPC {@link ManagedChannel} to use to communicate with fleet management server.
     *
     * @return
     *   a singleton {@link ManagedChannel} instance.
     */
    @Bean
    @Qualifier("fleetManagementServerChannel")
    public ManagedChannel fleetManagementServerChannel() {
        var url = URI.create(
                environment.getProperty(
                        ServerEndpoints.FLEET_MANAGEMENT_SERVER.getKey(),
                        ServerEndpoints.FLEET_MANAGEMENT_SERVER.getDefaultUrl()
                )
        );

        if (ServerEndpoints.SCHEME_GRPC_TLS.equals(url.getScheme())) {
            return ManagedChannelBuilder.forTarget(url.getAuthority()).build();
        } else {
            return ManagedChannelBuilder.forTarget(url.getAuthority()).usePlaintext().build();
        }
    }
}
