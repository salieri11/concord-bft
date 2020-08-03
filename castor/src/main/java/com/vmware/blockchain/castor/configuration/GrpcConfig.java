/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.configuration;

import java.io.File;
import java.io.IOException;
import java.net.URI;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;
import org.springframework.util.ResourceUtils;

import com.vmware.blockchain.castor.exception.CastorException;
import com.vmware.blockchain.castor.exception.ErrorCode;
import com.vmware.blockchain.deployment.v1.ProvisioningServiceV2Grpc;

import io.grpc.ManagedChannel;
import io.grpc.ManagedChannelBuilder;
import io.grpc.netty.shaded.io.grpc.netty.GrpcSslContexts;
import io.grpc.netty.shaded.io.grpc.netty.NettyChannelBuilder;

/**
 * {@link Configuration} declaring gRPC-related {@link org.springframework.stereotype.Component}s.
 */
@Configuration
public class GrpcConfig {
    private static Logger logger = LogManager.getLogger(GrpcConfig.class);
    private final Environment environment;

    private final String provisioningCertchainProp = "provisioning.service.certChain";

    @Autowired
    public GrpcConfig(Environment environment) {
        this.environment = environment;
    }

    @Autowired
    public ManagedChannel provisioningServerChannel;

    /**
     * gRPC {@link ManagedChannel} to use to communicate with provisioning server.
     *
     * @return a singleton {@link ManagedChannel} instance.
     */
    @Bean
    @Qualifier("provisioningServerChannel")
    public ManagedChannel provisioningServerChannel() throws IOException {
        ManagedChannelBuilder channelBuilder;

        var url = URI.create(
                environment.getProperty(
                        ServerEndpoints.PROVISIONING_SERVER.getKey(),
                        ServerEndpoints.PROVISIONING_SERVER.getDefaultUrl()
                )
        );

        if (ServerEndpoints.SCHEME_GRPC_TLS.equals(url.getScheme())) {
            String provisioningServiceCertChainProp = environment.getProperty(provisioningCertchainProp);
            if (provisioningServiceCertChainProp == null) {
                logger.error("grpc(s) is enabled but required property {} for grpc over TLS is not set",
                        provisioningCertchainProp);
                throw new CastorException(ErrorCode.BAD_GRPCS_CONFIGURATION, provisioningCertchainProp);
            }

            File provisioningServiceCertChain = ResourceUtils.getFile(provisioningServiceCertChainProp);
            channelBuilder = ManagedChannelBuilder.forTarget(url.getAuthority());
            ((NettyChannelBuilder) channelBuilder).useTransportSecurity()
                    .sslContext(GrpcSslContexts.forClient()
                                    .trustManager(provisioningServiceCertChain)
                                    .build());
        } else {
            channelBuilder = ManagedChannelBuilder.forTarget(url.getAuthority()).usePlaintext();
        }

        return channelBuilder.build();
    }

    /**
     * gRPC {@link ManagedChannel} to use to communicate with fleet management server.
     *
     * @return a singleton {@link ManagedChannel} instance.
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

    @Bean
    ProvisioningServiceV2Grpc.ProvisioningServiceV2Stub provisioningServiceStub() {
        return ProvisioningServiceV2Grpc.newStub(provisioningServerChannel);
    }

    @Bean
    ProvisioningServiceV2Grpc.ProvisioningServiceV2BlockingStub provisioningServiceBlockingStub() {
        return ProvisioningServiceV2Grpc.newBlockingStub(provisioningServerChannel);
    }
}
