/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.grpc;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import io.grpc.ManagedChannel;
import io.grpc.ManagedChannelBuilder;

/**
 * GRPC config.  Creates a channel bean.
 */
@Configuration
public class GrpcConfig {
    private String deployServiceAddress;

    @Autowired
    public GrpcConfig(@Value("${deployment.service.address}") String deployServiceAddress) {
        this.deployServiceAddress = deployServiceAddress;
    }

    @Bean
    public ManagedChannel channel() {
        return ManagedChannelBuilder.forTarget(deployServiceAddress).usePlaintext().build();
    }

}
