/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.server.exceptions;

import javax.validation.constraints.NotNull;

import com.vmware.blockchain.deployment.v1.ConfigurationServiceGrpc;
import com.vmware.blockchain.deployment.v1.ConfigurationServiceRequestV2;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;

import io.grpc.stub.StreamObserver;

/**
 * Implements test methods for Config service's skeleton.
 */
public class ConfigServiceFailureTestImpl extends ConfigurationServiceGrpc.ConfigurationServiceImplBase {

    public static final String ERR_MSG_BFT_NO_CONFIG = "Test exception - BFT config error";

    /**
     * The method imitates a failure during configuration retrieval in a gRPC call.
     * */
    @Override
    public void createConfigurationV2(@NotNull ConfigurationServiceRequestV2 request,
                                      @NotNull StreamObserver<ConfigurationSessionIdentifier> observer) {
        throw new ConfigServiceException(ErrorCode.BFT_CONFIGURATION_FAILURE, ERR_MSG_BFT_NO_CONFIG,
                                                                           new RuntimeException(ERR_MSG_BFT_NO_CONFIG));
    }
}
