/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/

package com.vmware.blockchain.services.provision;

import java.security.SecureRandom;
import java.util.Objects;

import com.vmware.blockchain.deployment.v1.ConfigurationServiceGrpc;
import com.vmware.blockchain.deployment.v1.ConfigurationServiceRequest;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;

import io.grpc.stub.StreamObserver;

/**
 * Test instance for configuration service.
 */
public class TestConfigurationService extends ConfigurationServiceGrpc.ConfigurationServiceImplBase {

    @Override
    public void createConfiguration(ConfigurationServiceRequest message,
                                    StreamObserver<ConfigurationSessionIdentifier> observer) {

        var request = Objects.requireNonNull(message);
        var response = Objects.requireNonNull(observer);
        var testId = ConfigurationSessionIdentifier.newBuilder().setIdentifier(new SecureRandom().nextLong()).build();
        response.onNext(testId);
        response.onCompleted();
    }
}
