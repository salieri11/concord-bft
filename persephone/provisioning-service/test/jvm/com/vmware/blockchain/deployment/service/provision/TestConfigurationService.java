/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/

package com.vmware.blockchain.deployment.service.provision;

import java.security.SecureRandom;

import org.jetbrains.annotations.NotNull;

import com.vmware.blockchain.deployment.model.ConfigurationServiceImplBase;
import com.vmware.blockchain.deployment.model.ConfigurationServiceRequest;
import com.vmware.blockchain.deployment.model.ConfigurationSessionIdentifier;

import io.grpc.stub.StreamObserver;

/**
 * Test instance for configuration service.
 */
public class TestConfigurationService  extends ConfigurationServiceImplBase {

    @Override
    public void createConfiguration(@NotNull ConfigurationServiceRequest request,
                                    @NotNull StreamObserver<ConfigurationSessionIdentifier> observer) {
        var testId = new ConfigurationSessionIdentifier(new SecureRandom().nextLong());
        observer.onNext(testId);
        observer.onCompleted();
    }
}
