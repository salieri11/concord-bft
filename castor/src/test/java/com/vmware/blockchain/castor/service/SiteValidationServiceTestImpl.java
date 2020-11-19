/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.service;

import java.util.concurrent.CompletableFuture;

import org.lognet.springboot.grpc.GRpcService;

import com.vmware.blockchain.deployment.v1.OrchestrationSiteServiceGrpc;
import com.vmware.blockchain.deployment.v1.ValidateOrchestrationSiteRequest;
import com.vmware.blockchain.deployment.v1.ValidateOrchestrationSiteResponse;

import io.grpc.stub.StreamObserver;
import lombok.RequiredArgsConstructor;

/**
 * Implement test methods for Site Validation service's skeleton.
 */
@GRpcService
@RequiredArgsConstructor
public class SiteValidationServiceTestImpl extends OrchestrationSiteServiceGrpc.OrchestrationSiteServiceImplBase {

    private final boolean successFlag;
    private final CompletableFuture<CastorDeploymentStatus> completableFuture;

    @Override
    public void validateOrchestrationSite(ValidateOrchestrationSiteRequest request,
                                          StreamObserver<ValidateOrchestrationSiteResponse> responseObserver) {
        if (successFlag) {
            responseObserver.onNext(ValidateOrchestrationSiteResponse.newBuilder().build());
            responseObserver.onCompleted();
        } else {
            throw new RuntimeException("FAILED");
        }
    }
}
