/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.service;

import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import org.lognet.springboot.grpc.GRpcService;

import com.vmware.blockchain.deployment.v1.DeploymentExecutionEvent;
import com.vmware.blockchain.deployment.v1.DeploymentRequest;
import com.vmware.blockchain.deployment.v1.DeploymentRequestResponse;
import com.vmware.blockchain.deployment.v1.DeprovisionDeploymentRequest;
import com.vmware.blockchain.deployment.v1.DeprovisionDeploymentResponse;
import com.vmware.blockchain.deployment.v1.ProvisioningServiceV2Grpc;
import com.vmware.blockchain.deployment.v1.StreamDeploymentSessionEventRequest;

import io.grpc.stub.StreamObserver;
import lombok.RequiredArgsConstructor;

/**
 * Implement test methods for Provisioning service's skeleton. This tests failure by
 * sending Status.FAILURE to the client callback.
 */
@GRpcService
@RequiredArgsConstructor
public class ProvisioningServiceFailureTestImpl extends ProvisioningServiceV2Grpc.ProvisioningServiceV2ImplBase {

    public static final UUID REQUEST_ID = UUID.randomUUID();
    private final CompletableFuture<CastorDeploymentStatus> completableFuture;

    @Override
    public void createDeployment(DeploymentRequest request,
                                 StreamObserver<DeploymentRequestResponse> responseObserver) {
        responseObserver.onNext(DeploymentRequestResponse.newBuilder()
                                        .setId(REQUEST_ID.toString())
                                        .build());
        responseObserver.onCompleted();
    }

    @Override
    public void streamDeploymentSessionEvents(StreamDeploymentSessionEventRequest request,
                                              StreamObserver<DeploymentExecutionEvent> responseObserver) {
        // Mark completed with failure
        responseObserver.onNext(DeploymentExecutionEvent.newBuilder()
                                        .setType(DeploymentExecutionEvent.Type.COMPLETED)
                                        .setStatus(DeploymentExecutionEvent.Status.FAILURE)
                                        .build()
        );
        responseObserver.onCompleted();
        completableFuture.complete(CastorDeploymentStatus.FAILURE);
    }

    @Override
    public void deprovisionDeployment(DeprovisionDeploymentRequest request,
                                      StreamObserver<DeprovisionDeploymentResponse> responseObserver) {
        responseObserver.onNext(DeprovisionDeploymentResponse.newBuilder().build());
        responseObserver.onCompleted();
    }
}
