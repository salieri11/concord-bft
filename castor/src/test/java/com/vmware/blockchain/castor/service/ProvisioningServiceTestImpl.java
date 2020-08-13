/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.service;

import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import org.lognet.springboot.grpc.GRpcService;

import com.vmware.blockchain.deployment.v1.DeployedResource;
import com.vmware.blockchain.deployment.v1.DeploymentExecutionEvent;
import com.vmware.blockchain.deployment.v1.DeploymentRequest;
import com.vmware.blockchain.deployment.v1.DeploymentRequestResponse;
import com.vmware.blockchain.deployment.v1.DeprovisionDeploymentRequest;
import com.vmware.blockchain.deployment.v1.DeprovisionDeploymentResponse;
import com.vmware.blockchain.deployment.v1.Properties;
import com.vmware.blockchain.deployment.v1.ProvisioningServiceV2Grpc;
import com.vmware.blockchain.deployment.v1.StreamDeploymentSessionEventRequest;

import io.grpc.stub.StreamObserver;
import lombok.RequiredArgsConstructor;

/**
 * Implement test methods for Provisioning service's skeleton.
 */
@GRpcService
@RequiredArgsConstructor
public class ProvisioningServiceTestImpl extends ProvisioningServiceV2Grpc.ProvisioningServiceV2ImplBase {

    public static final UUID REQUEST_ID = UUID.randomUUID();
    public static final String BLOCKCHAIN_ID = UUID.randomUUID().toString();
    public static final String NODE_ID_1 = UUID.randomUUID().toString();
    public static final String PUBLIC_IP_1 = "10.11.12.13";
    public static final String NODE_ID_2 = UUID.randomUUID().toString();
    public static final String PUBLIC_IP_2 = "14.15.16.17";
    public static final String LOGIN_PASSWORD = "The rain in Spain stays mainly in the plain";

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

        Properties.Builder propertiesBuilder = Properties.newBuilder()
                .putValues(DeployedResource.DeployedResourcePropertyKey.PUBLIC_IP.toString(), PUBLIC_IP_1)
                .putValues(DeployedResource.DeployedResourcePropertyKey.NODE_LOGIN.toString(), LOGIN_PASSWORD);
        DeployedResource.Builder deployedResourceBuilder = DeployedResource.newBuilder()
                .setNodeId(NODE_ID_1)
                .setName(NODE_ID_1)
                .setAdditionalInfo(propertiesBuilder.build());
        responseObserver.onNext(DeploymentExecutionEvent.newBuilder()
                                        .setType(DeploymentExecutionEvent.Type.RESOURCE)
                                        .setResource(deployedResourceBuilder.build())
                                        .build());

        propertiesBuilder = Properties.newBuilder()
                .putValues(DeployedResource.DeployedResourcePropertyKey.PUBLIC_IP.toString(), PUBLIC_IP_2)
                .putValues(DeployedResource.DeployedResourcePropertyKey.NODE_LOGIN.toString(), LOGIN_PASSWORD);
        deployedResourceBuilder = DeployedResource.newBuilder()
                .setNodeId(NODE_ID_2)
                .setName(NODE_ID_2)
                .setAdditionalInfo(propertiesBuilder.build());
        responseObserver.onNext(DeploymentExecutionEvent.newBuilder()
                                        .setType(DeploymentExecutionEvent.Type.RESOURCE)
                                        .setResource(deployedResourceBuilder.build())
                                        .build());

        responseObserver.onNext(DeploymentExecutionEvent.newBuilder()
                                        .setType(DeploymentExecutionEvent.Type.COMPLETED)
                                        .setStatus(DeploymentExecutionEvent.Status.SUCCESS)
                                        .setBlockchainId(BLOCKCHAIN_ID)
                                        .build()
        );
        completableFuture.complete(CastorDeploymentStatus.SUCCESS);
        responseObserver.onCompleted();
    }

    @Override
    public void deprovisionDeployment(DeprovisionDeploymentRequest request,
                                      StreamObserver<DeprovisionDeploymentResponse> responseObserver) {
    }
}
