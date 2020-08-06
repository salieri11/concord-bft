/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.service;

import java.util.Objects;
import java.util.concurrent.CompletableFuture;

import com.vmware.blockchain.deployment.v1.DeploymentExecutionEvent;

import io.grpc.stub.StreamObserver;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

/**
 * Callbacks for streaming gRPC from the Provisioning service.
 */
@Log4j2
@Getter
@RequiredArgsConstructor
public class DeploymentExecutionEventResponseObserver implements StreamObserver<DeploymentExecutionEvent> {
    private final String requestId;
    private final CompletableFuture<CastorDeploymentStatus> provisioningCompletionFuture;

    private volatile boolean provisioningFailed;

    /**
     * onNext callback.
     * @param deploymentExecutionEvent the event triggering this callback.
     */
    @Override
    public void onNext(DeploymentExecutionEvent deploymentExecutionEvent) {
        log.info("onNext event received for requestId: {}, event: {}", requestId, deploymentExecutionEvent);
        DeploymentExecutionEvent.Type type = deploymentExecutionEvent.getType();
        DeploymentExecutionEvent.Status status = deploymentExecutionEvent.getStatus();
        if (Objects.equals(DeploymentExecutionEvent.Type.COMPLETED, type)
            && Objects.equals(DeploymentExecutionEvent.Status.FAILURE, status)) {
            provisioningFailed = true;
        }
    }

    /**
     * onError callback.
     * @param t The exception triggering this callback.
     */
    @Override
    public void onError(Throwable t) {
        log.error("Error in blockchain provisioning", t);
        provisioningCompletionFuture.completeExceptionally(t);
    }

    /**
     * onCompleted callback.
     */
    @Override
    public void onCompleted() {
        if (provisioningFailed) {
            log.info("Deployment with requestId: {} failed", requestId);
            provisioningCompletionFuture.complete(CastorDeploymentStatus.FAILURE);
        } else {
            provisioningCompletionFuture.complete(CastorDeploymentStatus.SUCCESS);
            log.info("Deployment with requestId: {} succeeded", requestId);
        }
    }
}
