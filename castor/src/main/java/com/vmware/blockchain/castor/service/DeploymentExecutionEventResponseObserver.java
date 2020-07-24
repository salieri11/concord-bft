/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.service;

import java.util.Objects;
import java.util.concurrent.CompletableFuture;

import com.vmware.blockchain.castor.exception.CastorException;
import com.vmware.blockchain.castor.exception.ErrorCode;
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
    private final CompletableFuture<String> deploymentCompletionFuture;

    private volatile boolean deploymentFailed;

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
            deploymentFailed = true;
        }
    }

    /**
     * onError callback.
     * @param t The exception triggering this callback.
     */
    @Override
    public void onError(Throwable t) {
        log.error("Error in blockchain provisioning", t);
        deploymentCompletionFuture.completeExceptionally(t);
        throw new CastorException(ErrorCode.PROVISIONING_ERROR, t);
    }

    /**
     * onCompleted callback.
     */
    @Override
    public void onCompleted() {
        if (deploymentFailed) {
            log.info("Deployment with requestId: {} failed", requestId);
            deploymentCompletionFuture.complete("FAILED");
        } else {
            deploymentCompletionFuture.complete("SUCCESS");
            log.info("Deployment with requestId: {} succeeded", requestId);
        }
    }
}
