/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.service;

import java.io.PrintWriter;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;

import com.vmware.blockchain.deployment.v1.DeployedResource;
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
    private final PrintWriter printWriter;
    private final String requestId;
    private final CompletableFuture<CastorDeploymentStatus> provisioningCompletionFuture;

    private volatile boolean provisioningFailed;

    private void printResourceData(DeploymentExecutionEvent deploymentExecutionEvent) {
        DeploymentExecutionEvent.Type type = deploymentExecutionEvent.getType();
        switch (type) {
            case ACKNOWLEDGED:
                printWriter.printf("Blockchain Id: %s\n", deploymentExecutionEvent.getBlockchainId());
                break;
            case COMPLETED:
                printWriter.printf("Blockchain Id: %s, completion status: %s\n",
                                   deploymentExecutionEvent.getBlockchainId(),
                                   deploymentExecutionEvent.getStatus());
                break;
            case RESOURCE:
                DeployedResource resource = deploymentExecutionEvent.getResource();
                String nodeId = resource.getNodeId();
                String name = resource.getName();
                Map<String, String> values = resource.getAdditionalInfo().getValuesMap();
                values.entrySet().forEach(e -> {
                    printWriter.printf("Node Id: %s, name: %s, key: %s, value: %s\n", nodeId, name,
                                       e.getKey(), e.getValue());
                });
                break;
            default:
                break;
        }
    }

    /**
     * onNext callback.
     * @param deploymentExecutionEvent the event triggering this callback.
     */
    @Override
    public void onNext(DeploymentExecutionEvent deploymentExecutionEvent) {
        log.info("onNext event received for requestId: {}, event: {}", requestId, deploymentExecutionEvent);
        printResourceData(deploymentExecutionEvent);
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
        printWriter.printf("RequestId: %s finished with error: %s\n", requestId, t.getMessage());
        provisioningCompletionFuture.completeExceptionally(t);
    }

    /**
     * onCompleted callback.
     */
    @Override
    public void onCompleted() {
        if (provisioningFailed) {
            log.info("Deployment with requestId: {} failed", requestId);
            printWriter.printf("RequestId: %s failed\n", requestId);
            provisioningCompletionFuture.complete(CastorDeploymentStatus.FAILURE);
        } else {
            log.info("Deployment with requestId: {} succeeded", requestId);
            printWriter.printf("RequestId: %s succeeded\n", requestId);
            provisioningCompletionFuture.complete(CastorDeploymentStatus.SUCCESS);
        }
    }
}
