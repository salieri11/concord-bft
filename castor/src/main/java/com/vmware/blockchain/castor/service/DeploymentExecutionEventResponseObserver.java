/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor.service;

import java.io.PrintWriter;
import java.util.Comparator;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import com.vmware.blockchain.deployment.v1.DeployedResource;
import com.vmware.blockchain.deployment.v1.DeploymentExecutionEvent;

import io.grpc.stub.StreamObserver;
import lombok.EqualsAndHashCode;
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

    @RequiredArgsConstructor
    @Getter
    @EqualsAndHashCode
    private static class ResourceId {
        private final String id;
        private final String name;
    }

    private final PrintWriter printWriter;
    private final String requestId;
    private final CompletableFuture<CastorDeploymentStatus> provisioningCompletionFuture;

    // Key: {Resource (node) id + Resource name}, Value: list of properties as they are reported in the callback
    // Each property in the list has a String name, e.g. VM_MEMORY, and a String value, e.g. 8
    private ConcurrentMap<ResourceId, ConcurrentMap<String, String>> resourceToPropertiesMap =
            new ConcurrentHashMap<>();

    private volatile boolean provisioningFailed;

    private void printAllResources() {

        Comparator<Map.Entry<ResourceId, ConcurrentMap<String, String>>> resourceIdComparator =
            Comparator.comparing(r -> {
                return r.getKey().getId() + r.getKey().getName();
            });

        resourceToPropertiesMap.entrySet().stream()
            .sorted(resourceIdComparator).forEach(entry -> {
                ResourceId resourceId = entry.getKey();
                String nodeId = resourceId.getId();
                String name = resourceId.getName();
                Map<String, String> cachedPropertiesMap = entry.getValue();
                cachedPropertiesMap.entrySet().stream()
                    .sorted(Comparator.comparing(Map.Entry::getKey))
                    .forEach(propertyEntry -> {
                        String propertyKey = propertyEntry.getKey();
                        String propertyValue = propertyEntry.getValue();
                        printWriter.printf("Node Id: %s, name: %s, key: %s, value: %s\n",
                                           nodeId, name, propertyKey, propertyValue);
                    });
            });
    }

    private void printResourceData(DeploymentExecutionEvent deploymentExecutionEvent) {
        DeploymentExecutionEvent.Type type = deploymentExecutionEvent.getType();
        switch (type) {
            case ACKNOWLEDGED:
                printWriter.printf("Blockchain Id: %s\n", deploymentExecutionEvent.getBlockchainId());
                break;
            case COMPLETED:
                printAllResources();
                printWriter.printf("Blockchain Id: %s, completion status: %s\n",
                                   deploymentExecutionEvent.getBlockchainId(),
                                   deploymentExecutionEvent.getStatus());
                break;
            case RESOURCE:
                DeployedResource resource = deploymentExecutionEvent.getResource();
                String nodeId = resource.getNodeId();
                String name = resource.getName();
                Map<String, String> callbackProperties = resource.getAdditionalInfo().getValuesMap();
                ResourceId resourceId = new ResourceId(nodeId, name);
                ConcurrentMap<String, String> cachedProperties =
                        resourceToPropertiesMap.computeIfAbsent(resourceId, k -> new ConcurrentHashMap<>());
                cachedProperties.putAll(callbackProperties);
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
