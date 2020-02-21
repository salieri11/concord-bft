/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.provision;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.vmware.blockchain.deployment.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.reactive.ReactiveStream;

/**
 * Class implementation for deleting resources of a cluster.
 */
public class DeleteResource {
    private static Logger log = LoggerFactory.getLogger(DeleteResource.class);

    /**
     * Delete Network Allocation.
     */
    public static CompletableFuture<List<Orchestrator.NetworkAllocationEvent>> deleteNetworkAllocations(
        List<Map.Entry<Orchestrator, URI>> networkAllocList
    ) {
        List<CompletableFuture<Orchestrator.NetworkAllocationEvent>> works = new ArrayList<>();
        networkAllocList.stream().forEach(entry -> {
            var orchestrator = entry.getKey();
            var resourceUri = entry.getValue();
            Orchestrator.DeleteNetworkAllocationRequest networkDeallocReq =
                    new Orchestrator.DeleteNetworkAllocationRequest(resourceUri);
            var deleteNetworkPublisher = orchestrator.deleteNetworkAllocation(networkDeallocReq);
            works.add(ReactiveStream.toFuture(deleteNetworkPublisher));
        });

        return CompletableFuture.allOf(works.toArray(new CompletableFuture[works.size()]))
                .thenApply(res -> works.stream().map(CompletableFuture::join).collect(Collectors.toList()));
    }

    /**
     * Delete Deployment.
     */
    public static CompletableFuture<List<Orchestrator.ComputeResourceEvent>> deleteDeployments(
            List<Map.Entry<Orchestrator, URI>> computeList
    ) {
        List<CompletableFuture<Orchestrator.ComputeResourceEvent>> works = new ArrayList<>();
        computeList.stream().forEach(entry -> {
            var orchestrator = entry.getKey();
            var resourceUri = entry.getValue();
            Orchestrator.DeleteComputeResourceRequest delComputeResReq =
                    new Orchestrator.DeleteComputeResourceRequest(resourceUri);
            var deleteDeploymentPublisher = orchestrator.deleteDeployment(delComputeResReq);
            works.add(ReactiveStream.toFuture(deleteDeploymentPublisher));
        });

        return CompletableFuture.allOf(works.toArray(new CompletableFuture[works.size()]))
                .thenApply(res -> works.stream().map(CompletableFuture::join).collect(Collectors.toList()));
    }

    /**
     * Delete Network Address.
     */
    public static CompletableFuture<List<Orchestrator.NetworkResourceEvent>> deleteNetworkAddresses(
            List<Map.Entry<Orchestrator, URI>> networkAddrList
    ) {
        List<CompletableFuture<Orchestrator.NetworkResourceEvent>> works = new ArrayList<>();
        networkAddrList.stream().forEach(entry -> {
            var orchestrator = entry.getKey();
            var resourceUri = entry.getValue();
            Orchestrator.DeleteNetworkResourceRequest delNetworkResReq =
                    new Orchestrator.DeleteNetworkResourceRequest(resourceUri);
            var deleteNetworkAddrPublisher = orchestrator.deleteNetworkAddress(delNetworkResReq);
            works.add(ReactiveStream.toFuture(deleteNetworkAddrPublisher));
        });

        return CompletableFuture.allOf(works.toArray(new CompletableFuture[works.size()]))
                 .thenApply(res -> works.stream().map(CompletableFuture::join).collect(Collectors.toList()));
    }
}
