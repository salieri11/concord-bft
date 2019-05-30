/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.provision.deletedeployment;

import java.net.URI;
import java.util.concurrent.CompletableFuture;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.vmware.blockchain.deployment.model.ProvisionedResource;
import com.vmware.blockchain.deployment.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.reactive.ReactiveStream;

class DeleteResource {
    private static Logger log = LoggerFactory.getLogger(DeleteResource.class);

    private URI uri;
    private Orchestrator orchestrator;
    private ProvisionedResource.Type type;

    /**
     * Class responsible for deleting each resource element.
     * @param uri resource URI
     * @param orchestrator ProvisionService Orchestrator object
     * @param type ProvisionResource type
     */
    DeleteResource(URI uri, Orchestrator orchestrator, ProvisionedResource.Type type) {
        this.uri = uri;
        this.orchestrator = orchestrator;
        this.type = type;
    }

    /**
     * delete operation on an URI, given the  resource type.
     * @return CompletableFuture for delete action
     */
    CompletableFuture<? extends Orchestrator.OrchestrationEvent> delete() {
        if (this.type.equals(ProvisionedResource.Type.NETWORK_ALLOCATION)) {
            return deleteNetworkAllocation(uri).exceptionally(error -> {
                log.error("NAT deletion error", error);
                return null;
            });
        } else if (this.type.equals(ProvisionedResource.Type.NETWORK_RESOURCE)) {
            return deleteNetworkAddress(uri).exceptionally(error -> {
                log.error("Public IP release error", error);
                return null;
            });
        } else if (this.type.equals(ProvisionedResource.Type.COMPUTE_RESOURCE)) {
            return deleteDeployment(uri).exceptionally(error -> {
                log.error("VM Deletion error", error);
                return null;
            });
        } else {
            log.error("Incorrect resource type or not implemented: " + this.type.toString());
            return null;
        }
    }

    private CompletableFuture<? extends Orchestrator.OrchestrationEvent> deleteNetworkAllocation(
        URI resourceUri
    ) {
        Orchestrator.DeleteNetworkAllocationRequest networkDeallocReq =
                new Orchestrator.DeleteNetworkAllocationRequest(resourceUri);
        var deleteNetworkPublisher = orchestrator.deleteNetworkAllocation(networkDeallocReq);

        return ReactiveStream.toFuture(deleteNetworkPublisher);
    }

    private CompletableFuture<? extends Orchestrator.OrchestrationEvent> deleteDeployment(
            URI resourceUri
    ) {
        Orchestrator.DeleteComputeResourceRequest delComputeResReq =
                new Orchestrator.DeleteComputeResourceRequest(resourceUri);
        var deleteDeploymentPublisher = orchestrator.deleteDeployment(delComputeResReq);

        return ReactiveStream.toFuture(deleteDeploymentPublisher);
    }

    private CompletableFuture<? extends Orchestrator.OrchestrationEvent> deleteNetworkAddress(
            URI resourceUri
    ) {
        Orchestrator.DeleteNetworkResourceRequest delNetworkResReq =
                new Orchestrator.DeleteNetworkResourceRequest(resourceUri);
        var deleteNetworkAddrPublisher = orchestrator.deleteNetworkAddress(delNetworkResReq);

        return ReactiveStream.toFuture(deleteNetworkAddrPublisher);
    }

}
