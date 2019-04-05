/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.service.provision;

import java.net.URI;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ForkJoinPool;

import com.vmware.blockchain.deployment.model.orchestration.OrchestrationSiteInfo;
import com.vmware.blockchain.deployment.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.reactive.IteratingPublisher;
import kotlinx.coroutines.ExecutorsKt;
import org.reactivestreams.Publisher;

class TestOrchestrator implements Orchestrator {

    private final OrchestrationSiteInfo site;

    TestOrchestrator(OrchestrationSiteInfo site) {
        this.site = site;
    }

    @Override
    public void close() {
    }

    @Override
    public Publisher<ComputeResourceEvent> createDeployment(CreateComputeResourceRequest request) {
        var uri = URI.create("concord://" + UUID.randomUUID());
        var events = List.of(
                new ComputeResourceEvent.Created(uri, request.getNode()),
                new ComputeResourceEvent.Started(uri)
        );
        return new IteratingPublisher<>(events, ExecutorsKt.from(ForkJoinPool.commonPool()));
    }

    @Override
    public Publisher<ComputeResourceEvent> deleteDeployment(DeleteComputeResourceRequest request) {
        var events = Collections.<ComputeResourceEvent>emptyList();
        return new IteratingPublisher<>(events, ExecutorsKt.from(ForkJoinPool.commonPool()));
    }

    @Override
    public Publisher<NetworkResourceEvent> createNetworkAddress(
            CreateNetworkResourceRequest request
    ) {
        var events = Collections.<NetworkResourceEvent>emptyList();
        return new IteratingPublisher<>(events, ExecutorsKt.from(ForkJoinPool.commonPool()));
    }

    @Override
    public Publisher<NetworkResourceEvent> deleteNetworkAddress(
            DeleteNetworkResourceRequest request
    ) {
        var events = Collections.<NetworkResourceEvent>emptyList();
        return new IteratingPublisher<>(events, ExecutorsKt.from(ForkJoinPool.commonPool()));
    }

    @Override
    public Publisher<NetworkAllocationEvent> createNetworkAllocation(
            NetworkAllocationRequest request
    ) {
        var events = Collections.<NetworkAllocationEvent>emptyList();
        return new IteratingPublisher<>(events, ExecutorsKt.from(ForkJoinPool.commonPool()));
    }

    @Override
    public Publisher<NetworkAllocationEvent> deleteNetworkAllocation(
            NetworkAllocationRequest request
    ) {
        var events = Collections.<NetworkAllocationEvent>emptyList();
        return new IteratingPublisher<>(events, ExecutorsKt.from(ForkJoinPool.commonPool()));
    }
}
