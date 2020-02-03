/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/

package com.vmware.blockchain.deployment.service.provision;

import java.net.URI;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ForkJoinPool;

import org.reactivestreams.Publisher;

import com.vmware.blockchain.deployment.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.reactive.ErrorPublisher;
import com.vmware.blockchain.deployment.reactive.IteratingPublisher;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo;

import kotlinx.coroutines.Dispatchers;
import kotlinx.coroutines.ExecutorsKt;

/**
 * Implementation of {@link Orchestrator} that allocates virtual resources and retains book-keeping
 * in memory.
 */
class BookKeepingStubOrchestrator implements Orchestrator {

    private final OrchestrationSiteInfo site;
    private final OrchestrationSiteValidator validator;
    private final Map<URI, CreateComputeResourceRequest> computes = new ConcurrentHashMap<>();
    private final Map<URI, CreateNetworkResourceRequest> networks = new ConcurrentHashMap<>();
    private final Map<URI, CreateNetworkAllocationRequest> allocations = new ConcurrentHashMap<>();

    BookKeepingStubOrchestrator(
            OrchestrationSiteInfo site,
            OrchestrationSiteValidator validator
    ) {
        this.site = site;
        this.validator = validator;
    }

    @Override
    public Publisher<Object> initialize() {
        return new IteratingPublisher<>(Collections.emptyList(), Dispatchers.getDefault());
    }

    @Override
    public void close() {
    }

    @Override
    public Publisher<Boolean> validate() {
        return new IteratingPublisher<>(List.of(validator.test(site)), Dispatchers.getDefault());
    }

    @Override
    public Publisher<ComputeResourceEvent> createDeployment(
            CreateComputeResourceRequest request
    ) {
        var name = new UUID(request.getNode().getHigh(), request.getNode().getLow());
        var resource = URI.create("http://orchestrator.local/computes/" + name.toString());

        // Book-keep the new resource creation, do not override if resource already exists.
        var previous = computes.putIfAbsent(resource, request);
        if (previous == null) {
            var events = List.of(
                    new ComputeResourceEvent.Created(resource, request.getNode()),
                    new ComputeResourceEvent.Started(resource)
            );

            return new IteratingPublisher<>(events, ExecutorsKt.from(ForkJoinPool.commonPool()));
        } else {
            return new ErrorPublisher<>(
                    new IllegalStateException(
                            String.format("Resource(%s) already exists", resource)
                    )
            );
        }
    }

    @Override
    public Publisher<ComputeResourceEvent> deleteDeployment(
            DeleteComputeResourceRequest request
    ) {
        var requestData = computes.remove(request.getResource());
        if (requestData != null) {
            var events = List.of(new ComputeResourceEvent.Deleted(request.getResource()));

            return new IteratingPublisher<>(events, ExecutorsKt.from(ForkJoinPool.commonPool()));
        } else {
            return new ErrorPublisher<>(
                    new IllegalStateException(
                            String.format("Resource(%s) does not exist", request.getResource())
                    )
            );
        }
    }

    @Override
    public Publisher<NetworkResourceEvent> createNetworkAddress(
            CreateNetworkResourceRequest request
    ) {
        Random random = new Random();
        var name = request.getName();
        var privateAddress = "10.10.10." + random.nextInt(500);
        var publicAddress = "8.8.8.8";
        var privateResource = URI.create("http://orchestrator.local/networks/private/" + name);
        var publicResource = URI.create("http://orchestrator.local/networks/public/" + name);

        // Book-keep the new resource creation, do not override if resource already exists.
        var previousPrivate = networks.putIfAbsent(privateResource, request);
        var previousPublic = networks.putIfAbsent(publicResource, request);
        if (previousPrivate == null && previousPublic == null) {
            var events = List.of(
                    new NetworkResourceEvent.Created(privateResource, name, privateAddress, false),
                    new NetworkResourceEvent.Created(publicResource, name, publicAddress, true)
            );

            return new IteratingPublisher<>(events, ExecutorsKt.from(ForkJoinPool.commonPool()));
        } else {
            // Remove the insertions.
            if (previousPrivate != null) {
                networks.remove(privateResource, request);
                networks.remove(publicResource, request);
            }

            return new ErrorPublisher<>(
                    new IllegalStateException(
                            String.format("Resource(%s) already exists", privateResource)
                    )
            );
        }
    }

    @Override
    public Publisher<NetworkResourceEvent> deleteNetworkAddress(
            DeleteNetworkResourceRequest request
    ) {
        var requestData = networks.remove(request.getResource());
        if (requestData != null) {
            var events = List.of(new NetworkResourceEvent.Deleted(request.getResource()));

            return new IteratingPublisher<>(events, ExecutorsKt.from(ForkJoinPool.commonPool()));
        } else {
            return new ErrorPublisher<>(
                    new IllegalStateException(
                            String.format("Resource(%s) does not exist", request.getResource())
                    )
            );
        }
    }

    @Override
    public Publisher<NetworkAllocationEvent> createNetworkAllocation(
            CreateNetworkAllocationRequest request
    ) {
        var name = request.getName();
        var resource = URI.create("http://orchestrator.local/network-allocations/" + name);

        // Book-keep the new resource creation, do not override if resource already exists.
        var previous = allocations.putIfAbsent(resource, request);
        if (previous == null) {
            var events = List.of(
                    new NetworkAllocationEvent.Created(
                            resource,
                            name,
                            request.getCompute(),
                            request.getPublicNetwork(),
                            request.getPrivateNetwork()
                    )
            );

            return new IteratingPublisher<>(events, ExecutorsKt.from(ForkJoinPool.commonPool()));
        } else {
            return new ErrorPublisher<>(
                    new IllegalStateException(String.format("Resource(%s) already exists", resource))
            );
        }
    }

    @Override
    public Publisher<NetworkAllocationEvent> deleteNetworkAllocation(
            DeleteNetworkAllocationRequest request
    ) {
        var requestData = networks.remove(request.getResource());
        if (requestData != null) {
            var events = List.of(new NetworkAllocationEvent.Deleted(request.getResource()));

            return new IteratingPublisher<>(events, ExecutorsKt.from(ForkJoinPool.commonPool()));
        } else {
            return new ErrorPublisher<>(
                    new IllegalStateException(
                            String.format("Resource(%s) does not exist", request.getResource())
                    )
            );
        }
    }
}
