/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/

package com.vmware.blockchain.deployment.service.provision;

import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import com.vmware.blockchain.deployment.model.ConcordModelSpecification;
import com.vmware.blockchain.deployment.model.CreateClusterRequest;
import com.vmware.blockchain.deployment.model.DeploymentSession;
import com.vmware.blockchain.deployment.model.DeploymentSessionEvent;
import com.vmware.blockchain.deployment.model.DeploymentSessionIdentifier;
import com.vmware.blockchain.deployment.model.DeploymentSpecification;
import com.vmware.blockchain.deployment.model.MessageHeader;
import com.vmware.blockchain.deployment.model.OrchestrationSiteIdentifier;
import com.vmware.blockchain.deployment.model.PlacementSpecification;
import com.vmware.blockchain.deployment.model.PlacementSpecification.Entry;
import com.vmware.blockchain.deployment.model.ProvisionedResource;
import com.vmware.blockchain.deployment.model.StreamClusterDeploymentSessionEventRequest;
import com.vmware.blockchain.deployment.model.core.Credential;
import com.vmware.blockchain.deployment.model.core.Endpoint;
import com.vmware.blockchain.deployment.model.ethereum.Genesis;
import com.vmware.blockchain.deployment.model.orchestration.OrchestrationSiteInfo;

import io.grpc.stub.StreamObserver;


/**
 * Various test verifying functionality and semantics of {@link ProvisionService}.
 */
class ProvisionServiceTest {

    /** Default await-time value in milliseconds. */
    private static long awaitTime = 10000;

    /**
     * Create a new {@link ProvisionService}.
     *
     * @return
     *   a newly created {@link ProvisionService} instance.
     */
    private static ProvisionService newProvisionService(
            Map<OrchestrationSiteIdentifier, OrchestrationSiteInfo> orchestrations
    ) {
        return DaggerTestProvisionServer.builder()
                .orchestrations(orchestrations)
                .build()
                .provisionService();
    }

    /**
     * Create a new set of mappings that denote all available orchestration sites.
     *
     * @return
     *   a mapping of {@link OrchestrationSiteIdentifier} to {@link OrchestrationSiteInfo}.
     */
    private static Map<OrchestrationSiteIdentifier, OrchestrationSiteInfo> newOrchestrationSites() {
        return Map.of(
                new OrchestrationSiteIdentifier(1, 0), newOrchestrationSiteInfo(1),
                new OrchestrationSiteIdentifier(2, 0), newOrchestrationSiteInfo(2),
                new OrchestrationSiteIdentifier(3, 0), newOrchestrationSiteInfo(3),
                new OrchestrationSiteIdentifier(4, 0), newOrchestrationSiteInfo(4)
        );
    }

    /**
     * Create a new {@link DeploymentSpecification} based on supplied parameter information.
     *
     * @param clusterSize
     *   size of the cluster to deploy.
     *
     * @return
     *   a new {@link DeploymentSpecification} instance.
     */
    private static DeploymentSpecification newDeploymentSpecification(int clusterSize) {
        var list = IntStream.range(0, clusterSize)
                .mapToObj(i -> new Entry(
                        PlacementSpecification.Type.FIXED,
                        new OrchestrationSiteIdentifier(1, 0)
                ))
                .collect(Collectors.toList());
        var placementSpec = new PlacementSpecification(list);
        var genesis = new Genesis();
        ConcordModelSpecification spec = new ConcordModelSpecification();

        return new DeploymentSpecification(clusterSize, spec, placementSpec, genesis);
    }

    /**
     * Create a new {@link StreamObserver} that observes the stream of responses and saves the last
     * response as the result.
     *
     * @param result
     *   the promise to set with result value if result completion or failure were observed.
     * @param <T>
     *   type of result to be observed.
     *
     * @return
     *   a {@link CompletableFuture} that completes upon {@link StreamObserver#onCompleted()} with
     *   the last observed {@link StreamObserver#onNext} signal or completes exceptionally with
     *   {@link StreamObserver#onError} signal.
     */
    private static <T> StreamObserver<T> newResultObserver(CompletableFuture<T> result) {
        return new StreamObserver<>() {
            /** Holder of result value. */
            volatile T value;

            @Override
            public void onNext(T value) {
                this.value = value;
            }

            @Override
            public void onError(Throwable error) {
                result.completeExceptionally(error);
            }

            @Override
            public void onCompleted() {
                result.complete(value);
            }
        };
    }

    /**
     * Create a new {@link StreamObserver} that observes the stream of responses and saves all
     * response into a collection.
     *
     * @param result
     *   the promise to set with result values if result completion or failure were observed.
     * @param <T>
     *   type of result to be observed.
     *
     * @return
     *   a {@link CompletableFuture} that completes upon {@link StreamObserver#onCompleted()} with
     *   the all observed {@link StreamObserver#onNext} signal or completes exceptionally with
     *   {@link StreamObserver#onError} signal.
     */
    private static <T> StreamObserver<T> newCollectingObserver(
            CompletableFuture<Collection<T>> result
    ) {
        return new StreamObserver<>() {
            /**
             * Holder of result values.
             * Note: A map is used here to to leverage existing SDK concurrent data structures
             * without writing a new one. ConcurrentSkipList does not exist in the JDK.
             */
            Map<Integer, T> values = new ConcurrentHashMap<>();

            /** Integer counter. */
            AtomicInteger counter = new AtomicInteger(0);

            @Override
            public void onNext(T value) {
                values.put(counter.getAndIncrement(), value);
            }

            @Override
            public void onError(Throwable error) {
                result.completeExceptionally(error);
            }

            @Override
            public void onCompleted() {
                result.complete(values.values());
            }
        };
    }

    /**
     * Create a mock {@link OrchestrationSiteInfo} instance based on an integer parameter.
     *
     * @param i
     *   integer value to be stamped into the orchestration site's various identifiers.
     *
     * @return
     *   a new instance of {@link OrchestrationSiteInfo}.
     */
    private static OrchestrationSiteInfo newOrchestrationSiteInfo(int i) {
        var apiEndpoint = "https://apiserver-" + i;
        var datacenter = "datacenter-" + i;
        return new OrchestrationSiteInfo(
                OrchestrationSiteInfo.Type.VMC,
                new OrchestrationSiteInfo.Vmc(
                        new Endpoint(URI.create("https://authserver"), new Credential()),
                        new Endpoint(URI.create(apiEndpoint), new Credential()),
                        "test-org",
                        datacenter,
                        new Endpoint(URI.create("https://registry"), new Credential()),
                        "resource-pool",
                        "vm-folder",
                        "vmware-vpn",
                        0xA0A0A000,
                        24
                )
        );
    }

    /**
     * Test that {@link ProvisionService#createCluster} operation generation a valid deployment
     * session with {@link DeploymentSessionEvent}s.
     *
     * @throws Exception
     *   if test execution fails with uncaught exceptions.
     */
    @Test
    @Disabled("ProvisionService#generateClusterConfig() expects generation utility to be locally installed.")
    void clusterCreateShouldGenerateSession() throws Exception {
        var orchestrations = newOrchestrationSites();
        var service = newProvisionService(orchestrations);
        var initialized = service.initialize();
        initialized.get(awaitTime, TimeUnit.MILLISECONDS);
        Assertions.assertThat(initialized).isCompleted();

        var messageId = "id1";
        var messageUuid = UUID.nameUUIDFromBytes(messageId.getBytes(StandardCharsets.UTF_8));
        var clusterSize = 4;
        var createCluster = new CreateClusterRequest(
                new MessageHeader(messageId),
                newDeploymentSpecification(clusterSize)
        );
        var promise1 = new CompletableFuture<DeploymentSessionIdentifier>();
        service.createCluster(createCluster, newResultObserver(promise1));
        var sessionId = promise1.get(awaitTime, TimeUnit.MILLISECONDS);
        Assertions.assertThat(sessionId.getLow()).isEqualTo(messageUuid.getLeastSignificantBits());
        Assertions.assertThat(sessionId.getHigh()).isEqualTo(messageUuid.getMostSignificantBits());

        var promise2 = new CompletableFuture<Collection<DeploymentSessionEvent>>();
        var getSessionEvents = new StreamClusterDeploymentSessionEventRequest(
                new MessageHeader(),
                sessionId
        );
        service.streamClusterDeploymentSessionEvents(
                getSessionEvents,
                newCollectingObserver(promise2)
        );
        var events = promise2.get(awaitTime, TimeUnit.MILLISECONDS);
        var nodeEvents = events.stream()
                .filter(event -> event.getType() == DeploymentSessionEvent.Type.NODE_DEPLOYED)
                .collect(Collectors.toList());
        Assertions.assertThat(nodeEvents.size()).isEqualTo(clusterSize);

        var resourceEvents = events.stream()
                .filter(event -> event.getType() == DeploymentSessionEvent.Type.RESOURCE)
                .collect(Collectors.toList());

        var computeEvents = resourceEvents.stream()
                .filter(e -> e.getResource().getType() == ProvisionedResource.Type.COMPUTE_RESOURCE)
                .collect(Collectors.toList());
        Assertions.assertThat(computeEvents.size()).isEqualTo(clusterSize);

        var networkEvents = resourceEvents.stream()
                .filter(e -> e.getResource().getType() == ProvisionedResource.Type.NETWORK_RESOURCE)
                .collect(Collectors.toList());
        Assertions.assertThat(networkEvents.size()).isEqualTo(clusterSize * 2);

        var allocationEvents = resourceEvents.stream()
                .filter(e -> e.getResource().getType() == ProvisionedResource.Type.NETWORK_ALLOCATION)
                .collect(Collectors.toList());
        Assertions.assertThat(allocationEvents.size()).isEqualTo(clusterSize);

        for (DeploymentSessionEvent event : events) {
            if (event.getType() == DeploymentSessionEvent.Type.ACKNOWLEDGED) {
                Assertions.assertThat(event.getStatus()).isEqualTo(DeploymentSession.Status.ACTIVE);
            } else if (event.getType() == DeploymentSessionEvent.Type.CLUSTER_DEPLOYED) {
                var clusterInfo = event.getCluster().getInfo();
                Assertions.assertThat(clusterInfo.getMembers().size()).isEqualTo(clusterSize);
            } else if (event.getType() == DeploymentSessionEvent.Type.COMPLETED) {
                Assertions.assertThat(event.getStatus()).isEqualTo(DeploymentSession.Status.SUCCESS);
            }
        }

        // Clean up.
        var shutdown = service.shutdown();
        shutdown.get(awaitTime, TimeUnit.MILLISECONDS);
        Assertions.assertThat(shutdown).isCompleted();
    }
}
