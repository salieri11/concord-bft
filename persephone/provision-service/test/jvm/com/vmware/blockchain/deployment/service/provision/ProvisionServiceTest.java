/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
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

import com.vmware.blockchain.deployment.model.CreateClusterRequest;
import com.vmware.blockchain.deployment.model.DeploymentSessionEvent;
import com.vmware.blockchain.deployment.model.DeploymentSessionIdentifier;
import com.vmware.blockchain.deployment.model.DeploymentSpecification;
import com.vmware.blockchain.deployment.model.StreamClusterDeploymentSessionEventRequest;
import com.vmware.blockchain.deployment.model.MessageHeader;
import com.vmware.blockchain.deployment.model.OrchestrationSiteIdentifier;
import com.vmware.blockchain.deployment.model.core.Credential;
import com.vmware.blockchain.deployment.model.core.Endpoint;
import com.vmware.blockchain.deployment.model.orchestration.OrchestrationSiteInfo;
import io.grpc.stub.StreamObserver;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

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
    private static ProvisionService newProvisionService() {
        var orchestrations = Map.of(
                new OrchestrationSiteIdentifier(1, 0), newOrchestrationSiteInfo(1),
                new OrchestrationSiteIdentifier(2, 0), newOrchestrationSiteInfo(2),
                new OrchestrationSiteIdentifier(3, 0), newOrchestrationSiteInfo(3),
                new OrchestrationSiteIdentifier(4, 0), newOrchestrationSiteInfo(4)
        );

        return DaggerTestProvisionServer.builder()
                .orchestrations(orchestrations)
                .build()
                .provisionService();
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
            volatile T value = null;

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
             *
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
                        datacenter
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
    void clusterCreateShouldGenerateSession() throws Exception {
        var service = newProvisionService();
        var initialized = service.initialize();
        initialized.get(awaitTime, TimeUnit.MILLISECONDS);
        Assertions.assertThat(initialized).isCompleted();

        var messageId = "id1";
        var messageUuid = UUID.nameUUIDFromBytes(messageId.getBytes(StandardCharsets.UTF_8));
        var deploymentSpec = new DeploymentSpecification();
        var createCluster = new CreateClusterRequest(
                new MessageHeader(messageId),
                deploymentSpec
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
        Assertions.assertThat(events.size()).isEqualTo(2);

        // Clean up.
        var shutdown = service.shutdown();
        shutdown.get(awaitTime, TimeUnit.MILLISECONDS);
        Assertions.assertThat(shutdown).isCompleted();
    }
}
