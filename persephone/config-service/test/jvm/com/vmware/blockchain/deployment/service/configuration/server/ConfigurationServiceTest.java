/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.service.configuration.server;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import com.vmware.blockchain.deployment.service.configuration.generateconfig.ConcordConfigUtil;
import com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType;
import com.vmware.blockchain.deployment.v1.ConfigurationComponent;
import com.vmware.blockchain.deployment.v1.ConfigurationServiceRequest;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;
import com.vmware.blockchain.deployment.v1.MessageHeader;
import com.vmware.blockchain.deployment.v1.NodeConfigurationRequest;
import com.vmware.blockchain.deployment.v1.NodeConfigurationResponse;

import io.grpc.stub.StreamObserver;

/**
 * Test for {@link ConfigurationService}.
 */
public class ConfigurationServiceTest {

    /** Default await-time value in milliseconds. */
    private static long awaitTime = 10000;

    private static ConfigurationService service = newConfigurationService();

    /**
     * Create a new {@link ConfigurationService}.
     *
     * @return
     *   a newly created {@link ConfigurationService} instance.
     */
    private static ConfigurationService newConfigurationService() {
        return DaggerConfigurationServer.create().configurationService();
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
             *
             * <p>Note: A map is used here to to leverage existing SDK concurrent data structures
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

    @BeforeAll
    static void setup() throws InterruptedException, ExecutionException, TimeoutException {
        var initialized = service.initialize();
        initialized.get(awaitTime, TimeUnit.MILLISECONDS);
        Assertions.assertThat(initialized).isCompleted();
    }

    @Test
    @Disabled("ConcordConfigUtil#getConcordConfig() expects generation utility to be locally installed.")
    void testgenerateConfiguration() throws InterruptedException, ExecutionException, TimeoutException {
        List<String> hostIps = new ArrayList<String>();
        hostIps.add("10.0.0.1");
        hostIps.add("10.0.0.2");
        hostIps.add("10.0.0.3");
        hostIps.add("10.0.0.4");

        var messageId = "id1";

        ConfigurationServiceRequest request = new ConfigurationServiceRequest(new MessageHeader(messageId), hostIps,
                                                                              null, null,
                                                                              null);

        var promise = new CompletableFuture<ConfigurationSessionIdentifier>();
        service.createConfiguration(request, newResultObserver(promise));

        var identifier = promise.get(awaitTime, TimeUnit.MILLISECONDS);

        assert identifier != null;

        for (int i = 0; i < hostIps.size(); i++) {
            var promise1 = new CompletableFuture<NodeConfigurationResponse>();
            NodeConfigurationRequest request1 = new NodeConfigurationRequest(new MessageHeader(),
                    identifier, i);
            service.getNodeConfiguration(request1, newResultObserver(promise1));
            var response = promise1.get(awaitTime, TimeUnit.MILLISECONDS);

            int totalTlsIdentities = hostIps.size() + ConcordConfigUtil.CLIENT_PROXY_PER_NODE * hostIps.size();

            int tlsComponentCount = 2 // key and certificate for self node
                    // client certificates (only) for non principals
                    + (totalTlsIdentities - ConcordConfigUtil.CLIENT_PROXY_PER_NODE)
                    // server certificates (only) for non principals
                    + (totalTlsIdentities - ConcordConfigUtil.CLIENT_PROXY_PER_NODE)
                    + ConcordConfigUtil.CLIENT_PROXY_PER_NODE * (1 // certificate for client
                        + 1 // key for client
                        + 1 // certificate for server
                        + 1); // key for server

            int ethRpcConfig = 0;
            int tlsConfig = 0;
            int unknown = 0;

            for (ConfigurationComponent component : response.getConfigurationComponent()) {
                if (component.getType().equals(ServiceType.CONCORD)) {
                    tlsConfig++;
                } else if (component.getType().equals(ServiceType.ETHEREUM_API)) {
                    ethRpcConfig++;
                } else {
                    unknown++;
                }
            }

            assert tlsConfig == tlsComponentCount + 1; // one for config
            assert ethRpcConfig == 2; //ethrpc does not have config
            assert unknown == 0;
        }
    }

    @AfterAll
    static void cleanup() throws InterruptedException, ExecutionException, TimeoutException {
        var shutdown = service.shutdown();
        shutdown.get(awaitTime, TimeUnit.MILLISECONDS);
        Assertions.assertThat(shutdown).isCompleted();
    }
}
