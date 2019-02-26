/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.service.metadata;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import com.vmware.blockchain.deployment.model.AddModelRequest;
import com.vmware.blockchain.deployment.model.AddModelResponse;
import com.vmware.blockchain.deployment.model.ConcordModelIdentifier;
import com.vmware.blockchain.deployment.model.ConcordModelServiceStub;
import com.vmware.blockchain.deployment.model.ConcordModelSpecification;
import com.vmware.blockchain.deployment.model.ListModelsRequest;
import com.vmware.blockchain.deployment.model.ListModelsResponseEvent;
import com.vmware.blockchain.deployment.model.MessageHeader;
import io.grpc.CallOptions;
import io.grpc.inprocess.InProcessChannelBuilder;
import io.grpc.inprocess.InProcessServerBuilder;
import io.grpc.stub.StreamObserver;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Various test verifying functionality and semantics of {@link ConcordModelService}.
 */
class ConcordModelServiceTest {

    /**
     * Create a new {@link ConcordModelService}.
     *
     * @return
     *   a newly created {@link ConcordModelService} instance.
     */
    private static ConcordModelService newConcordModelService() {
        return DaggerMetadataServer.create().concordModelService();
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
     * Test that {@link ConcordModelService} operations when housed in GRPC server settings is
     * functional.
     *
     * @throws Exception
     *   if test execution fails with uncaught exceptions.
     */
    @Test
    void grpcHousing() throws Exception {
        var name = "recallableModel";
        var service = newConcordModelService();
        var initialized = service.initialize();
        initialized.get(500, TimeUnit.MILLISECONDS);
        Assertions.assertThat(initialized).isCompleted();

        // Create server instance.
        var server = InProcessServerBuilder.forName(name)
                .directExecutor()
                .addService(service)
                .build()
                .start();

        // Create client instance.
        var channel = InProcessChannelBuilder.forName(name).directExecutor().build();
        var client = new ConcordModelServiceStub(channel, CallOptions.DEFAULT);

        // Check that the API can be serviced normally after service initialization.
        var promise = new CompletableFuture<AddModelResponse>();
        var request = new AddModelRequest();
        client.addModel(request, newResultObserver(promise));
        promise.get(100, TimeUnit.MILLISECONDS);
        Assertions.assertThat(promise).isCompleted();

        // Cleanup.
        var shutdown = service.shutdown();
        shutdown.get(500, TimeUnit.MILLISECONDS);
        Assertions.assertThat(shutdown).isCompleted();
        channel.shutdown();
        Assertions.assertThat(channel.awaitTermination(500, TimeUnit.MILLISECONDS)).isTrue();
        server.shutdown();
        Assertions.assertThat(server.awaitTermination(500, TimeUnit.MILLISECONDS)).isTrue();
    }

    /**
     * Test that {@link ConcordModelService#addModel} adds models that can be recalled through
     * {@link ConcordModelService#listModels}.
     *
     * @throws Exception
     *   if test execution fails with uncaught exceptions.
     */
    @Test
    void recallableModel() throws Exception {
        var service = newConcordModelService();
        var initialized = service.initialize();
        initialized.get(500, TimeUnit.MILLISECONDS);
        Assertions.assertThat(initialized).isCompleted();

        // Add a number of models.
        var numModelsToAdd = 100;
        var expected = new HashMap<ConcordModelIdentifier, ConcordModelSpecification>();
        for (int i = 0; i < numModelsToAdd; i++) {
            var model = new ConcordModelSpecification(
                    "version-" + i,
                    "template-" + i,
                    Collections.emptyList()
            );
            var request = new AddModelRequest(new MessageHeader(), model);
            var promise = new CompletableFuture<AddModelResponse>();
            service.addModel(request, newResultObserver(promise));
            var response = promise.get(100, TimeUnit.MILLISECONDS);
            Assertions.assertThat(response.getId()).isNotNull();
            expected.put(response.getId(), model);
        }

        // Obtain the list of added models.
        var listRequest = new ListModelsRequest(
                new MessageHeader(),
                ListModelsRequest.OrderBy.UNSPECIFIED,
                numModelsToAdd
        );
        var listPromise = new CompletableFuture<Collection<ListModelsResponseEvent>>();
        service.listModels(listRequest, newCollectingObserver(listPromise));

        var actual = listPromise.get(100, TimeUnit.MILLISECONDS).stream()
                .collect(Collectors.toMap(
                        ListModelsResponseEvent::getId,
                        ListModelsResponseEvent::getSpecification)
                );
        Assertions.assertThat(actual).allSatisfy((key, value) -> {
            Assertions.assertThat(expected.containsKey(key)).isTrue();
            Assertions.assertThat(expected.get(key)).isEqualTo(value);
        });

        // Clean up.
        var shutdown = service.shutdown();
        shutdown.get(500, TimeUnit.MILLISECONDS);
        Assertions.assertThat(shutdown).isCompleted();
    }

    /**
     * Test that {@link ConcordModelService#addModel} adds models that can be recalled through
     * {@link ConcordModelService#listModels} across service restarts.
     */
    @Test
    void persistentModel() {
        // TODO: IMPLEMENT ME after there is actual data persistence.
    }

    /**
     * Test various service behaviors around {@link ConcordModelService#initialize()} and
     * {@link ConcordModelService#shutdown()}.
     *
     * @throws Exception
     *   if test execution fails with uncaught exceptions.
     */
    @Test
    void lifecycleSequentialAction() throws Exception {
        var service = newConcordModelService();

        // Attempt to call API before service is initialized.
        var request = new AddModelRequest();
        var promise1 = new CompletableFuture<AddModelResponse>();
        service.addModel(request, newResultObserver(promise1));
        Assertions.assertThatExceptionOfType(ExecutionException.class)
                .isThrownBy(() -> promise1.get(100, TimeUnit.MILLISECONDS))
                .withCauseInstanceOf(IllegalStateException.class);

        // Attempt to initialize twice sequentially.
        var initialized1 = service.initialize();
        initialized1.get(500, TimeUnit.MILLISECONDS);
        Assertions.assertThat(initialized1).isCompleted();

        var initialized2 = service.initialize();
        Assertions.assertThatExceptionOfType(ExecutionException.class)
                .isThrownBy(() -> initialized2.get(500, TimeUnit.MILLISECONDS))
                .withCauseInstanceOf(IllegalStateException.class);

        // Check that the same API can be serviced normally after service initialization.
        var promise2 = new CompletableFuture<AddModelResponse>();
        service.addModel(request, newResultObserver(promise2));
        promise2.get(100, TimeUnit.MILLISECONDS);
        Assertions.assertThat(promise2).isCompleted();

        // Attempt to shutdown twice sequentially.
        var shutdown1 = service.shutdown();
        shutdown1.get(500, TimeUnit.MILLISECONDS);
        Assertions.assertThat(shutdown1).isCompleted();

        var shutdown2 = service.shutdown();
        Assertions.assertThatExceptionOfType(ExecutionException.class)
                .isThrownBy(() -> shutdown2.get(500, TimeUnit.MILLISECONDS))
                .withCauseInstanceOf(IllegalStateException.class);

        // Attempt to call API after service is shutdown.
        var promise3 = new CompletableFuture<AddModelResponse>();
        service.addModel(request, newResultObserver(promise3));
        Assertions.assertThatExceptionOfType(ExecutionException.class)
                .isThrownBy(() -> promise3.get(100, TimeUnit.MILLISECONDS))
                .withCauseInstanceOf(IllegalStateException.class);
    }
}
