/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/

package com.vmware.blockchain.deployment.service.metadata;

import java.util.Map;
import java.util.Objects;
import java.util.Random;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.vmware.blockchain.deployment.model.AddModelRequest;
import com.vmware.blockchain.deployment.model.AddModelResponse;
import com.vmware.blockchain.deployment.model.ConcordModelIdentifier;
import com.vmware.blockchain.deployment.model.ConcordModelServiceImplBase;
import com.vmware.blockchain.deployment.model.ConcordModelSpecification;
import com.vmware.blockchain.deployment.model.ListModelsRequest;
import com.vmware.blockchain.deployment.model.ListModelsResponseEvent;
import com.vmware.blockchain.deployment.model.MessageHeader;
import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore;
import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Event;
import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Event.ChangeEvent;
import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Event.DeleteEvent;
import com.vmware.blockchain.deployment.persistence.kv.MonotonicInt;
import com.vmware.blockchain.deployment.reactive.BaseSubscriber;
import com.vmware.blockchain.deployment.reactive.ReactiveStream;

import io.grpc.stub.StreamObserver;

/**
 * Concrete implementation of {@link com.vmware.blockchain.deployment.model.ConcordModelService}.
 */
public class ConcordModelService extends ConcordModelServiceImplBase {

    /**
     * Enumeration of possible service instance state.
     */
    private enum State {
        STOPPED,
        INITIALIZING,
        ACTIVE,
        STOPPING
    }

    /** Logger instance. */
    private static Logger log = LoggerFactory.getLogger(ConcordModelService.class);

    /** Initial version value of all stored ConcordModelSpecification's. */
    private static MonotonicInt initialVersion = new MonotonicInt().next();

    /** Event pipe buffer size. */
    private static int eventBufferCapacity = 256;

    /** Atomic update for service instance state. */
    private static final AtomicReferenceFieldUpdater<ConcordModelService, State> STATE =
            AtomicReferenceFieldUpdater.newUpdater(ConcordModelService.class, State.class, "state");

    /** Executor to use for all async operations. */
    private final ExecutorService executor;

    /** Key-value storage of ConcordModelSpecification. */
    private final KeyValueStore<ConcordModelIdentifier, ConcordModelSpecification, MonotonicInt> storage;

    /** Service state. */
    private volatile State state = State.STOPPED;

    /** Source of randomness. */
    private final Random random = new Random();

    /** Materialized view of the key-value storage (client-view). */
    private final Map<ConcordModelIdentifier, ConcordModelSpecification> materializedView =
            new ConcurrentHashMap<>();

    /**
     * Event sink of all storage {@link Event}.
     * Note: The field is not currently a volatile field because the READ/WRITE access are all
     * controlled within initialize() and shutdown(), and used within the scope of the state
     * field, which is itself volatile, helping to establish memory barrier for this field.
     */
    private BaseSubscriber<Event<ConcordModelIdentifier, ConcordModelSpecification, MonotonicInt>> eventSink;

    public ConcordModelService(
            ExecutorService executor,
            KeyValueStore<ConcordModelIdentifier, ConcordModelSpecification, MonotonicInt> storage
    ) {
        this.executor = Objects.requireNonNull(executor);
        this.storage = Objects.requireNonNull(storage);
    }

    /**
     * Initialize the service instance asynchronously.
     *
     * @return
     *   {@link CompletableFuture} that completes when initialization is done.
     */
    public CompletableFuture<Void> initialize() {
        if (STATE.compareAndSet(this, State.STOPPED, State.INITIALIZING)) {
            return CompletableFuture.runAsync(() -> {
                // Create a new event sink instance.
                eventSink = ReactiveStream.newBaseSubscriber(
                    // TODO(jameschang - 20190228):
                    // Current back-pressure control for initial DB loading is non-existent.
                    subscription -> subscription.request(Long.MAX_VALUE),
                    this::processEvent,
                    error -> log.error("Event stream is terminated with error", error),
                    () -> log.info("Event stream is closed"),
                    () -> log.info("Event stream is cancelled from client side")
                );

                // Request server for event feed and subscribe to it.
                var publisher = storage.subscribe(eventBufferCapacity, true);
                publisher.subscribe(eventSink);

                // Set instance to ACTIVE state.
                STATE.set(this, State.ACTIVE);

                log.info("Service instance initialized");
            }, executor);
        } else {
            return CompletableFuture.failedFuture(
                    new IllegalStateException("Service instance is not in stopped state")
            );
        }
    }

    /**
     * Shutdown the service instance asynchronously.
     *
     * @return
     *   {@link CompletableFuture} that completes when shutdown is done.
     */
    public CompletableFuture<Void> shutdown() {
        if (STATE.compareAndSet(this, State.ACTIVE, State.STOPPING)) {
            return CompletableFuture.runAsync(() -> {
                log.info("Service instance shutting down");

                eventSink.cancel();

                // Set instance to STOPPED state.
                STATE.set(this, State.STOPPED);
            }, executor);
        } else {
            return CompletableFuture.failedFuture(
                    new IllegalStateException("Service instance is not in active state")
            );
        }
    }

    @Override
    public void addModel(AddModelRequest message, StreamObserver<AddModelResponse> observer) {
        var request = Objects.requireNonNull(message);
        var response = Objects.requireNonNull(observer);

        try {
            // Simplistic state checking. More sophisticated checks requires setting up a reference
            // while the request is not yet completed.
            if (STATE.get(this) != State.ACTIVE) {
                response.onError(new IllegalStateException("Service instance is not active"));
            } else {
                // Generate a new ID.
                var identifier = new ConcordModelIdentifier(random.nextLong(), random.nextLong());

                // Store the specification and pipeline result.
                var result = storage.set(identifier, initialVersion, request.getSpecification());
                result.subscribe(ReactiveStream.newBaseSubscriber(
                    subscription -> subscription.request(Long.MAX_VALUE),
                    element -> response.onNext(
                            new AddModelResponse(new MessageHeader(), identifier)
                    ),
                    response::onError,
                    response::onCompleted,
                    () -> { }
                ));
            }
        } catch (Throwable error) {
            response.onError(error);
        }
    }

    @Override
    public void listModels(
            ListModelsRequest message,
            StreamObserver<ListModelsResponseEvent> observer
    ) {
        Objects.requireNonNull(message);
        var response = Objects.requireNonNull(observer);

        try {
            // Simplistic state checking. More sophisticated checks requires setting up a reference
            // while the request is not yet completed.
            if (STATE.get(this) != State.ACTIVE) {
                response.onError(new IllegalStateException("Service instance is not active"));
            } else {
                // TODO(jameschang - 20190228):
                // Need to implement orderBy and limit functionality.

                // Serve the response from the current materialized view.
                // ListModels API currently does not specify strong consistency.
                // So implementation-wise currently the access to the view (both on read-side and
                // eventing-side) are concurrent. If there is a need to enforce strong read-
                // consistency, possibly a CHAMP-based HAMT data structure is needed to not block
                // the eventing-side while still serving strongly consistent view.
                materializedView.forEach((identifier, specification) -> {
                    var responseEvent = new ListModelsResponseEvent(
                            new MessageHeader(),
                            identifier,
                            specification
                    );
                    response.onNext(responseEvent);
                });

                response.onCompleted();
            }
        } catch (Throwable error) {
            response.onError(error);
        }
    }

    /**
     * Process {@link Event} according to its type.
     *
     * @param event
     *   incoming event to be processed.
     */
    private void processEvent(
            Event<ConcordModelIdentifier, ConcordModelSpecification, MonotonicInt> event
    ) {
        if (event instanceof ChangeEvent) {
            var changeEvent = (ChangeEvent<ConcordModelIdentifier, ConcordModelSpecification, MonotonicInt>) event;
            materializedView.put(changeEvent.getKey(), changeEvent.getValue());
        } else if (event instanceof DeleteEvent) {
            var deleteEvent = (DeleteEvent<ConcordModelIdentifier, ConcordModelSpecification, MonotonicInt>) event;
            materializedView.remove(deleteEvent.getKey());
        } else {
            log.debug("Unknown event type: {}", event.getClass().getCanonicalName());
        }
    }
}
