/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/

package com.vmware.blockchain.deployment.persistence.kv;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Event;
import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Value;
import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Version;
import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Versioned;
import com.vmware.blockchain.deployment.reactive.BaseSubscriber;
import com.vmware.blockchain.deployment.reactive.ReactiveStream;

/**
 * Basic functionality test of {@link KeyValueStore} operations, in Java.
 */
class KeyValueStoreJavaInteropTest {

    /** Default await-time value in milliseconds. */
    private static long awaitTime = 5000;

    /**
     * Implementation of {@link Value} that wraps a {@code byte[]}.
     */
    private static class ByteArrayValue implements Value {
        private final byte[] value;

        static ByteArrayValue of(String string) {
            return new ByteArrayValue(string.getBytes(StandardCharsets.UTF_8));
        }

        ByteArrayValue(byte[] value) {
            this.value = value;
        }

        @Override
        @SuppressWarnings("all")
        public byte[] asByteArray() {
            return value;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) {
                return true;
            }
            if (o == null || getClass() != o.getClass()) {
                return false;
            }
            ByteArrayValue that = (ByteArrayValue) o;
            return Arrays.equals(value, that.value);
        }

        @Override
        public int hashCode() {
            return Arrays.hashCode(value);
        }
    }

    /**
     * Create a new {@link BaseSubscriber} that stores any elements observed through
     * {@link org.reactivestreams.Subscriber#onNext} and invokes {@code onComplete} callback when
     * served with the associated {@link org.reactivestreams.Subscriber#onComplete} signals.
     *
     * @param observation
     *          mutable list to store signals to.
     * @param onComplete
     *          callback to be invoked on {@link org.reactivestreams.Subscriber#onComplete} signal.
     * @param <T>
     *          type of element received on the subscribe.
     *
     * @return  a new {@link BaseSubscriber} instance.
     */
    private <T> BaseSubscriber<T> newObservingSubscriber(List<T> observation, Runnable onComplete
    ) {
        return ReactiveStream.newBaseSubscriber(
            subscription -> subscription.request(Long.MAX_VALUE),
            observation::add,
            error -> {
                throw new RuntimeException(error);
            },
            onComplete,
            () -> {
            } // onCancel.
        );
    }

    /**
     * Create a new {@link BaseSubscriber} that invokes the supplied {@code onNext} and
     * {@code onComplete} callbacks when served with the associated reactive stream signals.
     *
     * @param onNext
     *          callback to be invoked on {@link org.reactivestreams.Subscriber#onNext} signal.
     * @param onComplete
     *          callback to be invoked on {@link org.reactivestreams.Subscriber#onComplete} signal.
     * @param <T>
     *          type of element received on the subscribe.
     *
     * @return  a new {@link BaseSubscriber} instance.
     */
    private <T> BaseSubscriber<T> newResultSubscriber(Consumer<T> onNext, Runnable onComplete) {
        return ReactiveStream.newBaseSubscriber(
            subscription -> subscription.request(Long.MAX_VALUE),
            onNext,
            error -> {
                throw new RuntimeException(error);
            },
            onComplete,
            () -> {
            } // onCancel.
        );
    }

    /**
     * Perform a series of CRUD operations using a given pair of typed key and value instance.
     *
     * @param server
     *          server to perform operations against.
     * @param key
     *          key to use.
     * @param value
     *          value to use.
     * @param initialVersion
     *          initial version value for key-value entry creation.
     * @param <K>
     *          type of key.
     * @param <V>
     *          type of value.
     * @param <T>
     *          type of version.
     *
     * @throws InterruptedException
     *          if a test concurrency primitive's waiting condition was interrupted.
     */
    private <K, V, T extends Version<T>> void performTypedCrudWithEventSourcing(
            KeyValueStore<K, V, T> server,
            K key,
            V value,
            T initialVersion
    ) throws InterruptedException {
        // Setup potential observations to be appended to.
        var potentialObservations = new ArrayList<Event<K, V, T>>();

        // Setup the event sink.
        var eventSink = server.subscribe(32, false);
        var eventingDone = new CountDownLatch(1);
        var observations = new ArrayList<Event<K, V, T>>();
        var eventSinkSubscriber = newObservingSubscriber(observations, eventingDone::countDown);
        eventSink.subscribe(eventSinkSubscriber);

        // Create a new key-value entry.
        var createPublisher = server.set(key, initialVersion, value);
        var createResult = new AtomicReference<Versioned<V, T>>(null);
        var createFinished = new CountDownLatch(1);
        var createSubscriber = this.<Versioned<V, T>>newResultSubscriber(
            element -> createResult.compareAndSet(null, element),
            () -> {
                potentialObservations
                        .add(new Event.ChangeEvent<>(key, value, initialVersion.next()));
                createFinished.countDown();
            }
        );
        createPublisher.subscribe(createSubscriber);
        Assertions.assertThat(createFinished.await(awaitTime, TimeUnit.MILLISECONDS)).isTrue();
        Assertions.assertThat(createResult.get()).isInstanceOf(Versioned.None.class);

        // Retrieve the created entry by its key.
        var getPublisher = server.get(key);
        var getResult = new AtomicReference<Versioned<V, T>>(null);
        var getFinished = new CountDownLatch(1);
        var getSubscriber = this.<Versioned<V, T>>newResultSubscriber(
            element -> getResult.compareAndSet(null, element),
            getFinished::countDown
        );
        getPublisher.subscribe(getSubscriber);
        Assertions.assertThat(getFinished.await(awaitTime, TimeUnit.MILLISECONDS)).isTrue();
        var getVersioned = getResult.get();
        Assertions.assertThat(getVersioned).isInstanceOf(Versioned.Just.class);
        var getVersion = ((Versioned.Just<V, T>) getVersioned).getVersion();
        Assertions.assertThat(getVersion).isEqualTo(initialVersion.next());

        // Delete the entry by its key.
        var deletePublisher = server.delete(key, getVersion);
        var deleteResult = new AtomicReference<Versioned<V, T>>(null);
        var deleteFinished = new CountDownLatch(1);
        var deleteSubscriber = this.<Versioned<V, T>>newResultSubscriber(
            element -> deleteResult.compareAndSet(null, element),
            () -> {
                potentialObservations.add(new Event.DeleteEvent<>(key, getVersion));
                deleteFinished.countDown();
            }
        );
        deletePublisher.subscribe(deleteSubscriber);
        Assertions.assertThat(deleteFinished.await(awaitTime, TimeUnit.MILLISECONDS)).isTrue();
        var deleteVersioned = deleteResult.get();
        Assertions.assertThat(deleteVersioned).isInstanceOf(Versioned.Just.class);
        var deleteVersion = ((Versioned.Just<V, T>) deleteVersioned).getVersion();
        Assertions.assertThat(deleteVersion).isEqualTo(getVersion);

        // Orderly close the publisher.
        server.unsubscribe(eventSink);
        Assertions.assertThat(eventingDone.await(awaitTime, TimeUnit.MILLISECONDS)).isTrue();

        // Verify the observations on event sinks.
        if (!observations.isEmpty()) {
            Assertions.assertThat(potentialObservations).containsSequence(observations);
        }
    }

    /**
     * Perform a series of CRUD operations using a given pair of untyped key and value instance.
     *
     * @param server
     *          server to perform operations against.
     * @param key
     *          key to use.
     * @param value
     *          value to use.
     * @param initialVersion
     *          initial version value for key-value entry creation.
     * @param <T>
     *          type of version.
     *
     * @throws InterruptedException
     *          if a test concurrency primitive's waiting condition was interrupted.
     */
    private <T extends Version<T>> void performUntypedCrudWithEventSourcing(
            UntypedKeyValueStore<T> server,
            Value key,
            Value value,
            T initialVersion
    ) throws InterruptedException {
        // Setup potential observations to be appended to.
        var potentialObservations = new ArrayList<Event<Value, Value, T>>();

        // Setup the event sink.
        var eventSink = server.subscribe(32, false);
        var eventingDone = new CountDownLatch(1);
        var observations = new ArrayList<Event<Value, Value, T>>();
        var eventSinkSubscriber = newObservingSubscriber(observations, eventingDone::countDown);
        eventSink.subscribe(eventSinkSubscriber);

        // Create a new key-value entry.
        var createPublisher = server.set(key, initialVersion, value);
        var createResult = new AtomicReference<Versioned<Value, T>>(null);
        var createFinished = new CountDownLatch(1);
        var createSubscriber = this.<Versioned<Value, T>>newResultSubscriber(
            element -> createResult.compareAndSet(null, element),
            () -> {
                potentialObservations
                        .add(new Event.ChangeEvent<>(key, value, initialVersion.next()));
                createFinished.countDown();
            }
        );
        createPublisher.subscribe(createSubscriber);
        Assertions.assertThat(createFinished.await(awaitTime, TimeUnit.MILLISECONDS)).isTrue();
        Assertions.assertThat(createResult.get()).isInstanceOf(Versioned.None.class);

        // Retrieve the created entry by its key.
        var getPublisher = server.get(key);
        var getResult = new AtomicReference<Versioned<Value, T>>(null);
        var getFinished = new CountDownLatch(1);
        var getSubscriber = this.<Versioned<Value, T>>newResultSubscriber(
            element -> getResult.compareAndSet(null, element),
            getFinished::countDown
        );
        getPublisher.subscribe(getSubscriber);
        Assertions.assertThat(getFinished.await(awaitTime, TimeUnit.MILLISECONDS)).isTrue();
        var getVersioned = getResult.get();
        Assertions.assertThat(getVersioned).isInstanceOf(Versioned.Just.class);
        var getVersion = ((Versioned.Just<Value, T>) getVersioned).getVersion();
        Assertions.assertThat(getVersion).isEqualTo(initialVersion.next());

        // Delete the entry by its key.
        var deletePublisher = server.delete(key, getVersion);
        var deleteResult = new AtomicReference<Versioned<Value, T>>(null);
        var deleteFinished = new CountDownLatch(1);
        var deleteSubscriber = this.<Versioned<Value, T>>newResultSubscriber(
            element -> deleteResult.compareAndSet(null, element),
            () -> {
                potentialObservations.add(new Event.DeleteEvent<>(key, getVersion));
                deleteFinished.countDown();
            }
        );
        deletePublisher.subscribe(deleteSubscriber);
        Assertions.assertThat(deleteFinished.await(awaitTime, TimeUnit.MILLISECONDS)).isTrue();
        var deleteVersioned = deleteResult.get();
        Assertions.assertThat(deleteVersioned).isInstanceOf(Versioned.Just.class);
        var deleteVersion = ((Versioned.Just<Value, T>) deleteVersioned).getVersion();
        Assertions.assertThat(deleteVersion).isEqualTo(getVersion);

        // Orderly close the publisher.
        server.unsubscribe(eventSink);
        Assertions.assertThat(eventingDone.await(awaitTime, TimeUnit.MILLISECONDS)).isTrue();

        // Verify the observations on event sinks.
        if (!observations.isEmpty()) {
            Assertions.assertThat(potentialObservations).containsSequence(observations);
        }
    }

    /**
     * Test {@link KeyValueStore} CRUD functionality with event-sourcing.
     */
    @Test
    void typedCrud() throws InterruptedException {
        var key = new PBSerializable("key-1", 1, 1);
        var keySerializer = PBSerializable.getSerializer();

        var value = new PBSerializable("value-1", 2, 2);
        var valueSerializer = PBSerializable.getSerializer();

        var server = new TypedKeyValueStore<PBSerializable, PBSerializable, MonotonicInt>(
                keySerializer,
                valueSerializer,
                new InMemoryUntypedKeyValueStore<>()
        );

        performTypedCrudWithEventSourcing(server, key, value, new MonotonicInt());

        // Cleanup.
        server.close();
    }

    /**
     * Test {@link UntypedKeyValueStore} CRUD functionality with event-sourcing.
     */
    @Test
    void untypedCrud() throws InterruptedException {
        var server = new InMemoryUntypedKeyValueStore<MonotonicInt>();
        var key = ByteArrayValue.of("key-1");
        var value = ByteArrayValue.of("value-1");

        performUntypedCrudWithEventSourcing(server, key, value, new MonotonicInt());

        // Cleanup.
        server.close();
    }
}
