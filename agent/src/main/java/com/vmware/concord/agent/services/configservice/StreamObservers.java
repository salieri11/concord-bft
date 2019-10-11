/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord.agent.services.configservice;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicReference;

import io.grpc.stub.StreamObserver;

/**
 * Convenience static functions involving {@link StreamObserver}.
 */
final class StreamObservers {

    private StreamObservers() {
    }

    /**
     * An implementation of {@link StreamObserver} that can be converted to a
     * {@link CompletableFuture}.
     *
     * @param <T>
     *   type of result to be observed.
     */
    static class MonoObserverFuture<T> implements StreamObserver<T> {

        /** Future to set with result value if result completion or failure were observed.*/
        private final CompletableFuture<T> promise = new CompletableFuture<>();

        /** Holder of value observed via onNext() signals. */
        private AtomicReference<T> value = new AtomicReference<>(null);

        @Override
        public void onNext(T value) {
            this.value.compareAndSet(null, value);
        }

        @Override
        public void onError(Throwable t) {
            promise.completeExceptionally(t);
        }

        @Override
        public void onCompleted() {
            promise.complete(value.get());
        }

        /**
         * Return the {@link StreamObserver} instance as {@link CompletableFuture}.
         *
         * @return
         *   a corresponding {@link CompletableFuture} that completes when {@link #onCompleted()} is
         *   signaled; or completes exceptionally when {@link #onError(Throwable)} signals.
         */
        public CompletableFuture<T> asCompletableFuture() {
            return promise;
        }
    }
}
