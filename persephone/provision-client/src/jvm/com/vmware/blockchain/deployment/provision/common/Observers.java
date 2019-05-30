/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.provision.common;

import java.util.Collection;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

import io.grpc.stub.StreamObserver;

/** Temporary class, due for deletion and put in super common package.
 */
public class Observers {
    /**
     * Taken from "Test" code.
     */
    public static <T> StreamObserver<T> newResultObserver(CompletableFuture<T> result) {
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
     * Another listener for collection kind of response.
     */
    public static <T> StreamObserver<T> newCollectingObserver(CompletableFuture<Collection<T>> result) {
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

}
