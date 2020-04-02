/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.futureutil;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Flow;
import java.util.function.Supplier;

import io.grpc.stub.StreamObserver;

/**
 * Util class to convert streams to futures.
 */
public class ReactiveStream {

    /**
     * Read a future for a single stream.
     */
    public static <T> CompletableFuture<T> toFutureSingle(Flow.Publisher<T> publisher) {
        CompletableFuture<T> promise = new CompletableFuture<>();
        final Flow.Subscription[] subscriptionF = new Flow.Subscription[1];
        publisher.subscribe(new Flow.Subscriber<T>() {
            @Override
            public void onSubscribe(Flow.Subscription subscription) {
                subscription.request(Long.MAX_VALUE);
                subscriptionF[0] = subscription;
            }

            @Override
            public void onNext(T t) {
                promise.complete(t);
                if (subscriptionF[0] != null) {
                    subscriptionF[0].cancel();
                }
            }

            @Override
            public void onError(Throwable throwable) {
                promise.completeExceptionally(throwable);
            }

            @Override
            public void onComplete() {
                if (!promise.isDone()) {
                    promise.complete(null);
                }
            }
        });
        return promise;
    }

    /**
     * Read a future from stream.
     */
    public static <T, V extends List<T>>  CompletableFuture<V> toFuture(Flow.Publisher<T> publisher,
                                                                        Supplier<V> factory) {
        CompletableFuture<V> promise = new CompletableFuture<>();
        V result = factory.get();
        publisher.subscribe(new Flow.Subscriber<>() {

            @Override
            public void onSubscribe(Flow.Subscription subscription) {
                subscription.request(Long.MAX_VALUE);
            }

            @Override
            public void onNext(T t) {
                result.add(t);
            }

            @Override
            public void onError(Throwable throwable) {
                promise.completeExceptionally(throwable);
            }

            @Override
            public void onComplete() {
                promise.complete(result);
            }
        });
        return promise;
    }

    /**
     * An observer to fake a blocked call.
     */
    public static  <T> StreamObserver<T> blockedResultObserver(CompletableFuture<T> result) {
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
}
