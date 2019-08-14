/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
@file:JvmName("ReactiveStream")
package com.vmware.blockchain.deployment.reactive

import java.util.concurrent.CompletableFuture
import java.util.function.Consumer
import java.util.function.Supplier

/**
 * Type alias redeclaration of [org.reactivestreams.Publisher] for JVM.
 */
actual typealias Publisher<T> = org.reactivestreams.Publisher<T>

/**
 * Type alias redeclaration of [org.reactivestreams.Subscriber] for JVM.
 */
actual typealias Subscriber<T> = org.reactivestreams.Subscriber<T>

/**
 * Type alias redeclaration of [org.reactivestreams.Subscription] for JVM.
 */
actual typealias Subscription = org.reactivestreams.Subscription

/**
 * Convenience factory function to create [BaseSubscriber] for callers from Java on the JVM.
 *
 * Note: Kotlin-callers should use [BaseSubscriber] constructor directly to maintain maximum code
 * portability.
 */
fun <T> newBaseSubscriber(
    onSubscribe: Consumer<Subscription>,
    onNext: Consumer<T>,
    onError: Consumer<Throwable>,
    onComplete: Runnable,
    onCancel: Runnable
): BaseSubscriber<T> {
    return BaseSubscriber(
            onSubscribe = { onSubscribe.accept(it) },
            onNext = { onNext.accept(it) },
            onError = { onError.accept(it) },
            onComplete = { onComplete.run() },
            onCancel = { onCancel.run() }
    )
}

/**
 * Convert a [Publisher] to [CompletableFuture] that completes when the first element is observed
 * via an internal subscriber to the [Publisher] instance, through [Subscriber.onNext(T)].
 *
 * @return
 *   a new instance of [CompletableFuture].
 */
fun <T : Any> Publisher<T>.toFuture(): CompletableFuture<T> {
    val promise = CompletableFuture<T>()
    var subscription: Subscription? = null
    subscribe(
            BaseSubscriber<T>(
                    onSubscribe = {
                        it.request(Long.MAX_VALUE)
                        subscription = it
                    },
                    onNext = {
                        promise.complete(it)
                        subscription?.cancel()
                    },
                    onComplete = {
                        if (!promise.isDone) {
                            promise.complete(null)
                        }
                    },
                    onError = { promise.completeExceptionally(it) }
            )
    )

    return promise
}

/**
 * Convert a [Publisher] to [CompletableFuture] that completes when [Subscriber.onComplete()] is
 * observed via an internal subscriber to the [Publisher] instance. All signals observed are
 * collected into a [MutableCollection] supplied by the input supplier function.
 *
 * @param[factory]
 *   factory to utilize to create an instance of [MutableCollection].
 * @return
 *   a new instance of [CompletableFuture].
 */
fun <T, C : MutableCollection<T>> Publisher<T>.toFuture(
    factory: Supplier<C>
): CompletableFuture<C> {
    val promise = CompletableFuture<C>()
    val result = factory.get() // Non-thread-safe is ok due to ReactiveStreams semantics.
    subscribe(
            BaseSubscriber<T>(
                    onNext = { result += it },
                    onComplete = { promise.complete(result) },
                    onError = { promise.completeExceptionally(it) }
            )
    )

    return promise
}
