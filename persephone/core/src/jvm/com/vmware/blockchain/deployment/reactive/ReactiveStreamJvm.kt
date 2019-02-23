/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
@file:JvmName("ReactiveStream")
package com.vmware.blockchain.deployment.reactive

import java.util.function.Consumer

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
