/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.reactive

/* ******************************************************************************************
 * Type definitions to allow a zero-cost, type-safe abstraction in case definition changes.
 *
 * If multi-platform build is utilized, `actual` definitions need to be either `typealias`ed
 * or implemented for the target platform.
 * *****************************************************************************************/

/**
 * A [Publisher] is a provider of a potentially unbounded number of sequenced elements, publishing
 * them according to the demand received from its [Subscriber]s.
 *
 * A [Publisher] can serve multiple [Subscriber]s subscribed dynamically at various points in time.
 *
 * @param[T]
 *   the type of element signaled.
 */
expect interface Publisher<T> {
    fun subscribe(subscriber: Subscriber<in T>)
}

/**
 * Will receive call to [onSubscribe(Subscription)] once after passing an instance of [Subscriber]
 * to [Publisher.subscribe(Subscriber)].
 *
 * No further notifications will be received until [Subscription.request(Long)] is called.
 *
 * After signaling demand:
 * - One or more invocations of [onNext(T)] up to the maximum number defined by
 *   [Subscription.request(Long)].
 * - Single invocation of [onError(Throwable)] or [onComplete()] which signals a terminal state
 *   after which no further events will be sent.
 *
 * Demand can be signaled via [Subscription.request(Long)] whenever the [Subscriber] instance is
 * capable of handling more.
 *
 * @param[T]
 *   the type of element signaled.
 */
expect interface Subscriber<T> {
    fun onSubscribe(subscription: Subscription)
    fun onNext(element: T)
    fun onError(t: Throwable)
    fun onComplete()
}

/**
 * A [Subscription] represents a one-to-one lifecycle of a [Subscriber] subscribing to a
 * [Publisher].
 *
 * It can only be used once by a single [Subscriber].
 *
 * It is used to both signal desire for data and cancel demand (and allow resource cleanup).
 */
expect interface Subscription {
    fun request(demand: Long)
    fun cancel()
}
