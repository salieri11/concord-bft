/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.reactive

import kotlinx.coroutines.CompletableDeferred
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Deferred
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.channels.BroadcastChannel
import kotlinx.coroutines.reactive.publish
import kotlin.coroutines.CoroutineContext

/**
 * An implementation of [Publisher] of element of type [T] that publishes to downstream subscribers
 * in a broadcasting manner.
 *
 * @param[T]
 *   type of element to be published.
 * @param[capacity]
 *   buffer capacity of the publisher to hold outstanding unconsumed broadcast elements.
 * @param[context]
 *   parent [coroutineContext] to base this instance's internal context on.
 */
class BroadcastingPublisher<T>(
    private val capacity: Int,
    private val context: CoroutineContext = Dispatchers.Default
) : Publisher<T>, CoroutineScope {

    /** [CoroutineContext] to launch all coroutines associated with this instance. */
    override val coroutineContext: CoroutineContext
        get() = context + job

    /** Parent [Job] of all coroutines associated with this instance's operation. */
    private val job: Job = Job()

    /** Internal buffered broadcast channel that is shared among all downstream subscribers. */
    private val broadcastChannel = BroadcastChannel<T>(capacity)

    /** Internal state tracking whether the instance is already closed. */
    private val active = kotlinx.atomicfu.atomic<Boolean>(true)

    private val subscribed = CompletableDeferred<Unit>()

    /** Internal [Publisher] instance for all [Subscriber]s. */
    private val publisher: Publisher<T> = publish(coroutineContext) {
        val receiveChannel = broadcastChannel.openSubscription()

        // Signal that publisher is now HOT.
        subscribed.complete(Unit)

        // Loop over incoming signals until upstream closes.
        for (element in receiveChannel) {
            send(element)
        }
    }

    override fun subscribe(subscriber: Subscriber<in T>?) {
        if (subscriber == null) {
            // Reactive Stream standard requires that NPE be thrown if subscriber is null.
            // (Publisher Rule 9)
            throw NullPointerException("Subscriber may not be null.")
        }

        publisher.subscribe(subscriber)
    }

    /**
     * Return a [Deferred] handle to wait on the condition when this [Publisher] instance turns
     * hot due to incoming subscription.
     *
     * @return
     *   a [Deferred] instance that returns `true` for [Deferred.isCompleted] when this publisher
     *   instance is subscribed to.
     */
    fun waitForSubscription(): Deferred<Unit> {
        return subscribed
    }

    /**
     * Broadcast the input element as the next signal to all downstream subscribers. If there is
     * no active subscriber, the signal is lost / dropped.
     *
     * @param[element]
     *   element to broadcast.
     *
     * @return
     *   `true` if element was broadcast, `false` otherwise.
     */
    fun broadcast(element: T): Boolean {
        // To prevent the caller from being stalled / suspended, we use offer() instead of send() to
        // detect whether the element can actually be sent to downstream.
        return broadcastChannel.offer(element)
    }

    /**
     * Explicitly close the publisher instance. All non-fully consumed subscriptions will be
     * served cancellation signal.
     *
     * Note: The instance can only be closed once. All subsequent calls after initial [close] have
     * no effect.
     */
    fun close(error: Throwable? = null) {
        active.getAndSet(false)
                .takeIf { it } // Proceed if went from false -> true.
                ?.run {
                    broadcastChannel.close(error)
                    job.cancel()
                }
    }
}
