/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.reactive

import kotlinx.coroutines.CoroutineDispatcher
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.reactive.publish
import kotlin.coroutines.CoroutineContext

/**
 * An implementation of [Publisher] of element of type [T] that publishes from an [Iterable]
 * sequence of elements.
 *
 * @param[T]
 *   type of element to be published.
 * @param[elements]
 *   an iterating sequence of elements.
 * @param[context]
 *   [CoroutineDispatcher] to use for publishing signals to subscribers.
 */
class IteratingPublisher<T>(
    private val elements: Iterable<T>,
    private val context: CoroutineContext = Dispatchers.Default
) : Publisher<T>, CoroutineScope {

    /** [CoroutineContext] to launch all coroutines associated with this instance. */
    override val coroutineContext: CoroutineContext
        get() = context + job

    /** Parent [Job] of all coroutines associated with this instance's operation. */
    private val job: Job = Job()

    /** Internal [Publisher] instance for all [Subscriber]s. */
    private val publisher: Publisher<T> = publish(coroutineContext) {
        for (element in elements) {
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
     * Explicitly close the publisher instance. All non-fully consumed subscriptions will be
     * served cancellation signal.
     */
    fun close() {
        job.cancel()
    }
}
