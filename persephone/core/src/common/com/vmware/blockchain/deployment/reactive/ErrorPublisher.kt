/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.reactive

/**
 * An implementation of [Publisher] that pushes error signal via [Subscriber.onError()] upon
 * [Subscription.request()], on the calling requester's thread.
 *
 * @param[T]
 *   type of element to be published.
 * @param[error]
 *   error signal to be pushed for any subscribing [Subscriber].
 */
class ErrorPublisher<T>(private val error: Throwable) : Publisher<T> {

    /**
     * An implementation of [Subscription] that signals error upon [Subscription.request(Long)].
     *
     * @param[subscriber]
     *   subscriber to signal to.
     * @param[error]
     *   error to be signaled when subscriber request additive demand.
     */
    class ErrorSubscription(
        subscriber: Subscriber<*>,
        private val error: Throwable
    ) : Subscription {

        /** Holder active subscriber tied to this subscription. */
        private val subscriber = kotlinx.atomicfu.atomic<Subscriber<*>?>(subscriber)

        override fun request(additiveDemand: Long) {
            subscriber.getAndSet(null)
                    // Signal on the caller's thread if a valid reference was CAS'ed out.
                    ?.onError(error)
        }

        override fun cancel() {
            subscriber.value = null
        }
    }

    override fun subscribe(subscriber: Subscriber<in T>?) {
        if (subscriber == null) {
            // Reactive Stream standard requires that NPE be thrown if subscriber is null.
            // (Publisher Rule 9)
            throw NullPointerException("Subscriber may not be null.")
        }

        subscriber.onSubscribe(ErrorSubscription(subscriber, error))
    }
}
