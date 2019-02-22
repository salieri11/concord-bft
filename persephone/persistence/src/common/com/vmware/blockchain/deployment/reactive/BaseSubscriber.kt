/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.reactive

/**
 * A base implementation of [Subscriber] that allows caller-supplied triggers on each signal.
 *
 * @param[T]
 *   type of element received from subscription.
 * @param[onSubscribe]
 *   callback to call on [Subscriber.onSubscribe()].
 * @param[onNext]
 *   callback to call on [Subscriber.onNext(T)].
 * @param[onError]
 *   callback to call on [Subscriber.onError(Throwable)].
 * @param[onComplete]
 *   callback to call on [Subscriber.onComplete()].
 */
open class BaseSubscriber<T>(
    protected val onSubscribe: ((Subscription) -> Unit)? = {
        subscription: Subscription -> subscription.request(Long.MAX_VALUE)
    },
    private val onNext: ((T) -> Unit)? = null,
    private val onError: ((Throwable) -> Unit)? = null,
    private val onComplete: (() -> Unit)? = null,
    private val onCancel: (() -> Unit)? = null
) : Subscriber<T>, Subscription {

    /**
     * Singleton object denoting a cancelled [Subscription].
     */
    object CancelledSubscription : Subscription {

        override fun request(additiveDemand: Long) {
            // Reactive Stream Specification: Subscription, rule 6.
            // Cancelled subscription must be NO-OP on subsequent request()'s.
        }
        override fun cancel() {
            // Reactive Stream Specification: Subscription, rule 7.
            // Cancelled subscription must be NO-OP on subsequent cancel()'s.
        }
    }

    /** Holder of active subscription tied to this subscriber. */
    private val subscription = kotlinx.atomicfu.atomic<Subscription?>(null)

    /**
     * Trampoline function that also processes a [Throwable] encountered during internal operations.
     *
     * @param[throwable]
     *   error to be processed.
     */
    protected fun internalError(throwable: Throwable): Throwable {
        // Attempt to explicitly cancel subscription if there is one.
        subscription.value?.cancel()

        // As of now, there isn't any need to repackage/wrap internally raised errors.
        return throwable
    }

    override fun request(demand: Long) {
        subscription.value?.request(demand)
    }

    override fun cancel() {
        // Swap out any existing subscription with a singleton denoting a cancelled subscription.
        subscription.getAndSet(CancelledSubscription)
                .takeUnless { it == CancelledSubscription }
                ?.cancel()
                ?.also {
                    try {
                        // Signal callback if subscription was not already cancelled.
                        onCancel?.invoke()
                    } catch (error: Throwable) {
                        // Do not signal error after a cancel().
                    }
                }
    }

    override fun onNext(element: T?) {
        // Reactive Stream Specification: Subscriber, rule 13.
        // Subscriber must throw NPE if argument is null.
        if (element == null) {
            throw NullPointerException("Supplied onNext element cannot be null")
        }

        try {
            onNext?.invoke(element)
        } catch (error: Throwable) {
            onError(internalError(error))
        }
    }

    override fun onError(throwable: Throwable?) {
        // Reactive Stream Specification: Subscriber, rule 13.
        // Subscriber must throw NPE if argument is null.
        if (throwable == null) {
            throw NullPointerException("Supplied error throwable cannot be null")
        }

        if (subscription.getAndSet(CancelledSubscription) != CancelledSubscription) {
            try {
                // Signal callback if subscription was not already cancelled.
                onError?.invoke(throwable)
            } catch (error: Throwable) {
                // Do not recursively call onError, drop the error.
            }
        }
    }

    override fun onComplete() {
        if (subscription.getAndSet(CancelledSubscription) != CancelledSubscription) {
            try {
                // Signal callback if subscription was not already cancelled.
                onComplete?.invoke()
            } catch (error: Throwable) {
                // Cannot continue with onError since onComplete is already signaled.
            }
        }
    }

    override fun onSubscribe(subscription: Subscription?) {
        // Reactive Stream Specification: Subscriber, rule 13.
        // Subscriber must throw NPE if argument is null.
        if (subscription == null) {
            throw NullPointerException("Supplied subscription cannot be null")
        }

        if (this.subscription.compareAndSet(null, subscription)) {
            try {
                onSubscribe?.invoke(subscription)
            } catch (error: Throwable) {
                onError(internalError(error))
            }
        } else {
            // Reactive Stream Specification: Subscriber, rule 5.
            // Must cancel new subscription if there is already an active subscription.
            subscription.cancel()
        }
    }
}
