/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.reactive

/**
 * A [Publisher] that applies one-to-one functional transformation of elements of type [T] emitted
 * by an upstream publisher, into elements of type [R].
 *
 * @param[T]
 *   type of element to transform from.
 * @param[R]
 *   type of element to transform to.
 * @param[upstream]
 *   upstream publisher.
 * @param[transform]
 *   element transform function.
 */
class MappingPublisher<T, R>(
    val upstream: Publisher<T>,
    private val transform: (T) -> R
) : Publisher<R> {
    /**
     * A sub-class of [BaseSubscriber] that takes a [Subscriber] of elements of type [R] and creates
     * an upstream [Subscriber] of elements of type [T], using a transformation function mapping
     * from [T] to [R].
     *
     * @param[T]
     *   type of element to transform from.
     * @param[R]
     *   type of element to transform to.
     * @param[subscriber]
     *   downstream subscriber that takes elements of type [R] as signals.
     * @param[transform]
     *   mapping function that converts an element of type [T] to an element of type [R].
     */
    class MappingSubscriber<T, R>(
        private val subscriber: Subscriber<R>,
        private val transform: (T) -> R
    ) : BaseSubscriber<T>(
            // Explicitly unset onSubscribe to allow the downstream subscriber to dictate behavior.
            onSubscribe = null
    ) {

        override fun onSubscribe(subscription: Subscription?) {
            super.onSubscribe(subscription)

            try {
                // Signal the delegated subscriber's onSubscribe().
                subscriber.onSubscribe(subscription)
            } catch (error: Throwable) {
                onError(internalError(error))
            }
        }

        override fun onNext(element: T?) {
            super.onNext(element)

            try {
                // Supertype already performs null-check as per Reactive Stream specification,
                // just use '!!' operator to assert NonNull.
                val notNullElement = element!!

                // Signal subscriber w/ the transformed element.
                subscriber.onNext(transform(notNullElement))
            } catch (error: Throwable) {
                onError(internalError(error))
            }
        }

        override fun onComplete() {
            super.onComplete()

            try {
                // Signal the delegated subscriber's onComplete().
                subscriber.onComplete()
            } catch (error: Throwable) {
                // Do not recursively call onError, drop the error.
            }
        }

        override fun onError(throwable: Throwable?) {
            super.onError(throwable)

            try {
                // Signal the delegated subscriber's onError().
                subscriber.onError(throwable)
            } catch (error: Throwable) {
                // Do not recursively call onError, drop the error.
            }
        }
    }

    override fun subscribe(subscriber: Subscriber<in R>?) {
        if (subscriber == null) {
            // Reactive Stream standard requires that NPE be thrown if subscriber is null.
            // (Publisher Rule 9)
            throw NullPointerException("Subscriber may not be null.")
        }

        val mappedSubscriber = MappingSubscriber(subscriber, transform)
        upstream.subscribe(mappedSubscriber)
    }
}
