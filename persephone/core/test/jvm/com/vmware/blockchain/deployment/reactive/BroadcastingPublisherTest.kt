/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.reactive

import kotlinx.atomicfu.atomic
import kotlinx.coroutines.future.asCompletableFuture
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import org.assertj.core.api.Assertions
import org.junit.jupiter.api.Test

/**
 * Basic functionality test of [BroadcastingPublisher] operations.
 */
class BroadcastingPublisherTest {

    /**
     * Verify that multiple subscribers receive the same signals when they are fully subscribed to
     * [BroadcastingPublisher] before the publisher has sent any elements.
     */
    @Test
    fun multipleSubscribers() {
        // Parameters.
        val numSubscribers = 10
        val numElements = 100

        // Setup publisher.
        val publisher = BroadcastingPublisher<Int>(capacity = numElements)

        // Setup all subscribers.
        val allObservations = mutableListOf<List<Int>>()
        val elementsReached = CountDownLatch(numSubscribers)
        val terminalCount = CountDownLatch(numSubscribers)
        val errors = atomic(0)
        repeat(numSubscribers) {
            val observations = mutableListOf<Int>().also { allObservations.add(it) }
            BaseSubscriber<Int>(
                    onNext = {
                        observations += it
                        if (observations.size >= numElements) {
                            elementsReached.countDown()
                        }
                    },
                    onComplete = { terminalCount.countDown() },
                    onError = {
                        errors += 1
                        terminalCount.countDown()
                    }
            ).also { publisher.subscribe(it) }
        }

        // Wait up to some maximum time for all subscribers to be subscribed.
        Assertions
                .assertThat(
                        publisher.waitForSubscription(waitCount = numSubscribers)
                                .asCompletableFuture()
                                .get(5000, TimeUnit.MILLISECONDS)
                )
                .isEqualTo(Unit)

        // Push elements to all subscribers.
        val publications = mutableListOf<Int>()
        (1..numElements).forEach {
            if (publisher.broadcast(it)) {
                publications += it
            } else {
                throw AssertionError("Unexpected publishing condition: Cannot offer next element")
            }
        }

        // Wait up to some maximum time for all subscribers to be done.
        Assertions.assertThat(elementsReached.await(5000, TimeUnit.MILLISECONDS)).isTrue()

        // Shutdown the publisher to signal completion.
        publisher.close()

        // Wait up to some maximum time for all subscribers to be done.
        Assertions.assertThat(terminalCount.await(5000, TimeUnit.MILLISECONDS)).isTrue()

        // Verify observed signals.
        allObservations.forEach {
            Assertions.assertThat(it).isEqualTo(publications)
        }

        // Verify that there were no errors observed.
        Assertions.assertThat(errors.value).isEqualTo(0)
    }

    /**
     * Verify that a subscriber that subscribes to the publisher after the [BroadcastingPublisher]
     * instance has already closed does not receive any signals.
     */
    @Test
    fun noSignalsAfterClose() {
        // Setup publisher.
        val numElements = 10
        val publisher = BroadcastingPublisher<Int>(capacity = numElements)

        // Set up a subscriber that will observe all signals.
        val observations1 = mutableListOf<Int>()
        val elementsReached1 = CountDownLatch(1)
        val terminationReached1 = CountDownLatch(1)
        val lastSeen1 = atomic<Any?>(null)
        BaseSubscriber<Int>(
                onNext = {
                    lastSeen1.value = it
                    observations1 += it
                    if (observations1.size >= numElements) {
                        elementsReached1.countDown()
                    }
                },
                onComplete = { terminationReached1.countDown() },
                onError = {
                    lastSeen1.value = it
                    terminationReached1.countDown()
                }
        ).also { publisher.subscribe(it) }

        // Wait up to some maximum time for all subscribers to be subscribed.
        Assertions
                .assertThat(
                        publisher.waitForSubscription(waitCount = 1)
                                .asCompletableFuture()
                                .get(5000, TimeUnit.MILLISECONDS)
                )
                .isEqualTo(Unit)

        // Push elements to all subscribers.
        val publications = mutableListOf<Int>()
        (1..numElements).forEach {
            if (publisher.broadcast(it)) {
                publications += it
            } else {
                throw AssertionError("Unexpected publishing condition: Cannot offer next element")
            }
        }

        // Wait up to some maximum time for the subscriber to be done.
        Assertions.assertThat(elementsReached1.await(5000, TimeUnit.MILLISECONDS)).isTrue()

        // Shutdown the publisher to signal completion.
        publisher.close()

        // Verify observed signals.
        Assertions.assertThat(terminationReached1.await(5000, TimeUnit.MILLISECONDS))
                .describedAs("Stream did not complete, last signal seen(%s)", lastSeen1.value)
                .isTrue()
        Assertions.assertThat(observations1).isEqualTo(publications)

        // Setup a second subscriber that should receive nothing.
        val observations2 = mutableListOf<Int>()
        val terminationReached2 = CountDownLatch(1)
        val lastSeen2 = atomic<Any?>(null)
        BaseSubscriber<Int>(
                onNext = {
                    lastSeen2.value = it
                    observations2 += it
                },
                onComplete = { terminationReached2.countDown() },
                onError = { lastSeen2.value = it }
        ).also { publisher.subscribe(it) }

        // Verify observed signals.
        Assertions.assertThat(terminationReached2.await(5000, TimeUnit.MILLISECONDS))
                .describedAs("Stream did not complete, last signal seen(%s)", lastSeen2.value)
                .isTrue()
        Assertions.assertThat(observations2).isEmpty()
    }
}
