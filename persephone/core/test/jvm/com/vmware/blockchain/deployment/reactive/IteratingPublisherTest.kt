/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.reactive

import java.util.concurrent.CompletableFuture
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import org.assertj.core.api.Assertions
import org.junit.jupiter.api.Test

/**
 * Basic functionality test of [IteratingPublisher] operations.
 */
class IteratingPublisherTest {

    /**
     * Verify that various scenarios pertaining to any single subscriber upholds the subscription
     * contract.
     */
    @Test
    fun singleSubscriber() {
        val data = 1..1000

        val publisher = IteratingPublisher(data)
        // Create a new subscriber requesting unlimited demand.
        val observations = mutableListOf<Int>()
        val fullLatch = CountDownLatch(1)
        BaseSubscriber<Int>(
                onNext = { observations += it },
                onComplete = { fullLatch.countDown() }
        ).also { publisher.subscribe(it) }

        // Wait up to some maximum time for all elements to be signaled.
        Assertions.assertThat(fullLatch.await(500, TimeUnit.MILLISECONDS)).isEqualTo(true)

        // Verify all observations.
        Assertions.assertThat(observations).isEqualTo(data.toList())

        // Create a new subscriber requesting partial demand.
        val demand = (data.last() / 2)
        val partialObservations = mutableListOf<Int>()
        val partialLatch = CountDownLatch(1)
        BaseSubscriber<Int>(
                onSubscribe = { it.request(demand.toLong()) },
                onNext = {
                    partialObservations += it
                    if (it == demand) {
                        partialLatch.countDown()
                    }
                }
        ).also { publisher.subscribe(it) }

        // Wait up to some maximum time for all elements to be signaled.
        Assertions.assertThat(partialLatch.await(500, TimeUnit.MILLISECONDS)).isEqualTo(true)

        // Verify all observations.
        Assertions.assertThat(partialObservations).isEqualTo((data.first()..demand).toList())
    }

    /**
     * Verify that multiple subscribers receive signals as they would a single-subscriber scenario
     * with each respective subscriber.
     */
    @Test
    fun multipleSubscribers() {
        val data = 1..1000
        val publisher = IteratingPublisher(data)

        val numSubscribers = 10
        val allObservations = mutableListOf<List<Int>>()
        val latch = CountDownLatch(numSubscribers)
        repeat(numSubscribers) {
            val observations = mutableListOf<Int>().also { allObservations.add(it) }
            BaseSubscriber<Int>(
                    onNext = { observations += it },
                    onComplete = { latch.countDown() }
            ).also { publisher.subscribe(it) }
        }

        // Wait up to some maximum time for all subscribers to be done.
        Assertions.assertThat(latch.await(500, TimeUnit.MILLISECONDS)).isEqualTo(true)

        // Verify all observations.
        allObservations.forEach { Assertions.assertThat(it).isEqualTo(data.toList()) }
    }

    /**
     * Verify that concurrent subscribers receive signals as they would a single-subscriber scenario
     * with each respective subscriber.
     */
    @Test
    fun concurrentSubscriptionRequests() {
        val data = 1..1000
        val publisher = IteratingPublisher(data)

        val numSubscribers = 10
        val allObservations = mutableListOf<List<Int>>()
        val latch = CountDownLatch(numSubscribers)
        val subscribers = (1..numSubscribers).map {
            val observations = mutableListOf<Int>().also { allObservations.add(it) }
            BaseSubscriber<Int>(
                    onSubscribe = { },
                    onNext = { observations += it },
                    onComplete = { latch.countDown() }
            ).also { publisher.subscribe(it) }
        }

        // Generate demand from different threads (to approximate simultaneous requests).
        subscribers.forEach { subscriber ->
            CompletableFuture.runAsync { subscriber.request(Long.MAX_VALUE) }
        }

        // Wait up to some maximum time for all subscribers to be done.
        Assertions.assertThat(latch.await(500, TimeUnit.MILLISECONDS)).isEqualTo(true)

        // Verify all observations.
        allObservations.forEach { Assertions.assertThat(it).isEqualTo(data.toList()) }
    }

    /**
     * Verify that generating demand on a closed publisher results in onComplete() being signaled.
     *
     * Note: This is different from subscriber cancelling a subscription, which Reactive Streams
     * specification has strict contractual requirements. This is testing whether the iterating
     * publisher implementation does the internal cancellation properly.
     */
    @Test
    fun publisherClosure() {
        val data = 1..1000
        val publisher = IteratingPublisher(data)

        // Create a new subscriber requesting no initial demand.
        val observations = mutableListOf<Int>()
        val latch = CountDownLatch(1)
        val subscriber = BaseSubscriber<Int>(
                onSubscribe = { },
                onNext = { observations += it },
                onComplete = {
                    latch.countDown()
                }
        ).also { publisher.subscribe(it) }

        // Stop the publisher.
        publisher.close()

        // Generate demand from the subscriber (after publisher has closed).
        subscriber.request(Long.MAX_VALUE)

        // Wait up to some maximum time for all subscribers to be done.
        Assertions.assertThat(latch.await(500, TimeUnit.MILLISECONDS)).isEqualTo(true)

        // Verify observed signals on subscriber.
        Assertions.assertThat(observations).isEmpty()
    }
}