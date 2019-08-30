/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.persistence.kv

import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Event
import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Value
import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Version
import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Versioned
import com.vmware.blockchain.deployment.reactive.BaseSubscriber
import com.vmware.blockchain.deployment.reactive.Publisher
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicReference
import org.assertj.core.api.Assertions

/**
 * Driver for basic functionality test of [UntypedKeyValueStore] operations.
 */
open class UntypedKeyValueStoreTestDriver {

    companion object {
        /** Default await-time value in milliseconds. */
        const val awaitTime: Long = 60000
    }

    /**
     * Implementation of [Value] that wraps a [String] value.
     */
    data class StringValue(val value: String) : Value {
        override fun asByteArray(): ByteArray = value.toByteArray(Charsets.UTF_8)
    }

    /**
     * Convert an arbitrary [Value] instance to a [StringValue] with equivalent byte content.
     */
    private fun Value.toStringValue(): StringValue {
        return StringValue(String(asByteArray(), Charsets.UTF_8))
    }

    private fun <T> Event<Value, Value, T>.toStringValueEvent(): Event<StringValue, StringValue, T> {
        return when (this) {
            is Event.ChangeEvent<Value, Value, T> -> {
                Event.ChangeEvent(key.toStringValue(), value.toStringValue(), version)
            }
            is Event.DeleteEvent -> {
                Event.DeleteEvent(key.toStringValue(), lastVersion)
            }
        }
    }

    /**
     * Subscribe to a given [Publisher] and retrieve the last element signal as the return value.
     *
     * @param[publisher]
     *   publisher to retrieve the last emitted signal from.
     *
     * @return
     *   the last `onNext` signal emitted from the subscription.
     */
    private fun <T : Version<T>> resultFrom(
        publisher: Publisher<Versioned<Value, T>>
    ): Versioned<Value, T> {
        var result = AssertionError("Result is not set.")
                .let { Result.failure<Versioned<Value, T>>(it) }
        val finished = CountDownLatch(1)
        BaseSubscriber<Versioned<Value, T>>(
                onNext = { result = Result.success(it) },
                onComplete = { finished.countDown() },
                onError = { result = Result.failure(it); finished.countDown() }
        ).also { publisher.subscribe(it) }

        Assertions.assertThat(finished.await(awaitTime, TimeUnit.MILLISECONDS)).isTrue()
        Assertions.assertThat(result.isSuccess).isTrue()

        return result.getOrThrow()
    }

    /**
     * Test CRUD functionality with event-sourcing.
     */
    fun testDriver_crud(server: UntypedKeyValueStore<MonotonicInt>) {
        // Setup the event sink.
        val eventSink1 = server.subscribe(32, false)
        val eventsDone1 = CountDownLatch(1)
        val lastSeen1 = AtomicReference<Any>(null)
        val observations1 = mutableListOf<Event<Value, Value, MonotonicInt>>()
        BaseSubscriber<Event<Value, Value, MonotonicInt>>(
                onSubscribe = { it.apply { request(Long.MAX_VALUE) }.run { lastSeen1.set(this) } },
                onNext = { it.apply { observations1 += this }.run { lastSeen1.set(this) } },
                onComplete = { eventsDone1.countDown() },
                onError = { lastSeen1.set(it) }
        ).also { eventSink1.subscribe(it) }

        // Retrieve the created entry by no-yet-existent key.
        val key = StringValue("key-1")
        Assertions.assertThat(resultFrom(server[key])).isEqualTo(Versioned.None)

        // Create a new key-value entry.
        val value1 = StringValue("value-1")
        val initialVersion = MonotonicInt()
        Assertions.assertThat(resultFrom(server.set(key, initialVersion, value1)))
                .isEqualTo(Versioned.None)

        // Setup a second event sink (that cannot have observed the first update event).
        val eventSink2 = server.subscribe(32, false)
        val eventsDone2 = CountDownLatch(1)
        val lastSeen2 = AtomicReference<Any>(null)
        val observations2 = mutableListOf<Event<Value, Value, MonotonicInt>>()
        BaseSubscriber<Event<Value, Value, MonotonicInt>>(
                onSubscribe = { it.apply { request(Long.MAX_VALUE) }.run { lastSeen2.set(this) } },
                onNext = { it.apply { observations2 += this }.run { lastSeen2.set(this) } },
                onComplete = { eventsDone2.countDown() },
                onError = { lastSeen2.set(it) }
        ).also { eventSink2.subscribe(it) }

        // Retrieve the created entry by its key.
        Assertions.assertThat(resultFrom(server[key]))
                .matches { it is Versioned.Just }
                .matches { (it as Versioned.Just).version == initialVersion.next() }
        val getVersion = initialVersion.next()

        // Update the created entry with a new value.
        val value2 = StringValue("value-2")
        Assertions.assertThat(resultFrom(server.set(key, getVersion, value2)))
                .matches { it is Versioned.Just }
                .matches { (it as Versioned.Just).version == getVersion }

        // Retrieve the updated entry by its key.
        Assertions.assertThat(resultFrom(server[key]))
                .matches { it is Versioned.Just }
                .matches { (it as Versioned.Just).version == getVersion.next() }
        val reGetVersion = getVersion.next()

        // Setup a third event sink that does not have any subscribers immediately.
        val eventSink3 = server.subscribe(32, false)

        // Delete the entry by its key.
        Assertions.assertThat(resultFrom(server.delete(key, reGetVersion)))
                .matches { it is Versioned.Just }
                .matches { (it as Versioned.Just).version == reGetVersion }

        // Retrieve the created entry by now-non-existent key.
        Assertions.assertThat(resultFrom(server[key]))
                .isEqualTo(Versioned.None)

        // Orderly close the first publisher.
        server.unsubscribe(eventSink1)
        Assertions.assertThat(eventsDone1.await(awaitTime, TimeUnit.MILLISECONDS))
                .describedAs("Event stream did not complete, last signal seen(%s)", lastSeen1.get())
                .isTrue()

        // Verify the observations on event sinks.
        val potentialObservations1 = listOf<Event<StringValue, StringValue, MonotonicInt>>(
                Event.ChangeEvent(key, value1, getVersion),
                Event.ChangeEvent(key, value2, reGetVersion),
                Event.DeleteEvent(key, reGetVersion)
        )
        if (observations1.isNotEmpty()) {
            Assertions.assertThat(potentialObservations1)
                    .containsSequence(observations1.map { it.toStringValueEvent() })
        }

        // Cleanup.
        server.close()

        // Check that even if second publisher does not unsubscribe, it gets served onComplete().
        Assertions.assertThat(eventsDone2.await(awaitTime, TimeUnit.MILLISECONDS))
                .describedAs("Event stream did not complete, last signal seen(%s)", lastSeen2.get())
                .isTrue()

        // Verify the observations on event sinks.
        val potentialObservations2 = listOf<Event<StringValue, StringValue, MonotonicInt>>(
                Event.ChangeEvent(key, value2, reGetVersion),
                Event.DeleteEvent(key, reGetVersion)
        )
        if (observations2.isNotEmpty()) {
            Assertions.assertThat(potentialObservations2)
                    .containsSequence(observations2.map { it.toStringValueEvent() })
        }

        // Check that a late subscriber after server close does not wait indefinitely.
        val eventsDone3 = CountDownLatch(1)
        val lastSeen3 = AtomicReference<Any>(null)
        val observations3 = mutableListOf<Event<Value, Value, MonotonicInt>>()
        BaseSubscriber<Event<Value, Value, MonotonicInt>>(
                onSubscribe = { it.apply { request(Long.MAX_VALUE) }.run { lastSeen3.set(this) } },
                onNext = { it.apply { observations3 += this }.run { lastSeen3.set(this) } },
                onComplete = { eventsDone3.countDown() },
                onError = { lastSeen3.set(it) }
        ).also { eventSink3.subscribe(it) }
        Assertions.assertThat(eventsDone3.await(awaitTime, TimeUnit.MILLISECONDS))
                .describedAs("Event stream did not complete, last signal seen(%s)", lastSeen3.get())
                .isTrue()
        Assertions.assertThat(observations3).isEmpty()

        // Check that event subscription after server close does not result in event emissions.
        val eventSink4 = server.subscribe(32, false)
        val eventsDone4 = CountDownLatch(1)
        val lastSeen4 = AtomicReference<Any>(null)
        val observations4 = mutableListOf<Event<Value, Value, MonotonicInt>>()
        BaseSubscriber<Event<Value, Value, MonotonicInt>>(
                onSubscribe = { it.apply { request(Long.MAX_VALUE) }.run { lastSeen4.set(this) } },
                onNext = { it.apply { observations4 += this }.run { lastSeen4.set(this) } },
                onComplete = {
                    lastSeen4.set(AssertionError("Should not receive onComplete() signal."))
                },
                onError = { eventsDone4.countDown() }
        ).also { eventSink4.subscribe(it) }
        Assertions.assertThat(eventsDone4.await(awaitTime, TimeUnit.MILLISECONDS))
                .describedAs("Event stream not end in error, last signal seen(%s)", lastSeen4.get())
                .isTrue()
        Assertions.assertThat(observations4).isEmpty()
    }

    /**
     * Test that new subscribers requesting prior state can recall a settled state barring
     * additional mutations to the store.
     */
    fun testDriver_eventSourceWithPriorState(server: UntypedKeyValueStore<MonotonicInt>) {
        // Setup.
        val expectedEntrySize = 100
        val inputs = (1..expectedEntrySize)
                .map { StringValue("key-$it") to StringValue("value-$it") }

        // Populate the store with a set of entries.
        val expectedStoreState = mutableMapOf<Value, Value>()
        (0 until expectedEntrySize).forEach {
            val (key, value) = inputs[it]
            val version = MonotonicInt()

            // Insert the entry.
            Assertions.assertThat(resultFrom(server.set(key, version, value)))
                    .isEqualTo(Versioned.None)

            // Update the expectation result.
            expectedStoreState[key] = value
        }

        // Setup the second event sink that obtains state history.
        // Note: The capacity of the buffer is set to be the number of items in the store since
        // the server adapter as of now has no concept of back pressure and will burst to client.
        val eventSink = server.subscribe(expectedEntrySize, true)
        val eventsDone = CountDownLatch(1)
        val expectedSizeReached = CountDownLatch(1)
        val observedStoreState = mutableMapOf<Value, Value>()
        val observations = mutableListOf<Event<Value, Value, MonotonicInt>>()
        val error = AtomicReference<Throwable>()
        BaseSubscriber<Event<Value, Value, MonotonicInt>>(
                onNext = {
                    observations += it
                    when (it) {
                        is Event.ChangeEvent<Value, Value, MonotonicInt> ->
                            observedStoreState[it.key] = it.value
                        is Event.DeleteEvent<Value, Value, MonotonicInt> ->
                            observedStoreState.remove(it.key)
                    }

                    if (observedStoreState.size == expectedEntrySize) {
                        expectedSizeReached.countDown()
                    }
                },
                onComplete = { eventsDone.countDown() },
                onError = { error.set(it) }
        ).also { eventSink.subscribe(it) }
        Assertions.assertThat(expectedSizeReached.await(awaitTime, TimeUnit.MILLISECONDS)).isTrue()

        // Cleanup.
        server.close()

        // Verify that event streams terminate.
        Assertions.assertThat(eventsDone.await(awaitTime, TimeUnit.MILLISECONDS))
                .describedAs("Event stream not terminate, onError(%s)", error.get())
                .isTrue()

        // Verify that snapshot state is fully received.
        val observedStateAsBytes = observedStoreState
                .mapKeys { String(it.key.asByteArray()) }
                .mapValues { it.value.asByteArray() }
        val expectedStateAsBytes = expectedStoreState
                .mapKeys { String(it.key.asByteArray()) }
                .mapValues { it.value.asByteArray() }
        Assertions.assertThat(observedStateAsBytes).hasSameSizeAs(expectedStateAsBytes)
        expectedStateAsBytes.forEach {
            val value = observedStateAsBytes[it.key]
            Assertions.assertThat(value).isNotNull()
            Assertions.assertThat(value!!).isEqualTo(it.value)
        }
    }
}
