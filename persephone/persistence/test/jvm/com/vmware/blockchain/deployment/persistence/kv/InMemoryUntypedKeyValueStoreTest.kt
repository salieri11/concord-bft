/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.persistence.kv

import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Event
import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Value
import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Versioned
import com.vmware.blockchain.deployment.reactive.BaseSubscriber
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import org.assertj.core.api.Assertions
import org.junit.jupiter.api.Test

/**
 * Basic functionality test of [InMemoryUntypedKeyValueStore] operations.
 */
class InMemoryUntypedKeyValueStoreTest {

    /**
     * Implementation of [Value] that wraps a [String] value.
     */
    data class StringValue(val value: String) : Value {
        override fun asByteArray(): ByteArray = value.toByteArray(Charsets.UTF_8)
    }

    /**
     * Test CRUD functionality with event-sourcing.
     */
    @Test
    fun crud() {
        // Setup.
        val server = InMemoryUntypedKeyValueStore<MonotonicInt>()

        // Setup the event sink.
        val eventSink1 = server.subscribe(32)
        val eventsDone1 = CountDownLatch(1)
        val observations1 = mutableListOf<Event<Value, Value, MonotonicInt>>()
        BaseSubscriber<Event<Value, Value, MonotonicInt>>(
                onNext = { observations1 += it },
                onComplete = { eventsDone1.countDown() },
                onError = { throw it }
        ).also { eventSink1.subscribe(it) }

        val key = StringValue("key-1")
        val failResult = AssertionError("Result is not set.")
                .let { Result.failure<Versioned<Value, MonotonicInt>>(it) }

        // Retrieve the created entry by no-yet-existent key.
        val emptyGetPublisher = server.get(key)
        var emptyGetResult = failResult
        val emptyGetFinished = CountDownLatch(1)
        BaseSubscriber<Versioned<Value, MonotonicInt>>(
                onNext = { emptyGetResult = Result.success(it) },
                onComplete = { emptyGetFinished.countDown() },
                onError = { throw it }
        ).also { emptyGetPublisher.subscribe(it) }
        Assertions.assertThat(emptyGetFinished.await(100, TimeUnit.MILLISECONDS)).isTrue()
        Assertions.assertThat(emptyGetResult.isSuccess).isTrue()
        emptyGetResult.onSuccess {
            Assertions.assertThat(it is Versioned.None).isTrue()
        }

        // Create a new key-value entry.
        val value1 = StringValue("value-1")
        val initialVersion = MonotonicInt()
        val createPublisher = server.put(key, value1, initialVersion)
        var createResult = failResult
        val createFinished = CountDownLatch(1)
        BaseSubscriber<Versioned<Value, MonotonicInt>>(
                onNext = { createResult = Result.success(it) },
                onComplete = { createFinished.countDown() },
                onError = { throw it }
        ).also { createPublisher.subscribe(it) }
        Assertions.assertThat(createFinished.await(100, TimeUnit.MILLISECONDS)).isTrue()
        Assertions.assertThat(createResult.isSuccess).isTrue()
        createResult.onSuccess {
            Assertions.assertThat(it is Versioned.None).isTrue()
        }

        // Retrieve the created entry by its key.
        val getPublisher = server.get(key)
        var getResult = failResult
        val getFinished = CountDownLatch(1)
        BaseSubscriber<Versioned<Value, MonotonicInt>>(
                onNext = { getResult = Result.success(it) },
                onComplete = { getFinished.countDown() },
                onError = { throw it }
        ).also { getPublisher.subscribe(it) }
        Assertions.assertThat(getFinished.await(100, TimeUnit.MILLISECONDS)).isTrue()
        Assertions.assertThat(getResult.isSuccess).isTrue()
        getResult.onSuccess {
            Assertions.assertThat(it is Versioned.Just).isTrue()
            Assertions.assertThat((it as Versioned.Just).version)
                    .isEqualTo(initialVersion.next()) // First version is initial's next().
        }
        val getVersion = (getResult.getOrThrow() as Versioned.Just<*, MonotonicInt>).version

        // Setup a second event sink (that cannot have observed the first update event).
        val eventSink2 = server.subscribe(32)
        val eventsDone2 = CountDownLatch(1)
        val observations2 = mutableListOf<Event<Value, Value, MonotonicInt>>()
        BaseSubscriber<Event<Value, Value, MonotonicInt>>(
                onNext = { observations2 += it },
                onComplete = { eventsDone2.countDown() },
                onError = { throw it }
        ).also { eventSink2.subscribe(it) }

        // Update the created entry with a new value.
        val value2 = StringValue("value-2")
        val updatePublisher = server.put(key, value2, getVersion)
        var updateResult = failResult
        val updateFinished = CountDownLatch(1)
        BaseSubscriber<Versioned<Value, MonotonicInt>>(
                onNext = { updateResult = Result.success(it) },
                onComplete = { updateFinished.countDown() },
                onError = { throw it }
        ).also { updatePublisher.subscribe(it) }
        Assertions.assertThat(updateFinished.await(100, TimeUnit.MILLISECONDS)).isTrue()
        Assertions.assertThat(updateResult.isSuccess).isTrue()
        updateResult.onSuccess {
            Assertions.assertThat(it is Versioned.Just).isTrue()
            Assertions.assertThat((it as Versioned.Just).version).isEqualTo(getVersion)
        }

        // Retrieve the updated entry by its key.
        val reGetPublisher = server.get(key)
        var reGetResult = failResult
        val reGetFinished = CountDownLatch(1)
        BaseSubscriber<Versioned<Value, MonotonicInt>>(
                onNext = { reGetResult = Result.success(it) },
                onComplete = { reGetFinished.countDown() },
                onError = { throw it }
        ).also { reGetPublisher.subscribe(it) }
        Assertions.assertThat(reGetFinished.await(100, TimeUnit.MILLISECONDS)).isTrue()
        Assertions.assertThat(reGetResult.isSuccess).isTrue()
        reGetResult.onSuccess {
            Assertions.assertThat(it is Versioned.Just).isTrue()
            // Updated version is first get()'s next().
            Assertions.assertThat((it as Versioned.Just).version).isEqualTo(getVersion.next())
        }
        val reGetVersion = (reGetResult.getOrThrow() as Versioned.Just<*, MonotonicInt>).version

        // Setup a third event sink that does not have any subscribers immediately.
        val eventSink3 = server.subscribe(32)

        // Delete the entry by its key.
        val deletePublisher = server.delete(key, reGetVersion)
        var deleteResult = failResult
        val deleteFinished = CountDownLatch(1)
        BaseSubscriber<Versioned<Value, MonotonicInt>>(
                onNext = { deleteResult = Result.success(it) },
                onComplete = { deleteFinished.countDown() },
                onError = { throw it }
        ).also { deletePublisher.subscribe(it) }
        Assertions.assertThat(deleteFinished.await(100, TimeUnit.MILLISECONDS)).isTrue()
        Assertions.assertThat(deleteResult.isSuccess).isTrue()
        deleteResult.onSuccess {
            Assertions.assertThat(it is Versioned.Just).isTrue()
            Assertions.assertThat((it as Versioned.Just).version).isEqualTo(reGetVersion)
        }

        // Retrieve the created entry by no-yet-existent key.
        val deletedGetPublisher = server.get(key)
        var deletedGetResult = failResult
        val deletedGetFinished = CountDownLatch(1)
        BaseSubscriber<Versioned<Value, MonotonicInt>>(
                onNext = { deletedGetResult = Result.success(it) },
                onComplete = { deletedGetFinished.countDown() },
                onError = { throw it }
        ).also { deletedGetPublisher.subscribe(it) }
        Assertions.assertThat(deletedGetFinished.await(100, TimeUnit.MILLISECONDS)).isTrue()
        Assertions.assertThat(deletedGetResult.isSuccess).isTrue()
        deletedGetResult.onSuccess {
            Assertions.assertThat(it is Versioned.None).isTrue()
        }

        // Verify the observations on event sinks.
        Assertions.assertThat(observations1)
                .containsExactly(
                        Event.ChangeEvent(key, value1, getVersion),
                        Event.ChangeEvent(key, value2, reGetVersion),
                        Event.DeleteEvent(key, reGetVersion)
                )
        Assertions.assertThat(observations2)
                .containsExactly(
                        Event.ChangeEvent(key, value2, reGetVersion),
                        Event.DeleteEvent(key, reGetVersion)
                )

        // Orderly close the first publisher.
        server.unsubscribe(eventSink1)
        Assertions.assertThat(eventsDone1.await(10000, TimeUnit.MILLISECONDS)).isTrue()

        // Cleanup.
        server.close()

        // Check that even if second publisher does not unsubscribe, it gets served onComplete().
        Assertions.assertThat(eventsDone2.await(100, TimeUnit.MILLISECONDS)).isTrue()

        // Check that a late subscriber after server close does not wait indefinitely.
        val eventsDone3 = CountDownLatch(1)
        val observations3 = mutableListOf<Event<Value, Value, MonotonicInt>>()
        BaseSubscriber<Event<Value, Value, MonotonicInt>>(
                onNext = { observations3 += it },
                onComplete = { eventsDone3.countDown() },
                onError = { throw it }
        ).also { eventSink3.subscribe(it) }
        Assertions.assertThat(eventsDone3.await(100, TimeUnit.MILLISECONDS)).isTrue()
        Assertions.assertThat(observations3).isEmpty()

        // Check that event subscription after server close does not result in event emissions.
        val eventSink4 = server.subscribe(32)
        val eventsDone4 = CountDownLatch(1)
        val observations4 = mutableListOf<Event<Value, Value, MonotonicInt>>()
        BaseSubscriber<Event<Value, Value, MonotonicInt>>(
                onNext = { observations4 += it },
                onComplete = { throw AssertionError("Should not receive onComplete() signal.") },
                onError = { eventsDone4.countDown() }
        ).also { eventSink4.subscribe(it) }
        Assertions.assertThat(eventsDone4.await(100, TimeUnit.MILLISECONDS)).isTrue()
        Assertions.assertThat(observations4).isEmpty()
    }
}
