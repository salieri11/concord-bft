/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.persistence.kv.rocksdb

import com.vmware.blockchain.deployment.logging.debug
import com.vmware.blockchain.deployment.logging.info
import com.vmware.blockchain.deployment.logging.logger
import com.vmware.blockchain.deployment.persistence.kv.AbstractUntypedKeyValueStore
import com.vmware.blockchain.deployment.persistence.kv.InterruptedEventStreamException
import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Event
import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Value
import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Version
import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Versioned
import com.vmware.blockchain.deployment.persistence.kv.TypedValue
import com.vmware.blockchain.deployment.persistence.kv.UntypedKeyValueStore
import com.vmware.blockchain.deployment.persistence.kv.UntypedKeyValueStore.Request
import com.vmware.blockchain.deployment.persistence.kv.UntypedKeyValueStore.Response
import com.vmware.blockchain.deployment.persistence.kv.UntypedValue
import com.vmware.blockchain.deployment.persistence.kv.VersionMismatchException
import com.vmware.blockchain.deployment.persistence.kv.toTypedValue
import com.vmware.blockchain.deployment.persistence.kv.toUntypedValue
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.channels.SendChannel
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.delay
import kotlinx.coroutines.isActive
import kotlinx.coroutines.joinAll
import kotlinx.coroutines.launch
import kotlinx.serialization.KSerializer
import kotlinx.serialization.SerialId
import kotlinx.serialization.Serializable
import kotlin.coroutines.CoroutineContext

/**
 * Implementation of [UntypedKeyValueStore] interface with RocksDB as backing store.
 *
 * Note: This implementation assumes that a given instance has exclusive global write access to the
 * underlying mapped data file.
 *
 * @param[T]
 *   subtype of [Version] to use to version the key-value entries.
 * @param[context]
 *   coroutine context to use as basis for underlying coroutine-based operations.
 */
class RocksDBUntypedKeyValueStore<T : Version<T>>(
    context: CoroutineContext = Dispatchers.Default,
    versionSerializer: KSerializer<T>,
    private val configuration: RocksDBConfiguration
) : AbstractUntypedKeyValueStore<T>(context, versionSerializer) {

    /**
     * A [Serializable] tuple container of two [UntypedValue] instances representing a [Versioned]
     * key-value store value.
     *
     * @property[version]
     *   serialized binary content of version.
     * @property[value]
     *   serialized binary content of value.
     * @property[tombstone]
     *   indicator that this instance is the final version (prior to deletion).
     */
    @Serializable
    data class VersionedValue(
        @SerialId(1) val version: UntypedValue,
        @SerialId(2) val value: UntypedValue,
        @SerialId(3) val tombstone: Boolean = false
    )

    /** Logging instance. */
    private val log by logger()

    /**
     * An actor coroutine that maintains internal RocksDB versioned key-value storage as state.
     *
     * Note: By design this function should be invoked at most once for the lifetime of an instance.
     */
    override fun newStorageServer(): Channel<Request<T>> {
        // Request channel to send to the storage server.
        val requestChannel = Channel<Request<T>>(Channel.RENDEZVOUS)

        launch(coroutineContext) {
            // Setup DB environment.
            newRocksDBInstance(configuration).use { db ->
                // To track all event subscriptions.
                val eventSinks = mutableMapOf<Channel<Event<Value, Value, T>>, Job>()

                // Loop until the channel closes.
                for (message in requestChannel) {
                    when (message) {
                        is Request.Get<T> -> handleGet(message, db)
                        is Request.Set<T> -> handleSet(message, db)
                        is Request.Delete<T> -> handleDelete(message, db)
                        is Request.Subscribe<T> -> handleSubscribe(message, db, eventSinks)
                        is Request.Unsubscribe<T> -> {
                            // Deregister from tracking map and cancel the coroutine subscription.
                            eventSinks.remove(message.subscription)?.cancel()
                        }
                    }
                }

                coroutineScope {
                    // Sync the WAL and flush all writes before closing the instance.
                    // Note: The suspend function inherits coroutine context but not the job handle.
                    // (i.e. cancellation propagation on the main job does not affect the flush()).
                    db.flush()

                    // Wait for all subscription coroutines to cancel and join (to avoid native
                    // memory still held while RocksDB instance already closed, which would lead to
                    // JVM crashing with SEGV).
                    eventSinks.values.apply {
                        forEach { it.cancel() }
                        joinAll()
                    }
                }
            }
        }

        return requestChannel
    }

    /**
     * Extension property to [VersionedValue] to allow conversion of the untyped
     * [VersionedValue.version] content to a typed instance of [T].
     */
    private val VersionedValue.typedVersion: T
        get() = version.toTypedValue(versionSerializer).asTyped()

    /**
     * Handler for [Request.Get] messages.
     *
     * @param[message]
     *   request message.
     * @param[db]
     *   RocksDB context.
     */
    private suspend fun handleGet(message: Request.Get<T>, db: RocksDB) {
        try {
            val response = db[message.key.asByteArray()]
                    ?.let {
                        val versioned = TypedValue(it, VersionedValue.serializer()).asTyped()
                        val version = TypedValue(versioned.version, versionSerializer).asTyped()

                        Response.Get(Versioned.Just(versioned.value, version))
                    }
                    ?: Response.Get<T>(Versioned.None)

            // Send the response back.
            message.response.send(response)
        } catch (error: Throwable) {
            message.response.close(error)
        }
    }

    /**
     * Handler for [Request.Set] messages.
     *
     * @param[message]
     *   request message.
     * @param[db]
     *   RocksDB context.
     */
    private suspend fun handleSet(message: Request.Set<T>, db: RocksDB) {
        try {
            val existing = db[message.key.asByteArray()]
                    ?.let { TypedValue(it, VersionedValue.serializer()).asTyped() }
            val response: Response<T> = when (val existingVersion = existing?.typedVersion) {
                null, message.expected -> {
                    val nextVersion = TypedValue(message.expected.next(), versionSerializer)
                    val versioned = TypedValue(
                            VersionedValue(
                                    version = nextVersion.toUntypedValue(),
                                    value = UntypedValue(message.value.asByteArray())
                            ),
                            VersionedValue.serializer()
                    )

                    // Store the value.
                    db[message.key.asByteArray()] = versioned.asByteArray()

                    // Response is set to none since prior value does not exist.
                    existingVersion
                            ?.let { Response.Set(Versioned.Just(existing.value, existingVersion)) }
                            ?: Response.Set(Versioned.None)
                }
                else -> Response.Error(VersionMismatchException(message.expected, existingVersion))
            }

            // Send the response back.
            message.response.send(response)
        } catch (error: Throwable) {
            message.response.close(error)
        }
    }

    /**
     * Handler for [Request.Delete] messages.
     *
     * @param[message]
     *   request message.
     * @param[db]
     *   RocksDB context.
     */
    private suspend fun handleDelete(message: Request.Delete<T>, db: RocksDB) {
        try {
            val existing = db[message.key.asByteArray()]
                    ?.let { TypedValue(it, VersionedValue.serializer()).asTyped() }
            val response: Response<T> = when (val existingVersion = existing?.typedVersion) {
                null -> Response.Delete(Versioned.None)
                message.expected -> {
                    // Delete the value.
                    val finalValue = TypedValue(
                            existing.copy(tombstone = true),
                            VersionedValue.serializer()
                    ).asByteArray()
                    db.tombstoneDelete(message.key.asByteArray(), finalValue)

                    // Response is set to none since prior value does not exist.
                    Response.Delete(Versioned.Just(existing.value, existingVersion))
                }
                else -> Response.Error(VersionMismatchException(message.expected, existingVersion))
            }

            // Send the response back.
            message.response.send(response)
        } catch (error: Throwable) {
            message.response.close(error)
        }
    }

    /**
     * Handler for [Request.Subscribe] messages.
     *
     * @param[message]
     *   request message.
     * @param[db]
     *   RocksDB context.
     * @param[eventSinks]
     *   collection of active event sinks.
     */
    private suspend fun handleSubscribe(
        message: Request.Subscribe<T>,
        db: RocksDB,
        eventSinks: MutableMap<Channel<Event<Value, Value, T>>, Job>
    ) {
        // Create an event sink for this subscription.
        val eventSink = Channel<Event<Value, Value, T>>(Channel.UNLIMITED)

        // Spawn coroutine to periodically poll for updates.
        val task = launch(coroutineContext) {
            // If history was requested, stream back snapshot as a reconstructed series
            // of mutation events, otherwise just set the starting sequence number.
            val (snapshotSequence, snapshot) = db.getSnapshotState(message.state)

            // Just loop through the "sequence", if state was not requested, the sequence is empty.
            for ((key, value) in snapshot) {
                val versioned = TypedValue(value, VersionedValue.serializer())
                        .asTyped()
                val event = Event.ChangeEvent<Value, Value, T>(
                        UntypedValue(key),
                        versioned.value,
                        TypedValue(versioned.version, versionSerializer).asTyped()
                )

                offerEvent(eventSink, event)
            }

            // Set event sink to start update at current snapshot (sequence number).
            var lastSequence = snapshotSequence

            log.info { "Event sink($eventSink) created at snapshot sequence($lastSequence)" }

            while (isActive) {
                val updates = db.getUpdates(lastSequence)

                // Go through every atomic write-batch in the sequence
                for ((sequence, operations) in updates) {
                    if (sequence > lastSequence) {
                        // Go through every operation in the atomic write-batch.
                        for (operation in operations) {
                            when (operation) {
                                is RocksDBOperation.Put -> offerEvent(eventSink, operation.toEvent())
                                is RocksDBOperation.Delete -> Unit // Nothing to do.
                            }

                            log.debug { "Event update($sequence / ${operation::class.simpleName})" }
                        }

                        // Update the last known sequence number.
                        lastSequence = sequence
                    }
                }

                // Wait for next poll iteration and update the
                delay(configuration.subscriptionUpdateInterval)
            }
        }

        // Register to tracking map and setup its eventual de-registration.
        eventSinks[eventSink] = task
        task.invokeOnCompletion {
            log.debug { "Event sink($eventSink) completed / removed"}
        }

        // Send the response back.
        val responseMessage = Response.Subscribe(eventSink)
        message.response.send(responseMessage)
    }

    /**
     * Convert an [RocksDBOperation.Put] instance to an [Event] instance.
     */
    private fun RocksDBOperation.Put.toEvent(): Event<Value, Value, T> {
        val versioned = TypedValue(value, VersionedValue.serializer()).asTyped()

        return if (versioned.tombstone) {
            Event.DeleteEvent(
                    UntypedValue(key),
                    TypedValue(versioned.version, versionSerializer).asTyped()
            )
        } else {
            Event.ChangeEvent(
                    UntypedValue(key),
                    versioned.value,
                    TypedValue(versioned.version, versionSerializer).asTyped()
            )
        }
    }

    /**
     * Dispatch an [Event] to a given [SendChannel].
     *
     * @param[sink]
     *   channel to dispatch event to.
     * @param[event]
     *   event to be dispatched.
     */
    private fun offerEvent(
        sink: SendChannel<Event<Value, Value, T>>,
        event: Event<Value, Value, T>
    ) {
        if (!sink.offer(event)) {
            // The event stream requires continuity once started, failing to send an element would
            // constitute a lossy stream.
            // For now, terminating the send channel would ensure the assumption on consumption side
            // is still upheld.
            sink.close(InterruptedEventStreamException)
        }
    }
}
