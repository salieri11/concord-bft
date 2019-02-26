/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.persistence.kv

import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Event
import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Value
import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Version
import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore.Versioned
import com.vmware.blockchain.deployment.reactive.ErrorPublisher
import com.vmware.blockchain.deployment.reactive.MappingPublisher
import com.vmware.blockchain.deployment.reactive.Publisher
import kotlinx.serialization.KSerializer
import kotlinx.serialization.protobuf.ProtoBuf

/**
 * A strongly-typed implementation of [KeyValueStore] interface.
 *
 * Note: This implementation is necessarily less-efficient than its untyped counterpart due to the
 * fact that typed entities are always indiscriminately serialized on input request and deserialized
 * on output response. An application that uses untyped version can choose to lazily serialize and
 * deserialize as needed, thereby skipping unnecessary computational churn.
 *
 * @param[K]
 *   type of key used for the key-value entries.
 * @param[V]
 *   type of value used for the key-value entries.
 * @param[T]
 *   subtype of [Version] to use to version the key-value entries.
 */
class TypedKeyValueStore<K, V, T : Version<T>>(
    private val keySerializer: KSerializer<K>,
    private val valueSerializer: KSerializer<V>,
    private val keyValueStore: UntypedKeyValueStore<T> = InMemoryUntypedKeyValueStore()
) : KeyValueStore<K, V, T> {

    /**
     * Wrapper object type denoting a protocol buffer, which is a [ByteArray] encoded in Protocol
     * Buffer wiring encoding.
     *
     * @param[typedValue]
     *   typed value entity to be represented as Protocol Buffer encoded.
     * @param[serializer]
     *   serializer capable of encoding the [typedValue] into Protocol Buffer wire encoding.
     */
    data class ProtocolBuffer<T>(
        private val typedValue: T,
        private val serializer: KSerializer<T>
    ) : Value {

        private val untypedValue: ByteArray by lazy {
            ProtoBuf.plain.dump(serializer, typedValue)
        }

        override fun asByteArray(): ByteArray = untypedValue
    }

    /**
     * Extension function to deserialize the content within a [ByteArray] to a typed value by
     * decoding using Protocol Buffer wire encoding.
     *
     * @param[deserializer]
     *   deserializer capable of decoding an Protocol Buffer encoded [ByteArray] to a typed value.
     */
    private fun <T> ByteArray.toTypedValue(deserializer: KSerializer<T>): T {
        return ProtoBuf.plain.load(deserializer, this)
    }

    /**
     * Shutdown the [UntypedKeyValueStore] instance and closes all resources.
     */
    override fun close() {
        keyValueStore.close()
    }

    override operator fun get(key: K): Publisher<Versioned<V, T>> {
        return try {
            val keyBytes = ProtocolBuffer(key, keySerializer)
            MappingPublisher(keyValueStore[keyBytes]) { element ->
                when (element) {
                    is Versioned.Just ->
                        Versioned.Just(
                                element.value.asByteArray().toTypedValue(valueSerializer),
                                element.version
                        )
                    is Versioned.None -> Versioned.None
                }
            }
        } catch (error: Throwable) {
            // Any execution error results in a short-circuit publisher that publish the error.
            ErrorPublisher(error)
        }
    }

    override fun set(key: K, expected: Version<T>, value: V): Publisher<Versioned<V, T>> {
        return try {
            val keyBytes = ProtocolBuffer(key, keySerializer)
            val valueBytes = ProtocolBuffer(value, valueSerializer)
            MappingPublisher(keyValueStore.set(keyBytes, expected, valueBytes)) { element ->
                when (element) {
                    is Versioned.Just ->
                        Versioned.Just(
                                element.value.asByteArray().toTypedValue(valueSerializer),
                                element.version
                        )
                    is Versioned.None -> Versioned.None
                }
            }
        } catch (error: Throwable) {
            // Any execution error results in a short-circuit publisher that publish the error.
            ErrorPublisher(error)
        }
    }

    override fun delete(key: K, expected: Version<T>): Publisher<Versioned<V, T>> {
        return try {
            val keyBytes = ProtocolBuffer(key, keySerializer)
            MappingPublisher(keyValueStore.delete(keyBytes, expected)) { element ->
                when (element) {
                    is Versioned.Just ->
                        Versioned.Just(
                                element.value.asByteArray().toTypedValue(valueSerializer),
                                element.version
                        )
                    is Versioned.None -> Versioned.None
                }
            }
        } catch (error: Throwable) {
            // Any execution error results in a short-circuit publisher that publish the error.
            ErrorPublisher(error)
        }
    }

    override fun subscribe(capacity: Int, state: Boolean): Publisher<Event<K, V, T>> {
        return try {
            MappingPublisher(keyValueStore.subscribe(capacity, state)) { element ->
                when (element) {
                    is Event.ChangeEvent ->
                        Event.ChangeEvent(
                                element.key.asByteArray().toTypedValue(keySerializer),
                                element.value.asByteArray().toTypedValue(valueSerializer),
                                element.version
                        )
                    is Event.DeleteEvent ->
                        Event.DeleteEvent<K, V, T>(
                                element.key.asByteArray().toTypedValue(keySerializer),
                                element.lastVersion
                        )
                }
            }
        } catch (error: Throwable) {
            // Any execution error results in a short-circuit publisher that publish the error.
            ErrorPublisher(error)
        }
    }

    override fun unsubscribe(eventSink: Publisher<Event<K, V, T>>) {
        return when (eventSink) {
            is MappingPublisher<*, *> ->
                @Suppress("UNCHECKED_CAST")
                keyValueStore.unsubscribe(eventSink.upstream as Publisher<Event<Value, Value, T>>)
            else -> Unit
        }
    }
}
