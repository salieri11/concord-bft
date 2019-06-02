package com.vmware.blockchain.deployment.persistence.kv

import kotlinx.serialization.KSerializer
import kotlinx.serialization.protobuf.ProtoBuf

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
) : KeyValueStore.Value {

    private val untypedValue: ByteArray by lazy {
        ProtoBuf.plain.dump(serializer, typedValue)
    }

    override fun asByteArray(): ByteArray = untypedValue
}
