/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.persistence.kv

import com.vmware.blockchain.protobuf.kotlinx.serialization.ByteString
import com.vmware.blockchain.protobuf.kotlinx.serialization.ProtoBuf
import kotlinx.serialization.KSerializer

/**
 * Implementation of [KeyValueStore.Value] that can deserialize into and serialize from a typed
 * object instance of type [T], using Protocol Buffer wire encoding.
 *
 * @param[T]
 *   type of the value when represented in deserialized form.
 * @param[value]
 *   initial input typed instance represented by this [TypedValue] instance.
 * @param[bytes]
 *   initial input binary content represented by this [TypedValue] instance.
 * @property[serializer]
 *   serializer capable of encoding and decoding Protocol Buffer wire encoding.
 * @property[typedValue]
 *   typed instance representation of this [TypedValue] instance.
 * @property[untypedValue]
 *   untyped binary value representation of this [TypedValue] instance.
 */
class TypedValue<T> private constructor(
    private val value: T? = null,
    private val bytes: ByteString? = null,
    private val serializer: KSerializer<T>
) : KeyValueStore.Value {

    private val typedValue: T by lazy {
        value?: ProtoBuf.plain.load(serializer, untypedValue.asByteArray())
    }

    private val untypedValue: ByteString by lazy {
        bytes?: ByteString.of(*ProtoBuf.plain.dump(serializer, typedValue))
    }

    /**
     * Create a [TypedValue] wrapping an instance of [T] as its typed representation.
     *
     * Note: Current implementation does not guard against input mutability. Caller should enforce
     * immutability contract.
     */
    constructor(value: T, serializer: KSerializer<T>): this(value, null, serializer)

    /**
     * Create a [TypedValue] wrapping a [ByteString] as its binary representation.
     */
    constructor(bytes: ByteString, serializer: KSerializer<T>): this(null, bytes, serializer)

    /**
     * Create a [TypedValue] wrapping a [ByteArray] as its binary representation.
     */
    constructor(
        bytes: ByteArray,
        serializer: KSerializer<T>
    ): this(null, ByteString.of(*bytes), serializer)

    override fun equals(other: Any?): Boolean {
        return when (other) {
            this -> true
            is TypedValue<*> -> other.untypedValue.asByteArray()
                    .contentEquals(untypedValue.asByteArray())
            else -> false
        }
    }

    override fun hashCode(): Int {
        return untypedValue.asByteArray().contentHashCode()
    }

    override fun asByteArray(): ByteArray = untypedValue.asByteArray()

    /**
     * Convert the entity to its typed representation.
     */
    fun asTyped(): T = typedValue
}
