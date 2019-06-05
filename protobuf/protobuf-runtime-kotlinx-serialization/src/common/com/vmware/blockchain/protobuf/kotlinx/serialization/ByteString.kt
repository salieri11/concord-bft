/* **********************************************************************
 * Copyright 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.protobuf.kotlinx.serialization

import kotlinx.serialization.Decoder
import kotlinx.serialization.Encoder
import kotlinx.serialization.KSerializer
import kotlinx.serialization.Serializable
import kotlinx.serialization.Serializer
import kotlinx.serialization.Transient

/**
 * Value-type implementation that represents an immutable sequence of bytes.
 */
@Serializable(with = ByteString.ByteStringSerializer::class)
open class ByteString(
    val data: ByteArray = EMPTY_ARRAY,
    @Transient val offset: Int = 0,
    @Transient val length: Int = data.size
) : Comparable<ByteString> {

    /**
     * Concrete [KSerializer] implementation for [ByteString].
     */
    @Serializer(forClass = ByteString::class)
    object ByteStringSerializer : KSerializer<ByteString> {

        // Note: Compiler plugin generates 'descriptor' field if not explicitly declared.
        // override val descriptor: SerialDescriptor

        override fun serialize(encoder: Encoder, obj: ByteString) {
            when (encoder) {
                is ProtoBuf.ProtobufWriter -> encoder.encodeTaggedBytes(obj.asByteArray())
                else -> encoder.encodeString(obj.encodeBase64().utf8())
            }
        }

        override fun deserialize(decoder: Decoder): ByteString {
            return when (decoder) {
                is ProtoBuf.ProtobufReader -> of(*decoder.decodeTaggedBytes())
                else -> of(*decoder.decodeString().toByteArray()).decodeBase64()
            }
        }
    }

    @Transient
    @kotlin.jvm.Transient private var hashCode: Int = 0

    companion object {
        @JvmField
        val EMPTY_ARRAY: ByteArray = ByteArray(0)

        @JvmField
        val EMPTY: ByteString = ByteString(EMPTY_ARRAY)

        @JvmStatic
        fun empty(): ByteString = EMPTY

        @JvmStatic
        fun validateSliceRange(
            instance: ByteString,
            offset: Int,
            length: Int
        ): Boolean {
            return offset >= 0 &&
                    length >= 0 &&
                    instance.offset + offset + length <= instance.length
        }

        @JvmStatic
        fun of(vararg data: Byte): ByteString = ByteString(data.copyOf())

        @JvmStatic
        fun of(data: ByteArray, offset: Int, length: Int): ByteString {
            if (length == 0) {
                return empty()
            }

            val copy = data.copyInto(ByteArray(length), 0, offset, offset + length)
            return ByteString(copy)
        }
    }

    override fun hashCode(): Int {
        // If hashCode is uninitialized, then compute it and return the value (race doesn't matter).
        if (hashCode == 0) {
            var result = 1
            for (i in offset until (offset + length)) {
                result = 31 * result + data[i]
            }

            hashCode = result
        }

        return hashCode
    }

    override fun equals(other: Any?): Boolean {
        return when {
            this === other -> true
            other !is ByteString -> false
            length != other.length -> false
            offset == other.offset -> data.contentEquals(other.data)
            else -> contentEquals(other)
        }
    }

    override operator fun compareTo(other: ByteString): Int {
        val size1 = length
        val size2 = other.length
        val size = kotlin.math.min(size1, size2)
        for (i in 0..size) {
            val byteA = data[offset + i].toInt() and 0xFF
            val byteB = other.data[other.offset + i].toInt() and 0xff
            if (byteA == byteB) {
                continue
            }

            return if (byteA < byteB) -1 else 1
        }

        return if (size1 == size2) 0 else (if (size1 < size2) -1 else 1)
    }

    /**
     * Retrieves a [ByteArray] containing the content within this [ByteString] instance.
     *
     * Unlike [toByteArray], the [ByteArray] returned from this call may be referencing the
     * raw buffer. In that case, the caller would end up sharing memory with this [ByteString]
     * instance. Thus, any mutation performed on this data may potentially violate the immutability
     * contract.
     *
     * @return
     *   potentially raw internal [ByteArray] data.
     */
    fun asByteArray(): ByteArray {
        return if (offset == 0 && length == data.size) data else toByteArray()
    }

    /**
     * Retrieves a [ByteArray] containing a copy of the content within this [ByteString].
     *
     * @return
     *   a copy of the internal [ByteArray] data.
     */
    fun toByteArray(): ByteArray {
        return if (offset == 0 && length == data.size) {
            data.copyOf()
        } else {
            /* Return a sub-region-only copy.*/
            data.copyInto(ByteArray(length), 0, offset, offset + length)
        }
    }

    /**
     * Creates a slice of this [ByteString] that represents a sub-region of the original content.
     *
     * @param[offset]
     *   starting offset of the slice.
     * @param[length]
     *   length of the slice.
     *
     * @return
     *   a new instance of [ByteString] representing the sliced data content.
     */
    fun slice(offset: Int, length: Int): ByteString? {
        return if (validateSliceRange(this, offset, length)) {
            if (length == 0) {
                 empty()
            } else {
                ByteString(data, this.offset + offset, length)
            }
        } else {
            null
        }
    }
}
