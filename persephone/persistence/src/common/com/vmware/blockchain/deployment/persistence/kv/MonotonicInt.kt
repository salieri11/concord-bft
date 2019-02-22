/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.persistence.kv

/**
 * A monotonic integer implementation of [KeyValueStore.Version].
 *
 * Note: This version does not guard against 32-bit integer overflow.
 */
data class MonotonicInt(val value: Int = 0) : KeyValueStore.Version<MonotonicInt> {

    init {
        require(value <= Int.MAX_VALUE)
    }

    override fun asByteArray(): ByteArray {
        val bytes = ByteArray(4)

        bytes[0] = value.toByte()
        bytes[1] = (value shr 8 and 0xFF).toByte()
        bytes[2] = (value shr 16 and 0xFF).toByte()
        bytes[3] = (value shr 24 and 0xFF).toByte()

        return bytes
    }

    override fun next(): MonotonicInt = MonotonicInt(value + 1)

    override fun compareTo(other: MonotonicInt): Int {
        return value.compareTo(other.value)
    }
}

/**
 * Convert a 4-byte [ByteArray] to a [MonotonicInt] instance.
 *
 * @return
 *   a new [MonotonicInt] instance.
 */
fun ByteArray.toMonotonicInt(): MonotonicInt {
    require(this.size == 4)

    val value = ((this[3].toInt() and 0xFF) shl 24) +
            ((this[2].toInt() and 0xFF) shl 16) +
            ((this[1].toInt() and 0xFF) shl 8) +
            (this[0].toInt() and 0xFF)

    return MonotonicInt(value)
}
