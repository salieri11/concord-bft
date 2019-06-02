/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.persistence.kv

import kotlinx.serialization.KSerializer
import kotlinx.serialization.SerialId
import kotlinx.serialization.Serializable
import kotlinx.serialization.protobuf.ProtoBuf

/**
 * A monotonic integer implementation of [KeyValueStore.Version].
 *
 * Note: This version does not guard against 32-bit integer overflow.
 */
@Serializable
data class MonotonicInt(@SerialId(1) val value: Int = 0) : KeyValueStore.Version<MonotonicInt> {

    companion object {
        fun getSerializer(): KSerializer<MonotonicInt> = serializer()
    }

    init {
        require(value <= Int.MAX_VALUE)
    }

    override fun asByteArray(): ByteArray = ProtoBuf.plain.dump(serializer(), this)

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
fun ByteArray.toMonotonicInt(): MonotonicInt = ProtoBuf.plain.load(MonotonicInt.serializer(), this)
