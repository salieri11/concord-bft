/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.persistence.kv.rocksdb

/**
 * Configuration settings for creating a [RocksDB] instance.
 */
data class RocksDBConfiguration(
    val path: String,
    val writeAheadLogMaxSize: Long = DEFAULT_WAL_MAX_SIZE_MB,
    val subscriptionUpdateInterval: Long = DEFAULT_SUBSCRIPTION_UPDATE_INTERVAL_MS
) {
    companion object {
        /** Maximum size of the write-ahead log file in MB. */
        const val DEFAULT_WAL_MAX_SIZE_MB: Long = 128

        /** Update interval for subscription event stream. */
        const val DEFAULT_SUBSCRIPTION_UPDATE_INTERVAL_MS: Long = 50
    }
}

/**
 * Denote the set of mutable operations executed on a given [RocksDB] instance.
 */
sealed class RocksDBOperation {
    class Put(val key: ByteArray, val value: ByteArray) : RocksDBOperation()
    class Delete(val key: ByteArray) : RocksDBOperation()
}

/**
 * In-Memory context associated with a RocksDB database.
 */
expect class RocksDB

/**
 * Create a new [RocksDB] instance based on a given [RocksDBConfiguration] setting.
 *
 * @param[configuration]
 *   configuration instance.
 *
 * @return
 *   a new [RocksDB] instance.
 */
expect fun newRocksDBInstance(configuration: RocksDBConfiguration): RocksDB

/**
 * Retrieve updates as a sequence of atomic write-update changes to [RocksDB].
 *
 * @param[since]
 *   starting sequence number to iterate from RocksDB's internal transaction log.
 *
 * @return
 *   a [Sequence] of two-tuples of sequence number to the [RocksDBOperation]s pertaining to that atomic
 *   write-update.
 */
expect fun RocksDB.getUpdates(since: Long): Sequence<Pair<Long, List<RocksDBOperation>>>

expect fun RocksDB.getSnapshotState(
    iterableSequence: Boolean = true
): Pair<Long, Sequence<Pair<ByteArray, ByteArray>>>

/**
 * Flush all data to file and perform fsync in the process.
 */
expect fun RocksDB.flush()

/**
 * Retrieve the value of the record associated with a given key.
 *
 * @param[key]
 *   key of the record to lookup.
 *
 * @return
 *   value of the record looked up, or `null` otherwise.
 */
@Suppress("EXTENSION_SHADOWED_BY_MEMBER")
expect operator fun RocksDB.get(key: ByteArray): ByteArray?

/**
 * Set the value of the record associated with a given key.
 *
 * @param[key]
 *   key of the record to set.
 * @param[value]
 *   value to set the record to.
 */
expect operator fun RocksDB.set(key: ByteArray, value: ByteArray)

/**
 * Perform a tombstone-based deletion, which is a batched write of the final value as the tombstone,
 * followed by a record delete.
 *
 * @param[key]
 *   key of the record to delete.
 * @param[finalValue]
 *   value to write as the tombstone value.
 */
expect fun RocksDB.tombstoneDelete(key: ByteArray, finalValue: ByteArray)
