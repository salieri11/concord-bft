/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.persistence.kv.rocksdb

import com.vmware.blockchain.deployment.persistence.kv.ServerInitializationException
import org.rocksdb.FlushOptions
import org.rocksdb.Options
import org.rocksdb.ReadOptions
import org.rocksdb.RocksDBException
import org.rocksdb.WriteBatch
import org.rocksdb.WriteOptions

actual typealias RocksDB = org.rocksdb.RocksDB

actual fun newRocksDBInstance(configuration: RocksDBConfiguration): RocksDB {
    try {
        // Static initialization if not done already.
        RocksDB.loadLibrary()

        // Set options according to configuration setting.
        val options = Options()
                .setCreateIfMissing(true)
                .setWalSizeLimitMB(configuration.writeAheadLogMaxSize)

        return RocksDB.open(options, configuration.path)
    } catch (error: RocksDBException) {
        throw ServerInitializationException
    }
}

actual fun RocksDB.getUpdates(since: Long): Sequence<Pair<Long, List<RocksDBOperation>>> {
    return sequence {
        val iterator = getUpdatesSince(since)
        while (iterator.isValid) {
            iterator.batch.apply {
                CollectingWriteBatchHandler().use { handler ->
                    writeBatch().use { it.iterate(handler) }
                    yield(sequenceNumber() to handler.operations)
                }
            }

            iterator.next()
        }

        // Explicitly close the iterator resource when done.
        iterator.close()
    }
}

actual fun RocksDB.getSnapshotState(
    iterableSequence: Boolean
): Pair<Long, Sequence<Pair<ByteArray, ByteArray>>> {
    val currentSnapshot = snapshot
    val sequenceNumber = currentSnapshot.sequenceNumber

    val sequence = if (iterableSequence) {
        sequence {
            val iterator = newIterator(ReadOptions().setSnapshot(currentSnapshot))
                    .apply { seekToFirst() }

            while (iterator.isValid) {
                yield(iterator.key() to iterator.value())

                iterator.next()
            }

            // Explicitly close the iterator resource when done.
            iterator.close()

            // Release the snapshot.
            releaseSnapshot(currentSnapshot)
        }
    } else {
        // Release the snapshot (since there is no iterator dependent on the snapshot).
        releaseSnapshot(currentSnapshot)

        emptySequence()
    }

    return sequenceNumber to sequence
}

actual fun RocksDB.flush() {
    flush(FlushOptions().setWaitForFlush(true))
}

@Suppress("EXTENSION_SHADOWED_BY_MEMBER")
actual operator fun RocksDB.get(key: ByteArray): ByteArray? = get(key)

actual operator fun RocksDB.set(key: ByteArray, value: ByteArray) = put(key, value)

actual fun RocksDB.tombstoneDelete(key: ByteArray, finalValue: ByteArray) {
    // Note: WriteBatch is AutoCloseable, hence the `.use()` idiom.
    WriteBatch().use { batch ->
        batch.put(key, finalValue)
        batch.delete(key)

        write(WriteOptions(), batch)
    }
}

/**
 * Concrete implementation of [WriteBatch.Handler] that collects the updates received from handler
 * callbacks to a list.
 */
class CollectingWriteBatchHandler : WriteBatch.Handler() {

    /** Collection of updates received from [WriteBatch.Handler] callbacks. */
    val operations: List<RocksDBOperation> get() = updates

    /** Internal mutable collection of updates received from [WriteBatch.Handler] callbacks. */
    private val updates: MutableList<RocksDBOperation> = mutableListOf()

    init {
        // Initialize the underlying RocksDB handle (native code).
        initializeNative()
    }

    override fun put(key: ByteArray, value: ByteArray) {
        updates += RocksDBOperation.Put(key, value)
    }

    override fun put(columnFamilyId: Int, key: ByteArray, value: ByteArray) {
        updates += RocksDBOperation.Put(key, value)
    }

    override fun delete(key: ByteArray) {
        updates += RocksDBOperation.Delete(key)
    }

    override fun delete(columnFamilyId: Int, key: ByteArray) {
        updates += RocksDBOperation.Delete(key)
    }

    override fun markNoop(emptyBatch: Boolean) {}
    override fun merge(columnFamilyId: Int, key: ByteArray, value: ByteArray) {}
    override fun merge(key: ByteArray, value: ByteArray) {}
    override fun singleDelete(columnFamilyId: Int, key: ByteArray) {}
    override fun singleDelete(key: ByteArray) {}
    override fun logData(blob: ByteArray) {}
    override fun markBeginPrepare() {}
    override fun markEndPrepare(xid: ByteArray) {}
    override fun markCommit(xid: ByteArray) {}
    override fun putBlobIndex(columnFamilyId: Int, key: ByteArray, value: ByteArray) {}
    override fun deleteRange(columnFamilyId: Int, beginKey: ByteArray, endKey: ByteArray) {}
    override fun deleteRange(beginKey: ByteArray?, endKey: ByteArray) {}
    override fun markRollback(xid: ByteArray?) {}
}
