/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.persistence.kv.rocksdb

import com.vmware.blockchain.deployment.persistence.kv.MonotonicInt
import com.vmware.blockchain.deployment.persistence.kv.UntypedKeyValueStoreTestDriver
import java.util.concurrent.Executors
import java.util.stream.Stream
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.asCoroutineDispatcher
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.MethodSource

class RocksDBUntypedKeyValueStoreTest : UntypedKeyValueStoreTestDriver() {

    companion object {
        /**
         * Create an enumeration of different new instantiations of [RocksDBUntypedKeyValueStore]
         * backed by different threading model.
         */
        @JvmStatic
        private fun servers(): Stream<RocksDBUntypedKeyValueStore<MonotonicInt>> {
            var counter = 0
            val serializer = MonotonicInt.serializer()
            return Stream.generate<RocksDBUntypedKeyValueStore<MonotonicInt>> {
                when (counter++.rem(3)) {
                    0 -> RocksDBUntypedKeyValueStore(Dispatchers.Default, serializer, newConfig())
                    1 -> RocksDBUntypedKeyValueStore(Dispatchers.Unconfined, serializer, newConfig())
                    else -> RocksDBUntypedKeyValueStore(
                            Executors.newSingleThreadExecutor().asCoroutineDispatcher(),
                            serializer,
                            newConfig()
                    )
                }
            }.limit(100)
        }

        @JvmStatic
        private fun newConfig(): RocksDBConfiguration {
            return RocksDBConfiguration(path = createTempDir().absolutePath)
        }
    }

    /**
     * Test CRUD functionality with event-sourcing.
     */
    @ParameterizedTest
    @MethodSource("servers")
    fun crud(server: RocksDBUntypedKeyValueStore<MonotonicInt>) {
        try {
            testDriver_crud(server)
        } finally {
            // Best-effort removal of temporary DB folder.
            // File(server.configuration.path).deleteRecursively()
        }
    }

    /**
     * Test that new subscribers requesting prior state can recall a settled state barring
     * additional mutations to the store.
     */
    @ParameterizedTest
    @MethodSource("servers")
    fun eventSourceWithPriorState(server: RocksDBUntypedKeyValueStore<MonotonicInt>) {
        try {
            testDriver_eventSourceWithPriorState(server)
        } finally {
            // Best-effort removal of temporary DB folder.
            // File(server.configuration.path).deleteRecursively()
        }
    }
}
