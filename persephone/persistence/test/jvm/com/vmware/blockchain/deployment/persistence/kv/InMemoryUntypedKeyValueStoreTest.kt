/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.persistence.kv

import java.util.concurrent.Executors
import java.util.stream.Stream
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.asCoroutineDispatcher
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.MethodSource
/**
 * Basic functionality test of [InMemoryUntypedKeyValueStore] operations.
 */
class InMemoryUntypedKeyValueStoreTest : UntypedKeyValueStoreTestDriver() {

    companion object {
        /**
         * Create an enumeration of different new instantiations of [UntypedKeyValueStore] backed by
         * different threading model.
         */
        @JvmStatic
        private fun servers(): Stream<UntypedKeyValueStore<MonotonicInt>> {
            var counter = -1
            val serializer = MonotonicInt.serializer()
            return Stream.generate<UntypedKeyValueStore<MonotonicInt>> {
                when (counter++.rem(3)) {
                    0 -> InMemoryUntypedKeyValueStore(Dispatchers.Default, serializer)
                    1 -> InMemoryUntypedKeyValueStore(Dispatchers.Unconfined, serializer)
                    else -> InMemoryUntypedKeyValueStore(
                            Executors.newSingleThreadExecutor().asCoroutineDispatcher(),
                            serializer
                    )
                }
            }.limit(100)
        }
    }

    /**
     * Test CRUD functionality with event-sourcing.
     */
    @ParameterizedTest
    @MethodSource("servers")
    fun crud(server: UntypedKeyValueStore<MonotonicInt>) {
        testDriver_crud(server)
    }

    /**
     * Test that new subscribers requesting prior state can recall a settled state barring
     * additional mutations to the store.
     */
    @ParameterizedTest
    @MethodSource("servers")
    fun eventSourceWithPriorState(server: UntypedKeyValueStore<MonotonicInt>) {
        testDriver_eventSourceWithPriorState(server)
    }
}
