/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.service.ipam

import com.vmware.blockchain.deployment.model.core.URI
import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore
import com.vmware.blockchain.deployment.persistence.kv.MonotonicInt
import com.vmware.blockchain.deployment.persistence.kv.TypedKeyValueStore
import com.vmware.blockchain.deployment.persistence.kv.rocksdb.RocksDBConfiguration
import com.vmware.blockchain.deployment.persistence.kv.rocksdb.RocksDBUntypedKeyValueStore
import com.vmware.blockchain.deployment.v1.AddressBlock
import com.vmware.blockchain.deployment.v1.AddressBlockSegment
import dagger.Module
import dagger.Provides
import kotlinx.coroutines.CoroutineDispatcher
import kotlinx.coroutines.Dispatchers
import javax.inject.Singleton

@Module
class IPAllocationServiceModule {

    /**
     * Provide an [CoroutineDispatcher] instance.
     *
     * @return
     *   a [CoroutineDispatcher] instance.
     */
    @Provides
    @Singleton
    fun providesCoroutineDispatcher(): CoroutineDispatcher {
        return Dispatchers.Default
    }

    /**
     * Provide an [KeyValueStore] instance for persisting [AddressBlock].
     *
     * @return
     *   a singleton [KeyValueStore] instance.
     */
    @Provides
    @Singleton
    fun providesAddressBlockKeyValueStore(
        dispatcher: CoroutineDispatcher,
        storageUrl: String
    ): KeyValueStore<ResourceName, AddressBlock, MonotonicInt> {
        val path = URI.create("$storageUrl/blocks").path
        return TypedKeyValueStore(
                ResourceName.serializer(),
                AddressBlock.serializer(),
                RocksDBUntypedKeyValueStore(
                        dispatcher,
                        MonotonicInt.serializer(),
                        RocksDBConfiguration(path = path)
                )
        )
    }

    /**
     * Provide an [KeyValueStore] instance for persisting [AddressBlockSegment].
     *
     * @return
     *   a singleton [KeyValueStore] instance.
     */
    @Provides
    @Singleton
    fun providesAddressBlockSegmentKeyValueStore(
        dispatcher: CoroutineDispatcher,
        storageUrl: String
    ): KeyValueStore<ResourceName, AddressBlockSegment, MonotonicInt> {
        val path = URI.create("$storageUrl/segments").path
        return TypedKeyValueStore(
                ResourceName.serializer(),
                AddressBlockSegment.serializer(),
                RocksDBUntypedKeyValueStore(
                        dispatcher,
                        MonotonicInt.serializer(),
                        RocksDBConfiguration(path = path)
                )
        )
    }

    /**
     * Provide an [IPAllocationService] instance.
     *
     * @return
     * a singleton [IPAllocationService] instance.
     */
    @Provides
    @Singleton
    fun providesIPAllocationService(
        dispatcher: CoroutineDispatcher,
        blockStore: KeyValueStore<ResourceName, AddressBlock, MonotonicInt>,
        segmentStore: KeyValueStore<ResourceName, AddressBlockSegment, MonotonicInt>
    ): IPAllocationService {
        return IPAllocationService(dispatcher, blockStore, segmentStore)
    }
}
