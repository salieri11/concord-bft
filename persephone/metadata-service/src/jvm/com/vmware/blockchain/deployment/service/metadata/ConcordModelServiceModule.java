/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * *************************************************************************/

package com.vmware.blockchain.deployment.service.metadata;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.ForkJoinPool;

import javax.inject.Singleton;

import com.vmware.blockchain.deployment.persistence.kv.InMemoryUntypedKeyValueStore;
import com.vmware.blockchain.deployment.persistence.kv.KeyValueStore;
import com.vmware.blockchain.deployment.persistence.kv.MonotonicInt;
import com.vmware.blockchain.deployment.persistence.kv.TypedKeyValueStore;
import com.vmware.blockchain.deployment.persistence.kv.UntypedKeyValueStore;
import com.vmware.blockchain.deployment.v1.ConcordModelIdentifier;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;

import dagger.Module;
import dagger.Provides;
import kotlinx.coroutines.Dispatchers;

/**
 * {@link Singleton}-scoped Dagger {@link Module} that declares the components and dependencies
 * for {@link ConcordModelService} instance.
 */
@Module
class ConcordModelServiceModule {

    /**
     * Provide an {@link ExecutorService} instance.
     *
     * @return
     *   a singleton {@link ExecutorService} instance.
     */
    @Provides
    @Singleton
    static ExecutorService providesExecutorService() {
        return ForkJoinPool.commonPool();
    }

    /**
     * Provide an {@link UntypedKeyValueStore} instance.
     *
     * @return
     *   a singleton {@link UntypedKeyValueStore} instance.
     */
    @Provides
    @Singleton
    static UntypedKeyValueStore<MonotonicInt> providesUntypedKeyValueStore() {
        return new InMemoryUntypedKeyValueStore<>(
                Dispatchers.getDefault(),
                MonotonicInt.Companion.getSerializer()
        );
    }

    /**
     * Provide an {@link KeyValueStore} instance.
     *
     * @return
     *   a singleton {@link KeyValueStore} instance.
     */
    @Provides
    @Singleton
    static KeyValueStore<ConcordModelIdentifier, ConcordModelSpecification, MonotonicInt> providesKeyValueStore(
            UntypedKeyValueStore<MonotonicInt> untypedKeyValueStore
    ) {
        return new TypedKeyValueStore<>(
                ConcordModelIdentifier.getSerializer(),
                ConcordModelSpecification.getSerializer(),
                untypedKeyValueStore
        );
    }

    /**
     * Provide an {@link ConcordModelService} instance.
     *
     * @return
     *   a singleton {@link ConcordModelService} instance.
     */
    @Provides
    @Singleton
    static ConcordModelService providesConcordModelService(
            ExecutorService executor,
            KeyValueStore<ConcordModelIdentifier, ConcordModelSpecification, MonotonicInt> store
    ) {
        return new ConcordModelService(executor, store);
    }
}
