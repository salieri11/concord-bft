/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.service.server;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.ForkJoinPool;
import javax.inject.Singleton;

import dagger.Module;
import dagger.Provides;

/**
 * {@link Singleton}-scoped Dagger {@link Module} that declares the components and dependencies
 * for {@link ConfigurationService} instance.
 */
@Module
public class ConfigurationServiceModule {
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
     * Provide an {@link ConfigurationService} instance.
     *
     * @return
     *   a singleton {@link ConfigurationService} instance.
     */
    @Provides
    @Singleton
    static ConfigurationService providesConfigurationService(ExecutorService executor) {
        return new ConfigurationService(executor);
    }
}
