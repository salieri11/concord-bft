/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/

package com.vmware.blockchain.deployment.service.provision;

import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ForkJoinPool;
import javax.inject.Singleton;

import com.vmware.blockchain.deployment.model.OrchestrationSite;

import dagger.Module;
import dagger.Provides;

@Module
class ProvisioningServiceModule {

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
     * Provide an {@link ProvisioningService} instance.
     *
     * @return
     *   a singleton {@link ProvisioningService} instance.
     */
    @Provides
    @Singleton
    static ProvisioningService providesProvisioningService(
            ExecutorService executor,
            OrchestratorProvider orchestratorProvider,
            List<OrchestrationSite> sites
    ) {
        return new ProvisioningService(executor, orchestratorProvider, sites);
    }
}
