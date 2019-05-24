/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/

package com.vmware.blockchain.deployment.service.provision;

import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ForkJoinPool;
import javax.inject.Singleton;

import com.vmware.blockchain.deployment.model.OrchestrationSiteIdentifier;
import com.vmware.blockchain.deployment.model.orchestration.OrchestrationSiteInfo;

import dagger.Module;
import dagger.Provides;

@Module
class ProvisionServiceModule {

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
     * Provide an {@link ProvisionService} instance.
     *
     * @return
     *   a singleton {@link ProvisionService} instance.
     */
    @Provides
    @Singleton
    static ProvisionService providesProvisionService(
            ExecutorService executor,
            OrchestratorProvider orchestratorProvider,
            Map<OrchestrationSiteIdentifier, OrchestrationSiteInfo> sites
    ) {
        return new ProvisionService(executor, orchestratorProvider, sites);
    }
}
