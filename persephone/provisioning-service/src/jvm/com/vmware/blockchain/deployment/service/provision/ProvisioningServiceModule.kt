/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/
package com.vmware.blockchain.deployment.service.provision

import com.vmware.blockchain.deployment.model.ConfigurationServiceStub
import com.vmware.blockchain.deployment.model.OrchestrationSite
import dagger.Module
import dagger.Provides
import java.util.concurrent.ExecutorService
import java.util.concurrent.ForkJoinPool
import javax.inject.Singleton

@Module
class ProvisioningServiceModule {

    /**
     * Provide an [ExecutorService] instance.
     *
     * @return
     *   a singleton [ExecutorService] instance.
     */
    @Provides
    @Singleton
    fun providesExecutorService(): ExecutorService {
        return ForkJoinPool.commonPool()
    }

    /**
     * Provide an [ProvisioningService] instance.
     *
     * @return
     *   a singleton [ProvisioningService] instance.
     */
    @Provides
    @Singleton
    fun providesProvisioningService(
        executor: ExecutorService,
        orchestratorProvider: OrchestratorProvider,
        sites: List<OrchestrationSite>,
        configService: ConfigurationServiceStub
    ): ProvisioningService {
        return ProvisioningService(executor, orchestratorProvider, sites, configService)
    }
}
