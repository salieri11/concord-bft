/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/
package com.vmware.blockchain.deployment.service.orchestrationsite

import com.vmware.blockchain.deployment.orchestration.OrchestratorProvider
import dagger.Module
import dagger.Provides
import kotlinx.coroutines.CoroutineDispatcher
import kotlinx.coroutines.Dispatchers
import javax.inject.Singleton

@Module
class OrchestrationSiteServiceModule {

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
     * Provide an [OrchestrationSiteService] instance.
     *
     * @return
     * a singleton [OrchestrationSiteService] instance.
     */
    @Provides
    @Singleton
    fun providesOrchestrationSiteService(
        dispatcher: CoroutineDispatcher,
        orchestratorProvider: OrchestratorProvider
    ): OrchestrationSiteService {
        return OrchestrationSiteService(dispatcher, orchestratorProvider)
    }
}
