/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/
package com.vmware.blockchain.deployment.service.orchestrationsite

import com.vmware.blockchain.deployment.model.OrchestrationSite
import dagger.Module
import dagger.Provides
import javax.inject.Singleton
import kotlinx.coroutines.CoroutineDispatcher
import kotlinx.coroutines.Dispatchers

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
        orchestrations: List<OrchestrationSite>
    ): OrchestrationSiteService {
        return OrchestrationSiteService(dispatcher, orchestrations)
    }
}
