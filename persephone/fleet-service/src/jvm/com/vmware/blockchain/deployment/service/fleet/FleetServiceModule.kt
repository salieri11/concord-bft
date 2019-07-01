/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.service.fleet

import dagger.Module
import dagger.Provides
import javax.inject.Singleton
import kotlinx.coroutines.CoroutineDispatcher
import kotlinx.coroutines.Dispatchers

@Module
class FleetServiceModule {

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
     * Provide an [FleetControlService] instance.
     *
     * @return
     * a singleton [FleetControlService] instance.
     */
    @Provides
    @Singleton
    fun providesFleetControlService(dispatcher: CoroutineDispatcher): FleetControlService {
        return FleetControlService(dispatcher)
    }

    /**
     * Provide an [FleetManagementService] instance.
     *
     * @return
     * a singleton [FleetManagementService] instance.
     */
    @Provides
    @Singleton
    fun provideFleetManagementService(
        dispatcher: CoroutineDispatcher
    ): FleetManagementService {
        return FleetManagementService(dispatcher)
    }
}
