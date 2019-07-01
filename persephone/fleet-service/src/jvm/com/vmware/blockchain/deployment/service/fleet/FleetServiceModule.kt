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
     * Provide an [FleetService] instance.
     *
     * @return
     * a singleton [FleetService] instance.
     */
    @Provides
    @Singleton
    fun providesFleetService(dispatcher: CoroutineDispatcher): FleetService {
        return FleetService(dispatcher)
    }
}
