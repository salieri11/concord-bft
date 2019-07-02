/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/
package com.vmware.blockchain.deployment.service.provision

import dagger.Module
import dagger.Provides
import java.util.concurrent.ExecutorService
import javax.inject.Singleton

@Module
class OrchestratorModule {

    /**
     * Provide an [OrchestratorProvider] instance.
     *
     * @return
     *   a singleton [OrchestratorProvider] instance.
     */
    @Provides
    @Singleton
    fun providesOrchestratorProvider(executor: ExecutorService): OrchestratorProvider {
        return OrchestratorFactory(executor)
    }
}
