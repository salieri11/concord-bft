/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.service.provision;

import java.util.concurrent.ExecutorService;
import javax.inject.Singleton;

import dagger.Module;
import dagger.Provides;

@Module
class OrchestratorModule {

    /**
     * Provide an {@link OrchestratorProvider} instance.
     *
     * @return
     *   a singleton {@link OrchestratorProvider} instance.
     */
    @Provides
    @Singleton
    static OrchestratorProvider providesOrchestratorProvider(ExecutorService executor) {
        return new OrchestratorFactory(executor);
    }
}
