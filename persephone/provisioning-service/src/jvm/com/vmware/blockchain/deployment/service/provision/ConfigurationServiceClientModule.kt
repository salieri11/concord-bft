/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/
package com.vmware.blockchain.deployment.service.provision

import com.vmware.blockchain.deployment.service.grpc.support.newClientRpcChannel
import com.vmware.blockchain.deployment.v1.ConfigurationServiceStub
import com.vmware.blockchain.deployment.v1.Endpoint
import dagger.Module
import dagger.Provides
import java.util.concurrent.ExecutorService
import java.util.function.Function
import javax.inject.Named
import javax.inject.Singleton

@Module
class ConfigurationServiceClientModule {

    /**
     * Provides a [ConfigurationServiceStub] provider function that takes an [Endpoint].
     *
     * @param[executor]
     *   executor to supply to the underlying client RPC channel for outbound calls.
     *
     * @return
     *   a provider function which yields a new [ConfigurationServiceStub] instance on each call.
     */
    @Provides
    @Singleton
    fun providesConfigurationServiceClientProvider(
        @Named("default-executor") executor: ExecutorService
    ): Function<Endpoint, ConfigurationServiceStub> {
        return Function { endpoint: Endpoint ->
            ConfigurationServiceStub(endpoint.newClientRpcChannel(executor))
        }
    }
}
