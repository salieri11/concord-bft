/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/
package com.vmware.blockchain.services.provision

import com.vmware.blockchain.deployment.v1.ConfigurationServiceStub
import com.vmware.blockchain.deployment.v1.Endpoint
import dagger.Module
import dagger.Provides
import io.grpc.Server
import io.grpc.inprocess.InProcessChannelBuilder
import io.grpc.inprocess.InProcessServerBuilder
import java.util.function.Function
import javax.inject.Singleton

@Module
class TestConfigurationServiceModule {

    /**
     * Provide an [Endpoint] instance referencing a test configuration service address.
     *
     * @return
     *   a singleton [Endpoint] instance
     */
    @Provides
    @Singleton
    fun providesConfigurationService(): Endpoint {
        return Endpoint(address = "TestConfigurationService")
    }

    /**
     * Provide a [Server] instance hosting a  [TestConfigurationService] instance.
     *
     * @param[configurationService]
     *   configuration service endpoint.
     *
     * @return
     *   a singleton [Server] instance
     */
    @Provides
    @Singleton
    fun providesConfigurationServiceServer(
        configurationService: Endpoint
    ): Server {
        return InProcessServerBuilder.forName(configurationService.address)
                .directExecutor()
                .addService(com.vmware.blockchain.services.provision.TestConfigurationService())
                .build()
    }

    /**
     * Provides a [ConfigurationServiceStub] provider function that takes an [Endpoint].
     *
     * @param[configurationService]
     *   configuration service endpoint.
     *
     * @return
     *   a provider function which yields a new [ConfigurationServiceStub] instance on each call.
     */
    @Provides
    @Singleton
    fun providesConfigurationServiceClientProvider(
        configurationService: Endpoint
    ): Function<Endpoint, ConfigurationServiceStub> {
        return Function {
            val channel = InProcessChannelBuilder.forName(configurationService.address)
                    .directExecutor()
                    .build()

            ConfigurationServiceStub(channel)
        }
    }
}
