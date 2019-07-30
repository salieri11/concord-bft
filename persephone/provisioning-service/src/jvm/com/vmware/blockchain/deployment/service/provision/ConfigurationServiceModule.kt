/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/
package com.vmware.blockchain.deployment.service.provision

import com.vmware.blockchain.deployment.model.ConfigurationServiceStub
import com.vmware.blockchain.deployment.model.Endpoint
import dagger.Module
import dagger.Provides
import io.grpc.CallOptions
import io.grpc.ManagedChannelBuilder
import javax.inject.Singleton

@Module
class ConfigurationServiceModule {

    /**
     * Provide an [ConfigurationServiceStub] instance.
     *
     * @return
     *   a singleton [ConfigurationServiceStub] instance
     */
    @Provides
    @Singleton
    fun providesConfigurationServiceStub(configurationServiceEndpoint: Endpoint): ConfigurationServiceStub {

        val channel = ManagedChannelBuilder.forTarget(configurationServiceEndpoint.address).build()
        return ConfigurationServiceStub(channel, CallOptions.DEFAULT)
    }
}