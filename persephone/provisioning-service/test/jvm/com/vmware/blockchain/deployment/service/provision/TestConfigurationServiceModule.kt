/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/
package com.vmware.blockchain.deployment.service.provision

import com.vmware.blockchain.deployment.model.ConfigurationServiceStub
import com.vmware.blockchain.deployment.model.Endpoint
import dagger.Module
import dagger.Provides
import io.grpc.CallOptions
import io.grpc.inprocess.InProcessChannelBuilder
import javax.inject.Singleton

@Module
class TestConfigurationServiceModule {

    /**
     * Provide an [ConfigurationServiceStub] instance.
     *
     * @return
     *   a singleton [ConfigurationServiceStub] instance
     */
    @Provides
    @Singleton
    fun providesConfigurationServiceStub(configurationServiceEndpoint: Endpoint): ConfigurationServiceStub {

        val channel = InProcessChannelBuilder.forName("TestConfigurationService").directExecutor().build()
        return ConfigurationServiceStub(channel, CallOptions.DEFAULT)
    }
}