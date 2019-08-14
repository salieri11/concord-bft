/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/
package com.vmware.blockchain.deployment.service.provision

import com.vmware.blockchain.deployment.model.ConfigurationServiceStub
import com.vmware.blockchain.deployment.model.Endpoint
import com.vmware.blockchain.deployment.orchestration.vmware.newClientRpcChannel
import dagger.Module
import dagger.Provides
import io.grpc.CallOptions
import javax.inject.Singleton

@Module
class ConfigurationServiceModule {

    /**
     * Provide an [ConfigurationServiceStub] instance.
     *
     * @param[configurationService]
     *   endpoint specification for configuration service.
     *
     * @return
     *   a singleton [ConfigurationServiceStub] instance
     */
    @Provides
    @Singleton
    fun providesConfigurationServiceStub(configurationService: Endpoint): ConfigurationServiceStub {
        // Note: Endpoint.newClientRpcChannel() is currently exported from orchestration-vmware
        // library module. This will need to be moved.
        val channel = configurationService.newClientRpcChannel()
        return ConfigurationServiceStub(channel, CallOptions.DEFAULT)
    }
}