/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/
package com.vmware.blockchain.services.provision

import com.vmware.blockchain.deployment.v1.Credential
import com.vmware.blockchain.deployment.v1.Endpoint
import com.vmware.blockchain.deployment.v1.TransportSecurity
import dagger.Module
import dagger.Provides
import javax.inject.Named
import javax.inject.Singleton

@Module
class TestEndpointProviderModule {

    @Provides
    @Singleton
    @Named("allocationServer")
    fun providesAllocationServer(): Endpoint {
        return Endpoint("", Credential(), TransportSecurity())
    }

    @Provides
    @Singleton
    @Named("containerRegistry")
    fun providesContainerRegistry(): Endpoint {
        return Endpoint("", Credential(), TransportSecurity())
    }

    @Provides
    @Singleton
    @Named("configurationService")
    fun providesConfigService(): Endpoint {
        return Endpoint("", Credential(), TransportSecurity())
    }

    @Provides
    @Singleton
    @Named("configurationServiceRest")
    fun providesConfigServiceRest(): Endpoint {
        return Endpoint("", Credential(), TransportSecurity())
    }
}
