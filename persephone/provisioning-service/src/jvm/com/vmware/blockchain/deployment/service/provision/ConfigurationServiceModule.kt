/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/
package com.vmware.blockchain.deployment.service.provision

import com.vmware.blockchain.deployment.model.ConfigurationServiceStub
import com.vmware.blockchain.deployment.model.Endpoint
import com.vmware.blockchain.deployment.model.core.URI
import dagger.Module
import dagger.Provides
import io.grpc.CallOptions
import io.grpc.netty.shaded.io.grpc.netty.GrpcSslContexts
import io.grpc.netty.shaded.io.grpc.netty.NettyChannelBuilder
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
        val configServiceTrustCertificate = URI.create("file:/config/persephone/provisioning/configservice.crt")

        val channel = NettyChannelBuilder
                .forTarget(configurationServiceEndpoint.address)
                .sslContext(
                        GrpcSslContexts.forClient()
                                .trustManager(java.io.File(configServiceTrustCertificate)).build())
                .build()
        return ConfigurationServiceStub(channel, CallOptions.DEFAULT)
    }
}