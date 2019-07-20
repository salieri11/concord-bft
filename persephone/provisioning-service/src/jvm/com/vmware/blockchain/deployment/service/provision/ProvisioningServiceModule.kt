/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/
package com.vmware.blockchain.deployment.service.provision

import com.vmware.blockchain.deployment.model.ConfigurationServiceEndpoint
import com.vmware.blockchain.deployment.model.ConfigurationServiceStub
import com.vmware.blockchain.deployment.model.OrchestrationSite
import com.vmware.blockchain.deployment.model.core.URI
import dagger.Module
import dagger.Provides
import io.grpc.CallOptions
import io.grpc.netty.shaded.io.grpc.netty.GrpcSslContexts
import io.grpc.netty.shaded.io.grpc.netty.NettyChannelBuilder
import java.util.concurrent.ExecutorService
import java.util.concurrent.ForkJoinPool
import javax.inject.Singleton

@Module
class ProvisioningServiceModule {

    /**
     * Provide an [ExecutorService] instance.
     *
     * @return
     *   a singleton [ExecutorService] instance.
     */
    @Provides
    @Singleton
    fun providesExecutorService(): ExecutorService {
        return ForkJoinPool.commonPool()
    }

    /**
     * Provide an [ConfigurationServiceStub] instance.
     *
     * @return
     *   a singleton [ConfigurationServiceStub] instance
     */
    @Provides
    @Singleton
    fun providesConfigurationServiceStub(): ConfigurationServiceStub {
        val configServiceTrustCertificate = URI.create("file:/config/persephone/provisioning/configservice.crt")

        // FIXME: Replace with config service endpoint
        val configurationServiceEndpoint = ConfigurationServiceEndpoint()

        val channel = NettyChannelBuilder
                .forTarget(configurationServiceEndpoint.endpoint.address)
                .sslContext(
                        GrpcSslContexts.forClient()
                                .trustManager(java.io.File(configServiceTrustCertificate)).build())
                .build()
        return ConfigurationServiceStub(channel, CallOptions.DEFAULT)
    }
    /**
     * Provide an [ProvisioningService] instance.
     *
     * @return
     *   a singleton [ProvisioningService] instance.
     */
    @Provides
    @Singleton
    fun providesProvisioningService(
        executor: ExecutorService,
        orchestratorProvider: OrchestratorProvider,
        sites: List<OrchestrationSite>,
        configService: ConfigurationServiceStub
    ): ProvisioningService {
        return ProvisioningService(executor, orchestratorProvider, sites, configService)
    }
}
