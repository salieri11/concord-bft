/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/
package com.vmware.blockchain.deployment.service.provision

import com.vmware.blockchain.deployment.model.ConfigurationServiceStub
import com.vmware.blockchain.deployment.model.Endpoint
import com.vmware.blockchain.deployment.model.TransportSecurity
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
     * @param[configurationService]
     *   endpoint specification for configuration service.
     *
     * @return
     *   a singleton [ConfigurationServiceStub] instance
     */
    @Provides
    @Singleton
    fun providesConfigurationServiceStub(configurationService: Endpoint): ConfigurationServiceStub {
        val channel = NettyChannelBuilder
                .forTarget(configurationService.address)
                .apply {
                    val transport = configurationService.transportSecurity
                    when (transport.type) {
                        TransportSecurity.Type.NONE -> usePlaintext()
                        TransportSecurity.Type.TLSv1_2 -> {
                            // Trusted certificates (favor local data over URL).
                            val trustedCertificates = when {
                                transport.trustedCertificatesData.isNotEmpty() ->
                                    transport.trustedCertificatesData.toByteArray()
                                transport.trustedCertificatesUrl.isNotEmpty() ->
                                    URI.create(transport.trustedCertificatesUrl).toURL().readBytes()
                                else -> null
                            }?.inputStream()

                            // Key certificate chain (favor local data over URL).
                            val keyCertificateChain = when {
                                transport.certificateData.isNotEmpty() ->
                                    transport.certificateData.toByteArray()
                                transport.certificateUrl.isNotEmpty() ->
                                    URI.create(transport.certificateUrl).toURL().readBytes()
                                else -> null
                            }?.inputStream()

                            // Private key (favor local data over URL).
                            val privateKey = when {
                                transport.privateKeyData.isNotEmpty() ->
                                    transport.privateKeyData.toByteArray()
                                transport.privateKeyUrl.isNotEmpty() ->
                                    URI.create(transport.privateKeyUrl).toURL().readBytes()
                                else -> null
                            }?.inputStream()

                            // Setup SSL context and enable TLS.
                            sslContext(
                                    GrpcSslContexts.forClient()
                                            .trustManager(trustedCertificates)
                                            .keyManager(keyCertificateChain, privateKey)
                                            .build()
                            )
                            useTransportSecurity()
                        }
                    }
                }
                .build()

        return ConfigurationServiceStub(channel, CallOptions.DEFAULT)
    }
}