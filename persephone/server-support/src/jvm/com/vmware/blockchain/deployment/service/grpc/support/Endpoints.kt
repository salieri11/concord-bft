/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/
package com.vmware.blockchain.deployment.service.grpc.support

import com.vmware.blockchain.deployment.model.core.URI
import com.vmware.blockchain.deployment.v1.Endpoint
import com.vmware.blockchain.deployment.v1.TransportSecurity
import io.grpc.ManagedChannel
import io.grpc.netty.shaded.io.grpc.netty.GrpcSslContexts
import io.grpc.netty.shaded.io.grpc.netty.NettyChannelBuilder
import java.util.concurrent.Executor
import java.util.concurrent.ForkJoinPool

/**
 * Resolve the transport security server settings so that any specified properties in URL are
 * retrieved and saved to the associated data properties.
 *
 * @return
 *  a copy of this [Endpoint] instance with URL-based server properties fully resolved.
 */
fun Endpoint.resolveTransportSecurityServerSetting(): Endpoint {
    // If trusted certificate data is available, use it, otherwise resolve if available, else blank.
    val trustedCertificate = when {
        transportSecurity.trustedCertificatesData.isNotEmpty() ->
            transportSecurity.trustedCertificatesData
        transportSecurity.trustedCertificatesUrl.isNotEmpty() ->
            URI.create(transportSecurity.trustedCertificatesUrl).toURL().readText()
        else -> ""
    }

    return copy(
            transportSecurity = TransportSecurity(
                    // Blind everything else except server settings.
                    type = transportSecurity.type,
                    trustedCertificatesData = trustedCertificate
            )
    )
}

/**
 * Create a new [ManagedChannel] matching the parameters specified in this [Endpoint] instance.
 *
 * @return
 *   a new [ManagedChannel] instance.
 */
fun Endpoint.newClientRpcChannel(executor: Executor = ForkJoinPool.commonPool()): ManagedChannel {
    return NettyChannelBuilder.forTarget(address).apply {
        when (transportSecurity.type) {
            TransportSecurity.Type.NONE -> usePlaintext()
            TransportSecurity.Type.TLSv1_2 -> {
                // Trusted certificates (favor local data over URL).
                val trustedCertificates = when {
                    transportSecurity.trustedCertificatesData.isNotEmpty() ->
                        transportSecurity.trustedCertificatesData.toByteArray()
                    transportSecurity.trustedCertificatesUrl.isNotEmpty() ->
                        URI.create(transportSecurity.trustedCertificatesUrl).toURL().readBytes()
                    else -> null
                }?.inputStream()

                // Key certificate chain (favor local data over URL).
                val keyCertificateChain = when {
                    transportSecurity.certificateData.isNotEmpty() ->
                        transportSecurity.certificateData.toByteArray()
                    transportSecurity.certificateUrl.isNotEmpty() ->
                        URI.create(transportSecurity.certificateUrl).toURL().readBytes()
                    else -> null
                }?.inputStream()

                // Private key (favor local data over URL).
                val privateKey = when {
                    transportSecurity.privateKeyData.isNotEmpty() ->
                        transportSecurity.privateKeyData.toByteArray()
                    transportSecurity.privateKeyUrl.isNotEmpty() ->
                        URI.create(transportSecurity.privateKeyUrl).toURL().readBytes()
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

        // Set the executor for background task execution.
        executor(executor)
    }.build()
}
