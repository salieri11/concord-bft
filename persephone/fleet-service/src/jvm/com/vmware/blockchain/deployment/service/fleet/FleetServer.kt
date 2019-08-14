/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.service.fleet

import com.vmware.blockchain.deployment.logging.error
import com.vmware.blockchain.deployment.logging.info
import com.vmware.blockchain.deployment.logging.logger
import com.vmware.blockchain.deployment.model.FleetManagementServerConfiguration
import com.vmware.blockchain.deployment.model.TransportSecurity
import dagger.Component
import io.grpc.netty.shaded.io.grpc.netty.GrpcSslContexts
import io.grpc.netty.shaded.io.grpc.netty.NettyServerBuilder
import io.grpc.netty.shaded.io.netty.handler.ssl.ClientAuth
import io.grpc.netty.shaded.io.netty.handler.ssl.SslContext
import io.grpc.netty.shaded.io.netty.handler.ssl.SslContextBuilder
import java.net.URI
import java.nio.file.Files
import java.nio.file.Paths
import javax.inject.Singleton
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonConfiguration
import kotlinx.serialization.modules.EmptyModule

/** Default server port number.  */
private const val DEFAULT_SERVER_PORT = 9004

/** Default configuration files path.  */
private val DEFAULT_SERVER_CONFIG_URL = URI.create("file:/config/persephone/fleet/config.json")

/** Default certificate chain file path.  */
private val DEFAULT_CERTIFICATE_CHAIN_URL = URI.create("file:/config/persephone/fleet/server.crt")

/** Default private key file path.  */
private val DEFAULT_PRIVATE_KEY_URL = URI.create("file:/config/persephone/fleet/server.pem")

/** Default trusted certificate collection file path.  */
private val DEFAULT_TRUST_CERTIFICATES_URL = URI.create("file:/config/persephone/fleet/ca.crt")

/**
 * gRPC server that serves IP-allocation management API operations.
 */
@Component(modules = [FleetServiceModule::class])
@Singleton
interface FleetServer {

    @Component.Builder
    interface Builder {
        fun build(): FleetServer
    }

    fun fleetControlService(): FleetControlService
    fun fleetManagementService(): FleetManagementService
}

/**
 * Create a new [SslContext].
 *
 * @param[trustedCertificatesPath]
 *   path to trusted certificates collection file.
 * @param[certificateChainPath]
 *   path to certificate chain file.
 * @param[privateKeyPath]
 *   path to private key file (PEM).
 *
 * @return
 *   a new configured [SslContext] instance.
 */
private fun newSslContext(
    trustedCertificatesPath: URI,
    certificateChainPath: URI,
    privateKeyPath: URI
): SslContext {
    val sslClientContextBuilder = SslContextBuilder
            .forServer(
                    certificateChainPath.toURL().openStream(),
                    privateKeyPath.toURL().openStream()
            )

    if (!trustedCertificatesPath.toString().isBlank()) {
        sslClientContextBuilder
                .trustManager(trustedCertificatesPath.toURL().openStream())
                .clientAuth(ClientAuth.REQUIRE)
    }

    return GrpcSslContexts.configure(sslClientContextBuilder).build()
}

/**
 * Shutdown the server instance.
 */
private suspend fun FleetServer.shutdown() {
    // Stop the service instances first.
    fleetControlService().shutdown()
    fleetManagementService().shutdown()
}

/**
 * Main entry point for the server instance.
 *
 * @param[args]
 *   server startup arguments from command-line.
 */
fun main(args: Array<String>) {
    // Obtain a logging instance.
    val log = logger(FleetServer::class)

    // Construct server configuration from input parameters.
    val json = Json(JsonConfiguration.Stable.copy(encodeDefaults = false), EmptyModule)
    val config = when {
        (args.size == 1 && Files.exists(Paths.get(args[0]))) -> {
            val configJson = Paths.get(args[0]).toUri().toURL().readText()
            json.parse(FleetManagementServerConfiguration.serializer(), configJson)
        }
        (Files.exists(Paths.get(DEFAULT_SERVER_CONFIG_URL))) -> {
            val configJson = DEFAULT_SERVER_CONFIG_URL.toURL().readText()
            json.parse(FleetManagementServerConfiguration.serializer(), configJson)
        }
        else -> FleetManagementServerConfiguration(
                DEFAULT_SERVER_PORT,
                TransportSecurity(
                        type = TransportSecurity.Type.NONE,
                        trustedCertificatesUrl = DEFAULT_TRUST_CERTIFICATES_URL.toString(),
                        certificateUrl = DEFAULT_CERTIFICATE_CHAIN_URL.toString(),
                        privateKeyUrl = DEFAULT_PRIVATE_KEY_URL.toString()
                )
        )
    }

    // Build the server and start.
    val fleetServer = DaggerFleetServer.builder().build()
    val sslContext = config.transportSecurity.type
            .takeIf { it != TransportSecurity.Type.NONE }
            ?.let {
                newSslContext(
                        URI.create(config.transportSecurity.trustedCertificatesUrl),
                        URI.create(config.transportSecurity.certificateUrl),
                        URI.create(config.transportSecurity.privateKeyUrl)
                )
            }
    val server = NettyServerBuilder.forPort(config.port)
            .addService(fleetServer.fleetControlService())
            .addService(fleetServer.fleetManagementService())
            .sslContext(sslContext)
            .build()
    try {
        log.info { "Starting API server instance" }
        server.start()
        Runtime.getRuntime().addShutdownHook(Thread {
            // Use stderr here since the logger may have been reset by its JVM shutdown hook.
            println("Shutting down server instance since JVM is shutting down")
            server.shutdown()
        })
    } catch (error: Throwable) {
        log.error { "Error encountered, message(${error.message})" }
        server.shutdown()
    } finally {
        server.awaitTermination()

        // Once the server loop is closed, make sure the rest of logical shutdown is done too.
        runBlocking {
            fleetServer.shutdown()
        }
    }
}
