/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/
package com.vmware.blockchain.deployment.service.provision

import com.vmware.blockchain.deployment.model.Endpoint
import com.vmware.blockchain.deployment.model.OrchestrationSite
import com.vmware.blockchain.deployment.model.ProvisioningServerConfiguration
import com.vmware.blockchain.deployment.model.TransportSecurity
import com.vmware.blockchain.deployment.model.core.URI
import com.vmware.blockchain.deployment.service.orchestrationsite.OrchestrationSiteService
import com.vmware.blockchain.deployment.service.orchestrationsite.OrchestrationSiteServiceModule
import dagger.BindsInstance
import dagger.Component
import io.grpc.netty.shaded.io.grpc.netty.GrpcSslContexts
import io.grpc.netty.shaded.io.grpc.netty.NettyServerBuilder
import io.grpc.netty.shaded.io.netty.handler.ssl.ClientAuth
import io.grpc.netty.shaded.io.netty.handler.ssl.SslContext
import io.grpc.netty.shaded.io.netty.handler.ssl.SslContextBuilder
import java.nio.file.Files
import java.nio.file.Paths
import java.util.concurrent.CompletableFuture
import javax.inject.Singleton
import kotlinx.coroutines.future.asCompletableFuture
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonConfiguration
import kotlinx.serialization.modules.EmptyModule
import org.slf4j.LoggerFactory

/**
 * gRPC server that serves provisioning-related API operations.
 */
@Component(modules = [
    ProvisioningServiceModule::class,
    OrchestrationSiteServiceModule::class,
    OrchestratorModule::class
])
@Singleton
internal interface ProvisioningServer {

    /** Singleton service instance for managing orchestration sites.  */
    fun orchestrationSiteService(): OrchestrationSiteService

    /** Singleton service instance for provisioning Concord clusters.  */
    fun provisioningService(): ProvisioningService

    @Component.Builder
    interface Builder {

        @BindsInstance
        fun orchestrations(entries: List<OrchestrationSite>): Builder

        @BindsInstance
        fun configServiceStub(configurationServiceEndpoint: Endpoint): Builder

        fun build(): ProvisioningServer
    }
}

/** Default server port number.  */
private const val DEFAULT_SERVER_PORT = 9002

/** Default configuration file path.  */
private val DEFAULT_SERVER_CONFIG = URI.create("file:/config/persephone/provisioning/config.json")

/** Default certificate chain file path.  */
private val DEFAULT_CERTIFICATE_CHAIN = URI.create("file:/config/persephone/provisioning/server.crt")

/** Default private key file path.  */
private val DEFAULT_PRIVATE_KEY = URI.create("file:/config/persephone/provisioning/server.pem")

/** Default trusted certificate collection file path.  */
private val DEFAULT_TRUST_CERTIFICATES = URI.create("file:/config/persephone/provisioning/ca.crt")

/** Default config service endpoint. */
private val DEFAULT_CONFIG_SERVICE_ENDPOINT = Endpoint("localhost:9003")

/**
 * Create a new [SslContext].
 *
 * @param trustedCertificatesPath
 *   path to trusted certificates collection file.
 * @param certificateChainPath
 *   path to certificate chain file.
 * @param privateKeyPath
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
 * Shutdown the server instance asynchronously.
 *
 * @return
 *   [CompletableFuture] that completes when shutdown is done.
 */
private fun shutdownServer(server: ProvisioningServer): CompletableFuture<Void> {
    return CompletableFuture.allOf(
            server.orchestrationSiteService().shutdownAsync().asCompletableFuture(),
            server.provisioningService().shutdown()
    )
}

/**
 * Main entry point for the server instance.
 *
 * @param args
 *   server startup arguments from command-line.
 */
fun main(args: Array<String>) {
    // Initialize logging.
    val log = LoggerFactory.getLogger(ProvisioningServer::class.java)

    // Construct orchestration mapping from input parameters.
    val json = Json(JsonConfiguration.Stable.copy(encodeDefaults = false), EmptyModule)
    val config = when {
        (args.size == 1 && Files.exists(Paths.get(args[0]))) -> {
            val configJson = Paths.get(args[0]).toUri().toURL().readText()
            json.parse(ProvisioningServerConfiguration.serializer(), configJson)
        }
        (Files.exists(Paths.get(DEFAULT_SERVER_CONFIG))) -> {
            val configJson = DEFAULT_SERVER_CONFIG.toURL().readText()
            json.parse(ProvisioningServerConfiguration.serializer(), configJson)
        }
        else -> ProvisioningServerConfiguration(
                DEFAULT_SERVER_PORT,
                TransportSecurity(
                        TransportSecurity.Type.TLSv1_2,
                        DEFAULT_TRUST_CERTIFICATES.toString(),
                        DEFAULT_CERTIFICATE_CHAIN.toString(),
                        DEFAULT_PRIVATE_KEY.toString()
                ),
                DEFAULT_CONFIG_SERVICE_ENDPOINT
        )
    }

    // Build the server and start.
    val provisioningServer = DaggerProvisioningServer.builder()
            .orchestrations(config.sites)
            .configServiceStub(config.configServiceEndpoint)
            .build()
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
            .addService(provisioningServer.provisioningService())
            .addService(provisioningServer.orchestrationSiteService())
            .sslContext(sslContext)
            .build()
    try {
        log.info("Initializing provisioning service")
        provisioningServer.provisioningService().initialize()
                .whenComplete { _, error ->
                    if (error != null) {
                        log.error("Error initializing provision service", error)
                        server.shutdown()
                    }
                }
        provisioningServer.orchestrationSiteService()
                .initializeAsync()
                .asCompletableFuture()
                .whenComplete { _, error ->
                    if (error != null) {
                        log.error("Error initializing orchestration site service", error)
                        server.shutdown()
                    }
                }
        log.info("Starting API server instance")
        server.start()
        Runtime.getRuntime().addShutdownHook(Thread {
            // Use stderr here since the logger may have been reset by its JVM shutdown hook.
            println("Shutting down server instance since JVM is shutting down")
            server.shutdown()
        })
    } catch (error: Throwable) {
        server.shutdown()
    } finally {
        server.awaitTermination()

        // Once the server loop is closed, make sure the rest of logical shutdown is done too.
        shutdownServer(provisioningServer).join()
    }
}
