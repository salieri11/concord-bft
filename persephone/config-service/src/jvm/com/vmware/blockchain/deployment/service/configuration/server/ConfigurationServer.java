/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.service.configuration.server;

import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;
import javax.inject.Singleton;
import javax.net.ssl.SSLException;

import org.slf4j.LoggerFactory;

import com.vmware.blockchain.deployment.model.ConfigurationServerConfiguration;
import com.vmware.blockchain.deployment.model.TransportSecurity;

import dagger.Component;
import io.grpc.Server;
import io.grpc.netty.shaded.io.grpc.netty.GrpcSslContexts;
import io.grpc.netty.shaded.io.grpc.netty.NettyServerBuilder;
import io.grpc.netty.shaded.io.netty.handler.ssl.ClientAuth;
import io.grpc.netty.shaded.io.netty.handler.ssl.SslContext;
import io.grpc.netty.shaded.io.netty.handler.ssl.SslContextBuilder;
import io.grpc.protobuf.services.ProtoReflectionService;
import kotlinx.serialization.UpdateMode;
import kotlinx.serialization.json.Json;
import kotlinx.serialization.json.JsonConfiguration;
import kotlinx.serialization.modules.EmptyModule;

/**
 * gRPC server that serves config-service-related API operations.
 */
@Component(modules = {ConfigurationServiceModule.class})
@Singleton
public interface ConfigurationServer {
    /** Default server port number. */
    int DEFAULT_SERVER_PORT = 9003;

    /** Default configuration file path. */
    URI DEFAULT_SERVER_CONFIG_URL = URI.create("file:/config/persephone/configuration/config.json");

    /** Default certificate chain file path. */
    URI DEFAULT_CERTIFICATE_CHAIN_URL = URI.create("file:/config/persephone/configuration/server.crt");

    /** Default certificate chain data. */
    String DEFAULT_CERTIFICATE_CHAIN_DATA = "";

    /** Default private key file path. */
    URI DEFAULT_PRIVATE_KEY_URL = URI.create("file:/config/persephone/configuration/server.pem");

    /** Default private key data. */
    String DEFAULT_PRIVATE_KEY_DATA = "";

    /** Default trusted certificate collection file path. */
    URI DEFAULT_TRUST_CERTIFICATES_URL = URI.create("file:/config/persephone/configuration/ca.crt");

    /** Default trusted certificate collection data. */
    String DEFAULT_TRUST_CERTIFICATES_DATA = "";

    /** Singleton service instance for config-service Concord clusters. */
    ConfigurationService configurationService();

    /**
     * Builder instance.
     */
    @Component.Builder
    interface Builder {

        ConfigurationServer build();
    }

    /**
     * Create a new {@link SslContext}.
     *
     * @param trustedCertificatesPath
     *   path to trusted certificates collection file.
     * @param certificateChainPath
     *   path to certificate chain file.
     * @param privateKeyPath
     *   path to private key file (PEM).
     *
     * @return
     *   a new configured {@link SslContext} instance.
     *
     * @throws IOException
     *   if context cannot be constructed due to IO or SSL provider failure.
     */
    private static SslContext newSslContext(
            URI trustedCertificatesPath,
            URI certificateChainPath,
            URI privateKeyPath
    ) throws IOException {
        SslContextBuilder sslClientContextBuilder = SslContextBuilder
                .forServer(
                        certificateChainPath.toURL().openStream(),
                        privateKeyPath.toURL().openStream()
                );

        if (!trustedCertificatesPath.toString().isBlank()) {
            sslClientContextBuilder
                    .trustManager(trustedCertificatesPath.toURL().openStream())
                    .clientAuth(ClientAuth.REQUIRE);
        }

        return GrpcSslContexts.configure(sslClientContextBuilder).build();
    }

    /**
     * Shutdown the server instance asynchronously.
     *
     * @return
     *   {@link CompletableFuture} that completes when shutdown is done.
     */
    static CompletableFuture<Void> shutdownServer(ConfigurationServer server) {
        return server.configurationService().shutdown();
    }

    /**
     * Main entry point for the server instance.
     *
     * @param args
     *   server startup arguments from command-line.
     *
     * @throws InterruptedException
     *   if process is interrupted while awaiting termination.
     * @throws IOException
     *   if configuration cannot be loaded from file.
     * @throws SSLException
     *   if server SSL context cannot be constructed due to SSL provider exception.
     */
    static void main(String[] args) throws InterruptedException, IOException, SSLException {
        // Initialize logging.
        var log = LoggerFactory.getLogger(ConfigurationServer.class);

        // Construct server configuration from input parameters.
        var json = new Json(
                new JsonConfiguration(
                        false, /* encodeDefaults */
                        true, /* strictMode */
                        false, /* unquoted */
                        false, /* prettyPrint */
                        "    ", /* indent */
                        false, /* useArrayPolymorphism */
                        "type", /* classDiscriminator */
                        UpdateMode.OVERWRITE /* updateMode */
                ),
                EmptyModule.INSTANCE
        );
        var serializer = ConfigurationServerConfiguration.getSerializer();
        ConfigurationServerConfiguration config;
        if (args.length == 1 && Files.exists(Paths.get(args[0]))) {
            var configJson = Files.lines(Paths.get(args[0]), StandardCharsets.UTF_8)
                    .collect(Collectors.joining());
            config = json.parse(serializer, configJson);
        } else if (Files.exists(Paths.get(DEFAULT_SERVER_CONFIG_URL))) {
            var configJson = Files.lines(Paths.get(DEFAULT_SERVER_CONFIG_URL), StandardCharsets.UTF_8)
                    .collect(Collectors.joining());
            config = json.parse(serializer, configJson);
        } else {
            config = new ConfigurationServerConfiguration(
                    DEFAULT_SERVER_PORT,
                    new TransportSecurity(
                            TransportSecurity.Type.NONE,
                            DEFAULT_TRUST_CERTIFICATES_URL.toString(),
                            DEFAULT_TRUST_CERTIFICATES_DATA,
                            DEFAULT_CERTIFICATE_CHAIN_URL.toString(),
                            DEFAULT_CERTIFICATE_CHAIN_DATA,
                            DEFAULT_PRIVATE_KEY_URL.toString(),
                            DEFAULT_PRIVATE_KEY_DATA
                    )

            );
        }

        // Build the server and start.
        var configurationServer = DaggerConfigurationServer.create();
        var sslContext = (config.getTransportSecurity().getType() != TransportSecurity.Type.NONE)
                ? newSslContext(
                URI.create(config.getTransportSecurity().getTrustedCertificatesUrl()),
                URI.create(config.getTransportSecurity().getCertificateUrl()),
                URI.create(config.getTransportSecurity().getPrivateKeyUrl())
        )
                : null;
        Server server = NettyServerBuilder.forPort(config.getPort())
                .addService(ProtoReflectionService.newInstance())
                .addService(configurationServer.configurationService())
                .sslContext(sslContext)
                .build();

        try {
            log.info("Initializing configuration service");
            configurationServer.configurationService().initialize()
                    .whenComplete((result, error) -> {
                        if (error != null) {
                            log.error("Error initializing configuration service", error);
                            server.shutdown();
                        }
                    });

            log.info("Starting API server instance");
            server.start();
            Runtime.getRuntime().addShutdownHook(new Thread(() -> {
                // Use stderr here since the logger may have been reset by its JVM shutdown hook.
                System.out.println("Shutting down server instance since JVM is shutting down");
                server.shutdown();
            }));
        } catch (Throwable error) {
            server.shutdown();
        } finally {
            server.awaitTermination();

            // Once the server loop is closed, make sure the rest of logical shutdown is done too.
            shutdownServer(configurationServer).join();
        }
    }
}
