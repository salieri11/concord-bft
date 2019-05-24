/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/

package com.vmware.blockchain.deployment.service.provision;

import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;
import javax.inject.Singleton;

import org.slf4j.LoggerFactory;

import com.vmware.blockchain.deployment.model.OrchestrationSiteIdentifier;
import com.vmware.blockchain.deployment.model.orchestration.OrchestrationSiteInfo;
import com.vmware.blockchain.deployment.model.orchestration.OrchestrationSiteInfoKt;

import dagger.BindsInstance;
import dagger.Component;
import io.grpc.Server;
import io.grpc.ServerBuilder;
import kotlinx.serialization.UpdateMode;
import kotlinx.serialization.json.Json;
import kotlinx.serialization.json.JsonConfiguration;
import kotlinx.serialization.modules.EmptyModule;


/**
 * gRPC server that serves provisioning-related API operations.
 */
@Component(modules = {ProvisionServiceModule.class, OrchestratorModule.class})
@Singleton
interface ProvisionServer {

    /** Default server port number. */
    int DEFAULT_SERVER_PORT = 9002;

    /** Default configuration files path. */
    URI DEFAULT_SERVER_CONFIG = URI.create("file:/config/persephone/provision-service/config.json");

    /** Singleton service instance for provisioning Concord clusters. */
    ProvisionService provisionService();

    @Component.Builder
    interface Builder {

        @BindsInstance
        Builder orchestrations(Map<OrchestrationSiteIdentifier, OrchestrationSiteInfo> entries);

        ProvisionServer build();
    }

    /**
     * Main entry point for the server instance.
     *
     * @param args
     *   server startup arguments from command-line.
     *
     * @throws InterruptedException
     *   if process is interrupted while awaiting termination.
     */
    static void main(String[] args) throws InterruptedException, IOException {
        // Initialize logging.
        var log = LoggerFactory.getLogger(ProvisionServer.class);

        // Construct orchestration mapping from input parameters.
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
        var serializer = OrchestrationSiteInfoKt.getOrchestrationSiteMapSerializer();
        var orchestrations = Collections.<OrchestrationSiteIdentifier, OrchestrationSiteInfo>emptyMap();

        if (args.length == 1 && Files.exists(Paths.get(args[0]))) {
            var config = Files.lines(Paths.get(args[0]), StandardCharsets.UTF_8)
                    .collect(Collectors.joining());
            orchestrations = json.parse(serializer, config);
        } else if (Files.exists(Paths.get(DEFAULT_SERVER_CONFIG))) {
            var config = Files.lines(Paths.get(DEFAULT_SERVER_CONFIG), StandardCharsets.UTF_8)
                    .collect(Collectors.joining());
            orchestrations = json.parse(serializer, config);
        }

        // Build the server and start.
        var provisionServer = DaggerProvisionServer.builder()
                .orchestrations(orchestrations)
                .build();
        Server server = ServerBuilder.forPort(DEFAULT_SERVER_PORT)
                .addService(provisionServer.provisionService())
                .build();
        try {
            log.info("Initializing provisioning service");
            provisionServer.provisionService().initialize()
                    .whenComplete((result, error) -> {
                        if (error != null) {
                            log.error("Error initializing provision service", error);
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
            shutdownServer(provisionServer).join();
        }
    }

    /**
     * Shutdown the server instance asynchronously.
     *
     * @return
     *   {@link CompletableFuture} that completes when shutdown is done.
     */
    static CompletableFuture<Void> shutdownServer(ProvisionServer server) {
        return server.provisionService().shutdown();
    }
}
