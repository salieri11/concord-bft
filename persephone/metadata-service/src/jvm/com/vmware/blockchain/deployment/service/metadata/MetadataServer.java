/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.service.metadata;

import java.util.concurrent.CompletableFuture;
import javax.inject.Singleton;

import dagger.Component;
import io.grpc.Server;
import io.grpc.ServerBuilder;

/**
 * GRPC server that serves metadata related API operations.
 */
@Component(modules = ConcordModelServiceModule.class)
@Singleton
interface MetadataServer {

    /** Default server port number. */
    int DEFAULT_SERVER_PORT = 9001;

    /**
     * Service endpoint that serves metadata pertaining Concord model information.
     */
    ConcordModelService concordModelService();

    /**
     * Main entry point for the server instance.
     *
     * @param args
     *   server startup arguments from command-line.
     *
     * @throws InterruptedException
     *   if process is interrupted while awaiting termination.
     */
    static void main(String[] args) throws InterruptedException {
        MetadataServer metadataServer = DaggerMetadataServer.create();
        metadataServer.concordModelService().initialize();
        Server server = ServerBuilder.forPort(DEFAULT_SERVER_PORT)
                .addService(metadataServer.concordModelService())
                .build();
        try {
            server.start();
            System.out.println("Started the server\n");
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
            shutdownMetadataServer(metadataServer).join();
        }
    }

    /**
     * Shutdown the server instance asynchronously.
     *
     * @return
     *   {@link CompletableFuture} that completes when shutdown is done.
     */
    static CompletableFuture<Void> shutdownMetadataServer(MetadataServer server) {
        return server.concordModelService().shutdown();
    }
}
