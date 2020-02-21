/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.server;

import java.io.IOException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ForkJoinPool;

import javax.net.ssl.SSLException;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;

import lombok.extern.slf4j.Slf4j;

/**
 * gRPC server that serves provisioning-service-related API operations.
 */
@SpringBootApplication
@Slf4j
public class Application {

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
    public static void main(String[] args) throws Exception {
        SpringApplication.run(Application.class, args);
    }

    @Bean
    public ExecutorService executorService() {
        return ForkJoinPool.commonPool();
    }

}
