/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.server;

import java.io.IOException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ForkJoinPool;

import javax.net.ssl.SSLException;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.http.converter.protobuf.ProtobufHttpMessageConverter;

/**
 * gRPC server that serves config-service-related API operations.
 */
@SpringBootApplication
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

    @Bean
    ProtobufHttpMessageConverter protobufHttpMessageConverter() {
        return new ProtobufHttpMessageConverter();
    }
}
