/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.ipam.server;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;

/**
 * gRPC server that serves IPAM API operations.
 */
@SpringBootApplication
@ComponentScan({"com.vmware.blockchain.ipam.*"})
public class Application {

    /**
     * Main entry point for the server instance.
     *
     * @param args
     *   server startup arguments from command-line.
     */
    public static void main(String[] args) throws Exception {
        SpringApplication.run(Application.class, args);
    }

}
