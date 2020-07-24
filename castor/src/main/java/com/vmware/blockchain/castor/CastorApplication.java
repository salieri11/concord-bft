/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.castor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import com.vmware.blockchain.castor.service.DeployerService;

/**
 * Main class for Castor
 *
 * <p>: Castor is an on-prem deployment shell.
 *      It connects to the provisioning service at the backend over gRPC.
 */
@SpringBootApplication
public class CastorApplication implements CommandLineRunner {

    @Autowired
    DeployerService deployerService;

    /**
     * Main entrypoint for Castor.
     */
    public static void main(String[] args) {
        SpringApplication.run(CastorApplication.class, args);
    }

    @Override
    public void run(String... args) throws Exception {
        deployerService.start();
    }
}
