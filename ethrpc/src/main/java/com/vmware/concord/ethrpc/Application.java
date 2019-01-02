/*
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord.ethrpc;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * The Ethereum RPC Stub.
 */
@SpringBootApplication
public class Application {
    public static void main(String[] args) {
        SpringApplication.run(Application.class, args);
    }
}
