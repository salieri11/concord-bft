/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;


/**
 * Main class for helen, does some basic initializations and then calls SpringApplication.run() method. This class also
 * does the job of providing all spring related configuration annotations.
 *
 * <p>Helen connects to Concord at the backend. Communication between Helen and Concord is via a TCP socket connection.
 * Messages are sent in Google Protocol Buffer format. Responses from Helen to the client are in Json format.
 */
@SpringBootApplication
public class Server {

    // Set current datetime for logging purposes
    static {
        SimpleDateFormat dateFormat = new SimpleDateFormat("dd-MM-yyyy-hh-mm-ss");
        System.setProperty("current.date.time", dateFormat.format(new Date()));
    }

    /**
     * Main entrypoint for Server.
     */
    public static void main(String[] args) throws IOException {
        final Logger logger = LogManager.getLogger(Server.class);
        String[] testEnv = {"--spring.profiles.active=test"};

        // backwards compatibility for testing
        if (args.length == 1) {
            // used to hand in a property file name. Make the argument the active profile
            if ("application-test.properties".equals(args[0])) {
                args = testEnv;
                logger.info("Setting test environment");
            }
        }

        SpringApplication.run(Server.class, args);
    }


}
