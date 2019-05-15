/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord.agent;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.http.converter.protobuf.ProtobufHttpMessageConverter;

/**
 * Concord-agent --
 * This is the main for running agent.
 */
@SpringBootApplication
public class Application {

    /**
     * Main - Entry into SpringBoot application.
     */
    public static void main(String[] args) {
        // Create the required configuration files etc for concord-node
        AgentDockerClient client = new AgentDockerClient();
        client.startConcord();
        SpringApplication.run(Application.class, args);
    }

    /**
     * This is required for converting ProtoBuf into HttpMessage.
     */
    @Bean
    ProtobufHttpMessageConverter protobufHttpMessageConverter() {
        return new ProtobufHttpMessageConverter();
    }
}
