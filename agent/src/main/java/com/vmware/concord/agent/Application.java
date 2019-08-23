/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord.agent;

import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.http.converter.protobuf.ProtobufHttpMessageConverter;

import com.vmware.blockchain.deployment.v1.ConcordAgentConfiguration;

import kotlinx.serialization.UpdateMode;
import kotlinx.serialization.json.Json;
import kotlinx.serialization.json.JsonConfiguration;
import kotlinx.serialization.modules.EmptyModule;

/**
 * Concord-agent --
 * This is the main for running agent.
 */
@SpringBootApplication
public class Application {

    private static final URI CONCORD_AGENT_MODEL_URI = URI.create("file:/config/agent/config.json");

    /**
     * Main - Entry into SpringBoot application.
     */
    public static void main(String[] args) throws Exception {
        // Construct configuration from input parameters.
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

        ConcordAgentConfiguration configuration;
        if (args.length == 1 && Files.exists(Path.of(args[0]))) {
            // Expect first parameter to be the URL path for configuration settings.
            configuration = json.parse(
                    ConcordAgentConfiguration.getSerializer(),
                    Files.readString(Path.of(args[0]))
            );
        } else if (Files.exists(Path.of(CONCORD_AGENT_MODEL_URI))) {
            configuration = json.parse(
                    ConcordAgentConfiguration.getSerializer(),
                    Files.readString(Path.of(CONCORD_AGENT_MODEL_URI))
            );
        } else {
            throw new RuntimeException("Configuration not provided to agent.");
        }

        // Create the required configuration files etc for this concord node.
        AgentDockerClient client = new AgentDockerClient(configuration);
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
