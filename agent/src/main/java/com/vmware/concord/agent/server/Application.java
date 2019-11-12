/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord.agent.server;

import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import com.vmware.blockchain.deployment.v1.ConcordAgentConfiguration;
import com.vmware.blockchain.deployment.v1.OutboundProxyInfo;
import com.vmware.concord.agent.services.AgentDockerClient;

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

    /** Logger instance. */
    private static Logger log = LoggerFactory.getLogger(Application.class);

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
                        false, /* allowStructuredMapKeys */
                        false, /* prettyPrint */
                        "    ", /* indent */
                        false, /* useArrayPolymorphism */
                        "type", /* classDiscriminator */
                        UpdateMode.OVERWRITE /* updateMode */
                ),
                EmptyModule.INSTANCE
        );

        if (Files.exists(Path.of(CONCORD_AGENT_MODEL_URI))) {
            ConcordAgentConfiguration configuration = json.parse(
                    ConcordAgentConfiguration.getSerializer(),
                    Files.readString(Path.of(CONCORD_AGENT_MODEL_URI))
            );

            // Setup process-level proxy before starting main agent machinery.
            configureNetworkProxyConfiguration(configuration.getOutboundProxyInfo());

            // Create the required configuration files etc for this concord node.
            AgentDockerClient client = new AgentDockerClient(configuration);
            client.bootstrapConcord();
            SpringApplication.run(Application.class, args);
        } else {
            throw new RuntimeException("Configuration not provided to agent.");
        }
    }

    /**
     * Configure the JVM process environment according to network proxy configuration.
     *
     * @param proxy
     *   network proxy configuration.
     */
    private static void configureNetworkProxyConfiguration(OutboundProxyInfo proxy) {
        if (!OutboundProxyInfo.Companion.getDefaultValue().equals(proxy)) {
            log.info("Configuring process for network proxy environment, info({})", proxy);

            System.setProperty("http.proxyHost", proxy.getHttpHost());
            System.setProperty("http.proxyPort", String.valueOf(proxy.getHttpPort()));
            System.setProperty("https.proxyHost", proxy.getHttpsHost());
            System.setProperty("https.proxyPort", String.valueOf(proxy.getHttpsPort()));
        }
    }
}
