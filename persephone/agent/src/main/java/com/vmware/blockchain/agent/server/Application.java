/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.server;

import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;

import com.google.protobuf.util.JsonFormat;
import com.vmware.blockchain.deployment.v1.ConcordAgentConfiguration;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;
import com.vmware.blockchain.deployment.v1.Endpoint;
import com.vmware.blockchain.deployment.v1.OutboundProxyInfo;

import lombok.extern.slf4j.Slf4j;

/**
 * Concord-agent --
 * This is the main for running agent.
 */
@SpringBootApplication
@Slf4j
@ComponentScan({"com.vmware.blockchain.agent.*"})
public class Application {

    static ConcordAgentConfiguration configuration;

    /**
     * Main - Entry into SpringBoot application.
     */
    public static void main(String[] args) throws Exception {
        // Default path used in production. Property is added to improve local development
        String filePath = "file:" + System.getProperty("config.file", "/config/agent/config.json");

        URI configUri = URI.create(filePath);
        if (Files.exists(Path.of(configUri))) {

            ConcordAgentConfiguration.Builder builder = ConcordAgentConfiguration.newBuilder();
            JsonFormat.parser().ignoringUnknownFields().merge(Files.readString(Path.of(configUri)),
                                                              builder);

            Application.configuration = builder.build();

            // Setup process-level proxy before starting main agent machinery.
            configureNetworkProxyConfiguration(configuration.getOutboundProxyInfo());

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
        if (!OutboundProxyInfo.getDefaultInstance().equals(proxy)) {
            log.info("Configuring process for network proxy environment, info({})", proxy);

            System.setProperty("http.proxyHost", proxy.getHttpHost());
            System.setProperty("http.proxyPort", String.valueOf(proxy.getHttpPort()));
            System.setProperty("https.proxyHost", proxy.getHttpsHost());
            System.setProperty("https.proxyPort", String.valueOf(proxy.getHttpsPort()));
        }
    }

    @Bean
    public ConcordAgentConfiguration concordAgentConfiguration() {
        return configuration;
    }

    @Bean
    public Endpoint configServiceEndpoint() {
        return configuration.getConfigService();
    }

    @Bean
    public ConcordModelSpecification.NodeType nodeType() {
        return configuration.getModel().getNodeType();
    }

    @Bean
    public String nodeIdentifier() {
        return configuration.getNodeId();
    }

    @Bean
    public Endpoint containerRegistry() {
        return configuration.getContainerRegistry();
    }
}
