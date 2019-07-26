/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord.agent;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;

import javax.net.ssl.SSLException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.dockerjava.api.model.AuthConfig;
import com.github.dockerjava.api.model.Bind;
import com.github.dockerjava.api.model.ExposedPort;
import com.github.dockerjava.api.model.HostConfig;
import com.github.dockerjava.api.model.Link;
import com.github.dockerjava.api.model.PortBinding;
import com.github.dockerjava.api.model.Ports;
import com.github.dockerjava.core.DefaultDockerClientConfig;
import com.github.dockerjava.core.DockerClientBuilder;
import com.github.dockerjava.core.command.PullImageResultCallback;
import com.vmware.blockchain.deployment.model.ConcordAgentConfiguration;
import com.vmware.blockchain.deployment.model.ConfigurationComponent;
import com.vmware.blockchain.deployment.model.ConfigurationServiceStub;
import com.vmware.blockchain.deployment.model.ConfigurationSessionIdentifier;
import com.vmware.blockchain.deployment.model.MessageHeader;
import com.vmware.blockchain.deployment.model.NodeConfigurationRequest;
import com.vmware.blockchain.deployment.model.NodeConfigurationResponse;

import io.grpc.CallOptions;
import io.grpc.netty.shaded.io.grpc.netty.GrpcSslContexts;
import io.grpc.netty.shaded.io.grpc.netty.NettyChannelBuilder;

/**
 * Utility class for talking to Docker and creating the required volumes, starting
 * concord-node etc.
 */
final class AgentDockerClient {

    private static final Logger log = LoggerFactory.getLogger(AgentDockerClient.class);

    private static final String DEFAULT_REGISTRY = "registry-1.docker.io";
    private static final String COMPONENT_CONCORD = "concord-core";
    private static final String COMPONENT_ETHEREUM_RPC = "ethrpc";

    /**
     * Regular expression pattern matching a Docker image name.
     *
     * <p>Note: The pattern does not attempt to match registry against a valid DNS host pattern.
     */
    private static final Pattern IMAGE_NAME_PATTERN =
            Pattern.compile("(?:(?<registry>[^:]+:?[0-9]+)/)?(?<repository>[^:]+)(?::(?<tag>.+))?");

    /**
     * Data class to contain metadata information about a container image.
     */
    private static class ContainerImage {

        private final String registry;
        private final String repository;
        private final String tag;

        private ContainerImage(String name) {
            // Setup default values in case parsing was unsuccessful.
            var registry = DEFAULT_REGISTRY;
            var repository = name;
            var tag = "latest";

            try {
                var matcher = IMAGE_NAME_PATTERN.matcher(name);
                if (matcher.matches()) {
                    if (matcher.group("registry") != null) {
                        registry = matcher.group("registry");
                    }
                    if (matcher.group("repository") != null) {
                        repository = matcher.group("repository");
                    }
                    if (matcher.group("tag") != null) {
                        tag = matcher.group("tag");
                    }
                }
            } catch (Exception error) {
                log.error("Encountered error while parsing name, using default values", error);
            } finally {
                // Regardless of match error or exception thrown, always map to some safe value.
                this.registry = registry;
                this.repository = repository;
                this.tag = tag;
            }
        }

        private String getRegistry() {
            return registry;
        }

        private String getRepository() {
            return repository;
        }

        private String getTag() {
            return tag;
        }
    }

    /** Configuration parameters for this agent instance. */
    private final ConcordAgentConfiguration configuration;

    /** Configuration Service RPC Stub. */
    private final ConfigurationServiceStub configurationService;

    /**
     * Default constructor.
     */
    AgentDockerClient(ConcordAgentConfiguration configuration) throws Exception {
        this.configuration = configuration;

        var configServiceTrustCertificate = URI.create("file:/config/persephone/provisioning/configservice.crt");

        try {
            var channel = NettyChannelBuilder
                    .forTarget(configuration.getConfigService().getAddress())
                    .sslContext(
                            GrpcSslContexts.forClient()
                                    .trustManager(new File(configServiceTrustCertificate)).build())
                    .build();
            this.configurationService = new ConfigurationServiceStub(channel, CallOptions.DEFAULT);
        } catch (SSLException e) {
            log.error("Could not create configuration service stub. Exception: " + e.getLocalizedMessage());
            throw new Exception(e);
        }
    }

    /**
     * Retrieves the configuration for the node represented by this agent from Configuration
     * service.
     *
     * @param session
     *   configuration session identifier.
     * @param node
     *   node identifier.
     *
     * @return
     *   list of {@link ConfigurationComponent}s.
     */
    private List<ConfigurationComponent> retrieveConfiguration(
            ConfigurationSessionIdentifier session,
            int node
    ) {
        var request = new NodeConfigurationRequest(new MessageHeader(), session, node);
        var responseObserver = new StreamObservers.MonoObserverFuture<NodeConfigurationResponse>();

        // Request for config.
        configurationService.getNodeConfiguration(request, responseObserver);

        // Synchronously wait for result to return.
        try {
            return responseObserver.asCompletableFuture().join().getConfigurationComponent();
        } catch (Throwable error) {
            log.error("Configuration retrieval failed", error);

            return Collections.emptyList();
        }
    }

    /**
     * Write out a set of configuration components to the artifact destination (reachable from the
     * associated service container).
     *
     * <p>Note: Artifact is reachable because the agent will mount the same configuration volume as the
     * service components.
     *
     * @param artifacts
     *   list of {@link ConfigurationComponent} to be written.
     */
    private void writeConfiguration(List<ConfigurationComponent> artifacts) {
        for (var artifact : artifacts) {
            var destination = URI.create(artifact.getComponentUrl());
            var filepath = Path.of("/config", destination.getPath());

            try {
                Files.createDirectories(filepath.getParent());

                // Use synchronous IO (can be changed if this becomes a performance bottleneck).
                var outputStream = new FileOutputStream(filepath.toFile(), false);
                outputStream.write(artifact.getComponent().getBytes(StandardCharsets.UTF_8));
            } catch (Exception error) {
                log.error("Error populating configuration for {}", destination, error);
            }
        }
    }

    /**
     * Start the local setup as a Concord node.
     */
    void startConcord() {
        String concordImageId = null;
        String ethrpcImageId = null;

        log.info("Reading config file");

        var configList = retrieveConfiguration(configuration.getConfigurationSession(), configuration.getNode());
        writeConfiguration(configList);

        log.info("Populated the config file");

        var registryUsername = configuration.getContainerRegistry()
                .getCredential().getPasswordCredential().getUsername();
        registryUsername = registryUsername.isBlank() ? null : registryUsername;
        var registryPassword = configuration.getContainerRegistry()
                .getCredential().getPasswordCredential().getPassword();
        registryPassword = registryPassword.isBlank() ? null : registryPassword;

        for (var component : configuration.getModel().getComponents()) {
            var componentImage = new ContainerImage(component.getName());
            var clientConfig = DefaultDockerClientConfig.createDefaultConfigBuilder()
                    .withRegistryUrl(configuration.getContainerRegistry().getAddress())
                    .withRegistryUsername(registryUsername)
                    .withRegistryPassword(registryPassword)
                    .build();
            var docker = DockerClientBuilder.getInstance(clientConfig).build();
            try {
                var command = docker.pullImageCmd(componentImage.getRepository())
                        .withTag(componentImage.getTag())
                        .withRegistry(componentImage.getRegistry())
                        .withAuthConfig(
                                new AuthConfig()
                                        .withUsername(registryUsername)
                                        .withPassword(registryPassword)
                        )
                        .exec(new PullImageResultCallback());

                // For now, just block synchronously.
                try {
                    command.awaitCompletion();
                } catch (Throwable error) {
                    // If imaging fetching fails, try to proceed forward anyway.
                    log.error("Pulling image({}:{}) from registry({}) failed",
                              componentImage.getRepository(),
                              componentImage.getTag(),
                              componentImage.getRegistry());
                }

                var imageId = docker.listImagesCmd().exec().stream()
                        .filter(image -> {
                            log.info("Image: id({}), created({})", image.getId(), image.getCreated());

                            if (image.getRepoTags() != null) {
                                for (var tag : image.getRepoTags()) {
                                    log.info("Tag: {}", tag);

                                    if (tag.contains(componentImage.getRepository())
                                            && tag.contains(componentImage.getTag())) {
                                        return true;
                                    }
                                }
                            }
                            return false;
                        })
                        .findFirst()
                        .orElseThrow();

                // Save the image ID of the images we pulled for components that we can start.
                if (componentImage.getRepository().contains(COMPONENT_CONCORD)) {
                    concordImageId = imageId.getId();
                } else if (componentImage.getRepository().contains(COMPONENT_ETHEREUM_RPC)) {
                    ethrpcImageId = imageId.getId();
                }
            } catch (Throwable error) {
                log.error("Error while starting component({})", component.getName(), error);
            } finally {
                try {
                    docker.close();
                } catch (IOException error) {
                    log.error("Error while closing docker client", error);
                }
            }
        }

        var dockerClient = DockerClientBuilder.getInstance().build();
        if (concordImageId != null) {
            var container = dockerClient.createContainerCmd("concord-core")
                    .withName("concord")
                    .withImage(concordImageId)
                    .withHostConfig(
                            HostConfig.newHostConfig()
                                    // .withNetworkMode("host")
                                    .withPortBindings(
                                            new PortBinding(Ports.Binding.bindPort(5458), ExposedPort.tcp(5458)),
                                            new PortBinding(Ports.Binding.bindPort(3501), ExposedPort.tcp(3501)),
                                            new PortBinding(Ports.Binding.bindPort(3502), ExposedPort.tcp(3502)),
                                            new PortBinding(Ports.Binding.bindPort(3503), ExposedPort.tcp(3503)),
                                            new PortBinding(Ports.Binding.bindPort(3504), ExposedPort.tcp(3504)),
                                            new PortBinding(Ports.Binding.bindPort(3505), ExposedPort.tcp(3505)),
                                            new PortBinding(Ports.Binding.bindPort(3501), ExposedPort.udp(3501)),
                                            new PortBinding(Ports.Binding.bindPort(3502), ExposedPort.udp(3502)),
                                            new PortBinding(Ports.Binding.bindPort(3503), ExposedPort.udp(3503)),
                                            new PortBinding(Ports.Binding.bindPort(3504), ExposedPort.udp(3504)),
                                            new PortBinding(Ports.Binding.bindPort(3505), ExposedPort.udp(3505)))
                                    .withBinds(Bind.parse("/config/concord/config-local:/concord/config-local"),
                                               Bind.parse("/config/concord/config-public:/concord/config-public")))
                    .exec();

            // Now lets start the docker image
            if (container == null) {
                log.error("Couldn't start the container...!");
            } else {
                log.info("Starting container: " + container.getId());
                dockerClient.startContainerCmd(container.getId()).exec();
                log.info("started concord: " + container.getId());
            }
        }
        // TODO: this should be done in Dockerfile and not as argument while starting the daemon.
        var cmd = "sed -i s/localhost/concord/g application.properties && java -jar concord-ethrpc.jar";
        if (ethrpcImageId != null) {
            var container = dockerClient.createContainerCmd("ethrpc")
                    .withName("ethrpc")
                    .withImage(ethrpcImageId)
                    .withCmd("/bin/bash", "-c", cmd)
                    .withHostConfig(
                            HostConfig.newHostConfig()
                                    .withPortBindings(
                                            new PortBinding(Ports.Binding.bindPort(8545), ExposedPort.tcp(8545))
                                    )
                                    .withLinks(new Link("concord", "concord"))
                    )
                    .exec();

            // Now lets start the docker image
            if (container == null) {
                log.error("Couldn't start ethrpc container...!");
            } else {
                log.info("Starting ethrpc: " + container.getId());
                dockerClient.startContainerCmd(container.getId()).exec();
                log.info("Started container: " + container.getId());
            }
        }
    }
}
