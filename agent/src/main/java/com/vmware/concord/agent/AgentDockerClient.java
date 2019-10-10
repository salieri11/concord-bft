/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord.agent;

import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.dockerjava.api.DockerClient;
import com.github.dockerjava.api.model.AuthConfig;
import com.github.dockerjava.api.model.HostConfig;
import com.github.dockerjava.core.DefaultDockerClientConfig;
import com.github.dockerjava.core.DockerClientBuilder;
import com.github.dockerjava.core.command.PullImageResultCallback;
import com.vmware.blockchain.deployment.v1.ConcordAgentConfiguration;
import com.vmware.blockchain.deployment.v1.ConcordComponent;
import com.vmware.blockchain.deployment.v1.ConfigurationComponent;

/**
 * Utility class for talking to Docker and creating the required volumes, starting
 * concord-node etc.
 */
final class AgentDockerClient {

    private static final Logger log = LoggerFactory.getLogger(AgentDockerClient.class);

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
        private String repository;
        private String tag;

        private ContainerImage(String name) {

            try {
                var matcher = IMAGE_NAME_PATTERN.matcher(name);
                if (matcher.matches()) {
                    if (matcher.group("repository") != null) {
                        repository = matcher.group("repository");
                    }
                    if (matcher.group("tag") != null) {
                        tag = matcher.group("tag");
                    }
                }
            } catch (Exception error) {
                log.error("Encountered error while parsing name, using default values", error);
                throw new RuntimeException("Error parsing Image name:" + name);
            }
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

    private final ConfigServiceInvoker configServiceInvoker;

    /**
     * Default constructor.
     */
    AgentDockerClient(ConcordAgentConfiguration configuration) {
        this.configuration = configuration;

        // Temporary hack for supporting proxy and rest.

        boolean useRest = false;

        if (!configuration.getOutboundProxyInfo().getHttpsHost().isBlank()) {
            useRest = true;

            // Also set the proxy.
            System.setProperty("http.proxyHost", configuration.getOutboundProxyInfo().getHttpHost());
            System.setProperty("http.proxyPort", String.valueOf(configuration.getOutboundProxyInfo().getHttpPort()));
            System.setProperty("https.proxyHost", configuration.getOutboundProxyInfo().getHttpsHost());
            System.setProperty("https.proxyPort", String.valueOf(configuration.getOutboundProxyInfo().getHttpsPort()));
        }

        this.configServiceInvoker = new ConfigServiceInvoker(configuration.getConfigService(), useRest);
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
        // Download configuration and certs.
        setupConfig();

        // Pull and order images
        List<ContainerConfig> containerConfigList = pullImages();

        containerConfigList.sort(Comparator.comparingInt(ContainerConfig::ordinal));

        var dockerClient = DockerClientBuilder.getInstance().build();
        containerConfigList.forEach(container -> launchContainer(dockerClient, container));
    }

    private List<ContainerConfig> pullImages() {
        final var registryUsername = configuration.getContainerRegistry()
                .getCredential().getPasswordCredential().getUsername();
        final var registryPassword = configuration.getContainerRegistry()
                .getCredential().getPasswordCredential().getPassword();

        List<CompletableFuture<ContainerConfig>> futures = new ArrayList<>();
        for (var component : configuration.getModel().getComponents()) {
            // Bypass non service type image pull...
            if (component.getServiceType() != ConcordComponent.ServiceType.GENERIC) {
                futures.add(CompletableFuture
                                    .supplyAsync(() -> getImageIdAfterDl(registryUsername,
                                                                         registryPassword, component)));
            }
        }

        return CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]))
                .thenApply((res) -> futures.stream()
                .map(CompletableFuture::join)
                .collect(Collectors.toList())).join();
    }

    private ContainerConfig getImageIdAfterDl(String registryUsername, String registryPassword,
                                              ConcordComponent component) {

        log.info("Pulling image {}", component.getName());
        var containerConfig = ContainerConfig.valueOf(component.getServiceType().name());
        var registryUrl = URI.create(configuration.getContainerRegistry().getAddress());
        var clientConfig = DefaultDockerClientConfig.createDefaultConfigBuilder()
                .withRegistryUrl(registryUrl.getAuthority())
                .withRegistryUsername(registryUsername)
                .withRegistryPassword(registryPassword)
                .build();
        var docker = DockerClientBuilder.getInstance(clientConfig).build();
        try {
            var componentImage = new ContainerImage(component.getName());

            // Full image name is the supplied registry authority + the component image name.
            var componentImageName = String.format(
                    "%s/%s",
                    registryUrl.getAuthority(),
                    componentImage.getRepository()
            );
            var command = docker.pullImageCmd(componentImageName)
                    .withRegistry(registryUrl.getAuthority())
                    .withTag(componentImage.getTag())
                    .withAuthConfig(
                            new AuthConfig()
                                    .withUsername(registryUsername)
                                    .withPassword(registryPassword)
                    )
                    .exec(new PullImageResultCallback());

            // For now, just block synchronously.
            try {
                log.info("Waiting image {}", component.getName());
                command.awaitCompletion();
                log.info("Pulled image {}", component.getName());
            } catch (Throwable error) {
                // If imaging fetching fails, try to proceed forward anyway.
                log.error("Pulling image({}:{}) from registry({}) failed",
                          componentImage.getRepository(),
                          componentImage.getTag(),
                          configuration.getContainerRegistry().getAddress());
            }

            var inspectResponse = docker
                    .inspectImageCmd(componentImageName + ":" + componentImage.getTag())
                    .exec();
            containerConfig.imageId = inspectResponse.getId();
            log.info("Container image ID: {}", containerConfig.imageId);
        } catch (Throwable error) {
            log.error("Error while pulling image for component({})", component.getName(), error);
        } finally {
            try {
                docker.close();
            } catch (IOException error) {
                log.error("Error while closing docker client", error);
            }
        }
        return containerConfig;
    }

    private void setupConfig() {
        log.info("Reading config file");

        var configList = configServiceInvoker.retrieveConfiguration(configuration.getConfigurationSession(),
                                                                    configuration.getNode());
        writeConfiguration(configList);

        // TODO Why do we need 2 paths?
        var localConfigPath = Path.of("/config/concord/config-local/concord.config");
        var withHostConfig = Path.of("/config/concord/config-local/concord_with_hostnames.config");

        // Do not over-write existing configuration.
        if (!Files.exists(withHostConfig)) {
            try {
                Files.copy(localConfigPath, withHostConfig, StandardCopyOption.REPLACE_EXISTING);
                log.info("Copied {} to {}", localConfigPath, withHostConfig);
            } catch (IOException error) {
                log.error("Cannot write to " + withHostConfig, error);
            }
        }

        log.info("Populated the config file");
    }

    private void launchContainer(DockerClient dockerClient, ContainerConfig containerParam) {

        var createContainerCmd = dockerClient.createContainerCmd(containerParam.containerName)
                .withName(containerParam.containerName)
                .withImage(containerParam.imageId);

        if (containerParam.cmds != null) {
            createContainerCmd.withCmd(containerParam.cmds);
        }

        if (containerParam.volumeBindings != null || containerParam.portBindings != null) {
            HostConfig hostConfig = HostConfig.newHostConfig();

            if (containerParam.volumeBindings != null) {
                hostConfig.withBinds(containerParam.volumeBindings);
            }

            if (containerParam.portBindings != null) {
                hostConfig.withPortBindings(containerParam.portBindings);
            }

            if (containerParam.links != null) {
                hostConfig.withLinks(containerParam.links);
            }
            createContainerCmd.withHostConfig(hostConfig);
        }

        if (containerParam.environment != null) {
            createContainerCmd.withEnv(containerParam.environment);
        }

        log.info("Create container: {}", createContainerCmd.toString());
        var container = createContainerCmd.exec();
        if (container == null) {
            log.error("Couldn't start {} container...!", containerParam.containerName);
        } else {
            log.info("Starting {}: Id {} ", containerParam.containerName, container.getId());
            dockerClient.startContainerCmd(container.getId()).exec();
            log.info("Started container {}: Id {} ", containerParam.containerName, container.getId());
        }
    }
}