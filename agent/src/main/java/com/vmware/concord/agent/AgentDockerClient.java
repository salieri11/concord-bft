/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord.agent;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.Properties;
import java.util.UUID;
import java.util.regex.Pattern;

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
import com.vmware.blockchain.awsutil.AwsS3Client;
import com.vmware.blockchain.deployment.model.ConcordAgentConfiguration;

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

    /** Path to retrieve configuration. */
    private String configurationPath;

    private AwsS3Client newS3Client() {
        /* Default application.properties file. */
        var propertyFile = "applications.properties";

        log.info("Reading the application properties file");

        try (var input = getClass().getClassLoader().getResourceAsStream(propertyFile)) {
            // TODO - these default values needs to be specific to CICD pipeline account.
            var awsKey = "AKIATWKQRJCJPZPBH4PK";
            var secretKey = "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";
            var region = "us-west-2";
            configurationPath = "concord-config";

            var prop = new Properties();
            if (input != null) {
                prop.load(input);
                awsKey = prop.getProperty("aws.accessKey");
                secretKey = prop.getProperty("aws.secretKey");
                region = prop.getProperty("aws.region");
                configurationPath = prop.getProperty("aws.bucket");
            } else {
                log.error("Unable to find property file: " + propertyFile);
            }

            var client = new AwsS3Client(awsKey, secretKey, region);

            log.info("Create s3 client successfully");

            return client;
        } catch (IOException ex) {
            log.error("Unable to read the property file: " + propertyFile);

            throw new RuntimeException(ex);
        }
    }

    /**
     * Default constructor.
     */
    AgentDockerClient(ConcordAgentConfiguration configuration) {
        this.configuration = configuration;
    }

    /**
     * Retrieve the configuration for this node.
     */
    private void populateConfig() {
        var localConfigPath = Path.of("/concord/config-local/concord.config");
        var withHostConfig = Path.of("/concord/config-local/concord_with_hostnames.config");
        var localConfigFileSize = 0L;

        // Try to determine the file size of the config file.
        // Note: This is done here instead of in-line check because of potential exceptions.
        try {
            localConfigFileSize = Files.size(localConfigPath);
        } catch (Throwable error) {
            log.info("Cannot determine file size of {}", localConfigPath);
        }

        // Do not over-write existing configuration.
        // As an intended side-effect, do not engage in network IO unless absolutely needed.
        if (!Files.exists(localConfigPath) || localConfigFileSize == 0) {
            try {
                var s3Client = newS3Client();
                var clusterId = new UUID(configuration.getCluster().getHigh(),
                                         configuration.getCluster().getLow()).toString();
                var nodeId = new UUID(configuration.getNode().getHigh(),
                                      configuration.getNode().getLow()).toString();
                var sourceConfigPath = clusterId + "/" + nodeId;
                log.info("Configuration path: {}", sourceConfigPath);

                var s3object = s3Client.getObject(configurationPath, sourceConfigPath);
                var inputStream = s3object.getObjectContent();
                Files.copy(inputStream, localConfigPath, StandardCopyOption.REPLACE_EXISTING);

                log.info("Copied {} to {}", sourceConfigPath, localConfigPath);
            } catch (IOException error) {
                log.error("Cannot read from configuration source", error);
            }
        }

        // Do not over-write existing configuration.
        if (!Files.exists(withHostConfig)) {
            try {
                Files.copy(localConfigPath, withHostConfig, StandardCopyOption.REPLACE_EXISTING);

                log.info("Copied {} to {}", localConfigPath, withHostConfig);
            } catch (IOException error) {
                log.error("Cannot write to {}", withHostConfig, error);
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

        populateConfig();

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
                                            new PortBinding(Ports.Binding.bindPort(3501), ExposedPort.udp(3501)),
                                            new PortBinding(Ports.Binding.bindPort(3502), ExposedPort.udp(3502)),
                                            new PortBinding(Ports.Binding.bindPort(3503), ExposedPort.udp(3503)),
                                            new PortBinding(Ports.Binding.bindPort(3504), ExposedPort.udp(3504)),
                                            new PortBinding(Ports.Binding.bindPort(3505), ExposedPort.udp(3505)))
                                    .withBinds(Bind.parse("/concord/config-local:/concord/config-local"),
                                               Bind.parse("/concord/config-public:/concord/config-public")))
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
