/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord.agent;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import java.util.regex.Pattern;

import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.amazonaws.services.s3.model.S3Object;
import com.amazonaws.services.s3.model.S3ObjectInputStream;
import com.github.dockerjava.api.DockerClient;
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
    private String concordImageId;
    private String ethrpcImageId;

    /** S3 Client. */
    private AwsS3Client awsClient;

    /** S3 bucket. */
    private String s3bucket;

    private void initializeS3Client() {
        /* Default application.properties file. */
        String propertyFile = "applications.properties";

        log.info("Reading the application properties file");

        try (InputStream input = getClass().getClassLoader().getResourceAsStream(propertyFile)) {
            // TODO - these default values needs to be specific to CICD pipeline account.
            String awsKey = "AKIATWKQRJCJPZPBH4PK";
            String secretKey = "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";
            String region = "us-west-2";
            s3bucket = "concord-config";

            Properties prop = new Properties();
            if (input != null) {
                prop.load(input);
                awsKey = prop.getProperty("aws.accessKey");
                secretKey = prop.getProperty("aws.secretKey");
                region = prop.getProperty("aws.region");
                s3bucket = prop.getProperty("aws.bucket");
            } else {
                log.error("Unable to find property file: " + propertyFile);
            }
            awsClient = new AwsS3Client(awsKey, secretKey, region);
            log.info("Create s3 client successfully");
        } catch (IOException ex) {
            log.error("Unable to read the property file: " + propertyFile);
        }
    }

    /**
     * Default constructor.
     */
    AgentDockerClient(ConcordAgentConfiguration configuration) {
        this.configuration = configuration;

        initializeS3Client();
    }

    private void populateConfig() {
        try {
            String clusterId = configuration.getCluster();
            String nodeId = configuration.getNode();
            String configPath = clusterId + "/" + nodeId;
            S3Object s3object = awsClient.getObject(s3bucket, configPath);
            S3ObjectInputStream inputStream = s3object.getObjectContent();
            FileUtils.copyInputStreamToFile(inputStream, new File("/concord/config-local/concord.config"));

            log.info("Copied the " + configPath + "to concord.config");
            File src = new File("/concord/config-local/concord.config");
            File copied = new File("/concord/config-local/concord_with_hostnames.config");
            FileUtils.copyFile(src, copied);
        } catch (IOException ex) {
            log.error("Couldn't write to s3 bucket");
        }
    }

    /**
     * Create the dockerClient volume in the root node.
     */
    void startConcord() {
        log.info("Reading config file");

        populateConfig();

        log.info("Populated the config file");

        var registryUsername = configuration.getContainerRegistry()
                .getCredential().getPasswordCredential().getUsername();
        var registryPassword = configuration.getContainerRegistry()
                .getCredential().getPasswordCredential().getPassword();
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

                // For now, just block synchronously (can't proceed without successful completion).
                command.awaitCompletion();

                var imageId = docker.listImagesCmd().exec().stream()
                        .filter(image -> {
                            for (var tag : image.getRepoTags()) {
                                if (tag.contains(componentImage.getRepository())
                                        && tag.contains(componentImage.getTag())) {
                                    return true;
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

        DockerClient dockerClient = DockerClientBuilder.getInstance().build();
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
