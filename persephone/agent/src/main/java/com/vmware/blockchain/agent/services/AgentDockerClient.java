/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services;

import java.io.IOException;
import java.net.URI;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.github.dockerjava.api.exception.DockerException;
import com.github.dockerjava.api.model.AuthConfig;
import com.github.dockerjava.api.model.Network;
import com.github.dockerjava.core.DefaultDockerClientConfig;
import com.github.dockerjava.core.DockerClientBuilder;
import com.github.dockerjava.core.command.PullImageResultCallback;
import com.vmware.blockchain.agent.services.configuration.BaseContainerSpec;
import com.vmware.blockchain.deployment.v1.Endpoint;


/**
 * Utility class for talking to Docker and creating the required volumes, starting
 * concord-node etc.
 */
@Component
public final class AgentDockerClient {

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

    private Endpoint containerRegistry;

    /**
     * Default constructor.
     */
    @Autowired
    public AgentDockerClient(Endpoint containerRegistry) {
        this.containerRegistry = containerRegistry;
    }

    BaseContainerSpec getImageIdAfterDl(BaseContainerSpec containerConfig,
                                        String registryUsername, String registryPassword,
                                        String imageName) {

        log.info("Pulling image {}", imageName);

        var registryUrl = URI.create(containerRegistry.getAddress());
        var clientConfig = DefaultDockerClientConfig.createDefaultConfigBuilder()
                .withRegistryUrl(registryUrl.getAuthority())
                .withRegistryUsername(registryUsername)
                .withRegistryPassword(registryPassword)
                .build();
        var docker = DockerClientBuilder.getInstance(clientConfig).build();
        try {
            var componentImage = new ContainerImage(imageName);

            // Full image name is the supplied registry authority + the component image name.
            var componentImageName = String.format(
                    "%s/%s",
                    registryUrl.getAuthority(),
                    componentImage.getRepository()
            );
            log.info("Component image name {} tag {}", componentImageName, componentImage.getTag());
            int pullTryCount = 3;
            boolean imagePullPending = true;
            while (imagePullPending && pullTryCount-- > 0) {
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
                    log.info("Waiting image {}", imageName);
                    command.awaitCompletion();
                    imagePullPending = false;
                    log.info("Pulled image {}", imageName);
                } catch (Throwable error) {
                    // If imaging fetching fails, try to proceed forward anyway.
                    log.error("Pulling image({}:{}) from registry({}) failed",
                              componentImage.getRepository(),
                              componentImage.getTag(),
                              containerRegistry.getAddress());
                }
            }

            var inspectResponse = docker
                    .inspectImageCmd(componentImageName + ":" + componentImage.getTag())
                    .exec();
            containerConfig.setImageId(inspectResponse.getId());
            log.info("Container image ID: {}", containerConfig.getImageId());
        } catch (Throwable error) {
            log.error("Error while pulling image for component({})", imageName, error);
            throw error;
        } finally {
            try {
                docker.close();
            } catch (IOException error) {
                log.error("Error while closing docker client", error);
            }
        }
        return containerConfig;
    }

    void createNetwork(String networkName) {
        var dockerClient = DockerClientBuilder.getInstance().build();
        var deleteNetworkCmd = dockerClient.removeNetworkCmd(networkName);
        var listNetworkCmd = dockerClient.listNetworksCmd();
        var createNetworkCmd = dockerClient.createNetworkCmd();
        createNetworkCmd.withName(networkName);
        createNetworkCmd.withCheckDuplicate(true);

        try {
            List<String> networks = listNetworkCmd.exec()
                    .stream()
                    .map(Network::getName)
                    .collect(Collectors.toList());
            if (networks.contains(networkName)) {
                deleteNetworkCmd.exec();
            }
            var id = createNetworkCmd.exec().getId();
            log.info("Created Network: {} Id: {}", networkName, id);
        } catch (DockerException de) {
            log.info("Docker Network creation failed with error:\n{}", de.getLocalizedMessage());
        }
    }
}
