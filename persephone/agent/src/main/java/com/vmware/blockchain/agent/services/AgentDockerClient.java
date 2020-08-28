/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services;

import java.io.IOException;
import java.net.URI;
import java.time.ZonedDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.github.dockerjava.api.DockerClient;
import com.github.dockerjava.api.command.InspectContainerResponse;
import com.github.dockerjava.api.exception.DockerException;
import com.github.dockerjava.api.model.AuthConfig;
import com.github.dockerjava.api.model.Network;
import com.github.dockerjava.core.DefaultDockerClientConfig;
import com.github.dockerjava.core.DockerClientBuilder;
import com.github.dockerjava.core.command.PullImageResultCallback;
import com.vmware.blockchain.agent.services.configuration.BaseContainerSpec;
import com.vmware.blockchain.agent.services.exceptions.AgentException;
import com.vmware.blockchain.agent.services.exceptions.ErrorCode;
import com.vmware.blockchain.agent.services.metrics.MetricsAgent;
import com.vmware.blockchain.agent.services.metrics.MetricsConstants;
import com.vmware.blockchain.deployment.v1.Endpoint;

import io.micrometer.core.instrument.Tag;
import io.micrometer.core.instrument.Timer;
import io.micrometer.core.instrument.simple.SimpleMeterRegistry;


/**
 * Utility class for talking to Docker and creating the required volumes, starting
 * concord-node etc.
 */
@Component
public class AgentDockerClient {

    private static final Logger log = LoggerFactory.getLogger(AgentDockerClient.class);

    /**
     * Regular expression pattern matching a Docker image name.
     *
     * <p>Note: The pattern does not attempt to match registry against a valid DNS host pattern.
     */
    private static final Pattern IMAGE_NAME_PATTERN =
            Pattern.compile("(?:(?<registry>[^:]+:?[0-9]+)/)?(?<repository>[^:]+)(?::(?<tag>.+))?");

    /** metrics from agent. **/
    List<Tag> tags = Arrays.asList(Tag.of(MetricsConstants.MetricsTags.TAG_SERVICE.name(),
            AgentDockerClient.class.getName()));
    private final MetricsAgent metricsAgent = new MetricsAgent(new SimpleMeterRegistry(), tags);

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
                throw new AgentException(ErrorCode.DOCKER_CLIENT_PARSING_IMAGE_NAME_FAILURE,
                                         "Encountered error while parsing name [" + name + "], using default values "
                                          + error.getMessage(), error);
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

        var startMillis = ZonedDateTime.now().toInstant().toEpochMilli();
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
            throw new AgentException(ErrorCode.DOCKER_CLIENT_PULLING_IMAGE_FAILURE,
                                    "Error while pulling image for component [" + imageName + "] " + error.getMessage(),
                                    error);
        } finally {
            try {
                docker.close();
            } catch (IOException error) {
                log.error("Error while closing docker client", error);
            }
        }

        var stopMillis = ZonedDateTime.now().toInstant().toEpochMilli();
        Timer timer = this.metricsAgent.getTimer("Pull each component image",
                MetricsConstants.MetricsNames.CONTAINER_PULL_IMAGE,
                List.of(Tag.of(MetricsConstants.MetricsTags.TAG_METHOD.name(), "getImageIdAfterDl"),
                        Tag.of(MetricsConstants.MetricsTags.TAG_IMAGE.name(), imageName)));
        timer.record(stopMillis - startMillis, TimeUnit.MILLISECONDS);
        return containerConfig;
    }

    void createNetwork(String networkName) {
        Timer timer = this.metricsAgent.getTimer("Create container network",
                MetricsConstants.MetricsNames.CREATE_NETWORK,
                List.of(Tag.of(MetricsConstants.MetricsTags.TAG_METHOD.name(), "createNetwork"),
                        Tag.of(MetricsConstants.MetricsTags.TAG_DOCKER_NETWORK.name(), networkName)));

        timer.record(() -> {
            var dockerClient = DockerClientBuilder.getInstance().build();
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
                    log.info("Network: {} already exists.", networkName);
                } else {
                    var id = createNetworkCmd.exec().getId();
                    log.info("Created Network: {} Id: {}", networkName, id);
                }
            } catch (DockerException de) {
                log.info("Docker Network creation failed with error:\n{}", de.getLocalizedMessage());
            }
        });
    }

    /**
     * Helper util to fetch a container id.
     */
    public InspectContainerResponse inspectContainer(DockerClient dockerClient, String name) {

        var inspectContainerCmd = dockerClient.inspectContainerCmd(name);

        var container = inspectContainerCmd.exec();
        if (container == null) {
            log.error("Couldn't GET {} container...!", name);
        }
        return container;
    }

    /**
     * Helper util to start a container.
     */
    public void startComponent(DockerClient dockerClient, BaseContainerSpec containerParam,
                                      String containerId) {
        Timer timer = this.metricsAgent.getTimer("Start each component container",
                MetricsConstants.MetricsNames.CONTAINER_LAUNCH,
                List.of(Tag.of(MetricsConstants.MetricsTags.TAG_METHOD.name(), "startComponent"),
                        Tag.of(MetricsConstants.MetricsTags.TAG_CONTAINER_ID.name(), containerId)));
        timer.record(() -> {
            log.info("Starting {}: Id {} ", containerParam.getContainerName(), containerId);
            dockerClient.startContainerCmd(containerId).exec();
            log.info("Started container {}: Id {} ", containerParam.getContainerName(), containerId);
        });
    }

    /**
     * Helper util to stop a container.
     */
    public void stopComponent(DockerClient dockerClient, BaseContainerSpec containerParam,
                               String containerId) {
        Timer timer = this.metricsAgent.getTimer("Stop each component container",
                MetricsConstants.MetricsNames.CONTAINER_STOP,
                List.of(Tag.of(MetricsConstants.MetricsTags.TAG_METHOD.name(), "stopComponent"),
                        Tag.of(MetricsConstants.MetricsTags.TAG_CONTAINER_ID.name(), containerId)));
        timer.record(() -> {
            log.info("Stopping {}: Id {} ", containerParam.getContainerName(), containerId);
            dockerClient.stopContainerCmd(containerId).exec();
            log.info("Stopped container {}: Id {} ", containerParam.getContainerName(), containerId);
        });
    }
}
