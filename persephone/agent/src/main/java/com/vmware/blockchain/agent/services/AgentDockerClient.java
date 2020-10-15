/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URI;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;

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
import com.vmware.blockchain.deployment.v1.TransportSecurity;

import io.micrometer.core.instrument.Tag;
import io.micrometer.core.instrument.Timer;


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
    Tag classTag = Tag.of(MetricsConstants.MetricsTags.TAG_SERVICE.metricsTagName, AgentDockerClient.class.getName());

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

    private final Endpoint containerRegistry;
    private final MetricsAgent metricsAgent;

    /**
     * Default constructor.
     */
    @Autowired
    public AgentDockerClient(Endpoint containerRegistry, MetricsAgent metricsAgent) {
        this.containerRegistry = containerRegistry;
        this.metricsAgent = metricsAgent;
    }

    BaseContainerSpec getImageIdAfterDl(BaseContainerSpec containerConfig,
                                        String registryUsername, String registryPassword,
                                        String imageName, Endpoint notaryServer, String notarySelfSignedCert) {

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

            // If the notary verification feature is enabled then perform notary verification
            String repoDigestFromNotary = "";
            if (!StringUtils.isEmpty(notaryServer.getAddress())) {
                repoDigestFromNotary = checkNotarySignatureVerification(notaryServer, notarySelfSignedCert,
                                                                        componentImageName, componentImage.getTag());
                log.info("RepoDigest from Notary: " + repoDigestFromNotary);
            }

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

            // Check if the Repo Digest from Docker matches the Digest provided by notary CLI
            if (!StringUtils.isEmpty(notaryServer.getAddress())) {
                List<String> repoDigestListFromDocker = inspectResponse.getRepoDigests();
                Boolean matchedRepoDigest = false;
                for (String repoDigestFromDockerWithRepo: repoDigestListFromDocker) {
                    String repoDigestFromDocker = StringUtils.substringAfter(repoDigestFromDockerWithRepo,
                                                                               "sha256:");
                    log.info("Repo Digest from Docker: " + repoDigestFromDocker);
                    if (repoDigestFromDocker.equals(repoDigestFromNotary)) {
                        matchedRepoDigest = true;
                        log.info("The Repo Digest of Container Image pulled matched Digest provided by notary");
                        break;
                    }
                }
                if (!matchedRepoDigest) {
                    log.error("The Repo Digest of Container Image pulled does not match Digest provided by notary");
                    throw new AgentException(ErrorCode.NOTARY_SIGNATURE_VERIFICATION_FAILED,
                                             "Error while matching Digest for Notary Verification",
                                             new RuntimeException());
                }
            }
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
        Timer timer = this.metricsAgent.getTimer(MetricsConstants.MetricsNames.CONTAINER_PULL_IMAGE,
                List.of(classTag, Tag.of(MetricsConstants.MetricsTags.TAG_METHOD.metricsTagName, "getImageIdAfterDl"),
                        Tag.of(MetricsConstants.MetricsTags.TAG_IMAGE.metricsTagName, imageName)));
        timer.record(stopMillis - startMillis, TimeUnit.MILLISECONDS);
        return containerConfig;
    }

    // Performs the notary verification for a docker image using Notary CLI and returns the digest provided by notary
    String checkNotarySignatureVerification(Endpoint notaryServer, String notarySelfSignedCert, String imageName,
                                            String imageTag) {
        log.info("Performing notary signature verification for image {} with tag {}", imageName, imageTag);
        int notaryVerificationRetryCount = 3;

        String notaryCommandOutput = "";
        while (notaryVerificationRetryCount-- > 0) {
            ProcessBuilder builder;

            // If self signed cert of notary server is passed, use it for tlscacert param for notary command
            if (notaryServer.getTransportSecurity() != null
                && notaryServer.getTransportSecurity().getType() != TransportSecurity.Type.NONE
                && !StringUtils.isEmpty(notaryServer.getTransportSecurity().getCertificateData())) {
                builder = new ProcessBuilder("/usr/bin/notary-Linux-amd64", "-s",
                                             notaryServer.getAddress(), "-d", "~/.docker/trust", "--tlscacert",
                                             notarySelfSignedCert, "lookup", imageName, imageTag);
            } else {
                builder = new ProcessBuilder("/usr/bin/notary-Linux-amd64", "-s",
                                             notaryServer.getAddress(), "-d", "~/.docker/trust", "lookup",
                                             imageName, imageTag);
            }

            log.info("notaryCommand is {}", builder.command());
            builder.redirectErrorStream(true);
            try {
                Process process = builder.start();
                InputStream is = process.getInputStream();
                BufferedReader reader = new BufferedReader(new InputStreamReader(is));
                String line;
                while ((line = reader.readLine()) != null) {
                    notaryCommandOutput += line;
                }
            } catch (IOException error) {
                log.error("Error while executing notary CLI command", error);
            }
            log.info("notaryCommandOutput is {}", notaryCommandOutput);

            // Handling case of error reaching the notary server
            // As this could be intermittent, retrying to recover from this situation
            // Note: This scenario should be tested before positive scenario, as in the case of the image already
            // been pulled, the notary CLI gives error along with the cached sha256 as stale verification
            if (notaryCommandOutput.contains("could not reach")) {
                if (notaryVerificationRetryCount-- > 0) {
                    log.error("Failed to reach notary server. Error:\n{}", notaryCommandOutput);
                    log.info("Retrying to verify trust data for image {} with tag {}", imageName, imageTag);
                } else {
                    log.error("Failed to reach notary server. Error:\n{}", notaryCommandOutput);
                }
                continue;
            }

            // Handling case of error of no valid trust and make agent exit gracefully
            // Note: This scenario should be checked before positive scenario, as in the case of the image already
            // been pulled, the notary CLI gives error along with the cached sha256 as stale verification
            if (notaryCommandOutput.contains("fatal: No valid trust data")) {
                log.error("No valid trust data found for the image {} with tag {}", imageName, imageTag);
                throw new AgentException(ErrorCode.NOTARY_SIGNATURE_VERIFICATION_FAILED,
                                         "Error while verifying notary signature", new RuntimeException());
            }

            // Handling the positive scenario of getting the required trust data from notary server
            if (notaryCommandOutput.contains(imageTag) && notaryCommandOutput.contains("sha256")) {
                // Extracting the digest from Notary command output
                String repoDigestFromNotary = StringUtils.substringBetween(notaryCommandOutput,
                                                                           "sha256:", " ");
                log.info("Received trust data successfully for image {} with tag {}", imageName, imageTag);
                return repoDigestFromNotary;
            }
        }

        // Handling all negative cases to make agent exit gracefully
        if (notaryCommandOutput.contains("could not reach")) {
            log.error("Failed to reach notary server. Error:\n{}", notaryCommandOutput);
        } else {
            log.error("Failed to verify trust data for the image {} with tag {}. Error:\n{}", imageName, imageTag,
                      notaryCommandOutput);
        }
        throw new AgentException(ErrorCode.NOTARY_SIGNATURE_VERIFICATION_FAILED,
                                 "Error while verifying notary signature", new RuntimeException());
    }

    void createNetwork(String networkName) {
        Timer timer = this.metricsAgent.getTimer(MetricsConstants.MetricsNames.CREATE_NETWORK,
                List.of(classTag, Tag.of(MetricsConstants.MetricsTags.TAG_METHOD.metricsTagName, "createNetwork"),
                        Tag.of(MetricsConstants.MetricsTags.TAG_DOCKER_NETWORK.metricsTagName, networkName)));

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
        Timer timer = this.metricsAgent.getTimer(MetricsConstants.MetricsNames.CONTAINER_LAUNCH,
                List.of(classTag, Tag.of(MetricsConstants.MetricsTags.TAG_METHOD.metricsTagName, "startComponent"),
                        Tag.of(MetricsConstants.MetricsTags.TAG_CONTAINER_ID.metricsTagName, containerId),
                        Tag.of(MetricsConstants.MetricsTags.TAG_CONTAINER_NAME.metricsTagName,
                                containerParam.getContainerName())));
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
        Timer timer = this.metricsAgent.getTimer(MetricsConstants.MetricsNames.CONTAINER_STOP,
                List.of(classTag, Tag.of(MetricsConstants.MetricsTags.TAG_METHOD.metricsTagName, "stopComponent"),
                        Tag.of(MetricsConstants.MetricsTags.TAG_CONTAINER_ID.metricsTagName, containerId),
                        Tag.of(MetricsConstants.MetricsTags.TAG_CONTAINER_NAME.metricsTagName,
                                containerParam.getContainerName())));
        timer.record(() -> {
            log.info("Stopping {}: Id {} ", containerParam.getContainerName(), containerId);
            dockerClient.stopContainerCmd(containerId).exec();
            log.info("Stopped container {}: Id {} ", containerParam.getContainerName(), containerId);
        });
    }

    /**
     * Helper util to delete a container.
     */
    public void deleteComponent(DockerClient dockerClient, BaseContainerSpec containerParam,
                              String containerId) {
        log.info("Removing {}: Id {} ", containerParam.getContainerName(), containerId);
        dockerClient.removeContainerCmd(containerId).exec();
        log.info("Removed container {}: Id {} ", containerParam.getContainerName(), containerId);
    }
}
