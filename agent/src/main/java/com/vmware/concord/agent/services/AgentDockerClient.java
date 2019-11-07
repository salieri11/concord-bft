/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.concord.agent.services;

import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.function.Predicate;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.github.dockerjava.api.DockerClient;
import com.github.dockerjava.api.model.AuthConfig;
import com.github.dockerjava.api.model.HostConfig;
import com.github.dockerjava.api.model.LogConfig;
import com.github.dockerjava.core.DefaultDockerClientConfig;
import com.github.dockerjava.core.DockerClientBuilder;
import com.github.dockerjava.core.command.PullImageResultCallback;
import com.vmware.blockchain.deployment.v1.ConcordAgentConfiguration;
import com.vmware.blockchain.deployment.v1.ConcordComponent;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;
import com.vmware.blockchain.deployment.v1.ConfigurationComponent;
import com.vmware.concord.agent.services.configservice.ConfigServiceInvoker;
import com.vmware.concord.agent.services.configuration.BaseContainerSpec;
import com.vmware.concord.agent.services.configuration.DamlCommitterConfig;
import com.vmware.concord.agent.services.configuration.DamlConfig;
import com.vmware.concord.agent.services.configuration.DamlParticipantConfig;
import com.vmware.concord.agent.services.configuration.EthereumConfig;
import com.vmware.concord.agent.services.configuration.HlfConfig;


/**
 * Utility class for talking to Docker and creating the required volumes, starting
 * concord-node etc.
 */
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

    /** Configuration parameters for this agent instance. */
    private final ConcordAgentConfiguration configuration;

    private final ConfigServiceInvoker configServiceInvoker;

    /**
     * Default constructor.
     */
    public AgentDockerClient(ConcordAgentConfiguration configuration) {
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
    public void bootstrapConcord() {
        // Download configuration and certs.
        setupConfig();

        // Pull and order images
        List<BaseContainerSpec> containerConfigList = pullImages();

        containerConfigList.sort(Comparator.comparingInt(BaseContainerSpec::ordinal));

        // Get Docker client instance
        var dockerClient = DockerClientBuilder.getInstance().build();

        //setup special hlf networking
        if (configuration.getModel().getBlockchainType() == ConcordModelSpecification.BlockchainType.HLF) {
            createNetwork(dockerClient, HlfConfig.HLF_DOCKER_NETWORK);
        }

        // Setup logging container's environment variables
        Predicate<BaseContainerSpec> predicate =
            containerConfig -> containerConfig.getContainerName().equals(LogConfig.LoggingType.FLUENTD.toString());
        Optional<BaseContainerSpec> loggingContainerConfigOptional = containerConfigList.stream()
                .filter(predicate)
                .findFirst();
        if (loggingContainerConfigOptional.isPresent() && !configuration.getLoggingEnvVariables().isEmpty()) {
            var loggingContainerConfig = loggingContainerConfigOptional.get();
            loggingContainerConfig.setEnvironment(configuration.getLoggingEnvVariables());
        }

        // Start all containers
        containerConfigList.forEach(container -> launchContainer(dockerClient, container,
                                    configuration.getModel().getBlockchainType()));
    }

    private List<BaseContainerSpec> pullImages() {
        final var registryUsername = configuration.getContainerRegistry()
                .getCredential().getPasswordCredential().getUsername();
        final var registryPassword = configuration.getContainerRegistry()
                .getCredential().getPasswordCredential().getPassword();

        List<CompletableFuture<BaseContainerSpec>> futures = new ArrayList<>();
        for (var component : configuration.getModel().getComponents()) {
            // Bypass non service type image pull...
            if (component.getServiceType() != ConcordComponent.ServiceType.GENERIC) {
                futures.add(CompletableFuture
                                    .supplyAsync(() -> getImageIdAfterDl(configuration.getModel().getBlockchainType(),
                                                                         registryUsername,
                                                                         registryPassword, component)));
            }
        }

        return CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]))
                .thenApply((res) -> futures.stream()
                .map(CompletableFuture::join)
                .collect(Collectors.toList())).join();
    }

    private BaseContainerSpec getImageIdAfterDl(ConcordModelSpecification.BlockchainType blockchainType,
                                                String registryUsername, String registryPassword,
                                                ConcordComponent component) {

        log.info("Pulling image {}", component.getName());
        BaseContainerSpec containerConfig;
        switch (blockchainType) {
            case ETHEREUM:
                containerConfig = EthereumConfig.valueOf(component.getServiceType().name());
                break;
            case DAML:
                switch (configuration.getModel().getNodeType()) {
                    case DAML_COMMITTER:
                        containerConfig = DamlCommitterConfig.valueOf(component.getServiceType().name());
                        break;
                    case DAML_PARTICIPANT:
                        containerConfig = DamlParticipantConfig.valueOf(component.getServiceType().name());
                        break;
                    default:
                        containerConfig = DamlConfig.valueOf(component.getServiceType().name());
                        break;
                }
                break;
            case HLF:
                containerConfig = HlfConfig.valueOf(component.getServiceType().name());
                break;
            default:
                throw new RuntimeException("Invalid blockchain node type");
        }
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
            log.info("Component image name {} tag {}", componentImageName, componentImage.getTag());
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
            containerConfig.setImageId(inspectResponse.getId());
            log.info("Container image ID: {}", containerConfig.getImageId());
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

        log.info("Populated the config file");
    }

    private void launchContainer(DockerClient dockerClient, BaseContainerSpec containerParam,
                                 ConcordModelSpecification.BlockchainType blockchainType) {

        var createContainerCmd = dockerClient.createContainerCmd(containerParam.getContainerName())
                .withName(containerParam.getContainerName())
                .withImage(containerParam.getImageId());

        if (containerParam.getCmds() != null) {
            createContainerCmd.withCmd(containerParam.getCmds());
        }

        if (containerParam.getVolumeBindings() != null || containerParam.getPortBindings() != null
            || blockchainType == ConcordModelSpecification.BlockchainType.HLF) {
            HostConfig hostConfig = HostConfig.newHostConfig();

            if (containerParam.getVolumeBindings() != null) {
                hostConfig.withBinds(containerParam.getVolumeBindings());
            }

            if (containerParam.getPortBindings() != null) {
                hostConfig.withPortBindings(containerParam.getPortBindings());
            }

            if (containerParam.getLinks() != null) {
                hostConfig.withLinks(containerParam.getLinks());
            }

            //special network for hlf
            if (blockchainType == ConcordModelSpecification.BlockchainType.HLF) {
                hostConfig.withNetworkMode(HlfConfig.HLF_DOCKER_NETWORK);
            }

            createContainerCmd.withHostConfig(hostConfig);
        }

        if (containerParam.getEnvironment() != null) {
            createContainerCmd.withEnv(containerParam.getEnvironment());
        }

        log.info("Create container: {}", createContainerCmd.toString());
        var container = createContainerCmd.exec();
        if (container == null) {
            log.error("Couldn't start {} container...!", containerParam.getContainerName());
        } else {
            log.info("Starting {}: Id {} ", containerParam.getContainerName(), container.getId());
            dockerClient.startContainerCmd(container.getId()).exec();
            log.info("Started container {}: Id {} ", containerParam.getContainerName(), container.getId());
        }
    }

    private void createNetwork(DockerClient dockerClient, String networkName) {
        var createNetworkCmd = dockerClient.createNetworkCmd();

        createNetworkCmd.withName(networkName);
        createNetworkCmd.withCheckDuplicate(true);

        var id = createNetworkCmd.exec().getId();
        log.info("Created Network: {} Id: {}", networkName, id);

    }

}
