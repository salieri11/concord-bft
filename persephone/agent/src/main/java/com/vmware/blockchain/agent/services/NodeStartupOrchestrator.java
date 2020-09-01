/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.agent.services;

import java.io.FileOutputStream;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.github.dockerjava.api.DockerClient;
import com.github.dockerjava.api.command.CreateContainerResponse;
import com.github.dockerjava.api.exception.ConflictException;
import com.github.dockerjava.api.model.Bind;
import com.github.dockerjava.api.model.HostConfig;
import com.github.dockerjava.core.DockerClientBuilder;
import com.vmware.blockchain.agent.services.configservice.ConfigServiceInvoker;
import com.vmware.blockchain.agent.services.configuration.BaseContainerSpec;
import com.vmware.blockchain.agent.services.configuration.DamlCommitterConfig;
import com.vmware.blockchain.agent.services.configuration.DamlParticipantConfig;
import com.vmware.blockchain.agent.services.configuration.EthereumConfig;
import com.vmware.blockchain.agent.services.configuration.HlfConfig;
import com.vmware.blockchain.agent.services.configuration.MetricsAndTracingConfig;
import com.vmware.blockchain.agent.services.exceptions.AgentException;
import com.vmware.blockchain.agent.services.exceptions.ErrorCode;
import com.vmware.blockchain.agent.services.metrics.MetricsAgent;
import com.vmware.blockchain.agent.services.metrics.MetricsConstants;
import com.vmware.blockchain.agent.services.node.health.daml.DamlHealthServiceInvoker;
import com.vmware.blockchain.deployment.v1.ConcordAgentConfiguration;
import com.vmware.blockchain.deployment.v1.ConcordComponent;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;
import com.vmware.blockchain.deployment.v1.ConfigurationComponent;

import io.micrometer.core.instrument.Counter;
import io.micrometer.core.instrument.Tag;
import io.micrometer.core.instrument.Timer;

import lombok.Getter;

/**
 * Utility class for talking to Docker and creating the required volumes, starting
 * concord-node etc.
 */
@Component
public class NodeStartupOrchestrator {

    private static final Logger log = LoggerFactory.getLogger(NodeStartupOrchestrator.class);

    /** Container network name alias. */
    private static final String CONTAINER_NETWORK_NAME = "blockchain-fabric";

    private static final String configDownloadMarker = "/config/agent/configDownloadMarker";

    /** Configuration parameters for this agent instance. */
    private final ConcordAgentConfiguration configuration;

    private final ConfigServiceInvoker configServiceInvoker;

    private final AgentDockerClient agentDockerClient;

    private final DamlHealthServiceInvoker damlHealthServiceInvoker;

    @Getter
    private List<BaseContainerSpec> components;

    private final MetricsAgent metricsAgent;

    /**
     * Default constructor.
     */
    @Autowired
    public NodeStartupOrchestrator(ConcordAgentConfiguration configuration, AgentDockerClient agentDockerClient,
                                   ConfigServiceInvoker configServiceInvoker,
                                   DamlHealthServiceInvoker damlHealthServiceInvoker) {
        this.configuration = configuration;
        this.agentDockerClient = agentDockerClient;
        this.configServiceInvoker = configServiceInvoker;
        this.damlHealthServiceInvoker = damlHealthServiceInvoker;

        List<Tag> tags = Arrays.asList(Tag.of(MetricsConstants.MetricsTags.TAG_SERVICE.name(),
                NodeStartupOrchestrator.class.getName()));
        this.metricsAgent = new MetricsAgent(tags);
    }

    /**
     * Start the local setup as a Concord node.
     */
    public void bootstrapConcord() {
        Counter counter = this.metricsAgent.getCounter("Number of containers launched",
                MetricsConstants.MetricsNames.CONTAINERS_LAUNCH_COUNT,
                Collections.singletonList(Tag.of(MetricsConstants.MetricsTags.TAG_METHOD.name(), "bootstrapConcord")));
        Timer timer = this.metricsAgent.getTimer("Bootstrap blockchain",
                MetricsConstants.MetricsNames.CONTAINERS_LAUNCH,
                Collections.singletonList(Tag.of(MetricsConstants.MetricsTags.TAG_METHOD.name(), "bootstrapConcord")));
        timer.record(() -> {
            try {
                // Download configuration and certs.
                setupConfig();

                // Pull and order images
                List<BaseContainerSpec> containerConfigList = pullImages();
                containerConfigList.sort(Comparator.comparingInt(BaseContainerSpec::ordinal));
                components = containerConfigList;

                agentDockerClient.createNetwork(CONTAINER_NETWORK_NAME);

                // Start all containers
                // Get Docker client instance
                var dockerClient = DockerClientBuilder.getInstance().build();
                try {
                    containerConfigList.forEach(container ->  {
                        var containerResponse = createContainer(dockerClient, container);
                        if (configuration.getNoLaunch()) {
                            log.info("Not Launching {}: Id {} ",
                                    container.getContainerName(), containerResponse.getId());
                        } else {
                            agentDockerClient.startComponent(dockerClient, container, containerResponse.getId());
                            counter.increment();

                            // Since "_" is not a valid literal in hostname, we can not use container name
                            if (container.getContainerName()
                                    .equalsIgnoreCase(damlHealthServiceInvoker.getService().getHost())) {
                                var inspectContainerResponse = agentDockerClient.inspectContainer(dockerClient,
                                        container.getContainerName());
                                String host = inspectContainerResponse.getNetworkSettings().getNetworks()
                                        .get(CONTAINER_NETWORK_NAME).getIpAddress();
                                damlHealthServiceInvoker.start(host);
                            }
                        }
                    });
                } catch (ConflictException e) {
                    log.warn("Did not launch the container again. Container already present", e);
                }
            } catch (Exception | InternalError e) {
                log.error("Unexpected exception encountered during launch sequence", e);
                log.warn("******Node not Functional********");
                throw new AgentException(ErrorCode.NODE_START_FAILED,
                                         "Failure during launch sequence " + e.getMessage(), e);
            }
        });
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
    private void writeConfiguration(List<ConfigurationComponent> artifacts) throws Exception {
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
                throw new AgentException(ErrorCode.ERROR_POPULATING_NODE_CONFIG,
                                         "Error populating configuration for  " + destination, error);
            }
        }

        // Inserting marker file
        var filepath = Path.of(configDownloadMarker);
        Files.createFile(filepath);
    }

    private List<BaseContainerSpec> pullImages() {

        var startMillis = ZonedDateTime.now().toInstant().toEpochMilli();
        final var registryUsername = configuration.getContainerRegistry()
                .getCredential().getPasswordCredential().getUsername();
        final var registryPassword = configuration.getContainerRegistry()
                .getCredential().getPasswordCredential().getPassword();

        List<CompletableFuture<BaseContainerSpec>> futures = new ArrayList<>();

        for (var component : configuration.getModel().getComponentsList()) {
            // Bypass non service type image pull...
            if (component.getServiceType() != ConcordComponent.ServiceType.GENERIC) {
                BaseContainerSpec containerSpec;

                if (getEnumNames(MetricsAndTracingConfig.class).contains(component.getServiceType().toString())) {
                    containerSpec = getMetricsAndTracingContainerSpec(component);
                } else {
                    containerSpec = getCoreContainerSpec(
                            configuration.getModel().getBlockchainType(), component);
                }
                futures.add(CompletableFuture.supplyAsync(() -> agentDockerClient.getImageIdAfterDl(containerSpec,
                        registryUsername, registryPassword, component.getName())));
            }
        }

        var result =  CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]))
                .thenApply((res) -> futures.stream()
                        .map(CompletableFuture::join)
                        .collect(Collectors.toList())).join();

        var stopMillis = ZonedDateTime.now().toInstant().toEpochMilli();
        Timer timer = this.metricsAgent.getTimer("Pull component docker images",
                MetricsConstants.MetricsNames.CONTAINERS_PULL_IMAGES,
                Collections.singletonList(Tag.of(MetricsConstants.MetricsTags.TAG_METHOD.name(), "pullImages")));
        timer.record(stopMillis - startMillis, TimeUnit.MILLISECONDS);

        return result;
    }

    private BaseContainerSpec getCoreContainerSpec(ConcordModelSpecification.BlockchainType blockchainType,
                                                   ConcordComponent component) {
        BaseContainerSpec containerSpec;

        switch (blockchainType) {
            case ETHEREUM:
                containerSpec = EthereumConfig.valueOf(component.getServiceType().name());
                break;
            case DAML:
                switch (configuration.getModel().getNodeType()) {
                    case DAML_COMMITTER:
                        containerSpec = DamlCommitterConfig.valueOf(component.getServiceType().name());
                        break;
                    case DAML_PARTICIPANT:
                        containerSpec = DamlParticipantConfig.valueOf(component.getServiceType().name());
                        break;
                    default:
                        throw new AgentException(ErrorCode.DAML_NODE_MISSING, "DAML Node type not provided. ",
                                                 new RuntimeException());
                }
                break;
            case HLF:
                containerSpec = HlfConfig.valueOf(component.getServiceType().name());
                break;
            default:
                var message = String.format("Invalid blockchain network type(%s)", blockchainType);
                throw new AgentException(ErrorCode.INVALID_BLOCKCHAIN_NETWORK_TYPE, message,
                                         new RuntimeException(message));
        }

        return containerSpec;
    }

    private BaseContainerSpec getMetricsAndTracingContainerSpec(ConcordComponent component) {

        BaseContainerSpec containerSpec;
        containerSpec = MetricsAndTracingConfig.valueOf(component.getServiceType().name());
        return containerSpec;
    }

    public static List<String> getEnumNames(Class<? extends Enum<?>> e) {
        return Arrays.stream(e.getEnumConstants()).map(Enum::name).collect(Collectors.toList());
    }

    private void setupConfig() throws Exception {
        log.info("Reading from Config Service");

        if (Files.notExists(Path.of(configDownloadMarker))) {
            var configList = configServiceInvoker.retrieveConfiguration(configuration.getConfigurationSession(),
                                                                        configuration.getNodeId());
            writeConfiguration(configList);
            log.info("Populated the configurations");
        } else {
            log.info("Configurations already downloaded.");
        }
    }

    private CreateContainerResponse createContainer(DockerClient dockerClient, BaseContainerSpec containerParam) {

        // Mount Default volume per container.
        String containerName = containerParam.getContainerName().replace("_", "-");

        // FIXME There is a standard change in naming containers.
        String defaultVolumeString = "/config/" + containerName + ":/config/" + containerName;
        Bind defaultVolume = Bind.parse(defaultVolumeString);
        List<Bind> volumes = new ArrayList<>();
        volumes.add(defaultVolume);

        // Mount "generic" to all containers
        String genericMountString = "/config/generic:/config/generic";
        Bind genericVolume = Bind.parse(genericMountString);
        volumes.add(genericVolume);

        if (containerParam.getVolumeBindings() != null) {
            volumes.addAll(containerParam.getVolumeBindings());
        }

        HostConfig hostConfig = HostConfig.newHostConfig()
                .withNetworkMode(CONTAINER_NETWORK_NAME)
                .withRestartPolicy(containerParam.getRestartPolicy())
                .withBinds(volumes);

        if (containerParam.getPortBindings() != null) {
            hostConfig.withPortBindings(containerParam.getPortBindings());
        }

        if (containerParam.getLinks() != null) {
            hostConfig.withLinks(containerParam.getLinks());
        }

        if (containerParam == DamlCommitterConfig.DAML_CONCORD
                || containerParam == EthereumConfig.CONCORD) {
            // TODO Evaluate this with security.
            log.warn("Setting privilege mode");
            hostConfig.withPrivileged(true);
        }

        var createContainerCmd = dockerClient.createContainerCmd(containerParam.getContainerName())
                .withName(containerParam.getContainerName())
                .withImage(containerParam.getImageId())
                .withHostConfig(hostConfig);

        if (containerParam.getEnvironment() != null) {
            createContainerCmd.withEnv(containerParam.getEnvironment());
        }

        log.info("Create container: {}", createContainerCmd.toString());
        var container = createContainerCmd.exec();
        if (container == null) {
            log.error("Couldn't create {} container...!", containerParam.getContainerName());
        }

        return container;
    }
}