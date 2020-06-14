/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.configuration;

import static com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType.CONCORD;
import static com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType.DAML_CONCORD;
import static com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType.DAML_EXECUTION_ENGINE;
import static com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType.DAML_INDEX_DB;
import static com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType.DAML_LEDGER_API;
import static com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType.ETHEREUM_API;
import static com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType.GENERIC;
import static com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType.JAEGER_AGENT;
import static com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType.LOGGING;
import static com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType.TELEGRAF;
import static com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType.WAVEFRONT_PROXY;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.google.common.collect.ImmutableMap;
import com.vmware.blockchain.deployment.services.exception.BadRequestPersephoneException;
import com.vmware.blockchain.deployment.v1.BlockchainType;
import com.vmware.blockchain.deployment.v1.ConcordComponent;
import com.vmware.blockchain.deployment.v1.DeploymentAttributes;
import com.vmware.blockchain.deployment.v1.NodeAssignment;
import com.vmware.blockchain.deployment.v1.NodeType;
import com.vmware.blockchain.deployment.v1.Properties;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

/**
 * This class provides the component configuration required to deploy.
 */
@Component
@Slf4j
public class NodeConfiguration {

    @Getter
    private String dockerImageBaseVersion;

    @Getter
    private String damlSdkVersion;

    @Autowired
    public NodeConfiguration(@Value("${docker.image.base.version:latest}") String dockerImageBuild,
                             @Value("${daml.sdk.version:NA}") String dockerSdkVersion) {
        this.dockerImageBaseVersion = dockerImageBuild;
        this.damlSdkVersion = dockerSdkVersion;

        log.info("Default Node Configuration loaded: dockerImageBuild: {}", dockerImageBuild);
    }

    //TODO make this configurable
    static final Map<ConcordComponent.ServiceType, String> STATIC_TAG_LIST = ImmutableMap.of(
            WAVEFRONT_PROXY, "6.1", TELEGRAF, "1.14.0", JAEGER_AGENT, "1.17"
    );

    private static final List<ConcordComponent.ServiceType> GENERIC_COMPONENTS = List.of(GENERIC, LOGGING, JAEGER_AGENT,
            WAVEFRONT_PROXY, TELEGRAF);

    private static final List<ConcordComponent.ServiceType> ETHEREUM_COMPONENTS = List.of(CONCORD, ETHEREUM_API);

    private static final List<ConcordComponent.ServiceType> DAML_CLIENT_COMPONENTS = List.of(DAML_INDEX_DB,
            DAML_LEDGER_API);
    private static final List<ConcordComponent.ServiceType> DAML_COMMITTER_COMPONENTS = List.of(DAML_CONCORD,
            DAML_EXECUTION_ENGINE);

    private static final Map<BlockchainType, Map<NodeType, List<ConcordComponent.ServiceType>>>
            componentListForBlockchainNodeType =
            ImmutableMap.of(BlockchainType.ETHEREUM, ImmutableMap.of(
                    NodeType.REPLICA,
                    Stream.concat(GENERIC_COMPONENTS.stream(), ETHEREUM_COMPONENTS.stream())
                            .collect(Collectors.toList())),
                            BlockchainType.DAML, ImmutableMap.of(
                            NodeType.REPLICA,
                            Stream.concat(GENERIC_COMPONENTS.stream(), DAML_COMMITTER_COMPONENTS.stream())
                                    .collect(Collectors.toList()),
                            NodeType.CLIENT,
                            Stream.concat(GENERIC_COMPONENTS.stream(), DAML_CLIENT_COMPONENTS.stream())
                                    .collect(Collectors.toList())));

    private static final Map<ConcordComponent.ServiceType, String> componentToImageName =
            ImmutableMap.<ConcordComponent.ServiceType, String>builder()
                    .put(GENERIC, "vmwblockchain/agent")
                    .put(LOGGING, "vmwblockchain/fluentd")
                    .put(WAVEFRONT_PROXY, "vmwblockchain/wavefront-proxy")
                    .put(JAEGER_AGENT, "vmwblockchain/jaeger-agent")
                    .put(DAML_EXECUTION_ENGINE, "vmwblockchain/daml-execution-engine")
                    .put(DAML_INDEX_DB, "vmwblockchain/daml-index-db")
                    .put(DAML_LEDGER_API, "vmwblockchain/daml-ledger-api")
                    .put(CONCORD, "vmwblockchain/concord-core")
                    .put(DAML_CONCORD, "vmwblockchain/concord-core")
                    .put(ETHEREUM_API, "vmwblockchain/ethrpc")
                    .put(TELEGRAF, "vmwblockchain/telegraf").build();

    private String getImageTag(final ConcordComponent.ServiceType componentName, final String imageTag) {
        log.debug("componentName: {}", componentName);

        StringBuilder sb = new StringBuilder(componentToImageName.get(componentName));
        sb.append(":");
        sb.append(STATIC_TAG_LIST.getOrDefault(componentName, imageTag));
        return sb.toString();
    }

    /**
     * Get component list.
     * @param nodeType node
     * @return components.
     */
    List<ConcordComponent> getComponentsByNodeType(BlockchainType blockchainType,
                                                          NodeType nodeType, Properties properties) {

        var componentsByNode = componentListForBlockchainNodeType.get(blockchainType);
        if (componentsByNode == null || componentsByNode.get(nodeType) == null) {
            throw new BadRequestPersephoneException(
                    "Invalid node type: " + nodeType + " for blockchain: " + blockchainType);
        }
        String dockerVersionToUse = properties.getValuesOrDefault(DeploymentAttributes.IMAGE_TAG.name(),
                                                                    dockerImageBaseVersion);
        return componentsByNode.get(nodeType).stream()
                .map(k ->
                             ConcordComponent.newBuilder()
                                     .setType(ConcordComponent.Type.CONTAINER_IMAGE)
                                     .setServiceType(k)
                                     .setName(getImageTag(k, properties.getValuesOrDefault(k.name(),
                                                                                           dockerVersionToUse)))
                                     .build()
                ).collect(Collectors.toList());
    }

    /**
     * Generate Model Specification.
     * @param blockchainType type
     * @param nodeAssignment node
     * @return map
     */
    public Map<UUID, List<ConcordComponent>> generateModelSpec(BlockchainType blockchainType,
                                                               NodeAssignment nodeAssignment) {

        Map<UUID, List<ConcordComponent>> output = new HashMap<>();

        nodeAssignment.getEntriesList().stream()
                .forEach(k -> {
                    output.put(UUID.fromString(k.getNodeId()),
                               getComponentsByNodeType(blockchainType, k.getType(), k.getProperties()));
                });
        return output;
    }
}