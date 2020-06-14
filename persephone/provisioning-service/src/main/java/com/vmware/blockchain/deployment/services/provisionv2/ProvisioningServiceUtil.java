/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.provisionv2;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import org.assertj.core.util.Strings;

import com.vmware.blockchain.deployment.services.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.v1.ConcordComponent;
import com.vmware.blockchain.deployment.v1.DeploymentExecutionEvent;
import com.vmware.blockchain.deployment.v1.DeploymentSessionEvent;
import com.vmware.blockchain.deployment.v1.MessageHeader;
import com.vmware.blockchain.deployment.v1.NodeAssignment;
import com.vmware.blockchain.deployment.v1.NodeType;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteIdentifier;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.Properties;
import com.vmware.blockchain.deployment.v1.Sites;

import lombok.extern.slf4j.Slf4j;

/**
 * Helper class for @ProvisioningServiceV2.
 */

@Slf4j
public class ProvisioningServiceUtil {

    /**
     * Generate a new {@link UUID} based on a given request {@link MessageHeader}, or generate a
     * random value as ID if request does not contain sufficient parametric data.
     *
     */
    static UUID extractOrGenerateId(String idString) {
        UUID uuid;
        if (idString.isEmpty()) {
            uuid = UUID.randomUUID();
        } else {
            // Hash the string value into an UUID.
            uuid = UUID.fromString(idString);
        }
        return uuid;
    }

    /**
     * Generate a new {@link UUID} based on a given request {@link MessageHeader}, or generate a
     * random value as ID if request does not contain sufficient parametric data.
     *
     */
    static NodeAssignment updateNodeAssignment(NodeAssignment nodeAssignment, Properties globalProperties,
                                               Map<String, Properties> nodeProperties) {

        return NodeAssignment.newBuilder()
                .addAllEntries(nodeAssignment.getEntriesList().stream()
                                       .map(entry -> {
                                           String nodeId = entry.getNodeId();
                                           if (Strings.isNullOrEmpty(nodeId)) {
                                               nodeId = UUID.randomUUID().toString();
                                           }
                                           Properties.Builder propertiesBuilder = Properties.newBuilder();
                                           propertiesBuilder.putAllValues(globalProperties.getValuesMap());
                                           if (nodeProperties.containsKey(entry.getType().name())) {
                                               propertiesBuilder.putAllValues(
                                                       nodeProperties.get(entry.getType().name()).getValuesMap());
                                           }
                                           propertiesBuilder.putAllValues(entry.getProperties().getValuesMap());
                                           return NodeAssignment.Entry.newBuilder(entry)
                                                   .setProperties(propertiesBuilder)
                                                   .setNodeId(nodeId).build();
                                       }).collect(Collectors.toUnmodifiableList())).build();
    }

    /**
     * Convert raw map to nodeType specific.
     */
    static Map<NodeType, Properties> convertToNodeTypeMap(Map<String, Properties> properties, Properties generic) {
        Map<NodeType, Properties> response = new HashMap<>();
        properties.forEach((key, value) -> {
            Map<String, String> props = new HashMap<>(generic.getValuesMap());

            // This is a really important flow, generic keys will get overriden per node.
            props.putAll(value.getValuesMap());
            response.put(NodeType.valueOf(key), Properties.newBuilder().putAllValues(props).build());
        });
        return response;
    }

    static Map<OrchestrationSiteIdentifier, OrchestrationSiteInfo> convertToSiteIdMap(Sites sites) {
        Map<OrchestrationSiteIdentifier, OrchestrationSiteInfo> response = new HashMap<>();
        sites.getInfoListList().forEach(k -> response.put(k.getId(), k.getInfo()));
        return response;
    }

    /**
     * Create an instance of {@link DeploymentExecutionEvent}.
     * @return a new instance of {@link DeploymentSessionEvent}.
     */
    static DeploymentExecutionEvent.Builder generateEvent(DeploymentExecutionContext deploymentSession,
                                                  DeploymentExecutionEvent.Type type,
                                                  DeploymentExecutionEvent.Status status) {
        return DeploymentExecutionEvent.newBuilder()
                .setType(type)
                .setSessionId(deploymentSession.id.toString())
                .setBlockchainId(deploymentSession.blockchainId.toString())
                .setConsortiumId(deploymentSession.consortiumId.toString())
                .setStatus(status);
    }


    static Map<UUID, List<ConcordComponent>> generateComponentsPerNode(NodeAssignment nodeAssignment,
                                                                       Map<OrchestrationSiteIdentifier, Orchestrator>
                                                                               orchestratorMap,
                                                                       Map<NodeType, List<ConcordComponent>>
                                                                               nodeTypeComponents) {
        Map<UUID, List<ConcordComponent>> componentsPerNode = new HashMap<>();
        nodeAssignment.getEntriesList().forEach(
            each -> {
                var baseList = nodeTypeComponents.get(each.getType());
                // Add filter for wavefront.
                // Add filter for logging.
                componentsPerNode.put(UUID.fromString(each.getNodeId()), baseList);
            }
        );
        return componentsPerNode;
    }

    static OrchestrationSiteInfo.Type deriveDeploymentType(Sites sites) {
        return sites.getInfoListList().size() > 0 ? sites.getInfoListList().get(0).getInfo().getType()
                                                  : OrchestrationSiteInfo.Type.VSPHERE;
    }
}