/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.provisionv2;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import com.google.common.collect.ImmutableMap;
import com.vmware.blockchain.deployment.server.BootstrapComponent;
import com.vmware.blockchain.deployment.services.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorData;
import com.vmware.blockchain.deployment.v1.BlockchainType;
import com.vmware.blockchain.deployment.v1.ConcordComponent;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;
import com.vmware.blockchain.deployment.v1.DeployedResource;
import com.vmware.blockchain.deployment.v1.DeploymentAttributes;
import com.vmware.blockchain.deployment.v1.Endpoint;
import com.vmware.blockchain.deployment.v1.NodeAssignment;
import com.vmware.blockchain.deployment.v1.NodeType;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.Properties;

import lombok.extern.slf4j.Slf4j;

/**
 * Helper class for Compute utilities.
 */

@Slf4j
public class ComputeHelper {

    static Map<BlockchainType, String> endpoint = ImmutableMap.of(BlockchainType.DAML, "https://{{ip}}:6865",
                                                                  BlockchainType.ETHEREUM, "https://{{ip}}:8545");

    private final BootstrapComponent bootstrapComponent;

    /**
     * Constructor for compute helper.
     * @param bootstrapComponent properties
     */
    public ComputeHelper(BootstrapComponent bootstrapComponent) {
        this.bootstrapComponent = bootstrapComponent;
    }

    OrchestratorData.ComputeResourceEventCreatedV2 deployNode(
            UUID nodeId,
            UUID blockchainId,
            BlockchainType blockchainType,
            NodeAssignment.Entry node,
            DeploymentExecutionContext.LocalNodeDetails nodeDetails,
            Orchestrator orchestrator,
            List<ConcordComponent> model,
            ConfigurationSessionIdentifier configGenId,
            Endpoint registry,
            Endpoint notaryServer) {

        var modelSpecBuilder = ConcordModelSpecification.newBuilder()
                .addAllComponents(model)
                .setTemplate(node.getProperties().getValuesMap().getOrDefault(DeploymentAttributes.TEMPLATE_ID.name(),
                        bootstrapComponent.template))
                .setBlockchainType(ConcordModelSpecification.BlockchainType.valueOf(blockchainType.name()));

        // Read replica is not tied to a blockchain type.
        if (node.getType() == NodeType.READ_REPLICA) {
            modelSpecBuilder.setNodeType(ConcordModelSpecification.NodeType.READ_REPLICA);
        }

        if (blockchainType == BlockchainType.DAML) {
            if (node.getType() == NodeType.REPLICA) {
                modelSpecBuilder.setNodeType(ConcordModelSpecification.NodeType.DAML_COMMITTER);
            } else if (node.getType() == NodeType.CLIENT) {
                modelSpecBuilder.setNodeType(ConcordModelSpecification.NodeType.DAML_PARTICIPANT);
            }
        } else {
            modelSpecBuilder.setNodeType(ConcordModelSpecification.NodeType.CONCORD);
        }
        var computeRequest = new OrchestratorData.CreateComputeResourceRequestV2(blockchainId,
                                                                                 nodeId,
                                                                                 new OrchestratorData
                                                                                         .CreateComputeResourceRequestV2
                                                                                         .CloudInitData(
                                                                                         registry,
                                                                                         notaryServer,
                                                                                         modelSpecBuilder.build(),
                                                                                         nodeDetails
                                                                                                 .privateIp,
                                                                                         configGenId,
                                                                                         bootstrapComponent
                                                                                                 .configService,
                                                                                         bootstrapComponent
                                                                                                 .configServiceRest),
                                                                                 node.getProperties().getValuesMap());

        return orchestrator.createDeploymentV2(computeRequest);
    }

    List<Map.Entry<String, CompletableFuture<DeployedResource>>> getComputeNodes(
            DeploymentExecutionContext session, ConfigurationSessionIdentifier configGenerated, NodeType nodeType) {
        return session.nodeAssignment.getEntriesList().stream()
                .filter(k -> k.getType() == nodeType)
                .map(placement -> Map.entry(placement.getNodeId(), CompletableFuture.supplyAsync(() -> {
                    var nodeId = UUID.fromString(placement.getNodeId());
                    var siteInfo = session.getSitesById().get(placement.getSite());

                    var registry = siteInfo.getType() == OrchestrationSiteInfo.Type.VSPHERE
                                   ? siteInfo.getVsphere().getContainerRegistry()
                                   : siteInfo.getVmc().getContainerRegistry();
                    var notaryServer = siteInfo.getType() == OrchestrationSiteInfo.Type.VSPHERE
                                       ? siteInfo.getVsphere().getNotaryServer()
                                       : siteInfo.getVmc().getNotaryServer();
                    var computeEvent = deployNode(nodeId,
                                                    session.blockchainId,
                                                    session.blockchainType,
                                                    placement,
                                                    session.localNodeDetailsMap.get(nodeId),
                                                    session.orchestrators.get(placement.getSite()),
                                                    session.componentsByNode.get(nodeId),
                                                    configGenerated,
                                                    registry,
                                                    notaryServer);
                    var resource = DeployedResource.newBuilder()
                            .setNodeId(placement.getNodeId())
                            .setSiteId(placement.getSite().getId())
                            .setType(DeployedResource.Type.COMPUTE_RESOURCE)
                            .setAdditionalInfo(Properties.newBuilder(placement.getProperties())
                                                       .putValues(DeployedResource.DeployedResourcePropertyKey
                                                                          .NODE_LOGIN.name(),
                                                                  computeEvent.getNodePassword()))
                            .setName(computeEvent.getResource().toString());
                    applyEndpointDetails(session.blockchainType, resource, nodeType);

                    return resource.build();
                })))
                .collect(Collectors.toUnmodifiableList());
    }

    private void applyEndpointDetails(BlockchainType blockchainType, DeployedResource.Builder resource,
                                      NodeType nodeType) {
        // Remove this when client for ETH is separate
        if (BlockchainType.ETHEREUM == blockchainType) {
            resource.getAdditionalInfoBuilder()
                    .putValues(DeployedResource.DeployedResourcePropertyKey.CLIENT_ENDPOINT.name(),
                               endpoint.get(BlockchainType.ETHEREUM));
        }

        if (BlockchainType.DAML == blockchainType && NodeType.CLIENT == nodeType) {
            resource.getAdditionalInfoBuilder()
                    .putValues(DeployedResource.DeployedResourcePropertyKey.CLIENT_ENDPOINT.name(),
                               endpoint.get(BlockchainType.DAML));
        }
    }
}