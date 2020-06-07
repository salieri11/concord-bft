/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.provisionv2;

import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

import com.vmware.blockchain.deployment.services.exception.PersephoneException;
import com.vmware.blockchain.deployment.services.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorData;
import com.vmware.blockchain.deployment.v1.DeployedResource;
import com.vmware.blockchain.deployment.v1.NodeAssignment;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteIdentifier;
import com.vmware.blockchain.deployment.v1.Properties;

import lombok.extern.slf4j.Slf4j;

/**
 * Helper class for Networking utilities.
 */

@Slf4j
public class NetworkHelper {

    /**
     * Create private IP address for nodes.
     *
     * @param nodes                nodeAssignment
     * @param sessionOrchestrators orchestrators
     * @param results              eventList
     * @return map of node-id to private ip
     */
    Map<UUID, DeploymentExecutionContext.LocalNodeDetails> createPrivateIpMap(
            NodeAssignment nodes,
            Map<OrchestrationSiteIdentifier, Orchestrator> sessionOrchestrators,
            ConcurrentHashMap.KeySetView<DeployedResource, Boolean> results) {
        var privateNetworkAddressMap =
                new ConcurrentHashMap<UUID, DeploymentExecutionContext.LocalNodeDetails>();
        var networkAddressPromises = nodes.getEntriesList().stream()
                .map(entry -> {
                    var orchestrator = sessionOrchestrators.get(entry.getSite());

                    var addressRequest = new OrchestratorData.CreateNetworkResourceRequest(entry.getNodeId(), false);
                    return CompletableFuture
                            .runAsync(() -> {
                                var eventInfo = orchestrator.createPrivateNetworkAddress(addressRequest);
                                var createdEvent = (OrchestratorData.NetworkResourceEventCreated) eventInfo;
                                DeployedResource deployedResource = DeployedResource.newBuilder()
                                        .setNodeId(entry.getNodeId())
                                        .setSiteId(entry.getSite().getId())
                                        .setType(DeployedResource.Type.NETWORK_RESOURCE)
                                        .setName(eventInfo.getResource().toString())
                                        .setAdditionalInfo(Properties.newBuilder()
                                                                   .putValues(
                                                                           DeployedResource
                                                                                   .DeployedResourcePropertyKey
                                                                                   .PRIVATE_IP
                                                                                   .name(),
                                                                           createdEvent.getAddress())
                                                                   .build())
                                        .build();
                                results.add(deployedResource);
                                DeploymentExecutionContext.LocalNodeDetails localNode
                                        = DeploymentExecutionContext.LocalNodeDetails.builder().build();
                                localNode.privateIp = createdEvent.getAddress();
                                privateNetworkAddressMap.put(UUID.fromString(entry.getNodeId()), localNode);
                            });
                })
                .toArray(CompletableFuture[]::new);

        try {
            CompletableFuture.allOf(networkAddressPromises).get(30L, TimeUnit.SECONDS);
        } catch (Exception e) {
            log.error("Error waiting for private ip creation", e);
            throw new PersephoneException(e, "Error allocating private ip.");
        }
        return privateNetworkAddressMap;
    }

    /**
     * Create private IP address for nodes.
     *
     * @param nodes                    nodeAssignment
     * @param sessionOrchestrators     orchestrators
     * @param privateNetworkAddressMap privateIps
     * @param results                  eventList
     */
    void applyPublicNetworkAddress(NodeAssignment nodes,
                                   Map<OrchestrationSiteIdentifier, Orchestrator> sessionOrchestrators,
                                   Map<UUID, DeploymentExecutionContext.LocalNodeDetails>
                                           privateNetworkAddressMap,
                                   ConcurrentHashMap.KeySetView<DeployedResource, Boolean> results) {
        var publicNetworkAddressMap =
                new ConcurrentHashMap<UUID, OrchestratorData.NetworkResourceEventCreated>();

        var publicAllocation = nodes.getEntriesList().stream()
                .map(entry -> {
                    var orchestrator = sessionOrchestrators.get(entry.getSite());
                    final UUID nodeId = UUID.fromString(entry.getNodeId());
                    var addressRequest = new OrchestratorData.CreateNetworkResourceRequest(nodeId.toString(),
                                                                                           true);

                    return CompletableFuture
                            .supplyAsync(() -> orchestrator.createPublicNetworkAddress(addressRequest))
                            .thenAcceptAsync(networkResourceEvent -> {
                                var createdEvent =
                                        (OrchestratorData.NetworkResourceEventCreated) networkResourceEvent;
                                DeployedResource deployedResource = DeployedResource.newBuilder()
                                        .setNodeId(entry.getNodeId())
                                        .setSiteId(entry.getSite().getId())
                                        .setType(DeployedResource.Type.NETWORK_RESOURCE)
                                        .setName(networkResourceEvent.getResource().toString())
                                        .setAdditionalInfo(Properties.newBuilder()
                                                                   .putValues(
                                                                           DeployedResource
                                                                                   .DeployedResourcePropertyKey
                                                                                   .PUBLIC_IP
                                                                                   .name(),
                                                                           createdEvent.getAddress())
                                                                   .build())
                                        .build();
                                results.add(deployedResource);
                                publicNetworkAddressMap.put(nodeId, createdEvent);

                                var allocationRequest =
                                        new OrchestratorData.CreateNetworkAllocationRequestV2(
                                                nodeId.toString(),
                                                publicNetworkAddressMap.get(nodeId).getAddress(),
                                                privateNetworkAddressMap.get(nodeId).privateIp);
                                DeployedResource deployedResourceAllocation = DeployedResource.newBuilder()
                                        .setNodeId(entry.getNodeId())
                                        .setSiteId(entry.getSite().getId())
                                        .setType(DeployedResource.Type.NETWORK_ALLOCATION)
                                        .setName(orchestrator.createVmcNetworkAllocation(allocationRequest)
                                                         .getResource().toString())
                                        .build();
                                results.add(deployedResourceAllocation);
                            });

                })
                .toArray(CompletableFuture[]::new);

        try {
            CompletableFuture.allOf(publicAllocation).get(30L, TimeUnit.SECONDS);
        } catch (Exception e) {
            log.error("Error allocating NAT rule", e);
        }
    }
}