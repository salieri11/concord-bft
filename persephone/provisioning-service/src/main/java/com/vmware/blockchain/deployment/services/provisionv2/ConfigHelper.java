/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.provisionv2;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.stream.Collectors;

import org.springframework.http.HttpStatus;

import com.google.common.util.concurrent.ListenableFuture;
import com.vmware.blockchain.deployment.server.BootstrapComponent;
import com.vmware.blockchain.deployment.services.configservice.ConfigServiceInvoker;
import com.vmware.blockchain.deployment.services.configuration.EthereumConfiguration;
import com.vmware.blockchain.deployment.services.exception.PersephoneException;
import com.vmware.blockchain.deployment.services.orchestrationsite.OrchestrationSites;
import com.vmware.blockchain.deployment.v1.BlockchainType;
import com.vmware.blockchain.deployment.v1.ConfigurationServiceGrpc;
import com.vmware.blockchain.deployment.v1.ConfigurationServiceRequestV2;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;
import com.vmware.blockchain.deployment.v1.MessageHeader;
import com.vmware.blockchain.deployment.v1.NodeAssignment;
import com.vmware.blockchain.deployment.v1.NodeProperty;
import com.vmware.blockchain.deployment.v1.NodeType;
import com.vmware.blockchain.deployment.v1.NodesInfo;
import com.vmware.blockchain.deployment.v1.Properties;

import lombok.extern.slf4j.Slf4j;

/**
 * Helper class for Compute utilities.
 */

@Slf4j
public class ConfigHelper {

    public final ConfigurationServiceGrpc.ConfigurationServiceFutureStub configurationServiceClient;

    /**
     * Constructor for compute helper.
     * @param bootstrapComponent properties
     */
    public ConfigHelper(BootstrapComponent bootstrapComponent) {
        this.configurationServiceClient = (new ConfigServiceInvoker(bootstrapComponent.configService))
                .generateConfigServiceFutureStub();
    }


    ConfigurationSessionIdentifier generateConfigurationId(DeploymentExecutionContext context,
                                                           Properties genericProperties) {

        var nodesByType = context.nodeAssignment.getEntriesList().stream()
                .collect(Collectors.groupingBy(NodeAssignment.Entry::getType));

        Map<NodeType, NodesInfo.Builder> nodeInfo = new HashMap<>();
        nodesByType.forEach((key, value) -> {
            NodesInfo.Builder infosBuilder = NodesInfo.newBuilder();
            value.forEach(
                eachNode -> {
                    var components = context.getComponentsByNode().get(UUID.fromString(eachNode.getNodeId()))
                            .stream().map(each -> each.getServiceType()).collect(Collectors.toList());
                    var nodeInfoBuilder = NodesInfo.Entry.newBuilder().setId(eachNode.getNodeId())
                            .setType(eachNode.getType())
                            .setId(eachNode.getNodeId())
                            .setNodeIp(context.localNodeDetailsMap.get(UUID.fromString(eachNode.getNodeId())).privateIp)
                            .addAllServices(components)
                            .setProperties(Properties.newBuilder()
                                                   .putAllValues(eachNode.getProperties().getValuesMap()));
                    addLogAndMetricProperties(context, eachNode, nodeInfoBuilder.getPropertiesBuilder());
                    infosBuilder.addEntries(nodeInfoBuilder);
                });
            nodeInfo.put(key, infosBuilder);
        });

        if (context.blockchainType == BlockchainType.ETHEREUM) {
            nodeInfo.get(NodeType.REPLICA).getEntriesBuilderList().forEach(
                eachNode -> eachNode.getPropertiesBuilder().putValues(NodeProperty.Name.GENESIS.name(),
                                                                      EthereumConfiguration.getGenesisObject()));
        } else if (context.blockchainType == BlockchainType.DAML) {
            if (nodeInfo.get(NodeType.CLIENT) != null) {
                var replicaIp = nodeInfo.get(NodeType.REPLICA)
                        .getEntriesBuilderList().stream()
                        .map(NodesInfo.Entry.Builder::getNodeIp).collect(Collectors.joining(":50051,", "",
                                                                                            ":50051"));
                nodeInfo.get(NodeType.CLIENT).getEntriesBuilderList().forEach(
                    eachNode -> eachNode.getPropertiesBuilder().putValues(NodeProperty.Name.COMMITTERS.name(),
                                                                          replicaIp));
            }
        }

        var requestBuilder = ConfigurationServiceRequestV2.newBuilder()
                .setHeader(MessageHeader.newBuilder().setId(context.id.toString()).build())
                .setConsortiumId(context.consortiumId.toString())
                .setBlockchainId(context.blockchainId.toString())
                .putAllNodes(nodeInfo.entrySet().stream().collect(Collectors.toMap((e) -> e.getKey().name(),
                    (e) -> e.getValue().build())))
                .setBlockchainType(context.blockchainType)
                .setGenericProperties(genericProperties);

        ListenableFuture<ConfigurationSessionIdentifier> completable
                = configurationServiceClient.withWaitForReady().createConfigurationV2(requestBuilder.build());
        try {
            return completable.get();
        } catch (InterruptedException | ExecutionException e) {
            throw new PersephoneException(HttpStatus.INTERNAL_SERVER_ERROR, e, "Error generating configuration");
        }
    }

    private void addLogAndMetricProperties(DeploymentExecutionContext context, NodeAssignment.Entry entry,
                                           Properties.Builder propertiesBuilder) {
        var logProperty = OrchestrationSites.getLogManagementJson(context.sitesById.get(entry.getSite()));
        propertiesBuilder.putValues(NodeProperty.Name.LOGGING_CONFIG.name(),
                                    logProperty);

        var wavefront = OrchestrationSites.getWavefront(context.sitesById.get(entry.getSite()));
        if (!wavefront.getUrl().isEmpty()) {
            propertiesBuilder.putValues(NodeProperty.Name.WAVEFRONT_URL.name(),
                                        wavefront.getUrl());
            propertiesBuilder.putValues(NodeProperty.Name.WAVEFRONT_TOKEN.name(),
                                        wavefront.getToken());

            var outboundProxy = OrchestrationSites.getOutboundProxy(context.sitesById.get(entry.getSite()));
            if (!outboundProxy.getHttpsHost().isEmpty()) {
                propertiesBuilder
                        .putValues(NodeProperty.Name.WAVEFRONT_PROXY_HOST.name(),
                                   outboundProxy.getHttpsHost());
                propertiesBuilder
                        .putValues(NodeProperty.Name.WAVEFRONT_PROXY_PORT.name(),
                                   String.valueOf(outboundProxy.getHttpsPort()));
            }
        }

        var elasticSearch = OrchestrationSites.getElasticsearch(context.sitesById.get(entry.getSite()));

        if (!elasticSearch.getUrl().isEmpty()) {
            propertiesBuilder
                    .putValues(NodeProperty.Name.ELASTICSEARCH_URL.name(),
                               elasticSearch.getUrl());
            propertiesBuilder
                    .putValues(NodeProperty.Name.ELASTICSEARCH_USER.name(),
                               elasticSearch.getUsername());
            propertiesBuilder
                    .putValues(NodeProperty.Name.ELASTICSEARCH_PWD.name(),
                               elasticSearch.getPassword());
        }
    }
}