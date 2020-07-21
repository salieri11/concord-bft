/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.server;

import java.security.Security;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;

import javax.validation.constraints.NotNull;

import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.lognet.springboot.grpc.GRpcService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import com.vmware.blockchain.configuration.eccerts.ConcordEcCertificatesGenerator;
import com.vmware.blockchain.configuration.generateconfig.BftClientConfigUtil;
import com.vmware.blockchain.configuration.generateconfig.ConcordConfigUtil;
import com.vmware.blockchain.deployment.v1.BlockchainType;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;
import com.vmware.blockchain.deployment.v1.ConfigurationComponent;
import com.vmware.blockchain.deployment.v1.ConfigurationServiceGrpc.ConfigurationServiceImplBase;
import com.vmware.blockchain.deployment.v1.ConfigurationServiceRequestV2;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;
import com.vmware.blockchain.deployment.v1.DeploymentAttributes;
import com.vmware.blockchain.deployment.v1.IdentityComponent;
import com.vmware.blockchain.deployment.v1.NodeConfigurationRequest;
import com.vmware.blockchain.deployment.v1.NodeConfigurationResponse;
import com.vmware.blockchain.deployment.v1.NodeType;

import io.grpc.Status;
import io.grpc.StatusException;
import io.grpc.stub.StreamObserver;
import lombok.extern.slf4j.Slf4j;

/**
 * Implementation of ConfigurationService server.
 */
@GRpcService
@Slf4j
public class ConfigurationService extends ConfigurationServiceImplBase {

    /**
     * Concord config Template path.
     **/
    private String concordConfigPath;

    /**
     * bftClient config Template path.
     **/
    private String bftClientConfigPath;

    /**
     * Executor to use for all async service operations.
     */
    private final ExecutorService executor;

    private final Cache<ConfigurationSessionIdentifier, Map<Integer, List<ConfigurationComponent>>> cache;

    private final Cache<String, Map<String, List<ConfigurationComponent>>> cacheByNodeId;

    private ConfigurationServiceHelper configurationServiceHelper;

    /**
     * Constructor.
     */
    @Autowired
    ConfigurationService(ExecutorService executor,
                         @Value("${config.template.path:ConcordConfigTemplate.yaml}")
                                 String concordConfigTemplatePath,
                         @Value("${config.template.path:BFTClientConfigTemplate.yaml}")
                                 String bftClientConfigTemplatePath,
                         ConfigurationServiceHelper configurationServiceHelper) {
        this.concordConfigPath = concordConfigTemplatePath;
        this.bftClientConfigPath = bftClientConfigTemplatePath;
        this.executor = executor;
        this.configurationServiceHelper = configurationServiceHelper;
        initialize();

        cache = CacheBuilder.newBuilder()
                .expireAfterAccess(10, TimeUnit.MINUTES)
                .expireAfterWrite(2, TimeUnit.HOURS)
                .build();

        cacheByNodeId = CacheBuilder.newBuilder()
                .expireAfterAccess(10, TimeUnit.MINUTES)
                .expireAfterWrite(2, TimeUnit.HOURS)
                .build();
    }

    /**
     * Initialize the service instance asynchronously.
     *
     * @return {@link CompletableFuture} that completes when initialization is done.
     */
    CompletableFuture<Void> initialize() {
        return CompletableFuture.runAsync(() -> {
            Security.addProvider(new BouncyCastleProvider());
            log.info("ConfigurationService instance initialized");
        }, executor);
    }

    @Override
    public void getNodeConfiguration(@NotNull NodeConfigurationRequest request,
                                     @NotNull StreamObserver<NodeConfigurationResponse> observer) {
        try {
            List<ConfigurationComponent> nodeComponents;

            log.info("Received request {}", request);
            if (!request.getNodeId().isEmpty()) {
                var components = cacheByNodeId.getIfPresent(request.getIdentifier().getId());
                log.info("Configurations found for session id {}", request.getIdentifier());
                log.info("List of node ids supported: " + (components != null ? components.keySet() : null));
                nodeComponents = components != null ? components.get(request.getNodeId()) : null;
            } else {
                var components = cache.getIfPresent(ConfigurationSessionIdentifier.newBuilder()
                                                            .setIdentifier(request.getIdentifier().getIdentifier())
                                                            .build());
                log.info("Configurations found for session id {}", request.getIdentifier());
                log.info("List of node ids supported: " + (components != null ? components.keySet() : null));
                nodeComponents = components != null ? components.get(request.getNode()) : null;
            }

            if (nodeComponents != null && nodeComponents.size() != 0) {
                NodeConfigurationResponse.Builder builder = NodeConfigurationResponse.newBuilder();
                builder.addAllConfigurationComponent(nodeComponents);

                observer.onNext(builder.build());
                observer.onCompleted();
            } else {
                observer.onError(new StatusException(Status.NOT_FOUND.withDescription(
                        "Node configuration is empty for node: " + request.getNode())));
            }
        } catch (Exception e) {
            var errorMsg = String.format("Retrieving configuration results failed for id: %s with error: %s",
                                         request.getIdentifier(), e.getLocalizedMessage());
            observer.onError(new StatusException(Status.INVALID_ARGUMENT.withDescription(errorMsg)));
        }
    }

    @Override
    public void createConfigurationV2(@NotNull ConfigurationServiceRequestV2 request,
                                      @NotNull StreamObserver<ConfigurationSessionIdentifier> observer) {
        log.info(request.toString());
        var committerIps = new ArrayList<String>();
        var participantIps = new ArrayList<String>();
        var committerIds = new ArrayList<String>();
        var participantNodeIds = new ArrayList<String>();

        // TODO : scope for refactoring
        request.getNodesMap().get(NodeType.REPLICA.name()).getEntriesList().forEach(
            each -> {
                committerIps.add(each.getNodeIp());
                committerIds.add(each.getId());
            });

        if (request.getNodesMap().containsKey(NodeType.CLIENT.name())) {
            request.getNodesMap().get(NodeType.CLIENT.name()).getEntriesList().forEach(each -> {
                participantIps.add(each.getNodeIp());
                participantNodeIds.add(each.getId());
            });
        }

        var nodeIdList = new ArrayList<>(committerIds);

        // Generate Configuration
        var certGen = new ConcordEcCertificatesGenerator();
        var configUtil = new ConcordConfigUtil(concordConfigPath);
        var bftClientConfigUtil = new BftClientConfigUtil(bftClientConfigPath);
        boolean isBftEnabled = false;
        Map<String, String> bftClientConfig = new HashMap<>();
        Map<String, String> concordConfig = configUtil.getConcordConfig(committerIds, committerIps,
                                                                    convertToLegacy(request.getBlockchainType()));

        if (request.getGenericProperties().getValuesMap()
                .getOrDefault(DeploymentAttributes.ENABLE_BFT_CLIENT.toString(), "False")
                .equalsIgnoreCase("True") && request.getBlockchainType().equals(BlockchainType.DAML)) {
            isBftEnabled = true;
            bftClientConfig.putAll(bftClientConfigUtil
                    .getbftClientConfig(participantNodeIds, committerIps, participantIps));
            nodeIdList.addAll(participantNodeIds);
        }

        Map<String, List<ConfigurationComponent>> configByNodeId = new HashMap<>();
        request.getNodesMap().values().forEach(nodesByType -> nodesByType
                .getEntriesList().forEach(eachNode ->
                        configByNodeId
                                .put(eachNode.getId(),
                                        configurationServiceHelper.nodeIndependentConfigs(
                                                request.getConsortiumId(),
                                                request.getBlockchainId(),
                                                eachNode.getServicesList(),
                                                eachNode))));

        Map<String, List<IdentityComponent>> concordIdentityComponents = configurationServiceHelper
                .getTlsNodeIdentities(configUtil, bftClientConfigUtil, certGen, nodeIdList, isBftEnabled);
        Map<String, List<IdentityComponent>> bftIdentityComponents = new HashMap<>();
        if (isBftEnabled) {
            bftIdentityComponents.putAll(configurationServiceHelper
                    .convertToBftTlsNodeIdentities(concordIdentityComponents));
        }

        var sessionId = ConfigurationServiceUtil.newSessionUId();
        log.info("Generated tls identity elements for session id: {}", sessionId);

        Map<String, List<ConfigurationComponent>> nodeComponent = new HashMap<>();

        configByNodeId.forEach((nodeId, componentList) -> {
            nodeComponent.put(nodeId, configurationServiceHelper.buildNodeConifigs(nodeId, componentList, certGen,
                    concordConfig, bftClientConfig, concordIdentityComponents, bftIdentityComponents));
        });
        log.info("Persisting configurations for session: {} in memory...", sessionId);
        cacheByNodeId.put(sessionId.getId(), nodeComponent);
        observer.onNext(sessionId);
        observer.onCompleted();
    }

    private ConcordModelSpecification.BlockchainType convertToLegacy(BlockchainType type) {
        return ConcordModelSpecification.BlockchainType.valueOf(type.name());
    }
}