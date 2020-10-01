/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.server;

import java.io.IOException;
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

    private final Cache<String, List<ConfigurationComponent>> cacheByNodeId;

    private ConfigurationServiceHelper configurationServiceHelper;

    private final String separator = "-";

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

        cacheByNodeId = CacheBuilder.newBuilder()
                .expireAfterAccess(20, TimeUnit.MINUTES)
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
            log.info("Received request {}", request);

            String key = String.join(separator, request.getIdentifier().getId(), request.getNodeId());
            List<ConfigurationComponent> nodeComponents = cacheByNodeId.getIfPresent(key);

            if (nodeComponents != null && nodeComponents.size() != 0) {
                NodeConfigurationResponse.Builder builder = NodeConfigurationResponse.newBuilder();
                builder.addAllConfigurationComponent(nodeComponents);

                observer.onNext(builder.build());
                observer.onCompleted();
            } else {
                observer.onError(new StatusException(Status.NOT_FOUND.withDescription(
                        "Node configuration is empty for session: "
                                + request.getIdentifier().getId() + ", node: " + request.getNodeId())));
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

        var sessionId = ConfigurationServiceUtil.newSessionUId();
        log.info("Request receieved. Session Id : {}\n Request : {}", sessionId, request.toString());
        var committerIps = new ArrayList<String>();
        var participantIps = new ArrayList<String>();
        var committerIds = new ArrayList<String>();
        var participantNodeIds = new ArrayList<String>();

        // TODO : scope for refactoring
        request.getNodesMap().get(NodeType.REPLICA.name()).getEntriesList().forEach(each -> {
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
        var configUtil = new ConcordConfigUtil(concordConfigPath, sessionId);
        var bftClientConfigUtil = new BftClientConfigUtil(bftClientConfigPath, sessionId);

        boolean isBftEnabled = false;
        Map<String, String> bftClientConfig = new HashMap<>();
        int numClients = 0;
        int clientProxyPerParticipant = Integer.parseInt(request.getGenericProperties().getValuesMap()
                .getOrDefault(DeploymentAttributes.NUM_BFT_CLIENTS.name(), "15"));

        // TODO remove post 1.0
        var isSplitConfigString = request.getGenericProperties().getValuesMap()
                .getOrDefault(DeploymentAttributes.SPLIT_CONFIG.name(), "True");

        boolean isSplitConfig = !isSplitConfigString.equalsIgnoreCase("False");

        if (request.getBlockchainType().equals(BlockchainType.DAML)) {
            isBftEnabled = true;
            try {
                bftClientConfig.putAll(bftClientConfigUtil
                        .getbftClientConfig(participantNodeIds, committerIps, participantIps,
                                clientProxyPerParticipant));
            } catch (IOException e) {
                var msg = "bftclient configurations not generated for session Id : " + sessionId;
                log.error(msg);
                observer.onError(e);
            }
            nodeIdList.addAll(participantNodeIds);

            log.info("Generated bft client configurations for session Id : {}", sessionId);

            numClients = clientProxyPerParticipant * participantIps.size();
        }

        boolean isPreexecutionDeployment = request.getGenericProperties().getValuesMap()
                .getOrDefault(DeploymentAttributes.PREEXECUTION_ENABLED.name(), "False")
                .equalsIgnoreCase("True");

        Map<String, Map<String, String>> concordConfig = null;
        try {
            concordConfig = configUtil.getConcordConfig(committerIds, committerIps,
                    convertToLegacy(request.getBlockchainType()), numClients, isSplitConfig,
                    isPreexecutionDeployment);
        } catch (IOException e) {
            var msg = "concord configurations not generated for session Id : " + sessionId;
            log.error(msg);
            observer.onError(e);
        }

        log.info("Generated concord configurations for session Id : {}", sessionId);

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

        var certGen = new ConcordEcCertificatesGenerator();
        Map<String, List<IdentityComponent>> concordIdentityComponents = new HashMap<>();

        log.info("Creating secrets for session Id {}", sessionId);
        concordIdentityComponents.putAll(ConfigurationServiceUtil
                .getTlsNodeIdentities(configUtil.maxPrincipalId,
                        configUtil.nodePrincipal,
                        bftClientConfigUtil.maxPrincipalId,
                        bftClientConfigUtil.nodePrincipal,
                        certGen, nodeIdList, isBftEnabled));

        Map<String, List<IdentityComponent>> bftIdentityComponents = new HashMap<>();
        if (isBftEnabled) {
            bftIdentityComponents.putAll(ConfigurationServiceUtil
                    .convertToBftTlsNodeIdentities(concordIdentityComponents));
        }

        log.info("Generated tls identity elements for session id: {}", sessionId);

        try {
            Map<String, Map<String, String>> finalConcordConfig = concordConfig;
            configByNodeId.forEach((nodeId, componentList) -> {

                String key = String.join(separator, sessionId.getId(), nodeId);
                log.info("Persisting configurations for session: {}, node: {} in memory...", sessionId, nodeId);
                cacheByNodeId.put(key, configurationServiceHelper.buildNodeConifigs(nodeId, componentList, certGen,
                        finalConcordConfig, bftClientConfig,
                        concordIdentityComponents,
                        bftIdentityComponents));
            });
        } catch (Exception e) {
            log.error("Error organizing the configurations for sessions {}", sessionId);
            observer.onError(e);
        }

        observer.onNext(sessionId);
        observer.onCompleted();
    }

    // TODO: remove while cleanup
    private ConcordModelSpecification.BlockchainType convertToLegacy(BlockchainType type) {
        return ConcordModelSpecification.BlockchainType.valueOf(type.name());
    }
}