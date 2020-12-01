/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.server;

import java.io.IOException;
import java.security.Security;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

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
import com.vmware.blockchain.configuration.generateconfig.ConcordOperatorConfigUtil;
import com.vmware.blockchain.configuration.generateconfig.ValidationUtil;
import com.vmware.blockchain.configuration.util.BlockchainFeatures;
import com.vmware.blockchain.configuration.util.BlockchainNodeList;
import com.vmware.blockchain.deployment.v1.BlockchainType;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;
import com.vmware.blockchain.deployment.v1.ConfigurationComponent;
import com.vmware.blockchain.deployment.v1.ConfigurationServiceGrpc.ConfigurationServiceImplBase;
import com.vmware.blockchain.deployment.v1.ConfigurationServiceRequestV2;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;
import com.vmware.blockchain.deployment.v1.DeploymentAttributes;
import com.vmware.blockchain.deployment.v1.NodeConfigurationRequest;
import com.vmware.blockchain.deployment.v1.NodeConfigurationResponse;
import com.vmware.blockchain.server.exceptions.ConfigServiceException;
import com.vmware.blockchain.server.exceptions.ErrorCode;
import com.vmware.blockchain.server.util.ConfigurationServiceUtil;
import com.vmware.blockchain.server.util.IdentityComponentsLists;
import com.vmware.blockchain.server.util.IdentityManagementUtil;

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
     * Operator config Template path.
     **/
    private String operatorConfigTemplatePath;

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
                         @Value("${config.template.path:ConcordOperatorConfigTemplate.yaml}")
                                 String operatorConfigTemplatePath,
                         ConfigurationServiceHelper configurationServiceHelper) {
        this.concordConfigPath = concordConfigTemplatePath;
        this.bftClientConfigPath = bftClientConfigTemplatePath;
        this.operatorConfigTemplatePath = operatorConfigTemplatePath;
        this.executor = executor;
        this.configurationServiceHelper = configurationServiceHelper;
        initialize();

        cacheByNodeId = CacheBuilder.newBuilder()
                .expireAfterAccess(45, TimeUnit.MINUTES)
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

        var sessionId = UUID.randomUUID();
        log.info("Request received. Session Id : {}\n Request : {}", sessionId, request.toString());
        BlockchainNodeList nodeList = ConfigurationServiceUtil.getNodeList(request.getNodesMap(), sessionId);

        // Validate the nodes based on the blockchain type.
        if (!ValidationUtil.isValidNodeCount(nodeList, request.getBlockchainType())) {
            throw new ConfigServiceException(ErrorCode.CLIENT_CONFIG_INVALID_INPUT_FAILURE,
                                             "Invalid number of nodes for Blockchain type " + request
                                                     .getBlockchainType());
        }

        // Generate Configuration
        var configUtil =
                new ConcordConfigUtil(concordConfigPath, sessionId, configurationServiceHelper.isKeepTempFiles());
        var bftClientConfigUtil =
                new BftClientConfigUtil(bftClientConfigPath, sessionId, configurationServiceHelper.isKeepTempFiles());

        Map<String, String> bftClientConfig = new HashMap<>();
        int numClients = 0;
        String clientProxyPerParticipantStr = request.getGenericProperties().getValuesMap()
                .getOrDefault(DeploymentAttributes.NUM_BFT_CLIENTS.name(), "15");
        int clientProxyPerParticipant = Pattern.matches("\\d+", clientProxyPerParticipantStr)
                                        ? Integer.parseInt(clientProxyPerParticipantStr) : 15;


        // TODO remove post 1.0
        var isSplitConfigString = request.getGenericProperties().getValuesMap()
                .getOrDefault(DeploymentAttributes.SPLIT_CONFIG.name(), "True");

        boolean isSplitConfig = !isSplitConfigString.equalsIgnoreCase("False");

        // So if the Blockchain type is DAML, then we populate bftClientConfig.
        // For all Blockchain types, bftClientConfig would be empty.
        if (request.getBlockchainType().equals(BlockchainType.DAML)) {
            try {
                bftClientConfig.putAll(bftClientConfigUtil.getBftClientConfig(nodeList, clientProxyPerParticipant));
            } catch (IOException e) {
                var msg = "Failed to generate BFT client configuration for session Id : " + sessionId;
                log.error(msg, e);
                throw new ConfigServiceException(ErrorCode.BFT_CONFIGURATION_FAILURE, msg, e);
            }
            log.info("Generated bft client configurations for session Id : {}", sessionId);

            numClients = clientProxyPerParticipant * nodeList.getClientSize();
        }

        boolean isPreexecutionDeployment = request.getGenericProperties().getValuesMap()
                .getOrDefault(DeploymentAttributes.PREEXECUTION_ENABLED.name(), "True")
                .equalsIgnoreCase("True");

        // Is Object store enabled?
        boolean isObjStoreEnabled = request.getGenericProperties().getValuesMap()
                .getOrDefault(DeploymentAttributes.OBJECT_STORE_ENABLED.name(), "False")
                .equalsIgnoreCase("True");

        boolean isTrsTrcTlsEnabled = request.getGenericProperties().getValuesMap()
                .getOrDefault(DeploymentAttributes.TRC_TRS_TLS_ENABLED.name(), "False")
                .equalsIgnoreCase("True");

        // Set pre-execution, split config and bft enabled states.
        // Capture features on this blockchain.
        BlockchainFeatures bcFeatures = BlockchainFeatures.builder()
                .isPreExecutionDeployment(isPreexecutionDeployment)
                .isSplitConfig(isSplitConfig)
                .isObjectStoreEnabled(isObjStoreEnabled)
                .isTrcTlsEnabled(isTrsTrcTlsEnabled)
                .operatorSigningKey(request.getGenericProperties().getValuesMap()
                                            .getOrDefault(DeploymentAttributes.DEPLOY_OPERATOR.name(), null))
                .build();

        Map<String, Map<String, String>> concordConfig = configUtil.getConcordConfig(nodeList,
                                                        convertToLegacy(request.getBlockchainType()), numClients,
                                                        bcFeatures);
        log.info("Generated concord configurations for session Id : {}", sessionId);

        var operatorConfigUtil =
                new ConcordOperatorConfigUtil(operatorConfigTemplatePath, sessionId);
        final String operatorConfig = operatorConfigUtil.getConcordOperatorConfig(nodeList, clientProxyPerParticipant,
                                                                                  bcFeatures);

        log.info("Generated operator configurations for session Id : {}", sessionId);

        Map<String, List<ConfigurationComponent>> configByNodeId = new HashMap<>();
        // TODO: There is inconsistency here wrt nodesInfo != deploymentAttributes.
        // Although, provisioning service takes care of it but CS is inconsistent in itself.
        // Eg: ConcordConfigUtil considers bcFeatures instead of nodesInfo, unlike in here.
        // Thus, a common feature may not be present in both these cases.
        try {
            request.getNodesMap().forEach((nodeType, nodesInfo) -> nodesInfo
                    .getEntriesList().forEach(eachNode -> configByNodeId.put(eachNode.getId(),
                                                                             configurationServiceHelper
                                                                                     .nodeIndependentConfigs(
                                                                                             request.getConsortiumId(),
                                                                                             request.getBlockchainId(),
                                                                                             eachNode,
                                                                                             nodeType,
                                                                                             operatorConfig))));
        } catch (ConfigServiceException e) {
            throw e;
        } catch (Exception e) {
            var msg = "Trouble gathering node independent configuration for session Id : " + sessionId;
            log.error(msg, e);
            throw new ConfigServiceException(ErrorCode.GENERATE_NODE_INDEPENDENT_CONFIG_FAILURE, msg, e);
        }
        log.info("Generated node independent configurations for session Id : {}", sessionId);

        var certGen = new ConcordEcCertificatesGenerator();
        IdentityManagementUtil identityManagementUtil = new IdentityManagementUtil(nodeList,
                bcFeatures, request.getBlockchainId());

        log.info("Creating secrets for session Id {}", sessionId);
        IdentityComponentsLists identityComponentsLists;
        try {
            identityComponentsLists = identityManagementUtil.getAllTlsNodeIdentities(configUtil.nodePrincipal,
                    bftClientConfigUtil.nodePrincipal, certGen, clientProxyPerParticipant);
        } catch (Exception e) {
            var msg = "Trouble generating concord bft TLS node identities for session Id : " + sessionId;
            log.error(msg, e);
            throw new ConfigServiceException(ErrorCode.GENERATE_TLS_NODE_IDENTITIES_FAILURE, msg, e);
        }

        log.info("Generated tls identity elements for session id: {}", sessionId);

        try {
            // Need these copy objects to satisfy lambda function.
            Map<String, Map<String, String>> finalConcordConfig = concordConfig;
            configByNodeId.forEach((nodeId, componentList) -> {
                String key = String.join(separator, sessionId.toString(), nodeId);
                log.info("Persisting configurations for session: {}, node: {} in memory...", sessionId, nodeId);
                cacheByNodeId.put(key, configurationServiceHelper.buildNodeConfigs(nodeId, componentList, certGen,
                                                                                    finalConcordConfig, bftClientConfig,
                                                                                    identityComponentsLists,
                                                                                   bcFeatures.getOperatorSigningKey()));
            });
        } catch (ConfigServiceException e) {
            throw e;
        } catch (Exception e) {
            log.error("Error organizing the configurations for sessions {}", sessionId, e);
            throw new ConfigServiceException(ErrorCode.CONFIGURATION_GENERATION_FAILURE,
                                             "Error organizing the configurations for sessions {}" + sessionId, e);
        }

        observer.onNext(ConfigurationSessionIdentifier.newBuilder()
                                .setId(sessionId.toString())
                                .build());
        observer.onCompleted();
    }

    // TODO: remove while cleanup
    private ConcordModelSpecification.BlockchainType convertToLegacy(BlockchainType type) {
        return ConcordModelSpecification.BlockchainType.valueOf(type.name());
    }
}