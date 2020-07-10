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
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import javax.validation.constraints.NotNull;

import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.lognet.springboot.grpc.GRpcService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import com.vmware.blockchain.configuration.eccerts.ConcordEcCertificatesGenerator;
import com.vmware.blockchain.configuration.generateconfig.ConcordConfigUtil;
import com.vmware.blockchain.deployment.v1.BlockchainType;
import com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;
import com.vmware.blockchain.deployment.v1.ConfigurationComponent;
import com.vmware.blockchain.deployment.v1.ConfigurationServiceGrpc.ConfigurationServiceImplBase;
import com.vmware.blockchain.deployment.v1.ConfigurationServiceRequestV2;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;
import com.vmware.blockchain.deployment.v1.Identity;
import com.vmware.blockchain.deployment.v1.IdentityComponent;
import com.vmware.blockchain.deployment.v1.IdentityFactors;
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
     * Telegraf config template path.
     **/
    private String telegrafConfigPath;

    /**
     * Wavefront config template path.
     **/
    private String wavefrontConfigPath;

    /** Logging config template path. **/
    private String loggingEnvTemplatePath;

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
                         @Value("${config.template.path:TelegrafConfigTemplate.conf}")
                                 String telegrafConfigTemplatePath,
                         @Value("${config.template.path:wavefrontConfigTemplate.conf}")
                                 String wavefrontConfigPath,
                         @Value("${config.template.path:LoggingTemplate.env}")
                                 String loggingEnvTemplatePath,
                         ConfigurationServiceHelper configurationServiceHelper) {
        this.concordConfigPath = concordConfigTemplatePath;
        this.telegrafConfigPath = telegrafConfigTemplatePath;
        this.wavefrontConfigPath = wavefrontConfigPath;
        this.loggingEnvTemplatePath = loggingEnvTemplatePath;
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

    private Map<String, List<IdentityComponent>> buildTlsIdentity(List<String> nodeIds,
            List<Identity> identities,
            Map<Integer, List<Integer>> principals,
            int numCerts, int numHosts) {

        Map<String, List<IdentityComponent>> result = new HashMap<>();

        // TODO: May remove logic once principals are available
        if (principals.size() == 0) {
            IntStream.range(0, numHosts).forEach(node -> {
                List<IdentityComponent> identityComponents = new ArrayList<>();
                identities.forEach(identity -> {
                    identityComponents.add(identity.getCertificate());
                    identityComponents.add(identity.getKey());
                });
                result.put(nodeIds.get(node), identityComponents);
            });
            return result;
        }

        for (int node : principals.keySet()) {
            List<IdentityComponent> nodeIdentities = new ArrayList<>();

            List<Integer> notPrincipal = IntStream.range(0, numCerts)
                    .boxed().collect(Collectors.toList());
            notPrincipal.removeAll(principals.get(node));

            List<Identity> serverList = new ArrayList<>(identities.subList(0, identities.size() / 2));
            List<Identity> clientList = new ArrayList<>(identities.subList(identities.size() / 2, identities.size()));

            notPrincipal.forEach(entry -> {
                nodeIdentities.add(serverList.get(entry).getCertificate());
                nodeIdentities.add(clientList.get(entry).getCertificate());
            });

            // add self keys
            nodeIdentities.add(serverList.get(node).getKey());
            nodeIdentities.add(clientList.get(node).getKey());

            principals.get(node).forEach(entry -> {
                nodeIdentities.add(serverList.get(entry).getCertificate());
                nodeIdentities.add(serverList.get(entry).getKey());
                nodeIdentities.add(clientList.get(entry).getCertificate());
                nodeIdentities.add(clientList.get(entry).getKey());
            });
            result.putIfAbsent(nodeIds.get(node), nodeIdentities);
        }

        log.info("Filtered tls identities based on nodes and principal ids.");
        return result;
    }

    @Override
    public void createConfigurationV2(@NotNull ConfigurationServiceRequestV2 request,
                                      @NotNull StreamObserver<ConfigurationSessionIdentifier> observer) {
        log.info(request.toString());
        var sessionId = ConfigurationServiceUtil.newSessionUId();

        // Add validation
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
        var hostList = new ArrayList<String>();
        var nodeIdList = new ArrayList<String>();
        request.getNodesMap().get(NodeType.REPLICA.name()).getEntriesList().stream().forEach(
            each -> {
                hostList.add(each.getNodeIp());
                nodeIdList.add(each.getId());
            });

        var certGen = new ConcordEcCertificatesGenerator();

        // Generate Configuration
        var configUtil = new ConcordConfigUtil(concordConfigPath);
        Map<String, String> tlsConfig = configUtil.getConcordConfig(nodeIdList, hostList,
                                                                    convertToLegacy(request.getBlockchainType()));

        List<Identity> tlsIdentityList =
                certGen.generateSelfSignedCertificates(configUtil.maxPrincipalId + 1,
                                                       ServiceType.CONCORD);
        log.info("Generated tls identity elements for session id: {}", sessionId);

        Map<String, List<IdentityComponent>> tlsNodeIdentities = buildTlsIdentity(nodeIdList,
                                                                                  tlsIdentityList,
                                                                                  configUtil.nodePrincipal,
                                                                                  configUtil.maxPrincipalId + 1,
                                                                                  hostList.size());

        Map<String, List<ConfigurationComponent>> nodeComponent = new HashMap<>();

        configByNodeId.forEach((k, v) -> {
            List<ConfigurationComponent> output = new ArrayList<>();
            output.addAll(v);

            if (tlsConfig.containsKey(k)) {
                output.add(ConfigurationComponent.newBuilder()
                                   .setType(ServiceType.CONCORD)
                                   .setComponentUrl(ConcordConfigUtil.configPath)
                                   .setComponent(tlsConfig.get(k))
                                   .setIdentityFactors(IdentityFactors.newBuilder().build())
                                   .build());
            }

            if (tlsNodeIdentities.containsKey(k)) {
                tlsNodeIdentities.get(k).forEach(entry ->
                                                         output.add(ConfigurationComponent.newBuilder()
                                                                            .setType(ServiceType.CONCORD)
                                                                            .setComponentUrl(entry.getUrl())
                                                                            .setComponent(entry.getBase64Value())
                                                                            .setIdentityFactors(
                                                                                    certGen.getIdentityFactor())
                                                                            .build()));
            }

            nodeComponent.put(k, output);
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