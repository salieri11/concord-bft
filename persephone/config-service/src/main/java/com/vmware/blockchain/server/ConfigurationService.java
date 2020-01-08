/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.server;

import java.security.SecureRandom;
import java.security.Security;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.jetbrains.annotations.NotNull;
import org.lognet.springboot.grpc.GRpcService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

import com.vmware.blockchain.configuration.eccerts.ConcordEcCertificatesGenerator;
import com.vmware.blockchain.configuration.generateconfig.ConcordConfigUtil;
import com.vmware.blockchain.configuration.generateconfig.DamlIndexDbUtil;
import com.vmware.blockchain.configuration.generateconfig.DamlLedgerApiUtil;
import com.vmware.blockchain.configuration.generateconfig.GenesisUtil;
import com.vmware.blockchain.configuration.generateconfig.LoggingUtil;
import com.vmware.blockchain.deployment.v1.ConcordComponent.ServiceType;
import com.vmware.blockchain.deployment.v1.ConfigurationComponent;
import com.vmware.blockchain.deployment.v1.ConfigurationServiceImplBase;
import com.vmware.blockchain.deployment.v1.ConfigurationServiceRequest;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;
import com.vmware.blockchain.deployment.v1.DeleteConfigurationRequest;
import com.vmware.blockchain.deployment.v1.DeleteConfigurationResponse;
import com.vmware.blockchain.deployment.v1.Identity;
import com.vmware.blockchain.deployment.v1.IdentityComponent;
import com.vmware.blockchain.deployment.v1.IdentityFactors;
import com.vmware.blockchain.deployment.v1.NodeConfigurationRequest;
import com.vmware.blockchain.deployment.v1.NodeConfigurationResponse;
import com.vmware.blockchain.ethereum.type.Genesis;

import io.grpc.Status;
import io.grpc.StatusException;
import io.grpc.stub.StreamObserver;

/**
 * Implementation of ConfigurationService server.
 */
@GRpcService
public class ConfigurationService extends ConfigurationServiceImplBase {

    private String configPath;

    /**
     * Enumeration of possible service instance state.
     */
    private enum State {
        STOPPED,
        INITIALIZING,
        ACTIVE,
        STOPPING
    }

    /** Logger instance. */
    private static Logger log = LoggerFactory.getLogger(ConfigurationService.class);

    /** Atomic update for service instance state. */
    private static final AtomicReferenceFieldUpdater<ConfigurationService, State> STATE =
            AtomicReferenceFieldUpdater.newUpdater(ConfigurationService.class, State.class, "state");

    /** Executor to use for all async service operations. */
    private final ExecutorService executor;

    /** Service state. */
    private volatile State state = State.STOPPED;

    /** per session all node tls configuration. */
    private final Map<ConfigurationSessionIdentifier,
            Map<Integer, List<ConfigurationComponent>>> sessionConfig = new ConcurrentHashMap<>();

    /**
     * Constructor.
     */
    @Autowired
    ConfigurationService(ExecutorService executor, @Value("${config.template.path:ConcordConfigTemplate.yaml}")
            String configTemplatePath)  {
        this.configPath = configTemplatePath;
        this.executor = executor;
        initialize();
    }

    /**
     * Initialize the service instance asynchronously.
     *
     * @return
     *   {@link CompletableFuture} that completes when initialization is done.
     */
    CompletableFuture<Void> initialize() {
        if (STATE.compareAndSet(this, State.STOPPED, State.INITIALIZING)) {
            return CompletableFuture.runAsync(() -> {
                Security.addProvider(new BouncyCastleProvider());

                // Set instance to ACTIVE state.
                STATE.set(this, State.ACTIVE);

                log.info("ConfigurationService instance initialized");
            }, executor);
        } else {
            return CompletableFuture.failedFuture(
                    new IllegalStateException("ConfigurationService instance is not in stopped state")
            );
        }
    }

    /**
     * Shutdown the service instance asynchronously.
     *
     * @return
     *   {@link CompletableFuture} that completes when shutdown is done.
     */
    CompletableFuture<Void> shutdown() {
        if (STATE.compareAndSet(this, State.ACTIVE, State.STOPPING)
                || STATE.compareAndSet(this, State.INITIALIZING, State.STOPPING)) {
            return CompletableFuture.runAsync(() -> {
                log.info("ConfigurationService instance shutting down");
                Security.removeProvider("BC");

                // Set instance to STOPPED state.
                STATE.set(this, State.STOPPED);
            }, executor);
        } else {
            return CompletableFuture.failedFuture(
                    new IllegalStateException("ConfigurationService instance is not in active state")
            );
        }
    }

    @Override
    public void createConfiguration(@NotNull ConfigurationServiceRequest request,
                                    @NotNull StreamObserver<ConfigurationSessionIdentifier> observer) {

        var sessionId = newSessionId();
        List<ConfigurationComponent> staticComponentList = new ArrayList<>();

        log.info(request.toString());
        // Static settings for each service Type.
        for (ServiceType serviceType : request.getServices()) {

            switch (serviceType) {
                case DAML_LEDGER_API:
                    DamlLedgerApiUtil ledgerApiUtil = new DamlLedgerApiUtil(
                            request.getProperties().getValues().get(DamlLedgerApiUtil.REPLICAS_KEY));
                    staticComponentList.add(new ConfigurationComponent(
                            serviceType,
                            DamlLedgerApiUtil.envVarPath,
                            ledgerApiUtil.generateConfig(),
                            new IdentityFactors())
                    );
                    break;
                case DAML_INDEX_DB:
                    DamlIndexDbUtil damlIndexDbUtil = new DamlIndexDbUtil();
                    staticComponentList.add(new ConfigurationComponent(
                            ServiceType.DAML_INDEX_DB,
                            DamlIndexDbUtil.envVarPath,
                            damlIndexDbUtil.generateConfig(),
                            new IdentityFactors())
                    );
                    break;
                case LOGGING:
                    LoggingUtil loggingUtil =
                            new LoggingUtil(request.getProperties().getValues().get(LoggingUtil.LOGGING_CONFIG));
                    staticComponentList.add(new ConfigurationComponent(
                            ServiceType.LOGGING,
                            LoggingUtil.envVarPath,
                            loggingUtil.generateConfig(),
                            new IdentityFactors())
                    );
                    break;
                case CONCORD:
                    log.info("Generated new session id {}", sessionId);
                    ConfigurationComponent genesisJson = createGenesisComponent(request.getGenesis(), sessionId);
                    staticComponentList.add(genesisJson);
                    break;
                case ETHEREUM_API:
                    staticComponentList.addAll(getEthereumComponent());
                    break;
                default:
                    log.info("No config required for serviceType {}", serviceType);
            }
        }

        Map<Integer, List<ConfigurationComponent>> nodeComponent = new HashMap<>();

        if (request.getServices().contains(ServiceType.CONCORD)
            || request.getServices().contains(ServiceType.DAML_CONCORD)
            || request.getServices().contains(ServiceType.HLF_CONCORD)) {

            var certGen = new ConcordEcCertificatesGenerator();

            // Generate Configuration
            var configUtil = new ConcordConfigUtil(configPath);
            var tlsConfig = configUtil.getConcordConfig(request.getHosts(), request.getBlockchainType());

            List<Identity> tlsIdentityList =
                    generateEtheriumConfig(certGen, configUtil.maxPrincipalId + 1, ServiceType.CONCORD);
            log.info("Generated tls identity elements for session id: {}", sessionId);

            Map<Integer, List<IdentityComponent>> tlsNodeIdentities = buildTlsIdentity(tlsIdentityList,
                                                                                       configUtil.nodePrincipal,
                                                                                       configUtil.maxPrincipalId + 1,
                                                                                       request.getHosts().size());

            for (int node = 0; node < request.getHosts().size(); node++) {
                List<ConfigurationComponent> componentList = new ArrayList<>();
                componentList.addAll(staticComponentList);

                // TLS list
                componentList.add(new ConfigurationComponent(
                        ServiceType.CONCORD,
                        configUtil.configPath,
                        tlsConfig.get(node),
                        new IdentityFactors())
                );

                tlsNodeIdentities.get(node)
                        .forEach(entry -> componentList.add(
                                new ConfigurationComponent(
                                        ServiceType.CONCORD,
                                        entry.getUrl(),
                                        entry.getBase64Value(),
                                        certGen.getIdentityFactor()
                                )
                        ));

                // put per node configs
                nodeComponent.putIfAbsent(node, componentList);
                log.info("Created configurations for session: {}", sessionId);

            }

        } else {
            for (int node = 0; node < request.getHosts().size(); node++) {
                List<ConfigurationComponent> componentList = new ArrayList<>();
                componentList.addAll(staticComponentList);
                nodeComponent.putIfAbsent(node, componentList);
                log.info("Created configurations for session: {}", sessionId);
            }
        }

        // Error out if no configurations are generated.
        if (staticComponentList.isEmpty()) {
            String msg = "No configurations were generated for servive type(s)" + request.getServices();
            log.error(msg);
            observer.onError(new StatusException(Status.INVALID_ARGUMENT.withDescription(msg)));
        } else {
            log.info("Persisting configurations for session: {} in memory...", sessionId);
            var persist = sessionConfig.putIfAbsent(sessionId, nodeComponent);

            if (persist == null) {
                log.info("Persisted configurations for session: {} in memory.", sessionId);
                observer.onNext(sessionId);
                observer.onCompleted();
            } else {
                observer.onError(new StatusException(
                        Status.INTERNAL.withDescription("Could not persist configuration results")));
            }
        }
    }

    private List<ConfigurationComponent> getEthereumComponent() {
        List<ConfigurationComponent> output = new ArrayList<>();

        var certGen = new ConcordEcCertificatesGenerator();
        List<Identity> ethrpcIdentityList =
                generateEtheriumConfig(certGen, 1, ServiceType.ETHEREUM_API);

        output.add(new ConfigurationComponent(
                ServiceType.ETHEREUM_API,
                ethrpcIdentityList.get(0).getCertificate().getUrl(),
                ethrpcIdentityList.get(0).getCertificate().getBase64Value(),
                certGen.getIdentityFactor()
        ));

        output.add(new ConfigurationComponent(
                ServiceType.ETHEREUM_API,
                ethrpcIdentityList.get(0).getKey().getUrl(),
                ethrpcIdentityList.get(0).getKey().getBase64Value(),
                certGen.getIdentityFactor()
        ));
        return output;
    }

    private List<Identity> generateEtheriumConfig(ConcordEcCertificatesGenerator certGen, int size,
                                                  ServiceType ethereumApi) {
        //Generate EthRPC Configuration
        return certGen.generateSelfSignedCertificates(size,
                                                      ethereumApi);
    }

    private ConfigurationComponent createGenesisComponent(@NotNull Genesis genesis,
                                          ConfigurationSessionIdentifier sessionId) {
        var genesisUtil = new GenesisUtil();
        log.info("Generated genesis for session id: {}", sessionId);
        return new ConfigurationComponent(
                ServiceType.CONCORD,
                GenesisUtil.genesisPath,
                genesisUtil.getGenesis(genesis),
                new IdentityFactors());
    }

    @Override
    public void getNodeConfiguration(@NotNull NodeConfigurationRequest request,
                                     @NotNull StreamObserver<NodeConfigurationResponse> observer) {

        try {
            var components = sessionConfig.get(request.getIdentifier());
            log.info("Configurations found for session id {}", request.getIdentifier());

            var nodeComponents = components.get(request.getNode());

            if (nodeComponents.size() != 0) {
                observer.onNext(new NodeConfigurationResponse(nodeComponents));
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
    public void deleteConfiguration(
            @NotNull DeleteConfigurationRequest request,
            @NotNull StreamObserver<DeleteConfigurationResponse> observer) {

        try {
            sessionConfig.remove(request.getId());
            log.info("Deleted configurations for session id: {}", request.getId());
            observer.onNext(new DeleteConfigurationResponse());
            observer.onCompleted();
        } catch (Exception e) {
            observer.onError(new StatusException(
                    Status.NOT_FOUND.withDescription("No configuration available for session id: " + request.getId())));
        }
    }

    /**
    * Generate a new {@link ConfigurationSessionIdentifier}.
    *
    * @return
    *   a new {@link ConfigurationSessionIdentifier} instance.
    */
    private static ConfigurationSessionIdentifier newSessionId() {
        return new ConfigurationSessionIdentifier(new SecureRandom().nextLong());
    }

    /**
     * Get the identity component list for all nodes.
     *
     * @param identities : identity list for all identities
     * @param principals : the principal map by configUtil
     * @param  numCerts : number of total certificate/keys
     * @return : map of node vs identity component list
     */
    private Map<Integer, List<IdentityComponent>>  buildTlsIdentity(
            List<Identity> identities,
            Map<Integer, List<Integer>> principals,
            int numCerts, int numHosts) {

        Map<Integer, List<IdentityComponent>> result = new HashMap<>();

        // TODO: May remove logic once principals are available
        if (principals.size() == 0) {
            IntStream.range(0, numHosts).forEach(node -> {
                List<IdentityComponent> identityComponents = new ArrayList<>();
                identities.forEach(identity -> {
                    identityComponents.add(identity.getCertificate());
                    identityComponents.add(identity.getKey());
                });
                result.put(node, identityComponents);
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
            result.putIfAbsent(node, nodeIdentities);
        }

        log.info("Filtered tls identities based on nodes and principal ids.");
        return result;
    }
}