/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.provisionv2;

import static com.vmware.blockchain.deployment.services.provisionv2.ProvisioningServiceUtil.generateEvent;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import org.lognet.springboot.grpc.GRpcService;
import org.springframework.beans.factory.annotation.Autowired;

import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import com.vmware.blockchain.deployment.server.BootstrapComponent;
import com.vmware.blockchain.deployment.services.configuration.NodeConfiguration;
import com.vmware.blockchain.deployment.services.exception.BadRequestPersephoneException;
import com.vmware.blockchain.deployment.services.exception.PersephoneException;
import com.vmware.blockchain.deployment.services.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorData;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorProvider;
import com.vmware.blockchain.deployment.services.orchestration.ipam.IpamClient;
import com.vmware.blockchain.deployment.services.orchestration.vmware.OrchestratorFactory;
import com.vmware.blockchain.deployment.services.orchestrationsite.OrchestrationSites;
import com.vmware.blockchain.deployment.services.provision.DeleteResource;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;
import com.vmware.blockchain.deployment.v1.DeployedResource;
import com.vmware.blockchain.deployment.v1.DeploymentExecutionEvent;
import com.vmware.blockchain.deployment.v1.DeploymentRequest;
import com.vmware.blockchain.deployment.v1.DeploymentRequestResponse;
import com.vmware.blockchain.deployment.v1.DeprovisionDeploymentRequest;
import com.vmware.blockchain.deployment.v1.DeprovisionDeploymentResponse;
import com.vmware.blockchain.deployment.v1.NodeType;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteIdentifier;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.ProvisioningServiceV2Grpc;
import com.vmware.blockchain.deployment.v1.Sites;
import com.vmware.blockchain.deployment.v1.StreamDeploymentSessionEventRequest;

import io.grpc.stub.StreamObserver;
import lombok.extern.slf4j.Slf4j;
import lombok.val;

/**
 * Implementation of ProvisioningServiceV2 server.
 */
@GRpcService
@Slf4j
public class ProvisioningServiceV2 extends ProvisioningServiceV2Grpc.ProvisioningServiceV2ImplBase {

    private final BootstrapComponent bootstrapComponent;

    private final NetworkHelper networkHelper;
    private final ComputeHelper computeHelper;
    private final ConfigHelper configHelper;
    private final NodeConfiguration nodeConfiguration;
    private final OrchestratorProvider orchestratorProvider;

    private final Cache<UUID, CompletableFuture<DeploymentExecutionContext>> deploymentLogCache;

    @Autowired
    ProvisioningServiceV2(BootstrapComponent bootstrapComponent, NodeConfiguration nodeConfiguration) {
        this.bootstrapComponent = bootstrapComponent;
        this.nodeConfiguration = nodeConfiguration;
        this.orchestratorProvider = new OrchestratorFactory();
        this.networkHelper = new NetworkHelper();
        this.computeHelper = new ComputeHelper(bootstrapComponent);
        configHelper = new ConfigHelper(bootstrapComponent);
        this.deploymentLogCache = CacheBuilder.newBuilder().expireAfterWrite(5, TimeUnit.HOURS).build();
    }

    @Override
    public void createDeployment(DeploymentRequest request,
                                 StreamObserver<DeploymentRequestResponse> responseObserver)     {

        /// ---- Validation and input manipulation/extraction ----
        val sessionId = ProvisioningServiceUtil.extractOrGenerateId(request.getHeader().getId());
        val consortiumId = ProvisioningServiceUtil.extractOrGenerateId(request.getSpec().getConsortiumId());
        val blockchainId = ProvisioningServiceUtil.extractOrGenerateId(request.getSpec().getBlockchainId());
        var nodeTypeMap = ProvisioningServiceUtil.convertToNodeTypeMap(request.getSpec().getNodePropertiesMap(),
                                                                       request.getSpec().getProperties());
        var nodeAssignment = ProvisioningServiceUtil.updateNodeAssignment(request.getSpec().getNodeAssignment(),
                                                                                nodeTypeMap);
        var orchestrators = createOrchestratorsFromSites(request.getSpec().getSites());
        var siteMap = ProvisioningServiceUtil.convertToSiteIdMap(request.getSpec().getSites());
        var deploymentType = ProvisioningServiceUtil.deriveDeploymentType(request.getSpec().getSites());
        var baseNodeTypeComponent = nodeConfiguration.generateModelSpec(request.getSpec().getBlockchainType(),
                                                                        nodeTypeMap);
        var componentsByNode = ProvisioningServiceUtil.generateComponentsPerNode(nodeAssignment, orchestrators,
                                                                                 baseNodeTypeComponent);
        //TODO add site specific restriction.

        /// ---- No input manipulation/extraction beyond this point ----

        var deploymentSession = DeploymentExecutionContext.builder()
                .id(sessionId)
                .consortiumId(consortiumId)
                .blockchainId(blockchainId)
                .blockchainType(request.getSpec().getBlockchainType())
                .sitesById(siteMap)
                .componentsByNode(componentsByNode)
                .nodeAssignment(nodeAssignment)
                .orchestrators(orchestrators)
                .deploymentType(deploymentType)
                .build();
        deploymentSession.events.add(generateEvent(deploymentSession, DeploymentExecutionEvent.Type.ACKNOWLEDGED,
                                                   DeploymentExecutionEvent.Status.ACTIVE));

        // If successfully recorded.
        if (!deploymentLogCache.asMap().containsKey(sessionId)) {
            deploymentLogCache.put(sessionId, new CompletableFuture<>());
            // Start the async workflow to carry out the deployment plan.
            CompletableFuture.runAsync(() -> deployBlockchain(deploymentSession));

            // Emit the acknowledgement and signal completion of the request.
            responseObserver
                    .onNext(DeploymentRequestResponse.newBuilder().setId(deploymentSession.id.toString()).build());
            responseObserver.onCompleted();
        } else {
            responseObserver.onError(new IllegalStateException("Cannot record deployment session due to duplication"));
        }
    }

    @Override
    public void streamDeploymentSessionEvents(
            StreamDeploymentSessionEventRequest message,
            StreamObserver<DeploymentExecutionEvent> observer
    ) {
        var request = Objects.requireNonNull(message);
        var response = Objects.requireNonNull(observer);

        CompletableFuture.runAsync(() -> {
            Consumer<DeploymentExecutionContext> sender = session -> {
                // Send all events.
                for (DeploymentExecutionEvent event : session.events) {
                    log.info("Deployment session event stream session({}), event({})",
                             session.getId(), event);
                    response.onNext(event);
                }

                // Send completion.
                log.info("Deployment session event stream completed, session({})",
                         session.getId());
                response.onCompleted();
            };

            var task = deploymentLogCache.asMap().get(UUID.fromString(request.getSessionId()));
            if (task != null) {
                task.thenAcceptAsync(sender);
            } else {
                var text = "Session does not have a background task";
                response.onError(new IllegalStateException(text));
            }
        }).exceptionally(error -> {
            response.onError(error);
            return null; // To satisfy type signature (Void).
        });

    }

    @Override
    public void deprovisionDeployment(
            DeprovisionDeploymentRequest message,
            StreamObserver<DeprovisionDeploymentResponse> observer
    ) {
        var request = Objects.requireNonNull(message);
        var response = Objects.requireNonNull(observer);

        log.info("Received deprovision request for : " + request.getSessionId());
        try {
            deprovision(request);
            response.onNext(DeprovisionDeploymentResponse.newBuilder().build());
            response.onCompleted();
        } catch (Throwable error) {
            response.onError(error);
        }
    }

    //////////////////// Private methods ///////////////////////////////////

    void deployBlockchain(DeploymentExecutionContext session) {

        DeploymentExecutionEvent.Status status = DeploymentExecutionEvent.Status.FAILURE;
        try {
            // Allocate private network addresses for every node.
            session.localNodeDetailsMap =
                    networkHelper.createPrivateIpMap(session.nodeAssignment, session.orchestrators, session.results);

            log.info("Generating configuration for the deployment");
            ConfigurationSessionIdentifier configGenerated = configHelper.generateConfigurationId(session);

            log.info("Create replica/committer nodes (if applicable)");
            var committerNodes = computeHelper.getComputeNodes(session, configGenerated, NodeType.REPLICA);

            committerNodes.forEach(each -> {
                each.getValue().join();
                each.getValue().thenAccept(session.results::add);
            });

            // Add the VM creation for Object Store/Read-Replica client.

            log.info("Create client nodes (if applicable)");
            var clientNodes = computeHelper.getComputeNodes(session, configGenerated, NodeType.CLIENT);

            clientNodes.forEach(each -> {
                each.getValue().join();
                each.getValue().thenAccept(session.results::add);
            });

            // If VMC, create IP's
            if (session.deploymentType == OrchestrationSiteInfo.Type.VMC) {
                //TODO break this flow to different API.
                log.info("VMC cloud deployment... then allocate public IP and NAT rule");
                networkHelper.applyPublicNetworkAddress(session.nodeAssignment, session.orchestrators,
                                                        session.localNodeDetailsMap, session.results);

            }

            session.status = DeploymentExecutionEvent.Status.SUCCESS;
            status = DeploymentExecutionEvent.Status.SUCCESS;
            log.info("Deployment session({}) completed", session.getId());
        } catch (PersephoneException e) {
            log.warn("Caught handled exception", e);
            session.status = DeploymentExecutionEvent.Status.FAILURE;
        } catch (Exception e) {
            log.error("Caught un-handled exception", e);
            session.status = DeploymentExecutionEvent.Status.FAILURE;
        } finally {
            session.results.forEach(each ->
                                                     session.events.add(DeploymentExecutionEvent.newBuilder()
                                                             .setType(DeploymentExecutionEvent.Type.RESOURCE)
                                                             .setSessionId(session.id.toString())
                                                             .setStatus(DeploymentExecutionEvent.Status.ACTIVE)
                                                             .setResource(each)
                                                             .setSessionId(session.id.toString())
                                                             .build()));
            session.events.add(generateEvent(session, DeploymentExecutionEvent.Type.COMPLETED, status));
            deploymentLogCache.asMap().get(session.id).complete(session);
        }
        //session.orchestrators.values().stream().forEach(each -> each.cl);
    }

    Map<OrchestrationSiteIdentifier, Orchestrator> createOrchestratorsFromSites(Sites sites) {
        return sites.getInfoListList().stream()
                .map(entry -> Map.entry(entry.getId(), orchestratorProvider.newOrchestrator(
                        OrchestrationSites.buildSiteInfo(
                                entry.getInfo(),
                                bootstrapComponent.containerRegistry
                        ), new IpamClient(bootstrapComponent.allocationService))
                     )
                )
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue, (v1, v2) -> v1));
    }

    private void deprovision(DeprovisionDeploymentRequest requestSession) {
        var orchestrators = createOrchestratorsFromSites(requestSession.getSites());
        if (requestSession.getResourceList().isEmpty()) {
            var sessionId = UUID.fromString(requestSession.getSessionId());
            if (deploymentLogCache.asMap().containsKey(sessionId)) {
                log.info("Using in memory session");
                try {
                    deleteResourceEvents(orchestrators,
                                         new ArrayList<>(deploymentLogCache.asMap().get(sessionId).get().results));
                } catch (Exception e) {
                    log.error("Error looking up session.");
                }
            } else {
                throw new BadRequestPersephoneException("invalid input");
            }
        } else {
            deleteResourceEvents(orchestrators, requestSession.getResourceList());
        }
        log.info("Deprovisioning completed");
    }

    private URI getUrl(String url) {
        try {
            return URI.create(url);
        } catch (Exception e) {
            return null;
        }
    }

    private ConcurrentHashMap.KeySetView<OrchestratorData.OrchestrationEvent, Boolean> deleteResourceEvents(
            Map<OrchestrationSiteIdentifier, Orchestrator> orchestrators,
            List<DeployedResource> events
    ) {
        final var deleteEvents = ConcurrentHashMap.<OrchestratorData.OrchestrationEvent>newKeySet();

        List<Map.Entry<Orchestrator, URI>> networkAllocList = new ArrayList<>();
        List<Map.Entry<Orchestrator, URI>> computeList = new ArrayList<>();
        List<Map.Entry<Orchestrator, URI>> networkAddrList = new ArrayList<>();

        for (DeployedResource event : events) {

            URI resourceUri = getUrl(event.getName());

            try {
                if (resourceUri != null) {
                    OrchestrationSiteIdentifier site = OrchestrationSiteIdentifier.newBuilder()
                            .setId(event.getSiteId()).build();
                    DeployedResource.Type resourceType = event.getType();

                    if (resourceType.equals(DeployedResource.Type.NETWORK_ALLOCATION)) {
                        networkAllocList.add(Map.entry(orchestrators.get(site), resourceUri));
                    }
                    if (resourceType.equals(DeployedResource.Type.COMPUTE_RESOURCE)) {
                        computeList.add(Map.entry(orchestrators.get(site), resourceUri));
                    }
                    if (resourceType.equals(DeployedResource.Type.NETWORK_RESOURCE)) {
                        networkAddrList.add(Map.entry(orchestrators.get(site), resourceUri));
                    }
                }
            } catch (Exception e) {
                log.warn("Some error", e);
            }
        }

        // FIXME: 1. populating deleteEvents inside whenComplete/handle is not behaving correctly. Investigate and fix
        // FIXME: 2. exception handling from event generator rather than here having one exception for all
        // [https://jira.eng.vmware.com/browse/VB-1146]
        try {
            deleteEvents.addAll(DeleteResource.deleteNetworkAllocations(networkAllocList).get());
        } catch (InterruptedException | ExecutionException e) {
            log.error("NAT deletion failed with exception: " + e);
        }

        try {
            deleteEvents.addAll(DeleteResource.deleteDeployments(computeList).get());
        } catch (InterruptedException | ExecutionException e) {
            log.error("Compute Resource deletion failed with exception: " + e);
        }

        try {
            deleteEvents.addAll(DeleteResource.deleteNetworkAddresses(networkAddrList).get());
        } catch (InterruptedException | ExecutionException e) {
            log.error("Network Address deletion failed with exception: " + e);
        }

        return deleteEvents;
    }
}