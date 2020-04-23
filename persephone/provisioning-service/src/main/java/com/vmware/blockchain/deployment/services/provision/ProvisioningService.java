/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.provision;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Random;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.lognet.springboot.grpc.GRpcService;
import org.springframework.beans.factory.annotation.Autowired;

import com.google.protobuf.InvalidProtocolBufferException;
import com.google.protobuf.util.JsonFormat;
import com.vmware.blockchain.deployment.server.BootstrapComponent;
import com.vmware.blockchain.deployment.services.configservice.ConfigServiceInvoker;
import com.vmware.blockchain.deployment.services.futureutil.ReactiveStream;
import com.vmware.blockchain.deployment.services.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorData;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorProvider;
import com.vmware.blockchain.deployment.services.orchestration.ipam.IpamClient;
import com.vmware.blockchain.deployment.services.orchestration.vmware.OrchestratorFactory;
import com.vmware.blockchain.deployment.v1.ConcordCluster;
import com.vmware.blockchain.deployment.v1.ConcordClusterIdentifier;
import com.vmware.blockchain.deployment.v1.ConcordClusterInfo;
import com.vmware.blockchain.deployment.v1.ConcordComponent;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;
import com.vmware.blockchain.deployment.v1.ConcordNode;
import com.vmware.blockchain.deployment.v1.ConcordNodeIdentifier;
import com.vmware.blockchain.deployment.v1.ConcordNodeStatus;
import com.vmware.blockchain.deployment.v1.ConfigurationServiceGrpc;
import com.vmware.blockchain.deployment.v1.ConfigurationServiceRequest;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;
import com.vmware.blockchain.deployment.v1.CreateClusterRequest;
import com.vmware.blockchain.deployment.v1.DeploymentSession;
import com.vmware.blockchain.deployment.v1.DeploymentSessionEvent;
import com.vmware.blockchain.deployment.v1.DeploymentSessionIdentifier;
import com.vmware.blockchain.deployment.v1.DeploymentSpecification;
import com.vmware.blockchain.deployment.v1.LogManagement;
import com.vmware.blockchain.deployment.v1.MessageHeader;
import com.vmware.blockchain.deployment.v1.NodeProperty;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteIdentifier;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteInfo;
import com.vmware.blockchain.deployment.v1.OutboundProxyInfo;
import com.vmware.blockchain.deployment.v1.PlacementAssignment;
import com.vmware.blockchain.deployment.v1.PlacementSpecification;
import com.vmware.blockchain.deployment.v1.Properties;
import com.vmware.blockchain.deployment.v1.ProvisionedResource;
import com.vmware.blockchain.deployment.v1.ProvisioningServiceGrpc;
import com.vmware.blockchain.deployment.v1.StreamAllClusterDeploymentSessionEventRequest;
import com.vmware.blockchain.deployment.v1.StreamClusterDeploymentSessionEventRequest;
import com.vmware.blockchain.deployment.v1.UpdateDeploymentSessionRequest;
import com.vmware.blockchain.deployment.v1.UpdateDeploymentSessionResponse;
import com.vmware.blockchain.deployment.v1.Wavefront;
import com.vmware.blockchain.ethereum.type.Genesis;

import io.grpc.stub.ServerCallStreamObserver;
import io.grpc.stub.StreamObserver;
import lombok.extern.slf4j.Slf4j;


/**
 * Implementation of ProvisioningService server.
 */
@GRpcService
@Slf4j
public class ProvisioningService extends ProvisioningServiceGrpc.ProvisioningServiceImplBase {

    /**
     * Orchestrator instance factory.
     */
    private final OrchestratorProvider orchestratorProvider;

    /**
     * Orchestrator pool to utilize for orchestration operations.
     */
    private final Map<OrchestrationSiteIdentifier, Orchestrator> orchestrators =
            new ConcurrentHashMap<>();

    /**
     * FIXME: In-Memory stand-in/crutches to keep track of deployment log.
     */
    private final Map<DeploymentSessionIdentifier, CompletableFuture<DeploymentSession>> deploymentLog =
            new ConcurrentHashMap<>();

    /**
     * list of all observers for StreamAllClusterDeploymentSessionEvents.
     */
    private static CopyOnWriteArrayList<StreamObserver<DeploymentSessionEvent>> eventsObserver =
            new CopyOnWriteArrayList<>();

    /**
     * Queue for all deployment session events.
     */
    private static final ConcurrentLinkedQueue<DeploymentSessionEvent> eventQueue =
            new ConcurrentLinkedQueue<>();

    /**
     * Background thread executor.
     */
    private static final ScheduledExecutorService backgroundExecutor =
            Executors.newSingleThreadScheduledExecutor();

    /**
     * Background task future.
     */
    private ScheduledFuture<?> eventEmitter;

    /**
     * Configuration service client.
     */
    private ConfigurationServiceGrpc.ConfigurationServiceStub configurationServiceClient;

    private final IpamClient ipamClient;
    private final BootstrapComponent bootstrapComponent;

    /**
     * Constructor.
     */
    @Autowired
    ProvisioningService(BootstrapComponent bootstrapComponent) {
        this.orchestratorProvider = new OrchestratorFactory();
        configurationServiceClient = (new ConfigServiceInvoker(bootstrapComponent.configService))
                .generateConfigServiceStub();

        this.bootstrapComponent = bootstrapComponent;
        this.ipamClient = new IpamClient(bootstrapComponent.allocationService);

        // Continuous polling of events happen, given out to observers whenever available.
        eventEmitter = backgroundExecutor
                .scheduleAtFixedRate(this::emitQueueEvents, 15, 1, TimeUnit.SECONDS);
        log.info("Background task scheduled to run at 1 second interval after 10sec of initial wait.");
    }

    @Override
    public void createCluster(CreateClusterRequest message, StreamObserver<DeploymentSessionIdentifier> observer) {
        var request = Objects.requireNonNull(message);
        var response = Objects.requireNonNull(observer);

        // Resolve / generate deployment session ID.
        var sessionId = ProvisioningServiceUtil.newSessionId(request.getHeader());
        var clusterId = ProvisioningServiceUtil.newClusterId();
        var deploymentSpec = request.getSpecification();
        // Generate node ID and affix the node placements.
        var placements = resolvePlacement(deploymentSpec);

        // Persist the deployment log.
        var session = DeploymentSession.newBuilder()
                .setId(sessionId)
                .setSpecification(deploymentSpec)
                .setCluster(clusterId)
                .setAssignment(placements)
                .setStatus(DeploymentSession.Status.ACTIVE)
                .addAllEvents(Collections.singletonList(ProvisioningServiceUtil.newInitialEvent(sessionId)))
                .build();
        var oldSession = deploymentLog.putIfAbsent(sessionId, new CompletableFuture<>());

        // If successfully recorded.
        if (oldSession == null) {
            // Start the async workflow to carry out the deployment plan.
            CompletableFuture.runAsync(() -> deployCluster(session));

            // Emit the acknowledgement and signal completion of the request.
            response.onNext(sessionId);
            response.onCompleted();
        } else {
            var text = "Cannot record deployment session";
            response.onError(new IllegalStateException(text));
        }
    }


    @Override
    public void streamAllClusterDeploymentSessionEvents(
            StreamAllClusterDeploymentSessionEventRequest message,
            StreamObserver<DeploymentSessionEvent> observer
    ) {
        var response = Objects.requireNonNull(observer);
        log.info("Adding observer " + response.toString() + " to event observer list");
        eventsObserver.add(response);
    }

    @Override
    public void streamClusterDeploymentSessionEvents(
            StreamClusterDeploymentSessionEventRequest message,
            StreamObserver<DeploymentSessionEvent> observer
    ) {
        var request = Objects.requireNonNull(message);
        var response = Objects.requireNonNull(observer);

        CompletableFuture.runAsync(() -> {
            // FIXME: Right now the logic only works correctly for single service instance.
            Consumer<DeploymentSession> sender = session -> {
                // Send all events.
                for (DeploymentSessionEvent event : session.getEventsList()) {
                    log.info("Deployment session event stream session({}), event({})",
                             session.getId(), event);
                    response.onNext(event);
                }

                // Send completion.
                log.info("Deployment session event stream completed, session({})",
                         session.getId());
                response.onCompleted();
            };

            var task = deploymentLog.get(request.getSession());
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
    public void updateDeploymentSession(UpdateDeploymentSessionRequest message,
                                        StreamObserver<UpdateDeploymentSessionResponse> observer) {
        var request = Objects.requireNonNull(message);
        var response = Objects.requireNonNull(observer);

        log.info("Received update request for : " + request.getSession());
        try {
            if (request.getAction().equals(UpdateDeploymentSessionRequest.Action.DEPROVISION_ALL)) {
                log.info("Received deprovision request for : " + request.getSession());
                deprovision(request.getSession());
            }
            response.onNext(UpdateDeploymentSessionResponse.getDefaultInstance());
            response.onCompleted();
        } catch (Throwable error) {
            response.onError(error);
        }

    }

    /**
     * Emits any queued events to active observers.
     */
    private void emitQueueEvents() {
        if (eventQueue.size() > 0 && !eventsObserver.isEmpty()) {
            DeploymentSessionEvent event = eventQueue.poll();
            List<Map.Entry<StreamObserver, Exception>> cleanupList = new ArrayList<>();
            eventsObserver.forEach(observer -> {
                try {
                    ServerCallStreamObserver callStreamObs = (ServerCallStreamObserver) observer;
                    if (callStreamObs.isCancelled()) {
                        log.info("streamObserver ({}) cancelled", observer);
                        cleanupList.add(Map.entry(observer, new Exception("Observer connection lost")));
                    } else {
                        log.info("sending ({}) to streamObserver ({})", event, observer.toString());
                        observer.onNext(event);
                    }
                } catch (Exception e) {
                    log.info("Observer ({}) had error {}", observer, e);
                    cleanupList.add(Map.entry(observer, e));
                }
            });

            if (!cleanupList.isEmpty()) {
                cleanupList.forEach(cleanObs -> {
                    try {
                        var obs = cleanObs.getKey();
                        log.info("Removing observer: {} from list", obs);
                        eventsObserver.remove(obs);
                        obs.onError(cleanObs.getValue());
                    } catch (Exception e) {
                        log.error("Error sending Error message to observer ({}). Original Exception: {}."
                                  + " Exception: {}", cleanObs.getKey(), cleanObs.getValue(), e);
                    }
                });
            }
        }
    }

    /**
     * Create a placement resolution for a given {@link DeploymentSpecification} such that all placement entries have a
     * specific designated orchestration site target.
     *
     * @param specification input placement specification.
     * @return a new instance of {@link PlacementAssignment} that contains mappings of {@link ConcordNodeIdentifier} to
     * the {@link OrchestrationSiteIdentifier} corresponding to the target deployment site for that node.
     */
    private PlacementAssignment resolvePlacement(DeploymentSpecification specification) {
        var availableSites = orchestrators.keySet().toArray(OrchestrationSiteIdentifier[]::new);
        var random = new Random();
        var assignments = specification.getPlacement().getEntriesList().stream()
                .map(entry -> {
                    var uuid = UUID.randomUUID();
                    var nodeId = ConcordNodeIdentifier.newBuilder()
                            .setLow(uuid.getLeastSignificantBits())
                            .setHigh(uuid.getMostSignificantBits())
                            .setId(uuid.toString())
                            .build();

                    OrchestrationSiteIdentifier site;
                    if (entry.getType() == PlacementSpecification.Type.FIXED) {
                        site = entry.getSite();
                    } else {
                        site = availableSites[random.nextInt(availableSites.length)];
                    }
                    return PlacementAssignment.Entry.newBuilder()
                            .setNode(nodeId)
                            .setSite(site)
                            .setSiteInfo(entry.getSiteInfo())
                            .build();
                })
                .collect(Collectors.toUnmodifiableList());

        return PlacementAssignment.newBuilder().addAllEntries(assignments).build();
    }

    private CompletableFuture<ConfigurationSessionIdentifier> generateConfigurationId(
            ConcurrentHashMap<PlacementAssignment.Entry, OrchestratorData.NetworkResourceEventCreated>
                    privateNetworkAddressMap, Map<ConcordNodeIdentifier, Integer> concordIdentifierMap,
            DeploymentSession session) {

        List<String> nodeIps = new ArrayList<>();
        Map<Integer, String> nodeIds = new HashMap<>();
        Map<Integer, String> loggingProperties = new HashMap<>();
        Map<Integer, String> wavefrontProxyUrl = new HashMap<>();
        Map<Integer, String> wavefrontProxyPort = new HashMap<>();
        Map<Integer, String> nodeIpMap = new HashMap<>();

        Wavefront wavefront = Wavefront.newBuilder().build();
        for (Map.Entry<PlacementAssignment.Entry,
                OrchestratorData.NetworkResourceEventCreated> entry : privateNetworkAddressMap.entrySet()) {
            var nodeIp = entry.getValue().getAddress();
            nodeIps.add(nodeIp);
            var nodeIndex = nodeIps.indexOf(nodeIp);
            // TODO : nodeIps can be deprecated in favor of nodeIpMap
            nodeIpMap.put(nodeIndex, nodeIp);

            var siteInfo = entry.getKey().getSiteInfo();

            var nodeUuid = new UUID(entry.getKey().getNode().getHigh(), entry.getKey().getNode().getLow()).toString();
            nodeIds.put(nodeIndex, nodeUuid);
            loggingProperties.put(nodeIndex, getLogManagementJson(siteInfo));
            concordIdentifierMap.put(entry.getKey().getNode(), nodeIndex);

            // PS: this keeps the provision open for multiple wavefront config in multiple zones,
            // however, taking only the last indeterministically. Need a design fix here.
            // wavefront URL and Token should be same for all zones ideally.
            wavefront = getWavefront(siteInfo);

            var outboundProxy = getOutboundProxy(siteInfo);
            if (!outboundProxy.getHttpsHost().isEmpty()) {
                wavefrontProxyUrl.put(nodeIndex, outboundProxy.getHttpsHost());
                wavefrontProxyPort.put(nodeIndex, String.valueOf(outboundProxy.getHttpsPort()));
            }

        }

        Map<String, String> properties = new HashMap<>(session.getSpecification().getProperties().getValuesMap());
        properties.put(NodeProperty.Name.CONSORTIUM_ID.toString(), session.getSpecification().getConsortium());
        properties.put(NodeProperty.Name.WAVEFRONT_URL.toString(), wavefront.getUrl());
        properties.put(NodeProperty.Name.WAVEFRONT_TOKEN.toString(), wavefront.getToken());

        if (!properties.containsKey(NodeProperty.Name.BLOCKCHAIN_ID.toString())) {
            var blockchainId = new UUID(session.getCluster().getHigh(), session.getCluster().getLow());
            properties.put(NodeProperty.Name.BLOCKCHAIN_ID.toString(), blockchainId.toString());
        }

        NodeProperty nodeProperty =
                NodeProperty.newBuilder().setName(NodeProperty.Name.NODE_ID).putAllValue(nodeIds).build();
        NodeProperty loggingProperty =
                NodeProperty.newBuilder().setName(NodeProperty.Name.LOGGING_CONFIG).putAllValue(loggingProperties)
                        .build();
        NodeProperty nodeIpProperty = NodeProperty.newBuilder().setName(NodeProperty.Name.NODE_IP)
                .putAllValue(nodeIpMap).build();

        List<NodeProperty> nodePropertyList = Arrays.asList(nodeProperty, nodeIpProperty, loggingProperty);

        if (!wavefrontProxyUrl.isEmpty()) {
            nodePropertyList.add(NodeProperty.newBuilder().setName(NodeProperty.Name.WAVEFRONT_PROXY_HOST)
                                         .putAllValue(wavefrontProxyUrl).build());
            nodePropertyList.add(NodeProperty.newBuilder().setName(NodeProperty.Name.WAVEFRONT_PROXY_PORT)
                                         .putAllValue(wavefrontProxyPort).build());
        }

        var request = ConfigurationServiceRequest.newBuilder()
                .setHeader(MessageHeader.getDefaultInstance())
                .addAllHosts(nodeIps)
                .setBlockchainType(session.getSpecification().getModel().getBlockchainType())
                .setGenesis(session.getSpecification().getGenesis())
                .addAllServices(session.getSpecification().getModel().getComponentsList().stream()
                                        .filter(x -> !x.getServiceType().equals(ConcordComponent.ServiceType.GENERIC))
                                        .map(x -> x.getServiceType()).collect(Collectors.toList()))
                .setProperties(Properties.newBuilder().putAllValues(properties).build())
                .addAllNodeProperties(nodePropertyList)
                .build();

        CompletableFuture<ConfigurationSessionIdentifier> completable
                = new CompletableFuture<>();
        configurationServiceClient.createConfiguration(request, ReactiveStream.blockedResultObserver(completable));
        return completable;
    }

    private OutboundProxyInfo getOutboundProxy(OrchestrationSiteInfo siteInfo) {
        OutboundProxyInfo outboundProxyInfo = OutboundProxyInfo.newBuilder().build();
        switch (siteInfo.getType()) {
            case VMC:
                outboundProxyInfo = siteInfo.getVmc().getVsphere().getOutboundProxy();
                break;
            case VSPHERE:
                outboundProxyInfo = siteInfo.getVsphere().getVsphere().getOutboundProxy();
                break;
            default:
                break;
        }
        return outboundProxyInfo;
    }

    private Wavefront getWavefront(OrchestrationSiteInfo siteInfo) {
        Wavefront wavefront = Wavefront.newBuilder().build();
        switch (siteInfo.getType()) {
            case VMC:
                wavefront = siteInfo.getVmc().getWavefront();
                break;
            case VSPHERE:
                wavefront = siteInfo.getVsphere().getWavefront();
                break;
            default:
                break;
        }
        return wavefront;
    }

    private String getLogManagementJson(OrchestrationSiteInfo siteInfo) {

        List<LogManagement> logManagements = new ArrayList<>();

        switch (siteInfo.getType()) {
            case VMC:
                logManagements.addAll(siteInfo.getVmc().getLogManagementsList());
                break;
            case VSPHERE:
                logManagements.addAll(siteInfo.getVsphere().getLogManagementsList());
                break;
            default:
                break;
        }

        // TODO: It takes only the first one, subject to change if design changes.
        if (!logManagements.isEmpty()) {
            LogManagement logManagement = logManagements.get(0);
            try {
                return JsonFormat.printer().print(logManagement);
            } catch (InvalidProtocolBufferException e) {
                log.error("error parsing log info" + e);
            }
        }
        return "";
    }

    /**
     * Execute a Concord cluster deprovisioning workflow.
     *
     * @param requestSession deploymentSessionIdentifier to carry out the workflow for.
     */
    private void deprovision(DeploymentSessionIdentifier requestSession) {

        CompletableFuture<DeploymentSession> sessionEventTask = deploymentLog.get(requestSession);
        if (sessionEventTask != null) {
            sessionEventTask.thenAcceptAsync(deploymentSession -> {
                final var deleteEvents = deleteResourceEvents(deploymentSession.getEventsList());
                var deprovEv = toDeprovisioningEvents(deploymentSession, deleteEvents,
                                                      DeploymentSession.Status.SUCCESS);
                CompletableFuture<DeploymentSession> sessionFuture = new CompletableFuture<>();
                sessionFuture.complete(DeploymentSession.newBuilder(deploymentSession).addAllEvents(deprovEv).build());
                deploymentLog.replace(requestSession, sessionFuture);
                log.info("Deprovisioning completed");
            })
                    .exceptionally(error -> {
                        throw new IllegalStateException("Deprovisioning failed for id: " + requestSession, error);
                    });
            sessionEventTask.join();
        } else {
            throw new IllegalStateException("Session does not have a background task");
        }
    }

    private ConcurrentHashMap.KeySetView<OrchestratorData.OrchestrationEvent, Boolean> deleteResourceEvents(
            List<DeploymentSessionEvent> events
    ) {
        final var deleteEvents = ConcurrentHashMap.<OrchestratorData.OrchestrationEvent>newKeySet();

        List<Map.Entry<Orchestrator, URI>> networkAllocList = new ArrayList<>();
        List<Map.Entry<Orchestrator, URI>> computeList = new ArrayList<>();
        List<Map.Entry<Orchestrator, URI>> networkAddrList = new ArrayList<>();

        for (DeploymentSessionEvent event : events) {
            if (!event.getType().equals(DeploymentSessionEvent.Type.RESOURCE)) {
                continue;
            }

            URI resourceUri = getUrl(event.getResource().getName());

            try {
                if (resourceUri != null) {
                    OrchestrationSiteIdentifier site = event.getResource().getSite();
                    ProvisionedResource.Type resourceType = event.getResource().getType();

                    if (resourceType.equals(ProvisionedResource.Type.NETWORK_ALLOCATION)) {
                        networkAllocList.add(Map.entry(orchestrators.get(site), resourceUri));
                    }
                    if (resourceType.equals(ProvisionedResource.Type.COMPUTE_RESOURCE)) {
                        computeList.add(Map.entry(orchestrators.get(site), resourceUri));
                    }
                    if (resourceType.equals(ProvisionedResource.Type.NETWORK_RESOURCE)) {
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

    private List<DeploymentSessionEvent> toDeprovisioningEvents(
            DeploymentSession session,
            Collection<OrchestratorData.OrchestrationEvent> events,
            DeploymentSession.Status status
    ) {
        List<DeploymentSessionEvent> deprovisioningEvent = new ArrayList<>();
        events.forEach(event -> {
            ProvisionedResource resource = null;
            if (event instanceof OrchestratorData.NetworkAllocationEvent) {
                var resEvent = (OrchestratorData.NetworkAllocationEvent) event;
                resource = ProvisionedResource.newBuilder().setType(ProvisionedResource.Type.NETWORK_ALLOCATION)
                        .setName(resEvent.getResource().toString())
                        .setSite(OrchestrationSiteIdentifier.getDefaultInstance())
                        .setCluster(session.getCluster())
                        .setNode(ConcordNodeIdentifier.getDefaultInstance())
                        .build();
            } else if (event instanceof OrchestratorData.NetworkResourceEvent) {
                var resEvent = (OrchestratorData.NetworkResourceEvent) event;
                resource = ProvisionedResource.newBuilder().setType(ProvisionedResource.Type.NETWORK_RESOURCE)
                        .setName(resEvent.getResource().toString())
                        .setSite(OrchestrationSiteIdentifier.getDefaultInstance())
                        .setCluster(session.getCluster())
                        .setNode(ConcordNodeIdentifier.getDefaultInstance())
                        .build();
            } else if (event instanceof OrchestratorData.ComputeResourceEvent) {
                var resEvent = (OrchestratorData.ComputeResourceEvent) event;
                resource = ProvisionedResource.newBuilder().setType(ProvisionedResource.Type.COMPUTE_RESOURCE)
                        .setName(resEvent.getResource().toString())
                        .setSite(OrchestrationSiteIdentifier.getDefaultInstance())
                        .setCluster(session.getCluster())
                        .setNode(ConcordNodeIdentifier.getDefaultInstance())
                        .build();
            }

            if (resource != null) {
                deprovisioningEvent.add(
                        DeploymentSessionEvent.newBuilder().setType(DeploymentSessionEvent.Type.RESOURCE_DEPROVISIONING)
                                .setSession(session.getId())
                                .setStatus(status)
                                .setResource(resource)
                                .setNode(ConcordNode.getDefaultInstance())
                                .setNodeStatus(ConcordNodeStatus.getDefaultInstance())
                                .setCluster(ConcordCluster.getDefaultInstance())
                                .build());
            }
        });

        eventQueue.addAll(deprovisioningEvent);
        return deprovisioningEvent;
    }

    private URI getUrl(String url) {
        try {
            return URI.create(url);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Execute a Concord cluster deployment workflow.
     *
     * @param session deployment session to carry out the workflow for.
     */
    private void deployCluster(DeploymentSession session) {
        final var results = ConcurrentHashMap.<OrchestratorData.OrchestrationEvent>newKeySet();
        try {
            // FIXME: The following is what NOT to do for reactive workflow!
            // gRPC stub expects single writer (e.g. response.onNext()) to emit the stream. So in effect
            // there can be only 1 reactive stream subscriber subscribing to "something", and react
            // according to the reactive stream's onNext() signals by emitting gRPC onNext() down the
            // stream.
            //
            // If multiple publishers are not first "merged" into a single publisher, then the only
            // option is to force all subscribers collect data into a buffer, and then have a single
            // writer emit all data when everything is collected.
            //
            // This is achieved in the following code by converting cold publishers into hot async
            // futures. Each future collects into its own "slot" in the concurrent hash "set", which is
            // a KeySetView of a ConcurrentHashMap. The second stage initiates when all node-related
            // futures are complete, and then engage in the single-writer emission.
            var sessionOrchestrators = session.getAssignment().getEntriesList().stream().distinct()
                    .map(entry -> Map.entry(entry.getSite(), orchestratorProvider.newOrchestrator(
                            OrchestrationSites.buildSiteInfo(
                                    entry.getSiteInfo(),
                                    bootstrapComponent.containerRegistry
                            ), ipamClient
                                            )
                         )
                    )
                    .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue, (v1, v2) -> v1));

            sessionOrchestrators.forEach((site, orchestrator) -> {
                orchestrators.put(site, orchestrator);
            });

            // Construct the context for this session execution.
            var context = new DeploymentSessionContext(sessionOrchestrators);

            // Allocate network addresses for every node.
            var networkAddressPublishers = session.getAssignment().getEntriesList().stream()
                    .map(entry -> {
                        var orchestrator = context.getOrchestrators().get(entry.getSite());

                        var node = entry.getNode();
                        var resource = ProvisioningServiceUtil.toResourceName(node);
                        var addressRequest = new OrchestratorData.CreateNetworkResourceRequest(resource, true);
                        var addressPublisher = orchestrator.createNetworkAddress(addressRequest);

                        return Map.entry(entry, addressPublisher);
                    })
                    .collect(Collectors.toUnmodifiableList());

            /* Create Network configuration */
            var publicNetworkAddressMap =
                    new ConcurrentHashMap<PlacementAssignment.Entry, OrchestratorData.NetworkResourceEventCreated>();

            var privateNetworkAddressMap =
                    new ConcurrentHashMap<PlacementAssignment.Entry, OrchestratorData.NetworkResourceEventCreated>();

            var networkAddressPromises = networkAddressPublishers.stream()
                    .map(entry -> ReactiveStream.toFuture(entry.getValue(), ArrayList::new)
                            .thenAcceptAsync(events -> {
                                // Put the events in the result collection.
                                results.addAll(events);

                                for (var event : events) {
                                    // Cast to creation event, let runtime cast throw exception if
                                    // cast fails, as then it will exceptionally trigger failure future
                                    // completion downstream.
                                    var createdEvent = (OrchestratorData.NetworkResourceEventCreated) event;

                                    // Place in the address lookup map by the placement entry key.
                                    if (createdEvent.isPublicResource()) {
                                        publicNetworkAddressMap.put(entry.getKey(), createdEvent);
                                    } else {
                                        privateNetworkAddressMap.put(entry.getKey(), createdEvent);
                                    }
                                }
                            })
                    )
                    .toArray(CompletableFuture[]::new);

            log.info("Generating configuration for the node");

            Map<ConcordNodeIdentifier, Integer> concordIdentifierMap = new HashMap<>();

            // Added retry to mitigate intermittent gRPC timeout. Make it more precise.
            int retry = 3;
            ConfigurationSessionIdentifier configId = null;
            do {
                /* Create configuration */
                try {
                    CompletableFuture<ConfigurationSessionIdentifier> configFuture =
                            CompletableFuture.allOf(networkAddressPromises)
                                    .thenComposeAsync(__ -> generateConfigurationId(
                                            privateNetworkAddressMap, concordIdentifierMap, session));
                    configId = configFuture.get();
                } catch (Throwable e) {
                    log.warn("Error received from config service", e);
                }
                retry--;
            } while (configId == null && retry > 0);

            /* Filter Wavefront workaround */
            var model = createMetricsExcludedModel(session.getSpecification().getModel(),
                                                   privateNetworkAddressMap);

            ConfigurationSessionIdentifier finalConfigId = configId;
            var nodePublishers = session.getAssignment().getEntriesList().stream()
                    .map(placement -> {
                        var publisher = CompletableFuture.supplyAsync(() -> deployNode(
                                concordIdentifierMap,
                                context.getOrchestrators().get(placement.getSite()),
                                session.getId(),
                                placement.getNode(),
                                model,
                                session.getSpecification().getGenesis(),
                                privateNetworkAddressMap.get(placement),
                                finalConfigId,
                                session.getSpecification().getProperties()));
                        return Map.entry(placement, publisher);
                    })
                    .collect(Collectors.toUnmodifiableList());

            var nodePromises = nodePublishers.stream()
                    .map(entry -> entry.getValue()
                            .thenComposeAsync(event -> {
                                // Put the events in the result collection.
                                results.add(event);

                                // Find any compute resource event and extract the URI.
                                // Hank to not touch other structures.
                                var computeResource = Arrays.asList(event).stream()
                                        .filter(OrchestratorData.ComputeResourceEvent.class::isInstance)
                                        .map(OrchestratorData.ComputeResourceEvent.class::cast)
                                        .map(OrchestratorData.ComputeResourceEvent::getResource)
                                        .findFirst()
                                        .orElseThrow(() -> new IllegalStateException(
                                                "No compute resource event")
                                        );

                                // Allocate network address to the created node.
                                // No need to do sequential.
                                var placement = entry.getKey();
                                var orchestrator = context.getOrchestrators().get(placement.getSite());

                                if (publicNetworkAddressMap.containsKey(placement)) {
                                    var publicNetworkResource = publicNetworkAddressMap
                                            .get(entry.getKey()).getResource();

                                    var privateNetworkResource = privateNetworkAddressMap
                                            .get(entry.getKey()).getResource();
                                    var resource = ProvisioningServiceUtil.toResourceName(placement.getNode());
                                    var allocationRequest =
                                            new OrchestratorData.CreateNetworkAllocationRequest(
                                                    resource,
                                                    computeResource,
                                                    publicNetworkResource,
                                                    privateNetworkResource
                                            );
                                    var allocationPublisher = orchestrator
                                            .createNetworkAllocation(allocationRequest);

                                    return ReactiveStream.toFutureSingle(allocationPublisher);
                                } else {
                                    return CompletableFuture.completedFuture(null);
                                }
                            })
                            // Put the network allocation event in the result collection.
                            .whenComplete((event, error) -> {
                                if (error != null) {
                                    log.error("Failed to deploy node({})", entry.getKey(), error);
                                } else {
                                    if (event != null) {
                                        results.add(event);
                                    }
                                }
                            })).toArray(CompletableFuture[]::new);

            CompletableFuture.allOf(nodePromises)
                    .thenRun(() -> {
                        // Create the updated deployment session instance.
                        var updatedSession = DeploymentSession.newBuilder(session)
                                .setStatus(DeploymentSession.Status.SUCCESS)
                                .clearEvents()
                                .addAllEvents(toDeploymentSessionEvents(session, results)).build();

                        deploymentLog.get(session.getId()).complete(updatedSession);

                        log.info("Deployment session({}) completed", session.getId());
                    });
        } catch (Throwable error) {
            try {
                log.info("Deployment session({}) failed", session.getId(), error);

                var event = ProvisioningServiceUtil.newCompleteEvent(session.getId(), DeploymentSession.Status.FAILURE);
                var updatedSession = DeploymentSession.newBuilder(session)
                        .setStatus(DeploymentSession.Status.FAILURE)
                        .addAllEvents(Arrays.asList(event)).build();

                log.info("Deployment session({}) failed", session.getId(), error);

                // Update the deployment log.
                // FIXME: This does not take into account of persistence nor retry.
                deploymentLog.get(session.getId()).complete(updatedSession);

                // Create the updated deployment session instance.
                var deleteEvents = deleteResourceEvents(toDeploymentSessionEvents(session, results));

                var deprovisioningSessionEvents = toDeprovisioningEvents(
                        session, deleteEvents, DeploymentSession.Status.FAILURE);

                session.getEventsList().addAll(deprovisioningSessionEvents);

                log.info("Deployment session({}) cleaned", session.getId(), error);
            } catch (Throwable e) {
                log.warn("Error during auto-cleanup");
            }

            deploymentLog.get(session.getId()).complete(
                    DeploymentSession.newBuilder(session).setStatus(DeploymentSession.Status.FAILURE)
                            .addAllEvents(Collections.singletonList(
                                    DeploymentSessionEvent.newBuilder().setType(DeploymentSessionEvent.Type.COMPLETED)
                                            .setSession(session.getId())
                                            .setStatus(DeploymentSession.Status.FAILURE)
                                            .setResource(ProvisionedResource.getDefaultInstance())
                                            .setNode(ConcordNode.getDefaultInstance())
                                            .setNodeStatus(ConcordNodeStatus.getDefaultInstance())
                                            .setCluster(ConcordCluster.getDefaultInstance())
                                            .build())).build()
            );
        }
    }

    /**
     * Temporary fix until wavefront team pushes feature to error out on incorrect url. PS: do not deploy wavefront when
     * even one zone does not have it.
     */
    private ConcordModelSpecification createMetricsExcludedModel(ConcordModelSpecification model,
                                                                 ConcurrentHashMap<PlacementAssignment.Entry,
                                                                         OrchestratorData.NetworkResourceEventCreated>
                                                                         privateNetworkAddressMap) {
        boolean isWavefront = true;
        List<ConcordComponent.ServiceType> exclusionList = List.of(
                ConcordComponent.ServiceType.WAVEFRONT_PROXY,
                ConcordComponent.ServiceType.TELEGRAF,
                ConcordComponent.ServiceType.JAEGER_AGENT);

        for (Map.Entry<PlacementAssignment.Entry,
                OrchestratorData.NetworkResourceEventCreated> entry : privateNetworkAddressMap.entrySet()) {
            var siteInfo = entry.getKey().getSiteInfo();
            Wavefront wavefront = getWavefront(siteInfo);
            if (wavefront.getUrl().isEmpty()
                || wavefront.getUrl().isBlank()
                || wavefront.getToken().isEmpty()
                || wavefront.getToken().isBlank()) {
                isWavefront = false;
            }
        }

        if (!isWavefront) {
            log.info("Wavfront URL and/or token not provided.\n"
                     + "wavefront-proxy, telegraf and jaeger-agent will not be deployed.");
            var compList = model.getComponentsList().stream()
                    .filter(comp -> !exclusionList.contains(comp))
                    .collect(Collectors.toList());
            return ConcordModelSpecification.newBuilder()
                    .setBlockchainType(model.getBlockchainType())
                    .setNodeType(model.getNodeType())
                    .setVersion(model.getVersion())
                    .setTemplate(model.getTemplate())
                    .addAllComponents(compList)
                    .build();
        } else {
            return model;
        }
    }

    /**
     * Execute a Concord node deployment workflow.
     *
     * @param orchestrator orchestrator to use to execute the node deployment workflow.
     * @param sessionId    overall deployment session this node deployment workflow belongs to.
     * @param nodeId       identifier of the Concord node to deploy.
     * @param model        model to use to setup the node.
     * @return a {@link Publisher} of {@link OrchestratorData.OrchestrationEvent}s corresponding to the execution of the
     * node deployment workflow.
     */
    private OrchestratorData.OrchestrationEvent deployNode(
            Map<ConcordNodeIdentifier, Integer> concordIdentifierMap,
            Orchestrator orchestrator,
            DeploymentSessionIdentifier sessionId,
            ConcordNodeIdentifier nodeId,
            ConcordModelSpecification model,
            Genesis genesis,
            OrchestratorData.NetworkResourceEventCreated networkResourceEvent,
            ConfigurationSessionIdentifier configGenId,
            Properties properties) {

        var computeRequest = new OrchestratorData.CreateComputeResourceRequest(
                ConcordClusterIdentifier.newBuilder().setLow(sessionId.getLow()).setHigh(sessionId.getHigh())
                        .setId(sessionId.toString())
                        .build(),
                nodeId,
                model,
                genesis,
                networkResourceEvent.getAddress(),
                configGenId,
                concordIdentifierMap.get(nodeId),
                bootstrapComponent.configService,
                bootstrapComponent.configServiceRest,
                properties.getValuesOrDefault(NodeProperty.Name.VM_PROFILE.toString(), "small")
        );

        return orchestrator.createDeployment(computeRequest);
    }


    /**
     * Create a {@link List} of {@link DeploymentSessionEvent}s based on a given {@link DeploymentSession} and the
     * additional corresponding {@link OrchestratorData.OrchestrationEvent} that the session engendered.
     *
     * @param session deployment session.
     * @param events  events engendered by the deployment session.
     * @return a listing of {@link DeploymentSessionEvent}s.
     */
    private List<DeploymentSessionEvent> toDeploymentSessionEvents(DeploymentSession session,
            Collection<OrchestratorData.OrchestrationEvent> events
    ) {
        var resources = ProvisioningServiceUtil.toProvisionedResources(session, events);
        log.info("List of provisioned resources {}", resources);

        var resourceEventStream = resources.stream()
                .map(resource -> ProvisioningServiceUtil.newResourceEvent(session.getId(), session.getStatus(),
                                                                          resource));

        // Generate Concord node models based on orchestration events.
        var nodes = ProvisioningServiceUtil.toConcordNodes(session, events);

        var clusterEvent = ProvisioningServiceUtil.newClusterDeploymentEvent(
                session.getId(), session.getStatus(),
                ConcordCluster.newBuilder().setId(session.getCluster())
                        .setInfo(ConcordClusterInfo.newBuilder().addAllMembers(nodes).build()).build());

        // Concatenate every event together.
        // (Existing events, all node events, cluster event, and completion event)
        var nodeEventStream = nodes.stream()
                .map(node -> ProvisioningServiceUtil.newNodeDeploymentEvent(session.getId(), session.getStatus(),
                                                                            node));
        var results = Stream
                .concat(
                        Stream.concat(
                                Stream.concat(session.getEventsList().stream(), resourceEventStream),
                                nodeEventStream
                        ),
                        Stream.of(
                                clusterEvent,
                                ProvisioningServiceUtil.newCompleteEvent(session.getId(),
                                                                         DeploymentSession.Status.SUCCESS)
                        )
                )
                .collect(Collectors.toList());
        eventQueue.addAll(results);
        return results;
    }
}