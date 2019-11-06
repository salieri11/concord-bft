/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/

package com.vmware.blockchain.deployment.service.provision;

import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
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
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.reactivestreams.Publisher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.vmware.blockchain.deployment.orchestration.NetworkAddress;
import com.vmware.blockchain.deployment.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.orchestration.Orchestrator.ComputeResourceEvent;
import com.vmware.blockchain.deployment.orchestration.Orchestrator.CreateComputeResourceRequest;
import com.vmware.blockchain.deployment.orchestration.Orchestrator.CreateNetworkAllocationRequest;
import com.vmware.blockchain.deployment.orchestration.Orchestrator.CreateNetworkResourceRequest;
import com.vmware.blockchain.deployment.orchestration.Orchestrator.NetworkAllocationEvent;
import com.vmware.blockchain.deployment.orchestration.Orchestrator.NetworkResourceEvent;
import com.vmware.blockchain.deployment.orchestration.Orchestrator.OrchestrationEvent;
import com.vmware.blockchain.deployment.orchestration.OrchestratorKt;
import com.vmware.blockchain.deployment.orchestration.OrchestratorProvider;
import com.vmware.blockchain.deployment.reactive.ReactiveStream;
import com.vmware.blockchain.deployment.service.grpc.support.EndpointsKt;
import com.vmware.blockchain.deployment.service.grpc.support.ExceptionsKt;
import com.vmware.blockchain.deployment.v1.ConcordCluster;
import com.vmware.blockchain.deployment.v1.ConcordClusterIdentifier;
import com.vmware.blockchain.deployment.v1.ConcordClusterInfo;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;
import com.vmware.blockchain.deployment.v1.ConcordNode;
import com.vmware.blockchain.deployment.v1.ConcordNodeEndpoint;
import com.vmware.blockchain.deployment.v1.ConcordNodeHostInfo;
import com.vmware.blockchain.deployment.v1.ConcordNodeIdentifier;
import com.vmware.blockchain.deployment.v1.ConcordNodeInfo;
import com.vmware.blockchain.deployment.v1.ConcordNodeStatus;
import com.vmware.blockchain.deployment.v1.ConfigurationServiceRequest;
import com.vmware.blockchain.deployment.v1.ConfigurationServiceStub;
import com.vmware.blockchain.deployment.v1.ConfigurationSessionIdentifier;
import com.vmware.blockchain.deployment.v1.CreateClusterRequest;
import com.vmware.blockchain.deployment.v1.DeploymentSession;
import com.vmware.blockchain.deployment.v1.DeploymentSessionEvent;
import com.vmware.blockchain.deployment.v1.DeploymentSessionIdentifier;
import com.vmware.blockchain.deployment.v1.DeploymentSpecification;
import com.vmware.blockchain.deployment.v1.Endpoint;
import com.vmware.blockchain.deployment.v1.MessageHeader;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteIdentifier;
import com.vmware.blockchain.deployment.v1.PlacementAssignment;
import com.vmware.blockchain.deployment.v1.PlacementSpecification;
import com.vmware.blockchain.deployment.v1.ProvisionedResource;
import com.vmware.blockchain.deployment.v1.ProvisioningServiceImplBase;
import com.vmware.blockchain.deployment.v1.StreamAllClusterDeploymentSessionEventRequest;
import com.vmware.blockchain.deployment.v1.StreamClusterDeploymentSessionEventRequest;
import com.vmware.blockchain.deployment.v1.UpdateDeploymentSessionRequest;
import com.vmware.blockchain.deployment.v1.UpdateDeploymentSessionResponse;
import com.vmware.blockchain.ethereum.type.Genesis;

import io.grpc.Status;
import io.grpc.stub.ServerCallStreamObserver;
import io.grpc.stub.StreamObserver;


/**
 * Implementation of ProvisioningService server.
 */
public class ProvisioningService extends ProvisioningServiceImplBase {

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
    private static final Logger log = LoggerFactory.getLogger(ProvisioningService.class);

    /** Atomic update for service instance state. */
    private static final AtomicReferenceFieldUpdater<ProvisioningService, State> STATE =
            AtomicReferenceFieldUpdater.newUpdater(ProvisioningService.class, State.class, "state");

    /** Executor to use for all async service operations. */
    private final ExecutorService executor;

    /** Orchestrator instance factory. */
    private final OrchestratorProvider orchestratorProvider;

    /** Service state. */
    private volatile State state = State.STOPPED;

    /** Configuration server endpoint. */
    private final Endpoint configurationService;

    /** Configuration service client stub provider function. */
    private final Function<Endpoint, ConfigurationServiceStub> configurationServiceClientProvider;

    /** Orchestrator pool to utilize for orchestration operations. */
    private final Map<OrchestrationSiteIdentifier, Orchestrator> orchestrators =
            new ConcurrentHashMap<>();

    /** FIXME: In-Memory stand-in/crutches to keep track of deployment log. */
    private final Map<DeploymentSessionIdentifier, CompletableFuture<DeploymentSession>> deploymentLog =
            new ConcurrentHashMap<>();

    /** list of all observers for StreamAllClusterDeploymentSessionEvents. */
    private static CopyOnWriteArrayList<StreamObserver<DeploymentSessionEvent>> eventsObserver =
            new CopyOnWriteArrayList<>();

    /** Queue for all deployment session events. */
    private static final ConcurrentLinkedQueue<DeploymentSessionEvent> eventQueue =
            new ConcurrentLinkedQueue<>();

    /** Background thread executor. */
    private static final ScheduledExecutorService backgroundExecutor =
            Executors.newSingleThreadScheduledExecutor();

    private static final Duration orchestratorCleanupDelay =
            Duration.of(OrchestratorKt.ORCHESTRATOR_LONG_TIMEOUT_MILLIS, ChronoUnit.MILLIS);

    /** Background task future. */
    private ScheduledFuture<?> eventEmitter;

    /** Configuration service client. */
    private ConfigurationServiceStub configurationServiceClient;

    /** Resolved configuration server endpoint. */
    private Endpoint resolvedConfigurationService;

    /** Default container registry endpoint to use. */
    private Endpoint containerRegistry;

    /** Default allocation server endpoint to use. */
    private Endpoint allocationServer;

    /** Default configuration server rest endpoint to use. */
    private Endpoint configurationServiceRest;

    /** Map of concord node identifiers vs concord identifier as provided. */
    // FIXME: This should not be required once concord gives a provision to define names
    private final Map<ConcordNodeIdentifier, Integer> concordIdentifierMap = new HashMap<>();

    /**
     * Constructor.
     */
    ProvisioningService(
            ExecutorService executor,
            OrchestratorProvider orchestratorProvider,
            Function<Endpoint, ConfigurationServiceStub> configurationServiceClientProvider,
            Endpoint configurationServer,
            Endpoint containerRegistry,
            Endpoint allocationServer,
            Endpoint configurationServiceRest
    ) {
        this.executor = executor;
        this.orchestratorProvider = orchestratorProvider;
        this.configurationServiceClientProvider = configurationServiceClientProvider;
        this.configurationService = configurationServer;
        this.containerRegistry = containerRegistry;
        this.allocationServer = allocationServer;
        this.configurationServiceRest = configurationServiceRest;
    }

    /**
     * Initialize the service instance asynchronously.
     *
     * @return
     *   {@link CompletableFuture} that completes when initialization is done.
     */
    CompletableFuture<Void> initialize() {
        if (STATE.compareAndSet(this, State.STOPPED, State.INITIALIZING)) {
            var initializeConfigurationServiceClient = CompletableFuture.runAsync(() -> {
                // Note: GrpcSupport is currently exported from orchestration-vmware library module.
                // This will need to be moved to a more common hosting library.
                resolvedConfigurationService =
                        EndpointsKt.resolveTransportSecurityServerSetting(configurationService);
                configurationServiceClient =
                        configurationServiceClientProvider.apply(resolvedConfigurationService);
            }, executor);

            // Continuous polling of events happen, given out to observers whenever available.
            eventEmitter = backgroundExecutor
                    .scheduleAtFixedRate(this::emitQueueEvents, 10, 1, TimeUnit.SECONDS);
            log.info("Background task scheduled to run at 1 second interval after 10sec of initial wait.");

            return CompletableFuture.allOf(
                    initializeConfigurationServiceClient
            ).thenRunAsync(() -> {
                /* Set instance to ACTIVE state.*/
                STATE.set(this, State.ACTIVE);

                log.info("Service instance initialized");
            }, executor);
        } else {
            return CompletableFuture.failedFuture(
                    new IllegalStateException("Service instance is not in stopped state")
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
                log.info("Service instance shutting down");

                // Shutdown the orchestrators.
                orchestrators.forEach((id, site) -> {
                    log.info("Shutting down orchestrator instance, id({})", id);

                    // Note: Assumption is that orchestrator.close() is fast and non-blocking.
                    // There may be a need for an awaitable-contract on Orchestrator interface.
                    site.close();
                });

                // background task shutdown
                log.info("Background tasks shutting down");
                eventEmitter.cancel(false);
                backgroundExecutor.shutdown();

                // Set instance to STOPPED state.
                STATE.set(this, State.STOPPED);
            }, executor);
        } else {
            return CompletableFuture.failedFuture(
                    new IllegalStateException("Service instance is not in active state")
            );
        }
    }

    @Override
    public void createCluster(
            CreateClusterRequest message,
            StreamObserver<DeploymentSessionIdentifier> observer
    ) {
        var request = Objects.requireNonNull(message);
        var response = Objects.requireNonNull(observer);

        try {
            // TODO: Verify number of placement entries should match cluster size.
            // placement.getEntries().size() == specification.getClusterSize()

            // Simplistic state checking. More sophisticated checks requires setting up a reference
            // while the request is not yet completed.
            if (STATE.get(this) != State.ACTIVE) {
                var text = "Service instance is not active";
                response.onError(newIllegalStateExceptionStatus(text).asException());
            } else {
                CompletableFuture.runAsync(() -> {
                    // Resolve / generate deployment session ID.
                    var sessionId = newSessionId(request.getHeader());
                    var clusterId = newClusterId();
                    var deploymentSpec = request.getSpecification();

                    // Generate node ID and affix the node placements.
                    var placements = resolvePlacement(deploymentSpec);

                    // Persist the deployment log.
                    var session = new DeploymentSession(
                            sessionId,
                            deploymentSpec,
                            clusterId,
                            placements,
                            DeploymentSession.Status.ACTIVE,
                            Collections.singletonList(newInitialEvent(sessionId))
                    );
                    var oldSession = deploymentLog.putIfAbsent(sessionId, new CompletableFuture<>());

                    // If successfully recorded.
                    if (oldSession == null) {
                        // Emit the acknowledgement and signal completion of the request.
                        response.onNext(sessionId);
                        response.onCompleted();

                        // Start the async workflow to carry out the deployment plan.
                        // FIXME: Convert this work flow into proper multi-stage task execution that
                        //   takes clustered service instances into account.
                        deployCluster(session);
                    } else {
                        // Cannot record this session for some reason.
                        // TODO: Need to think more about what to return in this case.
                        var text = "Cannot record deployment session";
                        response.onError(newIllegalStateExceptionStatus(text).asException());
                    }
                }, executor).exceptionally(error -> {
                    response.onError(ExceptionsKt.toStatus(error).asException());
                    return null; // To satisfy type signature (Void).
                });
            }
        } catch (Throwable error) {
            response.onError(ExceptionsKt.toStatus(error).asException());
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

        try {
            // Simplistic state checking. More sophisticated checks requires setting up a reference
            // while the request is not yet completed.
            if (STATE.get(this) != State.ACTIVE) {
                var text = "Service instance is not active";
                response.onError(newIllegalStateExceptionStatus(text).asException());
            } else {
                CompletableFuture.runAsync(() -> {
                    // FIXME: Right now the logic only works correctly for single service instance.
                    Consumer<DeploymentSession> sender = session -> {
                        // Send all events.
                        for (DeploymentSessionEvent event : session.getEvents()) {
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
                        task.thenAcceptAsync(sender, executor);
                    } else {
                        var text = "Session does not have a background task";
                        response.onError(newIllegalStateExceptionStatus(text).asException());
                    }
                }, executor).exceptionally(error -> {
                    response.onError(ExceptionsKt.toStatus(error).asException());
                    return null; // To satisfy type signature (Void).
                });
            }
        } catch (Throwable error) {
            response.onError(ExceptionsKt.toStatus(error).asException());
        }
    }


    @Override
    public void updateDeploymentSession(UpdateDeploymentSessionRequest message,
                                        StreamObserver<UpdateDeploymentSessionResponse> observer) {
        var request = Objects.requireNonNull(message);
        var response = Objects.requireNonNull(observer);

        try {
            if (STATE.get(this) != State.ACTIVE) {
                var text = "Service instance is not active";
                response.onError(newIllegalStateExceptionStatus(text).asException());
            } else {
                CompletableFuture.runAsync(() -> {
                    if (request.getAction().equals(UpdateDeploymentSessionRequest.Action.NOOP)) {
                        log.info("No action was provided. No action was taken.");
                        response.onNext(new UpdateDeploymentSessionResponse());
                        response.onCompleted();
                    } else if (request.getAction().equals(UpdateDeploymentSessionRequest.Action.DEPROVISION_ALL)) {
                        deprovision(request.getSession());
                        response.onNext(new UpdateDeploymentSessionResponse());
                        response.onCompleted();
                    }
                }, executor).exceptionally(error -> {
                    response.onError(ExceptionsKt.toStatus(error).asException());
                    return null; // To satisfy type signature (Void).
                });
            }
        } catch (Throwable error) {
            response.onError(ExceptionsKt.toStatus(error).asException());
        }

    }

    /**
     * Create a new {@link Status} corresponding to a new instance of {@link IllegalStateException}
     * with the supplied exception message.
     *
     * @param message
     *   message to be included in the embedded exception.
     *
     * @return
     *   a new instance of {@link Status#FAILED_PRECONDITION} with an associated exception instance.
     */
    private Status newIllegalStateExceptionStatus(String message) {
        return ExceptionsKt.toStatus(new IllegalStateException(message));
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
     * Create a placement resolution for a given {@link DeploymentSpecification} such that all
     * placement entries have a specific designated orchestration site target.
     *
     * @param specification
     *   input placement specification.
     *
     * @return
     *   a new instance of {@link PlacementAssignment} that contains mappings of
     *   {@link ConcordNodeIdentifier} to the {@link OrchestrationSiteIdentifier} corresponding to
     *   the target deployment site for that node.
     */
    private PlacementAssignment resolvePlacement(DeploymentSpecification specification) {
        var availableSites = orchestrators.keySet().toArray(OrchestrationSiteIdentifier[]::new);
        var random = new Random();
        var assignments = specification.getPlacement().getEntries().stream()
                .map(entry -> {
                    var uuid = UUID.randomUUID();
                    var nodeId = new ConcordNodeIdentifier(
                            uuid.getLeastSignificantBits(),
                            uuid.getMostSignificantBits()
                    );

                    OrchestrationSiteIdentifier site;
                    if (entry.getType() == PlacementSpecification.Type.FIXED) {
                        site = entry.getSite();
                    } else {
                        site = availableSites[random.nextInt(availableSites.length)];
                    }
                    return new PlacementAssignment.Entry(nodeId, site, entry.getSiteInfo());
                })
                .collect(Collectors.toUnmodifiableList());

        return new PlacementAssignment(assignments);
    }

    private CompletableFuture<ConfigurationSessionIdentifier> generateConfigurationId(
            ConcurrentHashMap<PlacementAssignment.Entry, NetworkResourceEvent.Created> privateNetworkAddressMap,
            ConcordModelSpecification.BlockchainType blockchainType,
            Genesis genesis) {
        List<String> nodeIps = new ArrayList<>();
        privateNetworkAddressMap.forEach((key, value) -> {
            var nodeIp = value.getAddress();
            nodeIps.add(nodeIp);
            concordIdentifierMap.put(key.getNode(), nodeIps.indexOf(nodeIp));
        });
        var request = new ConfigurationServiceRequest(
                new MessageHeader(),
                nodeIps, blockchainType,
                genesis);

        var promise = new CompletableFuture<ConfigurationSessionIdentifier>();
        configurationServiceClient.createConfiguration(request, newResultObserver(promise));
        return promise;
    }

    /**
     * Execute a Concord cluster deprovisioning workflow.
     *
     * @param requestSession
     *   deploymentSessionIdentifier to carry out the workflow for.
     */
    private void deprovision(DeploymentSessionIdentifier requestSession) {

        CompletableFuture<DeploymentSession> sessionEventTask = deploymentLog.get(requestSession);
        if (sessionEventTask != null) {
            sessionEventTask.thenAcceptAsync(deploymentSession -> {
                final var deleteEvents = deleteResourceEvents(deploymentSession.getEvents());
                var deprovEv = toDeprovisioningEvents(deploymentSession, deleteEvents,
                        DeploymentSession.Status.SUCCESS);
                deploymentSession.getEvents().addAll(deprovEv);
                deploymentLog.get(requestSession).complete(deploymentSession);
                log.info("Deprovisioning completed");
            }, executor)
                    .exceptionally(error -> {
                        throw new IllegalStateException("Deprovisioning failed for id: " + requestSession, error);
                    });
            sessionEventTask.join();
        } else {
            throw new IllegalStateException("Session does not have a background task");
        }
    }

    private ConcurrentHashMap.KeySetView<OrchestrationEvent, Boolean> deleteResourceEvents(
            List<DeploymentSessionEvent> events
    ) {
        final var deleteEvents = ConcurrentHashMap.<OrchestrationEvent>newKeySet();

        List<Map.Entry<Orchestrator, URI>> networkAllocList = new ArrayList<>();
        List<Map.Entry<Orchestrator, URI>> computeList = new ArrayList<>();
        List<Map.Entry<Orchestrator, URI>> networkAddrList = new ArrayList<>();

        for (DeploymentSessionEvent event : events) {
            if (!event.getType().equals(DeploymentSessionEvent.Type.RESOURCE)) {
                continue;
            }

            URI resourceUri = getUrl(event.getResource().getName());

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
            Collection<OrchestrationEvent> events,
            DeploymentSession.Status status
    ) {
        List<DeploymentSessionEvent> deprovisioningEvent = new ArrayList<>();
        events.forEach(event -> {
            ProvisionedResource resource = null;
            if (event instanceof NetworkAllocationEvent.Deleted) {
                var resEvent = (NetworkAllocationEvent.Deleted) event;
                var type = ProvisionedResource.Type.NETWORK_ALLOCATION;
                resource = new ProvisionedResource(type,
                        resEvent.getResource().toString(),
                        OrchestrationSiteIdentifier.Companion.getDefaultValue(),
                        session.getCluster(),
                        ConcordNodeIdentifier.Companion.getDefaultValue());
            } else if (event instanceof NetworkResourceEvent.Deleted) {
                var resEvent = (NetworkResourceEvent.Deleted) event;
                resource = new ProvisionedResource(ProvisionedResource.Type.NETWORK_RESOURCE,
                        resEvent.getResource().toString(),
                        OrchestrationSiteIdentifier.Companion.getDefaultValue(),
                        session.getCluster(),
                        ConcordNodeIdentifier.Companion.getDefaultValue());
            } else if (event instanceof ComputeResourceEvent.Deleted) {
                var resEvent = (ComputeResourceEvent.Deleted) event;
                resource = new ProvisionedResource(ProvisionedResource.Type.COMPUTE_RESOURCE,
                        resEvent.getResource().toString(),
                        OrchestrationSiteIdentifier.Companion.getDefaultValue(),
                        session.getCluster(),
                        ConcordNodeIdentifier.Companion.getDefaultValue());
            }

            if (resource != null) {
                deprovisioningEvent.add(
                        new DeploymentSessionEvent(
                                DeploymentSessionEvent.Type.RESOURCE_DEPROVISIONING,
                                session.getId(), status, resource,
                                ConcordNode.Companion.getDefaultValue(),
                                ConcordNodeStatus.Companion.getDefaultValue(),
                                ConcordCluster.Companion.getDefaultValue())
                );
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
     * @param session
     *   deployment session to carry out the workflow for.
     */
    private void deployCluster(DeploymentSession session) {
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
            final var results = ConcurrentHashMap.<OrchestrationEvent>newKeySet();

            var sessionOrchestrators = session.getAssignment().getEntries().stream()
                    .map(entry ->
                        Map.entry(
                                entry.getSite(),
                                orchestratorProvider.newOrchestrator(
                                        OrchestrationSites.Companion.buildSiteInfo(
                                                entry.getSiteInfo(),
                                                containerRegistry,
                                                allocationServer
                                        )
                                )
                        )
                    )
                    .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue, (v1, v2) -> v1));
            var orchestratorInitializations = sessionOrchestrators.values().stream()
                    .map(orchestrator -> ReactiveStream.toFuture(orchestrator.initialize()))
                    .toArray(CompletableFuture[]::new);

            // FIXME: This blocks calling thread until all sites for the session is initialized.
            CompletableFuture.allOf(orchestratorInitializations).join();

            // Construct the context for this session execution.
            var context = new DeploymentSessionContext(sessionOrchestrators);

            // Allocate public network addresses for every node.
            var networkAddressPublishers = session.getAssignment().getEntries().stream()
                    .map(entry -> {
                        var orchestrator = context.getOrchestrators().get(entry.getSite());

                        var node = entry.getNode();
                        var resource = toResourceName(node);
                        var addressRequest = new CreateNetworkResourceRequest(resource, true);
                        var addressPublisher = orchestrator.createNetworkAddress(addressRequest);

                        return Map.entry(entry, addressPublisher);
                    })
                    .collect(Collectors.toUnmodifiableList());

            // Await on all the network address creations, and collect the result.
            // Subscribe to network address publishers and collect the results in a map asynchronously.
            // FIXME: This is working around current system limitation working without local node agent.
            var publicNetworkAddressMap =
                    new ConcurrentHashMap<PlacementAssignment.Entry, NetworkResourceEvent.Created>();
            var privateNetworkAddressMap =
                    new ConcurrentHashMap<PlacementAssignment.Entry, NetworkResourceEvent.Created>();

            var networkAddressPromises = networkAddressPublishers.stream()
                    .map(entry -> ReactiveStream.toFuture(entry.getValue(), ArrayList::new)
                            .thenAcceptAsync(events -> {
                                // Put the events in the result collection.
                                results.addAll(events);

                                for (var event : events) {
                                    // Cast to creation event, let runtime cast throw exception if
                                    // cast fails, as then it will exceptionally trigger failure future
                                    // completion downstream.
                                    var createdEvent = (NetworkResourceEvent.Created) event;

                                    // Place in the address lookup map by the placement entry key.
                                    if (createdEvent.getPublic()) {
                                        publicNetworkAddressMap.put(entry.getKey(), createdEvent);
                                    } else {
                                        privateNetworkAddressMap.put(entry.getKey(), createdEvent);
                                    }
                                }
                            }, executor)
                    )
                    .toArray(CompletableFuture[]::new);

            CompletableFuture.allOf(networkAddressPromises)
                    .thenComposeAsync(__ -> generateConfigurationId(
                            privateNetworkAddressMap,
                            session.getSpecification().getModel().getBlockchainType(),
                            session.getSpecification().getGenesis()), executor
                    )
                    // Setup node deployment workflow with its assigned network address.
                    .thenComposeAsync(configGenId -> {
                        var model = session.getSpecification().getModel();
                        var nodePublishers = session.getAssignment().getEntries().stream()
                                .map(placement -> {
                                    var publisher = deployNode(
                                            context.getOrchestrators().get(placement.getSite()),
                                            session.getId(),
                                            placement.getNode(),
                                            model,
                                            session.getSpecification().getGenesis(),
                                            privateNetworkAddressMap.get(placement),
                                            configGenId
                                    );

                                    return Map.entry(placement, publisher);
                                })
                                .collect(Collectors.toUnmodifiableList());

                        var nodePromises = nodePublishers.stream()
                                .map(entry -> ReactiveStream.toFuture(entry.getValue(), ArrayList::new)
                                        .thenComposeAsync(events -> {
                                            // Put the events in the result collection.
                                            results.addAll(events);

                                            // Find any compute resource event and extract the URI.
                                            var computeResource = events.stream()
                                                    .filter(ComputeResourceEvent.class::isInstance)
                                                    .map(ComputeResourceEvent.class::cast)
                                                    .map(ComputeResourceEvent::getResource)
                                                    .findFirst()
                                                    .orElseThrow(() -> new IllegalStateException(
                                                            "No compute resource event")
                                                    );

                                            // Allocate network address to the created node.
                                            var placement = entry.getKey();
                                            var orchestrator = context.getOrchestrators().get(placement.getSite());

                                            if (publicNetworkAddressMap.containsKey(placement)) {
                                                var publicNetworkResource = publicNetworkAddressMap
                                                        .get(entry.getKey()).getResource();

                                                var privateNetworkResource = privateNetworkAddressMap
                                                        .get(entry.getKey()).getResource();
                                                var resource = toResourceName(placement.getNode());
                                                var allocationRequest = new CreateNetworkAllocationRequest(
                                                        resource,
                                                        computeResource,
                                                        publicNetworkResource,
                                                        privateNetworkResource
                                                );
                                                var allocationPublisher = orchestrator
                                                        .createNetworkAllocation(allocationRequest);

                                                return ReactiveStream.toFuture(allocationPublisher);
                                            } else {
                                                return CompletableFuture.completedFuture(null);
                                            }
                                        }, executor)
                                        // Put the network allocation event in the result collection.
                                        .whenComplete((event, error) -> {
                                            if (error != null) {
                                                log.error("Failed to deploy node({})",
                                                          entry.getKey(), error);
                                            } else {
                                                if (event != null) {
                                                    results.add(event);
                                                }
                                            }
                                        })
                                )
                                .toArray(CompletableFuture[]::new);

                        return CompletableFuture.allOf(nodePromises);
                    }, executor)
                    .thenRunAsync(() -> {
                        // Create the updated deployment session instance.
                        var updatedSession = new DeploymentSession(
                                session.getId(),
                                session.getSpecification(),
                                session.getCluster(),
                                session.getAssignment(),
                                DeploymentSession.Status.SUCCESS,
                                toDeploymentSessionEvents(session, results)
                        );

                        // Update the deployment log.
                        // FIXME: This does not take into account of persistence nor retry.
                        deploymentLog.get(session.getId()).complete(updatedSession);

                        log.info("Deployment session({}) completed", session.getId());
                    }, executor)
                    .whenCompleteAsync((result, error) -> {
                        if (error == null) {
                            sessionOrchestrators.forEach((site, orchestrator) -> {
                                var existing = orchestrators.put(site, orchestrator);
                                if (existing != null) {
                                    // Close evicted orchestrator after some amount of "grace" time.
                                    backgroundExecutor.schedule(
                                            existing::close,
                                            orchestratorCleanupDelay.toMillis(),
                                            TimeUnit.MILLISECONDS
                                    );
                                }
                                // Close any session orchestrator that is no longer tracked.
                                if (!orchestrators.containsValue(orchestrator)) {
                                    orchestrator.close();
                                }
                            });
                        }
                    }, executor)
                    .exceptionally(error -> {
                        log.info("Deployment session({}) failed", session.getId(), error);

                        var event = newCompleteEvent(session.getId(), DeploymentSession.Status.FAILURE);
                        var updatedSession = new DeploymentSession(
                                session.getId(),
                                session.getSpecification(),
                                session.getCluster(),
                                session.getAssignment(),
                                DeploymentSession.Status.FAILURE,
                                Stream.concat(session.getEvents().stream(), Stream.of(event))
                                        .collect(Collectors.toList())
                        );

                        log.info("Deployment session({}) failed", session.getId(), error);

                        // Update the deployment log.
                        // FIXME: This does not take into account of persistence nor retry.
                        deploymentLog.get(session.getId()).complete(updatedSession);

                        // Create the updated deployment session instance.
                        var deleteEvents = deleteResourceEvents(toDeploymentSessionEvents(session, results));

                        var deprovisioningSessionEvents = toDeprovisioningEvents(
                                session, deleteEvents, DeploymentSession.Status.FAILURE);

                        session.getEvents().addAll(deprovisioningSessionEvents);

                        log.info("Deployment session({}) cleaned", session.getId(), error);
                        return null; // To satisfy type signature (Void).

                    });
        } catch (Throwable error) {
            log.error("Error triggering work on the session", error);

            deploymentLog.get(session.getId()).complete(new DeploymentSession(
                    session.getId(),
                    session.getSpecification(),
                    session.getCluster(),
                    session.getAssignment(),
                    DeploymentSession.Status.FAILURE,
                    Collections.singletonList(new DeploymentSessionEvent(
                            DeploymentSessionEvent.Type.COMPLETED,
                            session.getId(),
                            DeploymentSession.Status.FAILURE,
                            ProvisionedResource.Companion.getDefaultValue(),
                            ConcordNode.Companion.getDefaultValue(),
                            ConcordNodeStatus.Companion.getDefaultValue(),
                            ConcordCluster.Companion.getDefaultValue()
                    ))));
        }
    }

    /**
     * Execute a Concord node deployment workflow.
     *
     * @param orchestrator
     *   orchestrator to use to execute the node deployment workflow.
     * @param sessionId
     *   overall deployment session this node deployment workflow belongs to.
     * @param nodeId
     *   identifier of the Concord node to deploy.
     * @param model
     *   model to use to setup the node.
     *
     * @return
     *   a {@link Publisher} of {@link OrchestrationEvent}s corresponding to the execution
     *   of the node deployment workflow.
     */
    private Publisher<? extends OrchestrationEvent> deployNode(
            Orchestrator orchestrator,
            DeploymentSessionIdentifier sessionId,
            ConcordNodeIdentifier nodeId,
            ConcordModelSpecification model,
            Genesis genesis,
            NetworkResourceEvent.Created networkResourceEvent,
            ConfigurationSessionIdentifier configGenId
    ) {
        var computeRequest = new CreateComputeResourceRequest(
                new ConcordClusterIdentifier(sessionId.getLow(), sessionId.getHigh()),
                nodeId,
                model,
                genesis,
                networkResourceEvent.getAddress(),
                configGenId,
                concordIdentifierMap.get(nodeId),
                resolvedConfigurationService,
                configurationServiceRest
        );
        return orchestrator.createDeployment(computeRequest);
    }

    /**
     * Generate a new {@link DeploymentSessionIdentifier} based on a given request
     * {@link MessageHeader}, or generate a random value as ID if request does not contain
     * sufficient parametric data.
     *
     * @param requestHeader
     *   header of the deployment request message.
     *
     * @return
     *   a corresponding instance of the resulting deployment session ID.
     */
    private static DeploymentSessionIdentifier newSessionId(MessageHeader requestHeader) {
        var id = requestHeader.getId();
        UUID uuid;
        if (id.isEmpty() || id.isBlank()) {
            uuid = UUID.randomUUID();
        } else {
            // Hash the string value into an UUID.
            uuid = UUID.nameUUIDFromBytes(id.getBytes(StandardCharsets.UTF_8));
        }

        return new DeploymentSessionIdentifier(
                uuid.getLeastSignificantBits(),
                uuid.getMostSignificantBits()
        );
    }

    /**
     * Generate a new {@link ConcordClusterIdentifier}.
     *
     * @return
     *   a new {@link ConcordClusterIdentifier} instance.
     */
    private static ConcordClusterIdentifier newClusterId() {
        var uuid = UUID.randomUUID();

        return new ConcordClusterIdentifier(
                uuid.getLeastSignificantBits(),
                uuid.getMostSignificantBits()
        );
    }

    /**
     * Create an instance of {@link DeploymentSessionEvent} denoting the initial event of a
     * {@link DeploymentSession} associated with a given {@link DeploymentSessionIdentifier}.
     *
     * @param sessionId
     *   identifier of the deployment session to create the session event for.
     *
     * @return
     *   a new instance of {@link DeploymentSessionEvent}.
     */
    private static DeploymentSessionEvent newInitialEvent(DeploymentSessionIdentifier sessionId) {
        return new DeploymentSessionEvent(
                DeploymentSessionEvent.Type.ACKNOWLEDGED,
                sessionId,
                DeploymentSession.Status.ACTIVE,
                ProvisionedResource.Companion.getDefaultValue(),
                ConcordNode.Companion.getDefaultValue(),
                ConcordNodeStatus.Companion.getDefaultValue(),
                ConcordCluster.Companion.getDefaultValue()
        );
    }

    /**
     * Create an instance of {@link DeploymentSessionEvent} denoting the completion event of a
     * {@link DeploymentSession} associated with a given {@link DeploymentSessionIdentifier}.
     *
     * @param sessionId
     *   identifier of the deployment session to create the session event for.
     * @param status
     *   completion status.
     *
     * @return
     *   a new instance of {@link DeploymentSessionEvent}.
     */
    private static DeploymentSessionEvent newCompleteEvent(
            DeploymentSessionIdentifier sessionId,
            DeploymentSession.Status status
    ) {
        return new DeploymentSessionEvent(
                DeploymentSessionEvent.Type.COMPLETED,
                sessionId,
                status,
                ProvisionedResource.Companion.getDefaultValue(),
                ConcordNode.Companion.getDefaultValue(),
                ConcordNodeStatus.Companion.getDefaultValue(),
                ConcordCluster.Companion.getDefaultValue()
        );
    }

    /**
     * Create an instance of {@link DeploymentSessionEvent} denoting the cluster deployment event of
     * a {@link DeploymentSession} associated with a given {@link DeploymentSessionIdentifier}.
     *
     * @param sessionId
     *   identifier of the deployment session to create the session event for.
     * @param status
     *   completion status.
     * @param resource
     *   data payload to set for the session event.
     *
     * @return
     *   a new instance of {@link DeploymentSessionEvent}.
     */
    private static DeploymentSessionEvent newResourceEvent(
            DeploymentSessionIdentifier sessionId,
            DeploymentSession.Status status,
            ProvisionedResource resource
    ) {
        return new DeploymentSessionEvent(
                DeploymentSessionEvent.Type.RESOURCE,
                sessionId,
                status,
                resource,
                ConcordNode.Companion.getDefaultValue(),
                ConcordNodeStatus.Companion.getDefaultValue(),
                ConcordCluster.Companion.getDefaultValue()
        );
    }

    /**
     * Create an instance of {@link DeploymentSessionEvent} denoting the cluster deployment event of
     * a {@link DeploymentSession} associated with a given {@link DeploymentSessionIdentifier}.
     *
     * @param sessionId
     *   identifier of the deployment session to create the session event for.
     * @param status
     *   completion status.
     * @param node
     *   data payload to set for the session event.
     *
     * @return
     *   a new instance of {@link DeploymentSessionEvent}.
     */
    private static DeploymentSessionEvent newNodeDeploymentEvent(
            DeploymentSessionIdentifier sessionId,
            DeploymentSession.Status status,
            ConcordNode node
    ) {
        return new DeploymentSessionEvent(
                DeploymentSessionEvent.Type.NODE_DEPLOYED,
                sessionId,
                status,
                ProvisionedResource.Companion.getDefaultValue(),
                node,
                ConcordNodeStatus.Companion.getDefaultValue(),
                ConcordCluster.Companion.getDefaultValue()
        );
    }

    /**
     * Create an instance of {@link DeploymentSessionEvent} denoting the cluster deployment event of
     * a {@link DeploymentSession} associated with a given {@link DeploymentSessionIdentifier}.
     *
     * @param sessionId
     *   identifier of the deployment session to create the session event for.
     * @param status
     *   completion status.
     * @param cluster
     *   data payload to set for the session event.
     *
     * @return
     *   a new instance of {@link DeploymentSessionEvent}.
     */
    private static DeploymentSessionEvent newClusterDeploymentEvent(
            DeploymentSessionIdentifier sessionId,
            DeploymentSession.Status status,
            ConcordCluster cluster
    ) {
        return new DeploymentSessionEvent(
                DeploymentSessionEvent.Type.CLUSTER_DEPLOYED,
                sessionId,
                status,
                ProvisionedResource.Companion.getDefaultValue(),
                ConcordNode.Companion.getDefaultValue(),
                ConcordNodeStatus.Companion.getDefaultValue(),
                cluster
        );
    }

    /**
     * Convert the {@link ConcordNodeIdentifier} to its canonical resource name.
     *
     * @param identifier
     *   identifier to convert into resource name.
     *
     * @return
     *   resource name as a {@link String}.
     */
    private static String toResourceName(ConcordNodeIdentifier identifier) {
        return new UUID(identifier.getHigh(), identifier.getLow()).toString();
    }

    /**
     * Convert a canonical resource name to a {@link ConcordNodeIdentifier}.
     *
     * @param name
     *   resource name to convert into identifier.
     *
     * @return
     *   identifier as a {@link ConcordNodeIdentifier}.
     */
    private static ConcordNodeIdentifier toNodeIdentifier(String name) {
        var uuid = UUID.fromString(name);
        return new ConcordNodeIdentifier(
                uuid.getLeastSignificantBits(),
                uuid.getMostSignificantBits()
        );
    }


    /**
     * Create a new {@link ConcordNodeHostInfo} instance based on a
     * {@link ComputeResourceEvent.Created} event, using additional input as context information for
     * instance creation.
     *
     * @param event
     *   event signaled for the concord node.
     * @param placementEntryByNodeName
     *   mappings of concord node resource name to its associated {@link PlacementAssignment.Entry}.
     *
     * @return
     *   a new instance of {@link ConcordNodeHostInfo} with relevant properties filled in with
     *   non-default values.
     */
    private static ConcordNodeHostInfo toConcordNodeHostInfo(
            ComputeResourceEvent.Created event,
            Map<String, PlacementAssignment.Entry> placementEntryByNodeName
    ) {
        return new ConcordNodeHostInfo(
                placementEntryByNodeName.get(toResourceName(event.getNode())).getSite(),
                Collections.emptyMap(),
                Collections.emptyMap()
        );
    }

    /**
     * Create a new {@link ConcordNodeHostInfo} instance based on a
     * {@link NetworkResourceEvent.Created} event, using additional input as context information
     * for instance creation.
     *
     * @param publicNetworkEvent
     *   event signaled for the concord node.
     * @param privateNetworkEvent
     *   event signaled for the concord node.
     * @param placementEntryByNodeName
     *   mappings of concord node resource name to its associated {@link PlacementAssignment.Entry}.
     *
     * @return
     *   a new instance of {@link ConcordNodeHostInfo} with relevant properties filled in with
     *   non-default values.
     */
    private static ConcordNodeHostInfo toConcordNodeHostInfo(
            NetworkResourceEvent.Created publicNetworkEvent,
            NetworkResourceEvent.Created privateNetworkEvent,
            Map<String, PlacementAssignment.Entry> placementEntryByNodeName,
            ConcordModelSpecification.BlockchainType blockchainType
    ) {
        // FIXME: Ideally the API service names must be defined somewhere rather than
        //   "fabricated out of thin air" here. A logical place would be ConcordModelSpecification
        //   itself, which defines the API endpoint URI minus the authority portion, and merged with
        //   the public IP addresses, which is known by this point.
        // FIXME (Followup): ConcordComponent.ServiceType is now available to use and properly
        //   defines an API enum constant for this purpose. But in order to switch over, need to
        //   make sure that all clients are not making use of the name beyond just an opaque String
        //   literal, and has not yet persisted the data in persistent storage.
        //   The exact information here must make use of the [ConcordComponent]s declared in the
        //   deployment specification. This can possibly be captured by an enum based on
        //   [ServiceType].

        Map<String, ConcordNodeEndpoint> endpoints;
        switch (blockchainType) {
            case DAML:
                endpoints = Map.of(
                        "daml-ledger-api",
                        new ConcordNodeEndpoint(
                                URI.create("https://{{ip}}:6865"
                                                   .replace("{{ip}}", publicNetworkEvent.getAddress()))
                                        .toString(),
                                ""
                        )
                );
                break;
            case HLF:
                endpoints = Map.of(
                        "concord-hlf",
                        new ConcordNodeEndpoint(
                                URI.create("https://{{ip}}:50051"
                                                   .replace("{{ip}}", publicNetworkEvent.getAddress()))
                                        .toString(),
                                ""
                        )
                );
                break;

            case ETHEREUM:
            default:
                endpoints = Map.of(
                        "ethereum-rpc",
                        new ConcordNodeEndpoint(
                                URI.create("https://{{ip}}:8545"
                                                   .replace("{{ip}}", publicNetworkEvent.getAddress()))
                                        .toString(),
                                ""
                        )
                );
        }



        return new ConcordNodeHostInfo(
                placementEntryByNodeName.get(publicNetworkEvent.getName()).getSite(),
                Map.of(
                        NetworkAddress.toIPv4Address(publicNetworkEvent.getAddress()),
                        NetworkAddress.toIPv4Address(privateNetworkEvent.getAddress())
                ),
                endpoints
        );
    }

    /**
     * Create a new {@link ConcordNode} instance based on a {@link ComputeResourceEvent.Created}
     * event, using additional input as context information for instance creation.
     *
     * @param event
     *   event signaled for the concord node.
     * @param placementEntryByNodeName
     *   mappings of concord node resource name to its associated {@link PlacementAssignment.Entry}.
     *
     * @return
     *   a new instance of {@link ConcordNode} with relevant properties filled in with
     *   non-default values.
     */
    private static ConcordNode toConcordNode(
            ComputeResourceEvent.Created event,
            Map<String, PlacementAssignment.Entry> placementEntryByNodeName,
            ConcordModelSpecification.BlockchainType blockchainType
    ) {
        return new ConcordNode(
                event.getNode(),
                ConcordNodeInfos.Companion.toConcordNode(blockchainType),
                toConcordNodeHostInfo(event, placementEntryByNodeName)
        );
    }

    /**
     * Create a new {@link ConcordNode} instance based on a {@link NetworkResourceEvent.Created}
     * event, using additional input as context information for instance creation.
     *
     * @param publicNetworkEvent
     *   event signaled for the concord node.
     * @param privateNetworkEvent
     *   event signaled for the concord node.
     * @param placementEntryByNodeName
     *   mappings of concord node resource name to its associated {@link PlacementAssignment.Entry}.
     *
     * @return
     *   a new instance of {@link ConcordNode} with relevant properties filled in with
     *   non-default values.
     */
    private static ConcordNode toConcordNode(
            NetworkResourceEvent.Created publicNetworkEvent,
            NetworkResourceEvent.Created privateNetworkEvent,
            Map<String, PlacementAssignment.Entry> placementEntryByNodeName,
            ConcordModelSpecification.BlockchainType blockchainType
    ) {
        return new ConcordNode(
                placementEntryByNodeName.get(publicNetworkEvent.getName()).getNode(),
                ConcordNodeInfos.Companion.toConcordNode(blockchainType),
                toConcordNodeHostInfo(
                        publicNetworkEvent,
                        privateNetworkEvent,
                        placementEntryByNodeName,
                        blockchainType
                )
        );
    }

    /**
     * Merge two {@link ConcordNodeInfo} instances, favoring to preserve content from the first
     * instance whenever conflict arise.
     *
     * @param first
     *   first instance to be merged.
     * @param second
     *   second instance to be merged.
     *
     * @return
     *   a new {@link ConcordNodeInfo} instance combining content from input sources.
     */
    private static ConcordNodeInfo merge(ConcordNodeInfo first, ConcordNodeInfo second) {
        return new ConcordNodeInfo(
                first.getModel(), // Preserve existing value.
                Stream.concat(
                        first.getIpv4Addresses().entrySet().stream(),
                        second.getIpv4Addresses().entrySet().stream()
                ).collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                    (oldEntry, newEntry) -> oldEntry // Preserve existing value.
                )),
                first.getBlockchainType()
        );
    }

    /**
     * Merge two {@link ConcordNodeHostInfo} instances, favoring to preserve content from the first
     * instance whenever conflict arise.
     *
     * @param first
     *   first instance to be merged.
     * @param second
     *   second instance to be merged.
     *
     * @return
     *   a new {@link ConcordNodeHostInfo} instance combining content from input sources.
     */
    private static ConcordNodeHostInfo merge(ConcordNodeHostInfo first, ConcordNodeHostInfo second) {
        return new ConcordNodeHostInfo(
                first.getSite(), // Preserve existing value.
                Stream.concat(
                        first.getIpv4AddressMap().entrySet().stream(),
                        second.getIpv4AddressMap().entrySet().stream()
                ).collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                    (oldEntry, newEntry) -> oldEntry // Preserve existing value.
                )),
                Stream.concat(
                        first.getEndpoints().entrySet().stream(),
                        second.getEndpoints().entrySet().stream()
                ).collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                    (oldEntry, newEntry) -> oldEntry // Preserve existing value.
                ))
        );
    }

    /**
     * Merge two {@link ConcordNode} instances, favoring to preserve content from the first instance
     * whenever conflict arise.
     *
     * @param first
     *   first instance to be merged.
     * @param second
     *   second instance to be merged.
     *
     * @return
     *   a new {@link ConcordNode} instance combining content from input sources.
     */
    private static ConcordNode merge(ConcordNode first, ConcordNode second) {
        return new ConcordNode(
                first.getId(),
                merge(first.getInfo(), second.getInfo()),
                merge(first.getHostInfo(), second.getHostInfo())
        );
    }

    /**
     * Create a {@link List} of {@link ConcordNode} information instances based on a given
     * {@link DeploymentSession} and the corresponding {@link OrchestrationEvent} that the session
     * engenders.
     *
     * @param session
     *   deployment session.
     * @param events
     *   events engendered by the deployment session.
     *
     * @return
     *   a listing of {@link ConcordNode} instances.
     */
    private static List<ConcordNode> toConcordNodes(
            DeploymentSession session,
            Collection<OrchestrationEvent> events
    ) {
        var placementEntryByNodeName = session.getAssignment().getEntries().stream()
                .map(entry -> Map.entry(toResourceName(entry.getNode()), entry))
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
        var computeResourceEvents = new HashMap<URI, ComputeResourceEvent.Created>();
        var networkResourceEvents = new HashMap<URI, NetworkResourceEvent.Created>();
        var networkAllocationEvents = new HashMap<URI, NetworkAllocationEvent.Created>();
        var privateNetworkResourceByNodeName = new HashMap<String, NetworkResourceEvent.Created>();

        // Partition respective orchestration events into separate groups of reverse-mapping by URI.
        for (OrchestrationEvent event : events) {
            if (event instanceof ComputeResourceEvent.Created) {
                var resourceEvent = (ComputeResourceEvent.Created) event;

                computeResourceEvents.put(resourceEvent.getResource(), resourceEvent);
            } else if (event instanceof NetworkResourceEvent.Created) {
                var resourceEvent = (NetworkResourceEvent.Created) event;

                // FIXME: For the purpose of calculating deployment events pertaining to network
                //   resources, only public network addresses need to be considered for event info.
                if (resourceEvent.getPublic()) {
                    networkResourceEvents.put(resourceEvent.getResource(), resourceEvent);
                } else {
                    privateNetworkResourceByNodeName.put(resourceEvent.getName(), resourceEvent);
                }
            } else if (event instanceof NetworkAllocationEvent.Created) {
                var resourceEvent = (NetworkAllocationEvent.Created) event;

                // Key by compute resource for lookup.
                networkAllocationEvents.put(resourceEvent.getCompute(), resourceEvent);
            }
        }

        // Create the Concord node information list.
        return networkAllocationEvents.values().stream()
                .map(created -> {
                    // Look up allocation by compute resource, then look up network resource event.
                    var computeResourceEvent = Objects.requireNonNull(
                            computeResourceEvents.get(created.getCompute())
                    );
                    var publicNetworkResourceEvent = Objects.requireNonNull(
                            networkResourceEvents.get(created.getPublicNetwork())
                    );

                    // Obtain private network address resource event, if available.
                    var privateNetworkResourceEvent =
                            privateNetworkResourceByNodeName.get(publicNetworkResourceEvent.getName());

                    // Create ConcordNode info based on compute resource event.
                    var computeInfo = toConcordNode(computeResourceEvent, placementEntryByNodeName,
                                                    session.getSpecification().getModel().getBlockchainType());

                    // Create ConcordNode info based on network resource event.
                    var networkInfo = toConcordNode(
                            publicNetworkResourceEvent,
                            privateNetworkResourceEvent,
                            placementEntryByNodeName,
                            session.getSpecification().getModel().getBlockchainType()
                    );

                    // Merge the information.
                    // Note: Current invocation favors compute-derived information. But since there
                    // should be no conflict, "favoring" should not by intent induce filtering
                    // effect on network-derived information.
                    return merge(computeInfo, networkInfo);
                })
                .collect(Collectors.toList());
    }

    /**
     * Create a {@link List} of {@link ProvisionedResource} information instances based on a given
     * {@link DeploymentSession} and the corresponding {@link OrchestrationEvent} that the session
     * engenders.
     *
     * @param session
     *   deployment session.
     * @param events
     *   events engendered by the deployment session.
     *
     * @return
     *   a listing of {@link ProvisionedResource} instances.
     */
    private static List<ProvisionedResource> toProvisionedResources(
            DeploymentSession session,
            Collection<OrchestrationEvent> events
    ) {
        var orchestrationSiteByNode = session.getAssignment().getEntries().stream()
                .map(entry -> Map.entry(entry.getNode(), entry.getSite()))
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

        return events.stream()
                .map(event -> {
                    // By default, do not map into a ProvisionedResource unless the event type is
                    // matched.
                    ProvisionedResource resource = null;

                    if (event instanceof ComputeResourceEvent.Created) {
                        var resourceEvent = (ComputeResourceEvent.Created) event;
                        var node = resourceEvent.getNode();
                        var site = orchestrationSiteByNode.getOrDefault(
                                node,
                                OrchestrationSiteIdentifier.Companion.getDefaultValue()
                        );
                        resource = new ProvisionedResource(
                                ProvisionedResource.Type.COMPUTE_RESOURCE,
                                resourceEvent.getResource().toString(),
                                site,
                                session.getCluster(),
                                resourceEvent.getNode()
                        );
                    } else if (event instanceof NetworkResourceEvent.Created) {
                        var resourceEvent = (NetworkResourceEvent.Created) event;
                        var node = toNodeIdentifier(resourceEvent.getName());
                        var site = orchestrationSiteByNode.getOrDefault(
                                node,
                                OrchestrationSiteIdentifier.Companion.getDefaultValue()
                        );
                        resource = new ProvisionedResource(
                                ProvisionedResource.Type.NETWORK_RESOURCE,
                                resourceEvent.getResource().toString(),
                                site,
                                session.getCluster(),
                                node
                        );
                    } else if (event instanceof NetworkAllocationEvent.Created) {
                        var resourceEvent = (NetworkAllocationEvent.Created) event;
                        var node = toNodeIdentifier(resourceEvent.getName());
                        var site = orchestrationSiteByNode.getOrDefault(
                                node,
                                OrchestrationSiteIdentifier.Companion.getDefaultValue()
                        );
                        resource = new ProvisionedResource(
                                ProvisionedResource.Type.NETWORK_ALLOCATION,
                                resourceEvent.getResource().toString(),
                                site,
                                session.getCluster(),
                                node
                        );
                    }

                    return resource;
                })
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
    }

    /**
     * Create a {@link List} of {@link DeploymentSessionEvent}s based on a given
     * {@link DeploymentSession} and the additional corresponding {@link OrchestrationEvent} that
     * the session engendered.
     *
     * @param session
     *   deployment session.
     * @param events
     *   events engendered by the deployment session.
     *
     * @return
     *   a listing of {@link DeploymentSessionEvent}s.
     */
    private static List<DeploymentSessionEvent> toDeploymentSessionEvents(
            DeploymentSession session,
            Collection<OrchestrationEvent> events
    ) {
        var resources = toProvisionedResources(session, events);
        var resourceEventStream = resources.stream()
                .map(resource -> newResourceEvent(session.getId(), session.getStatus(), resource));

        // Generate Concord node models based on orchestration events.
        var nodes = toConcordNodes(session, events);

        var clusterEvent = newClusterDeploymentEvent(
                session.getId(),
                session.getStatus(),
                new ConcordCluster(session.getCluster(), new ConcordClusterInfo(nodes))
        );

        // Concatenate every event together.
        // (Existing events, all node events, cluster event, and completion event)
        var nodeEventStream = nodes.stream()
                .map(node -> newNodeDeploymentEvent(session.getId(), session.getStatus(), node));
        var results = Stream
                .concat(
                        Stream.concat(
                                Stream.concat(session.getEvents().stream(), resourceEventStream),
                                nodeEventStream
                        ),
                        Stream.of(
                                clusterEvent,
                                newCompleteEvent(session.getId(), DeploymentSession.Status.SUCCESS)
                        )
                )
                .collect(Collectors.toList());
        eventQueue.addAll(results);
        return results;
    }

    private static <T> StreamObserver<T> newResultObserver(CompletableFuture<T> result) {
        return new StreamObserver<>() {
            /** Holder of result value. */
            volatile T value;

            @Override
            public void onNext(T value) {
                this.value = value;
            }

            @Override
            public void onError(Throwable error) {
                result.completeExceptionally(error);
            }

            @Override
            public void onCompleted() {
                result.complete(value);
            }
        };
    }
}
