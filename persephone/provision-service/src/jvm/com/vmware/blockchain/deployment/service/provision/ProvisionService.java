/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.service.provision;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import com.vmware.blockchain.deployment.model.ConcordCluster;
import com.vmware.blockchain.deployment.model.ConcordClusterIdentifier;
import com.vmware.blockchain.deployment.model.ConcordModelSpecification;
import com.vmware.blockchain.deployment.model.ConcordNode;
import com.vmware.blockchain.deployment.model.ConcordNodeHostInfo;
import com.vmware.blockchain.deployment.model.ConcordNodeIdentifier;
import com.vmware.blockchain.deployment.model.ConcordNodeInfo;
import com.vmware.blockchain.deployment.model.ConcordNodeStatus;
import com.vmware.blockchain.deployment.model.CreateClusterRequest;
import com.vmware.blockchain.deployment.model.DeploymentSession;
import com.vmware.blockchain.deployment.model.DeploymentSessionEvent;
import com.vmware.blockchain.deployment.model.DeploymentSessionIdentifier;
import com.vmware.blockchain.deployment.model.DeploymentSpecification;
import com.vmware.blockchain.deployment.model.MessageHeader;
import com.vmware.blockchain.deployment.model.OrchestrationSiteIdentifier;
import com.vmware.blockchain.deployment.model.PlacementAssignment;
import com.vmware.blockchain.deployment.model.PlacementSpecification;
import com.vmware.blockchain.deployment.model.ProvisionServiceImplBase;
import com.vmware.blockchain.deployment.model.StreamClusterDeploymentSessionEventRequest;
import com.vmware.blockchain.deployment.model.orchestration.OrchestrationSiteInfo;
import com.vmware.blockchain.deployment.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.orchestration.Orchestrator.NetworkResourceEvent;
import com.vmware.blockchain.deployment.orchestration.Orchestrator.OrchestrationEvent;
import com.vmware.blockchain.deployment.orchestration.InactiveOrchestrator;
import com.vmware.blockchain.deployment.reactive.ReactiveStream;
import io.grpc.stub.StreamObserver;
import org.reactivestreams.Publisher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ProvisionService extends ProvisionServiceImplBase {

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
    private static final Logger log = LoggerFactory.getLogger(ProvisionService.class);

    /** Atomic update for service instance state. */
    private static final AtomicReferenceFieldUpdater<ProvisionService, State> STATE =
            AtomicReferenceFieldUpdater.newUpdater(ProvisionService.class, State.class, "state");

    /** Default {@link Orchestrator} instance's {@link OrchestrationSiteIdentifier} */
    private static final OrchestrationSiteIdentifier defaultOrchestratorId =
            OrchestrationSiteIdentifier.Companion.getDefaultValue();

    /** Default NOOP {@link DeploymentSessionEvent} instance. */
    private static final DeploymentSessionEvent noopDeploymentEvent = new DeploymentSessionEvent(
            DeploymentSessionEvent.Type.NOOP,
            DeploymentSessionIdentifier.Companion.getDefaultValue(),
            ConcordNode.Companion.getDefaultValue(),
            ConcordNodeStatus.Companion.getDefaultValue(),
            ConcordCluster.Companion.getDefaultValue()
    );

    /** Executor to use for all async service operations. */
    private final ExecutorService executor;

    /** Orchestrator instance factory. */
    private final OrchestratorProvider orchestratorProvider;

    /** Service state. */
    private volatile State state = State.STOPPED;

    /** Orchestrator pool to utilize for orchestration operations. */
    private final Map<OrchestrationSiteIdentifier, OrchestrationSiteInfo> orchestrations;

    /** Orchestrator pool to utilize for orchestration operations. */
    private final Map<OrchestrationSiteIdentifier, Orchestrator> orchestrators =
            new ConcurrentHashMap<>();

    /** TODO: [REPLACE] In-Memory stand-in/crutches to keep track of deployment log. */
    private final Map<DeploymentSessionIdentifier, CompletableFuture<DeploymentSession>> deploymentLog =
            new ConcurrentHashMap<>();

    public ProvisionService(
            ExecutorService executor,
            OrchestratorProvider orchestratorProvider,
            Map<OrchestrationSiteIdentifier, OrchestrationSiteInfo> orchestrations
    ) {
        this.executor = executor;
        this.orchestratorProvider = orchestratorProvider;
        this.orchestrations = orchestrations;
    }

    /**
     * Initialize the service instance asynchronously.
     *
     * @return
     *   {@link CompletableFuture} that completes when initialization is done.
     */
    public CompletableFuture<Void> initialize() {
        if (STATE.compareAndSet(this, State.STOPPED, State.INITIALIZING)) {
            // Spawn off sub-tasks to async-create the orchestrator instances.
            return CompletableFuture.allOf(
                    // Initialize all orchestrator, then on completion insert into orchestrator map.
                    orchestrations.entrySet().stream()
                            .map(entry -> orchestratorProvider.newOrchestrator(entry.getValue())
                                    .thenAccept(orchestrator -> orchestrators.put(entry.getKey(),
                                                                                  orchestrator)))
                            .toArray(CompletableFuture[]::new)
            ).thenRunAsync(() -> {
                // Set a default orchestrator.
                var defaultOrchestrator = orchestrators.entrySet().stream()
                        .filter(entry -> !(entry.getValue() instanceof InactiveOrchestrator))
                        .findFirst()
                        .map(Map.Entry::getValue)
                        .orElseThrow();
                orchestrators.putIfAbsent(defaultOrchestratorId, defaultOrchestrator);

                // Set instance to ACTIVE state.
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
    public CompletableFuture<Void> shutdown() {
        if (STATE.compareAndSet(this, State.ACTIVE, State.STOPPING) ||
                STATE.compareAndSet(this, State.INITIALIZING, State.STOPPING)) {
            return CompletableFuture.runAsync(() -> {
                log.info("Service instance shutting down");

                // Shutdown the orchestrators.
                orchestrators.forEach((id, site) -> {
                    log.info("Shutting down orchestrator instance, id({})", id);

                    // Note: Assumption is that orchestrator.close() is fast and non-blocking.
                    // There may be a need for an awaitable-contract on Orchestrator interface.
                    site.close();
                });

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

            // TODO: Verify that for any FIXED placement, the orchestration site ID exists.
            // orchestrators.containsKey(placementEntry.getSite())

            // Simplistic state checking. More sophisticated checks requires setting up a reference
            // while the request is not yet completed.
            if (STATE.get(this) != State.ACTIVE) {
                response.onError(new IllegalStateException("Service instance is not active"));
            } else {
                CompletableFuture.runAsync(() -> {
                    // Resolve / generate deployment session ID.
                    var sessionId = newSessionId(request.getHeader());
                    var deploymentSpec = request.getSpecification();

                    // Generate node ID and affix the node placements.
                    var placements = resolvePlacement(deploymentSpec);

                    // Persist the deployment log.
                    var session = new DeploymentSession(
                            sessionId,
                            deploymentSpec,
                            placements,
                            false /* complete */,
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
                        response.onError(
                                new IllegalStateException("Cannot record deployment session")
                        );
                    }
                }, executor).exceptionally(error -> {
                    response.onError(error);
                    return null; // To satisfy type signature (Void).
                });
            }
        } catch (Throwable error) {
            response.onError(error);
        }
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
                response.onError(new IllegalStateException("Service instance is not active"));
            } else {
                CompletableFuture.runAsync(() -> {
                    // FIXME: Right now the logic only works correctly for single service instance.
                    Consumer<DeploymentSession> sender = session -> {
                        // Send all events.
                        for (DeploymentSessionEvent event : session.getEvents()) {
                            response.onNext(event);
                        }

                        // Send completion.
                        response.onCompleted();
                    };

                    var task = deploymentLog.get(request.getSession());
                    if (task != null) {
                        task.thenAcceptAsync(sender, executor);
                    } else {
                        response.onError(
                                new IllegalStateException("Session does not have a background task")
                        );
                    }
                }, executor).exceptionally(error -> {
                    response.onError(error);
                    return null; // To satisfy type signature (Void).
                });
            }
        } catch (Throwable error) {
            response.onError(error);
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
                        site = orchestrators.keySet().stream().findAny()
                                .orElse(defaultOrchestratorId);
                    }

                    return new PlacementAssignment.Entry(nodeId, site);
                })
                .collect(Collectors.toList());

        return new PlacementAssignment(assignments);
    }

    /**
     * generateClusterConfig() -> Map<PlacementAssignment.Entry, String>
     *         //   where String is the per-node config.
     * TODO: Bare-bone stub to act as a placeholder for centralized cluster config generation.
     */
    private CompletableFuture<Map<PlacementAssignment.Entry, String>> generateClusterConfig(
            List<Map.Entry<PlacementAssignment.Entry, Publisher<NetworkResourceEvent>>> addressMap
    ) {
        // Subscribe to network address publishers and collect the results in a map asynchronously.
        Map<PlacementAssignment.Entry, NetworkResourceEvent> addresses = new ConcurrentHashMap<>();
        var futureMap = addressMap.stream()
                .map(entry -> {
                    var result = ReactiveStream.toFuture(entry.getValue())
                            .thenAcceptAsync(event -> addresses.put(entry.getKey(), event));
                    return Map.entry(entry.getKey(), result);
                })
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

        var promise = new CompletableFuture<Map<PlacementAssignment.Entry, String>>();
        CompletableFuture.allOf(futureMap.values().toArray(CompletableFuture[]::new))
                .thenRunAsync(() -> {
                    // All futures are complete at this point. generate the config with addresses.
                    // Generates the input YAML file.
                    var nodeIPs = addresses.values().stream()
                            .filter(event -> event instanceof NetworkResourceEvent.Created)
                            .map(event -> ((NetworkResourceEvent.Created) event).getAddress())
                            .collect(Collectors.toList());
                    if (addresses.size() != nodeIPs.size()) {
                        throw new IllegalStateException("Incorrect number of network addresses!");
                    }

                    // Next run the config utility.
                    try {
                        // Prepare the input file (with default input location).
                        ConfigYaml configUtil = new ConfigYaml();
                        boolean inputGenerated = configUtil.generateConfigUtil(nodeIPs);

                        var outputPath = Files.createTempDirectory(null);
                        var future = new ProcessBuilder("/app/conc_genconfig",
                                                        "--configuration-input",
                                                        configUtil.getConfigYamlFilePath())
                                .directory(outputPath.toFile())
                                .start()
                                .onExit();
                        future.whenCompleteAsync((process, error) -> {
                            if (error == null) {
                                try {
                                    var result = new HashMap<PlacementAssignment.Entry, String>();
                                    for (int i = 1; i <= addressMap.size(); i++) {
                                        var path = outputPath.resolve("concord" + i + ".config");
                                        result.put(addressMap.get(i - 1).getKey(),
                                                   Files.readString(path));
                                    }

                                    // Complete the future.
                                    promise.complete(result);
                                } catch (Throwable collectError) {
                                    log.error("Cannot collect generated cluster configuration",
                                              collectError);

                                    promise.completeExceptionally(collectError);
                                }
                            } else {
                                log.error("Cannot run config generation process", error);

                                promise.completeExceptionally(error);
                            }
                        });
                    } catch (Throwable error) {
                        log.error("Cannot start config generation process", error);

                        // Wrap checked exception into RuntimeException to handle errors in a
                        // generic fashion through exceptionally().
                        throw new RuntimeException(error);
                    }
                }, executor)
                .exceptionally(error -> {
                    log.error("Failed to generate cluster configuration", error);

                    promise.completeExceptionally(error);
                    return null; // To satisfy type signature (Void).
                });

        return promise;
    }

    /**
     * Execute a Concord cluster deployment workflow.
     *
     * @param session
     *   deployment session to carry out the workflow for.
     */
    private void deployCluster(DeploymentSession session) {
        // Allocate public network addresses for every node.
        var networkAddressList = session.getAssignment().getEntries().stream()
                .map(entry -> {
                    var node = entry.getNode();
                    var orchestrator = orchestrators.get(entry.getSite());
                    var resource = new UUID(node.getHigh(), node.getLow()).toString();
                    var addressRequest = new Orchestrator.CreateNetworkResourceRequest(resource, true);
                    var addressPublisher = orchestrator.createNetworkAddress(addressRequest);

                    return Map.entry(entry, addressPublisher);
                })
                .collect(Collectors.toList());

        // Reorganize into map for easier lookup (during async lambda).
        var networkAddressMap = networkAddressList.stream()
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

        // Generate configuration per node.
        var configMapResult = generateClusterConfig(networkAddressList);

        // Setup node deployment workflow.
        configMapResult.thenAcceptAsync(configMap -> {
            var model = session.getSpecification().getModel();
            var nodeMap = configMap.entrySet().parallelStream()
                    .map(entry -> {
                        var placement = entry.getKey();
                        var config = entry.getValue();
                        var publisher = deployNode(orchestrators.get(placement.getSite()),
                                                   session.getId(),
                                                   placement.getNode(),
                                                   model,
                                                   config);

                        return Map.entry(entry, publisher);
                    })
                    .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
            var publishers = nodeMap.values();

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
            final var results = ConcurrentHashMap.<List<OrchestrationEvent>>newKeySet();
            var promises = publishers.stream()
                    .map(publisher -> ReactiveStream.toFuture(publisher, ArrayList::new)
                            .thenAcceptAsync(input -> {
                                @SuppressWarnings("unchecked")
                                var events = (List<OrchestrationEvent>) input;
                                results.add(events);
                            }, executor)
                    )
                    .toArray(CompletableFuture[]::new);

            CompletableFuture.allOf(promises).thenRunAsync(() -> {
                // Create / merge all deployment events.
                var sessionEvents = new ArrayList<>(session.getEvents());
                results.stream()
                        .flatMap(Collection::stream)
                        .map(ProvisionService::toDeploymentSessionEvent)
                        .filter(event -> event.getType() != DeploymentSessionEvent.Type.NOOP)
                        .forEach(sessionEvents::add);
                sessionEvents.add(newCompleteEvent(session.getId()));

                // Create the updated deployment session instance.
                var updatedSession = new DeploymentSession(
                        session.getId(),
                        session.getSpecification(),
                        session.getAssignment(),
                        true /* complete */,
                        sessionEvents
                );

                // Update the deployment log.
                // FIXME: This does not take into account of persistence nor retry.
                deploymentLog.get(session.getId()).complete(updatedSession);
            }, executor).exceptionally(error -> {
                log.error("Failed to deploy cluster", error);

                // Create the updated deployment session instance.
                var updatedSession = new DeploymentSession(
                        session.getId(),
                        session.getSpecification(),
                        session.getAssignment(),
                        true /* complete */,
                        session.getEvents()
                );

                // Update the deployment log.
                // FIXME: This does not take into account of persistence nor retry.
                deploymentLog.get(session.getId()).complete(updatedSession);

                return null; // To satisfy type signature (Void).
            });
        }, executor);
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
     * @param configuration
     *   payload to the node's initial configuration data.
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
            String configuration
    ) {
        var computeRequest = new Orchestrator.CreateComputeResourceRequest(
                new ConcordClusterIdentifier(sessionId.getLow(), sessionId.getHigh()),
                nodeId,
                model,
                configuration
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
     * Create an instance of {@link DeploymentSessionEvent} denoting the initial event of a
     * {@link DeploymentSession} associated with a given {@link DeploymentSessionIdentifier}.
     *
     * @param sessionId
     *   identifier of the deployment session to create the initial event for.
     *
     * @return
     *   a new instance of {@link DeploymentSessionEvent}.
     */
    private static DeploymentSessionEvent newInitialEvent(DeploymentSessionIdentifier sessionId) {
        return new DeploymentSessionEvent(
                DeploymentSessionEvent.Type.ACKNOWLEDGED,
                sessionId,
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
     *   identifier of the deployment session to create the completion event for.
     *
     * @return
     *   a new instance of {@link DeploymentSessionEvent}.
     */
    private static DeploymentSessionEvent newCompleteEvent(DeploymentSessionIdentifier sessionId) {
        return new DeploymentSessionEvent(
                DeploymentSessionEvent.Type.COMPLETED,
                sessionId,
                ConcordNode.Companion.getDefaultValue(),
                ConcordNodeStatus.Companion.getDefaultValue(),
                ConcordCluster.Companion.getDefaultValue()
        );
    }

    /**
     * Convert {@link OrchestrationEvent} to a {@link DeploymentSessionEvent} if applicable.
     *
     * TODO: Consider changing to a visitor pattern on OrchestrationEvent instead.
     *
     * @param event
     *   orchestration event to map from.
     *
     * @return
     *   a new {@link DeploymentSessionEvent} instance, {@link #noopDeploymentEvent} otherwise.
     */
    private static DeploymentSessionEvent toDeploymentSessionEvent(OrchestrationEvent event) {
        if (event instanceof Orchestrator.ComputeResourceEvent.Created) {
            var createEvent = (Orchestrator.ComputeResourceEvent.Created) event;
            var node = new ConcordNode(
                    createEvent.getNode(),
                    ConcordNodeInfo.Companion.getDefaultValue(),
                    ConcordNodeHostInfo.Companion.getDefaultValue()
            );
            return new DeploymentSessionEvent(
                    DeploymentSessionEvent.Type.NODE_DEPLOYED,
                    DeploymentSessionIdentifier.Companion.getDefaultValue(),
                    node,
                    ConcordNodeStatus.Companion.getDefaultValue(),
                    ConcordCluster.Companion.getDefaultValue()
            );
        } else {
            return noopDeploymentEvent;
        }
    }
}
