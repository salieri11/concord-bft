/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc.  All rights reserved. VMware Confidential
 * *************************************************************************/
package com.vmware.blockchain.deployment.service.provision;

import java.io.IOException;
import java.net.URI;
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
import java.util.stream.Stream;

import com.vmware.blockchain.deployment.model.ConcordCluster;
import com.vmware.blockchain.deployment.model.ConcordClusterIdentifier;
import com.vmware.blockchain.deployment.model.ConcordClusterInfo;
import com.vmware.blockchain.deployment.model.ConcordModelSpecification;
import com.vmware.blockchain.deployment.model.ConcordNode;
import com.vmware.blockchain.deployment.model.ConcordNodeEndpoint;
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
import com.vmware.blockchain.deployment.model.ethereum.Genesis;
import com.vmware.blockchain.deployment.model.orchestration.OrchestrationSiteInfo;
import com.vmware.blockchain.deployment.orchestration.NetworkAddress;
import com.vmware.blockchain.deployment.orchestration.Orchestrator;
import com.vmware.blockchain.deployment.orchestration.Orchestrator.ComputeResourceEvent;
import com.vmware.blockchain.deployment.orchestration.Orchestrator.CreateComputeResourceRequest;
import com.vmware.blockchain.deployment.orchestration.Orchestrator.CreateNetworkAllocationRequest;
import com.vmware.blockchain.deployment.orchestration.Orchestrator.CreateNetworkResourceRequest;
import com.vmware.blockchain.deployment.orchestration.Orchestrator.NetworkAllocationEvent;
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

    /** FIXME: In-Memory stand-in/crutches to keep track of deployment log. */
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
                .collect(Collectors.toUnmodifiableList());

        return new PlacementAssignment(assignments);
    }

    /**
     * Generate a Concord cluster configuration based on a listing of
     * {@link PlacementAssignment.Entry} of Concord nodes with the intended network address resource
     * from the node's orchestration site host associated with it. The paired network address
     * resource will be utilized to expose the placed Concord node to network external to the
     * orchestration site hosting the Concord node.
     *
     * @param addresses
     *   mapping of assignment entry to its associated external network address resource.
     *
     * @return
     *   an awaitable future of the mapping of {@link PlacementAssignment.Entry} with the Concord
     *   node configuration associated with that entry.
     */
    private CompletableFuture<Map<PlacementAssignment.Entry, String>> generateClusterConfig(
            List<Map.Entry<PlacementAssignment.Entry, NetworkResourceEvent.Created>> addresses
    ) {
        var promise = new CompletableFuture<Map<PlacementAssignment.Entry, String>>();

        // Next run the config utility asynchronously in background.
        CompletableFuture.runAsync(() -> {
            try {
                var nodeIPs = addresses.stream()
                        .map(Map.Entry::getValue)
                        .map(NetworkResourceEvent.Created::getAddress)
                        .collect(Collectors.toUnmodifiableList());
                if (addresses.size() != nodeIPs.size()) {
                    throw new IllegalStateException("Incorrect number of network addresses!");
                }

                // Prepare the input file (with default input location).
                ConfigYaml configUtil = new ConfigYaml();
                configUtil.generateConfigUtil(nodeIPs);

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
                            for (int i = 1; i <= addresses.size(); i++) {
                                var path = outputPath.resolve("concord" + i + ".config");
                                result.put(addresses.get(i - 1).getKey(), Files.readString(path));
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
            } catch (IOException error) {
                log.error("Cannot complete config generation process", error);

                // Wrap checked exception into RuntimeException to handle errors in a
                // generic fashion through exceptionally().
                throw new RuntimeException(error);
            }
        }, executor).exceptionally(error -> {
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

        // Allocate public network addresses for every node.
        var networkAddressPublishers = session.getAssignment().getEntries().stream()
                .map(entry -> {
                    var node = entry.getNode();
                    var orchestrator = orchestrators.get(entry.getSite());
                    var resource = new UUID(node.getHigh(), node.getLow()).toString();
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
                // Generate cluster configuration for every node.
                .thenComposeAsync(
                        __ -> generateClusterConfig(
                                // Note: Preserve the original listing order.
                                session.getAssignment().getEntries().stream()
                                        .map(entry -> Map.entry(entry, privateNetworkAddressMap.get(entry)))
                                        .collect(Collectors.toUnmodifiableList())
                        ),
                        executor
                )
                // Setup node deployment workflow with its assigned network address.
                .thenComposeAsync(configMap -> {
                    var model = session.getSpecification().getModel();
                    var nodePublishers = configMap.entrySet().parallelStream()
                            .map(entry -> {
                                var placement = entry.getKey();
                                var config = entry.getValue();
                                var publisher = deployNode(orchestrators.get(placement.getSite()),
                                                           session.getId(),
                                                           placement.getNode(),
                                                           model,
                                                           session.getSpecification().getGenesis(),
                                                           config);

                                return Map.entry(entry.getKey(), publisher);
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
                                        var orchestrator = orchestrators.get(placement.getSite());
                                        var allocationRequest = new CreateNetworkAllocationRequest(
                                                computeResource,
                                                publicNetworkAddressMap.get(entry.getKey()).getResource()
                                        );
                                        var allocationPublisher = orchestrator
                                                .createNetworkAllocation(allocationRequest);

                                        return ReactiveStream.toFuture(allocationPublisher);
                                    }, executor)
                                    // Put the network allocation event in the result collection.
                                    .whenComplete((event, error) -> {
                                        if (error != null) {
                                            log.error("Failed to deploy node({})",
                                                      entry.getKey(), error);
                                        } else {
                                            results.add(event);
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
                .exceptionally(error -> {
                    // Create the updated deployment session instance.
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

                    // Update the deployment log.
                    // FIXME: This does not take into account of persistence nor retry.
                    deploymentLog.get(session.getId()).complete(updatedSession);

                    log.error("Deployment session({}) failed", session.getId(), error);

                    return null; // To satisfy type signature (Void).
                });
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
            Genesis genesis,
            String configuration
    ) {
        var computeRequest = new CreateComputeResourceRequest(
                new ConcordClusterIdentifier(sessionId.getLow(), sessionId.getHigh()),
                nodeId,
                model,
                genesis,
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
                ConcordCluster.Companion.getDefaultValue(),
                DeploymentSession.Status.ACTIVE
        );
    }

    /**
     * Create an instance of {@link DeploymentSessionEvent} denoting the completion event of a
     * {@link DeploymentSession} associated with a given {@link DeploymentSessionIdentifier}.
     *
     * @param sessionId
     *   identifier of the deployment session to create the completion event for.
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
                ConcordNode.Companion.getDefaultValue(),
                ConcordNodeStatus.Companion.getDefaultValue(),
                ConcordCluster.Companion.getDefaultValue(),
                status
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
     * Create a new {@link ConcordNodeInfo} instance based on input parameters.
     * <p>NOTE: This is a stub function right now, and therefore takes no parameters yet.
     *
     * @return
     *   a new instance of {@link ConcordNodeInfo}.
     */
    private static ConcordNodeInfo toConcordNodeInfo() {
        // FIXME: Due to the current workaround of lack of East-West communication between metadata
        //   service and provision service, PlacementSpecification is taking
        //   ConcordModelSpecification instead of ConcordModelIdentifier as parameter.
        //   This unfortunately has ripple effect through the API models. Rather than making more
        //   concessions on temporary API workarounds, given that model ID is really just
        //   informational output rather than additional input parameter to other services, just
        //   return a default value for ID.
        //   Since internal IP addresses are also of not high value, defer the entire construction
        //   of ConcordNodeInfo until meaningful values can be returned here.
        return ConcordNodeInfo.Companion.getDefaultValue();
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
            NetworkResourceEvent.Created event,
            Map<String, PlacementAssignment.Entry> placementEntryByNodeName
    ) {
        // FIXME: Ideally the API service names must be defined somewhere rather than
        //   "fabricated out of thin air" here. A logical place would be ConcordModelSpecification
        //   itself, which defines the API endpoint URI minus the authority portion, and merged with
        //   the public IP addresses, which is known by this point.
        var endpoints = Map.of(
                "ethereum-rpc",
                new ConcordNodeEndpoint(
                        URI.create("https://{{ip}}:8545".replace("{{ip}}", event.getAddress()))
                                .toString(),
                        ""
                )
        );

        return new ConcordNodeHostInfo(
                placementEntryByNodeName.get(event.getName()).getSite(),
                Map.of(NetworkAddress.toIPv4Address(event.getAddress()), 0),
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
            Map<String, PlacementAssignment.Entry> placementEntryByNodeName
    ) {
        return new ConcordNode(
                event.getNode(),
                toConcordNodeInfo(),
                toConcordNodeHostInfo(event, placementEntryByNodeName)
        );
    }

    /**
     * Create a new {@link ConcordNode} instance based on a {@link NetworkResourceEvent.Created}
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
            NetworkResourceEvent.Created event,
            Map<String, PlacementAssignment.Entry> placementEntryByNodeName
    ) {
        return new ConcordNode(
                placementEntryByNodeName.get(event.getName()).getNode(),
                toConcordNodeInfo(),
                toConcordNodeHostInfo(event, placementEntryByNodeName)
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
                ).collect(Collectors.toMap(
                        Map.Entry::getKey,
                        Map.Entry::getValue,
                        (oldEntry, newEntry) -> oldEntry // Preserve existing value.
                ))
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
                ).collect(Collectors.toMap(
                        Map.Entry::getKey,
                        Map.Entry::getValue,
                        (oldEntry, newEntry) -> oldEntry // Preserve existing value.
                )),
                Stream.concat(
                        first.getEndpoints().entrySet().stream(),
                        second.getEndpoints().entrySet().stream()
                ).collect(Collectors.toMap(
                        Map.Entry::getKey,
                        Map.Entry::getValue,
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
                    var networkResourceEvent = Objects.requireNonNull(
                            networkResourceEvents.get(created.getNetwork())
                    );

                    // Create ConcordNode info based on compute resource event.
                    var computeInfo = toConcordNode(computeResourceEvent, placementEntryByNodeName);

                    // Create ConcordNode info based on network resource event.
                    var networkInfo = toConcordNode(networkResourceEvent, placementEntryByNodeName);

                    // Merge the information.
                    // Note: Current invocation favors compute-derived information. But since there
                    // should be no conflict, "favoring" should not by intent induce filtering
                    // effect on network-derived information.
                    return merge(computeInfo, networkInfo);
                })
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
        var nodes = toConcordNodes(session, events);
        var nodeEventStream = nodes.stream()
                .map(node -> new DeploymentSessionEvent(
                        DeploymentSessionEvent.Type.NODE_DEPLOYED,
                        DeploymentSessionIdentifier.Companion.getDefaultValue(),
                        node,
                        ConcordNodeStatus.Companion.getDefaultValue(),
                        ConcordCluster.Companion.getDefaultValue(),
                        DeploymentSession.Status.ACTIVE
                ));

        // Create Concord cluster model.
        var clusterEvent = new DeploymentSessionEvent(
                DeploymentSessionEvent.Type.CLUSTER_DEPLOYED,
                DeploymentSessionIdentifier.Companion.getDefaultValue(),
                ConcordNode.Companion.getDefaultValue(),
                ConcordNodeStatus.Companion.getDefaultValue(),
                new ConcordCluster(session.getCluster(), new ConcordClusterInfo(nodes)),
                DeploymentSession.Status.ACTIVE
        );

        // Concatenate every event together.
        // (Existing events, all node events, cluster event, and completion event)
        return Stream
                .concat(
                        Stream.concat(session.getEvents().stream(), nodeEventStream),
                        Stream.of(
                                clusterEvent,
                                newCompleteEvent(session.getId(), DeploymentSession.Status.SUCCESS)
                        )
                )
                .collect(Collectors.toList());
    }
}
