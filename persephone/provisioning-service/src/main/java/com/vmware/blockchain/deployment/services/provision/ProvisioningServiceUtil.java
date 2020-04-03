/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/

package com.vmware.blockchain.deployment.services.provision;

import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.vmware.blockchain.deployment.services.orchestration.NetworkAddress;
import com.vmware.blockchain.deployment.services.orchestration.OrchestratorData;
import com.vmware.blockchain.deployment.v1.ConcordCluster;
import com.vmware.blockchain.deployment.v1.ConcordClusterIdentifier;
import com.vmware.blockchain.deployment.v1.ConcordModelSpecification;
import com.vmware.blockchain.deployment.v1.ConcordNode;
import com.vmware.blockchain.deployment.v1.ConcordNodeEndpoint;
import com.vmware.blockchain.deployment.v1.ConcordNodeHostInfo;
import com.vmware.blockchain.deployment.v1.ConcordNodeIdentifier;
import com.vmware.blockchain.deployment.v1.ConcordNodeInfo;
import com.vmware.blockchain.deployment.v1.ConcordNodeStatus;
import com.vmware.blockchain.deployment.v1.DeploymentSession;
import com.vmware.blockchain.deployment.v1.DeploymentSessionEvent;
import com.vmware.blockchain.deployment.v1.DeploymentSessionIdentifier;
import com.vmware.blockchain.deployment.v1.MessageHeader;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteIdentifier;
import com.vmware.blockchain.deployment.v1.PlacementAssignment;
import com.vmware.blockchain.deployment.v1.ProvisionedResource;

/**
 * Helper class for @OrchestrationSite.
 */

public class ProvisioningServiceUtil {

    /**
     * Generate a new {@link DeploymentSessionIdentifier} based on a given request {@link MessageHeader}, or generate a
     * random value as ID if request does not contain sufficient parametric data.
     *
     * @param requestHeader header of the deployment request message.
     * @return a corresponding instance of the resulting deployment session ID.
     */
    public static DeploymentSessionIdentifier newSessionId(MessageHeader requestHeader) {
        var id = requestHeader.getId();
        UUID uuid;
        if (id.isEmpty() || id.isBlank()) {
            uuid = UUID.randomUUID();
        } else {
            // Hash the string value into an UUID.
            uuid = UUID.nameUUIDFromBytes(id.getBytes(StandardCharsets.UTF_8));
        }
        return DeploymentSessionIdentifier.newBuilder().setLow(uuid.getLeastSignificantBits())
                .setHigh(uuid.getMostSignificantBits()).build();
    }


    /**
     * Generate a new {@link ConcordClusterIdentifier}.
     *
     * @return a new {@link ConcordClusterIdentifier} instance.
     */
    public static ConcordClusterIdentifier newClusterId() {
        var uuid = UUID.randomUUID();

        return ConcordClusterIdentifier.newBuilder().setLow(uuid.getLeastSignificantBits())
                .setHigh(uuid.getMostSignificantBits()).build();
    }


    /**
     * Create a new {@link ConcordNode} instance based on a {@link OrchestratorData.ComputeResourceEventCreated} event,
     * using additional input as context information for instance creation.
     *
     * @param event                    event signaled for the concord node.
     * @param placementEntryByNodeName mappings of concord node resource name to its associated {@link
     *                                 PlacementAssignment.Entry}.
     * @return a new instance of {@link ConcordNode} with relevant properties filled in with non-default values.
     */
    private static ConcordNode toConcordNode(
            OrchestratorData.ComputeResourceEventCreated event,
            Map<String, PlacementAssignment.Entry> placementEntryByNodeName,
            ConcordModelSpecification.BlockchainType blockchainType
    ) {
        return ConcordNode.newBuilder().setId(event.getNode())
                .setInfo(ConcordNodeInfo.newBuilder().setBlockchainType(blockchainType).build())
                .setHostInfo(toConcordNodeHostInfo(event, placementEntryByNodeName)).build();
    }

    /**
     * Create a new {@link ConcordNode} instance based on a {@link OrchestratorData.NetworkResourceEventCreated} event,
     * using additional input as context information for instance creation.
     *
     * @param publicNetworkEvent       event signaled for the concord node.
     * @param privateNetworkEvent      event signaled for the concord node.
     * @param placementEntryByNodeName mappings of concord node resource name to its associated {@link
     *                                 PlacementAssignment.Entry}.
     * @return a new instance of {@link ConcordNode} with relevant properties filled in with non-default values.
     */
    private static ConcordNode toConcordNode(
            OrchestratorData.NetworkResourceEventCreated publicNetworkEvent,
            OrchestratorData.NetworkResourceEventCreated privateNetworkEvent,
            Map<String, PlacementAssignment.Entry> placementEntryByNodeName,
            ConcordModelSpecification.BlockchainType blockchainType
    ) {
        return ConcordNode.newBuilder().setId(placementEntryByNodeName.get(publicNetworkEvent.getName()).getNode())
                .setInfo(ConcordNodeInfo.newBuilder().setBlockchainType(blockchainType).build())
                .setHostInfo(toConcordNodeHostInfo(
                        publicNetworkEvent,
                        privateNetworkEvent,
                        placementEntryByNodeName,
                        blockchainType
                )).build();
    }

    /**
     * Merge two {@link ConcordNodeInfo} instances, favoring to preserve content from the first instance whenever
     * conflict arise.
     *
     * @param first  first instance to be merged.
     * @param second second instance to be merged.
     * @return a new {@link ConcordNodeInfo} instance combining content from input sources.
     */
    private static ConcordNodeInfo merge(ConcordNodeInfo first, ConcordNodeInfo second) {
        return ConcordNodeInfo.newBuilder().setModel(first.getModel())
                .putAllIpv4Addresses(Stream.concat(
                        first.getIpv4AddressesMap().entrySet().stream(),
                        second.getIpv4AddressesMap().entrySet().stream()
                ).collect(Collectors.toMap(Map.Entry::getKey,
                                           Map.Entry::getValue, (oldEntry, newEntry) -> oldEntry // Preserve exis value.
                )))
                .setBlockchainType(first.getBlockchainType()).build();
    }

    /**
     * Merge two {@link ConcordNodeHostInfo} instances, favoring to preserve content from the first instance whenever
     * conflict arise.
     *
     * @param first  first instance to be merged.
     * @param second second instance to be merged.
     * @return a new {@link ConcordNodeHostInfo} instance combining content from input sources.
     */
    private static ConcordNodeHostInfo merge(ConcordNodeHostInfo first, ConcordNodeHostInfo second) {

        return ConcordNodeHostInfo.newBuilder()
                .setSite(first.getSite())
                .putAllIpv4AddressMap(Stream.concat(
                        first.getIpv4AddressMapMap().entrySet().stream(),
                        second.getIpv4AddressMapMap().entrySet().stream()
                ).collect(Collectors.toMap(Map.Entry::getKey,
                                           Map.Entry::getValue, (oldEntry, newEntry) -> oldEntry // Preserve exis value.
                )))
                .putAllEndpoints(Stream.concat(
                        first.getEndpointsMap().entrySet().stream(),
                        second.getEndpointsMap().entrySet().stream()
                ).collect(Collectors.toMap(Map.Entry::getKey,
                                           Map.Entry::getValue, (oldEntry, newEntry) -> oldEntry // Preserve exis value.
                )))
                .build();
    }

    /**
     * Merge two {@link ConcordNode} instances, favoring to preserve content from the first instance whenever conflict
     * arise.
     *
     * @param first  first instance to be merged.
     * @param second second instance to be merged.
     * @return a new {@link ConcordNode} instance combining content from input sources.
     */
    private static ConcordNode merge(ConcordNode first, ConcordNode second) {
        return ConcordNode.newBuilder().setId(first.getId())
                .setInfo(merge(first.getInfo(), second.getInfo()))
                .setHostInfo(merge(first.getHostInfo(), second.getHostInfo()))
                .build();
    }

    /**
     * Create a {@link List} of {@link ConcordNode} information instances based on a given {@link DeploymentSession} and
     * the corresponding {@link OrchestratorData.OrchestrationEvent} that the session engenders.
     *
     * @param session deployment session.
     * @param events  events engendered by the deployment session.
     * @return a listing of {@link ConcordNode} instances.
     */
    public static List<ConcordNode> toConcordNodes(
            DeploymentSession session,
            Collection<OrchestratorData.OrchestrationEvent> events
    ) {
        var placementEntryByNodeName = session.getAssignment().getEntriesList().stream()
                .map(entry -> Map.entry(toResourceName(entry.getNode()), entry))
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
        var computeResourceEvents = new HashMap<URI, OrchestratorData.ComputeResourceEventCreated>();
        var networkResourceEvents = new HashMap<URI, OrchestratorData.NetworkResourceEventCreated>();
        var networkAllocationEvents = new HashMap<URI, OrchestratorData.NetworkAllocationEventCreated>();
        var privateNetworkResourceByNodeName = new HashMap<String, OrchestratorData.NetworkResourceEventCreated>();

        // Partition respective orchestration events into separate groups of reverse-mapping by URI.
        for (OrchestratorData.OrchestrationEvent event : events) {
            if (event instanceof OrchestratorData.ComputeResourceEventCreated) {
                var resourceEvent = (OrchestratorData.ComputeResourceEventCreated) event;

                computeResourceEvents.put(resourceEvent.getResource(), resourceEvent);
            } else if (event instanceof OrchestratorData.NetworkResourceEventCreated) {
                var resourceEvent = (OrchestratorData.NetworkResourceEventCreated) event;

                if (resourceEvent.isPublicResource()) {
                    networkResourceEvents.put(resourceEvent.getResource(), resourceEvent);
                } else {
                    privateNetworkResourceByNodeName.put(resourceEvent.getName(), resourceEvent);
                }
            } else if (event instanceof OrchestratorData.NetworkAllocationEventCreated) {
                var resourceEvent = (OrchestratorData.NetworkAllocationEventCreated) event;

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
     * Create a {@link List} of {@link ProvisionedResource} information instances based on a given {@link
     * DeploymentSession} and the corresponding {@link OrchestratorData.OrchestrationEvent} that the session engenders.
     *
     * @param session deployment session.
     * @param events  events engendered by the deployment session.
     * @return a listing of {@link ProvisionedResource} instances.
     */
    public static List<ProvisionedResource> toProvisionedResources(
            DeploymentSession session,
            Collection<OrchestratorData.OrchestrationEvent> events
    ) {
        var orchestrationSiteByNode = session.getAssignment().getEntriesList().stream()
                .map(entry -> Map.entry(entry.getNode(), entry.getSite()))
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

        return events.stream()
                .map(event -> {
                    // By default, do not map into a ProvisionedResource unless the event type is matched.
                    ProvisionedResource resource = null;

                    if (event instanceof OrchestratorData.ComputeResourceEventCreated) {
                        var resourceEvent = (OrchestratorData.ComputeResourceEventCreated) event;
                        var node = resourceEvent.getNode();
                        var site = orchestrationSiteByNode.getOrDefault(
                                node,
                                OrchestrationSiteIdentifier.getDefaultInstance()
                        );
                        resource = ProvisionedResource.newBuilder().setType(ProvisionedResource.Type.COMPUTE_RESOURCE)
                                .setName(resourceEvent.getResource().toString())
                                .setSite(site)
                                .setCluster(session.getCluster())
                                .setNode(node)
                                .build();

                    } else if (event instanceof OrchestratorData.NetworkResourceEventCreated) {
                        var resourceEvent = (OrchestratorData.NetworkResourceEventCreated) event;
                        var node = toNodeIdentifier(resourceEvent.getName());
                        var site = orchestrationSiteByNode.getOrDefault(
                                node,
                                OrchestrationSiteIdentifier.getDefaultInstance()
                        );

                        resource = ProvisionedResource.newBuilder().setType(ProvisionedResource.Type.NETWORK_RESOURCE)
                                .setName(resourceEvent.getResource().toString())
                                .setSite(site)
                                .setCluster(session.getCluster())
                                .setNode(node)
                                .build();
                    } else if (event instanceof OrchestratorData.NetworkAllocationEventCreated) {
                        var resourceEvent = (OrchestratorData.NetworkAllocationEventCreated) event;
                        var node = toNodeIdentifier(resourceEvent.getName());
                        var site = orchestrationSiteByNode.getOrDefault(
                                node,
                                OrchestrationSiteIdentifier.getDefaultInstance()
                        );
                        resource = ProvisionedResource.newBuilder().setType(ProvisionedResource.Type.NETWORK_ALLOCATION)
                                .setName(resourceEvent.getResource().toString())
                                .setSite(site)
                                .setCluster(session.getCluster())
                                .setNode(node)
                                .build();
                    }

                    return resource;
                })
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
    }

    /**
     * Create an instance of {@link DeploymentSessionEvent} denoting the initial event of a {@link DeploymentSession}
     * associated with a given {@link DeploymentSessionIdentifier}.
     *
     * @param sessionId identifier of the deployment session to create the session event for.
     * @return a new instance of {@link DeploymentSessionEvent}.
     */
    public static DeploymentSessionEvent newInitialEvent(DeploymentSessionIdentifier sessionId) {
        return DeploymentSessionEvent.newBuilder().setType(DeploymentSessionEvent.Type.ACKNOWLEDGED)
                .setSession(sessionId)
                .setStatus(DeploymentSession.Status.ACTIVE)
                .setResource(ProvisionedResource.getDefaultInstance())
                .setNode(ConcordNode.getDefaultInstance())
                .setNodeStatus(ConcordNodeStatus.getDefaultInstance())
                .setCluster(ConcordCluster.getDefaultInstance())
                .build();
    }

    /**
     * Create an instance of {@link DeploymentSessionEvent} denoting the completion event of a {@link DeploymentSession}
     * associated with a given {@link DeploymentSessionIdentifier}.
     *
     * @param sessionId identifier of the deployment session to create the session event for.
     * @param status    completion status.
     * @return a new instance of {@link DeploymentSessionEvent}.
     */
    public static DeploymentSessionEvent newCompleteEvent(
            DeploymentSessionIdentifier sessionId,
            DeploymentSession.Status status
    ) {
        return DeploymentSessionEvent.newBuilder().setType(DeploymentSessionEvent.Type.COMPLETED)
                .setSession(sessionId)
                .setStatus(status)
                .setResource(ProvisionedResource.getDefaultInstance())
                .setNode(ConcordNode.getDefaultInstance())
                .setNodeStatus(ConcordNodeStatus.getDefaultInstance())
                .setCluster(ConcordCluster.getDefaultInstance())
                .build();
    }

    /**
     * Create an instance of {@link DeploymentSessionEvent} denoting the cluster deployment event of a {@link
     * DeploymentSession} associated with a given {@link DeploymentSessionIdentifier}.
     *
     * @param sessionId identifier of the deployment session to create the session event for.
     * @param status    completion status.
     * @param resource  data payload to set for the session event.
     * @return a new instance of {@link DeploymentSessionEvent}.
     */
    public static DeploymentSessionEvent newResourceEvent(
            DeploymentSessionIdentifier sessionId,
            DeploymentSession.Status status,
            ProvisionedResource resource
    ) {
        return DeploymentSessionEvent.newBuilder().setType(DeploymentSessionEvent.Type.RESOURCE)
                .setSession(sessionId)
                .setStatus(status)
                .setResource(resource)
                .setNode(ConcordNode.getDefaultInstance())
                .setNodeStatus(ConcordNodeStatus.getDefaultInstance())
                .setCluster(ConcordCluster.getDefaultInstance())
                .build();
    }

    /**
     * Create an instance of {@link DeploymentSessionEvent} denoting the cluster deployment event of a {@link
     * DeploymentSession} associated with a given {@link DeploymentSessionIdentifier}.
     *
     * @param sessionId identifier of the deployment session to create the session event for.
     * @param status    completion status.
     * @param node      data payload to set for the session event.
     * @return a new instance of {@link DeploymentSessionEvent}.
     */
    public static DeploymentSessionEvent newNodeDeploymentEvent(
            DeploymentSessionIdentifier sessionId,
            DeploymentSession.Status status,
            ConcordNode node
    ) {
        return DeploymentSessionEvent.newBuilder().setType(DeploymentSessionEvent.Type.NODE_DEPLOYED)
                .setSession(sessionId)
                .setStatus(status)
                .setResource(ProvisionedResource.getDefaultInstance())
                .setNode(node)
                .setNodeStatus(ConcordNodeStatus.getDefaultInstance())
                .setCluster(ConcordCluster.getDefaultInstance())
                .build();
    }

    /**
     * Create an instance of {@link DeploymentSessionEvent} denoting the cluster deployment event of a {@link
     * DeploymentSession} associated with a given {@link DeploymentSessionIdentifier}.
     *
     * @param sessionId identifier of the deployment session to create the session event for.
     * @param status    completion status.
     * @param cluster   data payload to set for the session event.
     * @return a new instance of {@link DeploymentSessionEvent}.
     */
    public static DeploymentSessionEvent newClusterDeploymentEvent(
            DeploymentSessionIdentifier sessionId,
            DeploymentSession.Status status,
            ConcordCluster cluster
    ) {
        return DeploymentSessionEvent.newBuilder().setType(DeploymentSessionEvent.Type.CLUSTER_DEPLOYED)
                .setSession(sessionId)
                .setStatus(status)
                .setResource(ProvisionedResource.getDefaultInstance())
                .setNode(ConcordNode.getDefaultInstance())
                .setNodeStatus(ConcordNodeStatus.getDefaultInstance())
                .setCluster(cluster)
                .build();
    }

    /**
     * Convert the {@link ConcordNodeIdentifier} to its canonical resource name.
     *
     * @param identifier identifier to convert into resource name.
     * @return resource name as a {@link String}.
     */
    public static String toResourceName(ConcordNodeIdentifier identifier) {
        return new UUID(identifier.getHigh(), identifier.getLow()).toString();
    }

    /**
     * Convert a canonical resource name to a {@link ConcordNodeIdentifier}.
     *
     * @param name resource name to convert into identifier.
     * @return identifier as a {@link ConcordNodeIdentifier}.
     */
    public static ConcordNodeIdentifier toNodeIdentifier(String name) {
        var uuid = UUID.fromString(name);
        return ConcordNodeIdentifier.newBuilder().setLow(uuid.getLeastSignificantBits())
                .setHigh(uuid.getMostSignificantBits()).build();
    }


    /**
     * Create a new {@link ConcordNodeHostInfo} instance based on a {@link OrchestratorData.ComputeResourceEventCreated}
     * event, using additional input as context information for instance creation.
     *
     * @param event                    event signaled for the concord node.
     * @param placementEntryByNodeName mappings of concord node resource name to its associated {@link
     *                                 PlacementAssignment.Entry}.
     * @return a new instance of {@link ConcordNodeHostInfo} with relevant properties filled in with non-default values.
     */
    private static ConcordNodeHostInfo toConcordNodeHostInfo(
            OrchestratorData.ComputeResourceEventCreated event,
            Map<String, PlacementAssignment.Entry> placementEntryByNodeName
    ) {
        return ConcordNodeHostInfo.newBuilder()
                .setSite(placementEntryByNodeName.get(toResourceName(event.getNode())).getSite()).build();
    }

    /**
     * Create a new {@link ConcordNodeHostInfo} instance based on a {@link OrchestratorData.NetworkResourceEventCreated}
     * event, using additional input as context information for instance creation.
     *
     * @param publicNetworkEvent       event signaled for the concord node.
     * @param privateNetworkEvent      event signaled for the concord node.
     * @param placementEntryByNodeName mappings of concord node resource name to its associated {@link
     *                                 PlacementAssignment.Entry}.
     * @return a new instance of {@link ConcordNodeHostInfo} with relevant properties filled in with non-default values.
     */
    private static ConcordNodeHostInfo toConcordNodeHostInfo(
            OrchestratorData.NetworkResourceEventCreated publicNetworkEvent,
            OrchestratorData.NetworkResourceEventCreated privateNetworkEvent,
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
                        ConcordNodeEndpoint.newBuilder().setUrl(URI.create("https://{{ip}}:6865"
                                                                                   .replace("{{ip}}",
                                                                                            publicNetworkEvent
                                                                                                    .getAddress()))
                                                                        .toString()).build()
                );
                break;
            case HLF:
                endpoints = Map.of(
                        "concord-hlf",
                        ConcordNodeEndpoint.newBuilder().setUrl(URI.create("https://{{ip}}:50051"
                                                                                   .replace("{{ip}}",
                                                                                            publicNetworkEvent
                                                                                                    .getAddress()))
                                                                        .toString()).build()
                );
                break;

            case ETHEREUM:
            default:
                endpoints = Map.of(
                        "ethereum-rpc",
                        ConcordNodeEndpoint.newBuilder().setUrl(URI.create("https://{{ip}}:8545"
                                                                                   .replace("{{ip}}",
                                                                                            publicNetworkEvent
                                                                                                    .getAddress()))
                                                                        .toString()).build()
                );
        }

        return ConcordNodeHostInfo.newBuilder()
                .setSite(placementEntryByNodeName.get(publicNetworkEvent.getName()).getSite())
                .putAllIpv4AddressMap(Map.of(
                        NetworkAddress.toIPv4Address(publicNetworkEvent.getAddress()),
                        NetworkAddress.toIPv4Address(privateNetworkEvent.getAddress())
                ))
                .putAllEndpoints(endpoints)
                .build();
    }
}