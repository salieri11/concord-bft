/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;

import com.vmware.blockchain.auth.AuthHelper;
import com.vmware.blockchain.common.fleetmanagment.FleetUtils;
import com.vmware.blockchain.deployment.v1.ConcordCluster;
import com.vmware.blockchain.deployment.v1.ConcordNode;
import com.vmware.blockchain.deployment.v1.ConcordNodeEndpoint;
import com.vmware.blockchain.deployment.v1.DeploymentSession;
import com.vmware.blockchain.deployment.v1.DeploymentSessionEvent;
import com.vmware.blockchain.deployment.v1.OrchestrationSiteIdentifier;
import com.vmware.blockchain.operation.OperationContext;
import com.vmware.blockchain.services.blockchains.Blockchain.BlockchainType;
import com.vmware.blockchain.services.blockchains.Blockchain.NodeEntry;
import com.vmware.blockchain.services.blockchains.replicas.Replica;
import com.vmware.blockchain.services.blockchains.replicas.Replica.ReplicaType;
import com.vmware.blockchain.services.blockchains.replicas.ReplicaService;
import com.vmware.blockchain.services.clients.Client.ClientType;
import com.vmware.blockchain.services.tasks.Task;
import com.vmware.blockchain.services.tasks.TaskService;

import io.grpc.stub.StreamObserver;

/**
 * Stream observer for creating a blockchain cluster.  Need to save the current Auth Context,
 * and update task and blockchain objects as they occur.
 */
public class BlockchainObserver implements StreamObserver<DeploymentSessionEvent> {
    private static final Logger logger = LogManager.getLogger(BlockchainObserver.class);

    private Authentication auth;
    private AuthHelper authHelper;
    private OperationContext operationContext;
    private BlockchainService blockchainService;
    private ReplicaService replicaService;
    private TaskService taskService;
    private UUID taskId;
    private UUID consortiumId;
    private UUID blockchainId;
    private final List<NodeEntry> nodeList = new ArrayList<>();
    private DeploymentSession.Status status = DeploymentSession.Status.UNKNOWN;
    private UUID clusterId;
    private String opId;
    private BlockchainType type;
    private ClientType clientType;
    private ReplicaType replicaType;

    /**
     * Create a new Blockchain Observer.  This handles the callbacks from the deployment
     * process.
     * @param blockchainService Blockchain service
     * @param taskService       Task service
     * @param taskId            The task ID we are reporting this on.
     * @param consortiumId      ID for the consortium creating this blockchain
     * @param blockchainId      ID for the blockchain if present.
     */
    public BlockchainObserver(
            AuthHelper authHelper,
            OperationContext operationContext,
            BlockchainService blockchainService,
            ReplicaService replicaService,
            TaskService taskService,
            UUID taskId,
            UUID consortiumId,
            UUID blockchainId,
            BlockchainType type,
            ReplicaType  replicaType,
            ClientType clientType) {
        this.authHelper = authHelper;
        this.operationContext = operationContext;
        this.blockchainService = blockchainService;
        this.replicaService = replicaService;
        this.taskService = taskService;
        this.taskId = taskId;
        this.consortiumId = consortiumId;
        this.blockchainId = blockchainId;
        auth = SecurityContextHolder.getContext().getAuthentication();
        opId = operationContext.getId();
        this.type = type;
        this.replicaType = replicaType;
        this.clientType = clientType;
    }

    private void logNode(ConcordNode node) {
        logger.info("Node ID: {}, HostInfo: {}, Info: {}",
                    FleetUtils.toUuid(node.getId()), node.getHostInfo(), node.getInfo());
    }

    private void logCluster(ConcordCluster cluster) {
        logger.info("Cluser ID: {}, Info: {}", FleetUtils.toUuid(cluster.getId()), cluster.getInfo());
    }

    @Override
    public void onNext(DeploymentSessionEvent value) {
        logger.info("On Next: {}", value.getType());
        // Set auth in this thread to whoever invoked the observer
        SecurityContextHolder.getContext().setAuthentication(auth);
        operationContext.setId(opId);
        String message = "Deployment in progress";
        logger.info("Type: {}, Node: {}, Cluster: {}", value.getType(), value.getNode(), value.getCluster());
        logNode(value.getNode());
        logCluster(value.getCluster());
        try {
            switch (value.getType()) {
                case NODE_DEPLOYED:
                    ConcordNode cNode = value.getNode();
                    message = String.format("Node %s deployed, status %s",
                                            FleetUtils.toUuid(cNode.getId()), value.getNodeStatus().getStatus());
                    logger.info("Node {} deployed", FleetUtils.toUuid(cNode.getId()));
                    break;

                case CLUSTER_DEPLOYED:

                    ConcordCluster cluster = value.getCluster();
                    if (replicaType ==  replicaType.DAML_PARTICIPANT) {
                        // Temporary hack to allow client deployment.
                        clusterId = blockchainId;
                    } else {
                        // force the blockchain id to be the same as the cluster id
                        clusterId = FleetUtils.toUuid(cluster.getId());
                        message = String.format("Cluster %s deployed", clusterId);
                        logger.info("Blockchain ID: {}", clusterId);

                        // create the nodeList for the cluster
                        cluster.getInfo().getMembersList().stream()
                                .map(BlockchainObserver::toNodeEntry)
                                .peek(node -> logger.info("Node entry, id {}", node.getNodeId()))
                                .forEach(nodeList::add);
                    }

                    // create the and save the replicas.
                    cluster.getInfo().getMembersList().stream()
                            .map(n -> toReplica(clusterId, n, this.replicaType,
                                                this.clientType))
                            .peek(replica -> logger.info("Node entry, id {}", replica.getId()))
                            .forEach(replicaService::put);

                    break;
                case COMPLETED:
                    status = value.getStatus();
                    message = String.format("Deployment completed on cluster %s, status %s", clusterId, status);
                    logger.info("On Next(COMPLETED): status({})", status);
                    break;
                default:
                    break;
            }

            // Persist the current state of the task.
            final Task task = taskService.get(taskId);
            // need a final for the lambda
            final String mes = message;
            // Not clear if we need the merge here,
            taskService.merge(task, m -> {
                // if the latest entry is in completed, don't change anything
                if (m.getState() != Task.State.SUCCEEDED && m.getState() != Task.State.FAILED) {
                    // Otherwise, set the fields
                    m.setMessage(mes);
                }
            });
        } finally {
            SecurityContextHolder.getContext().setAuthentication(null);
            operationContext.removeId();
        }
    }

    @Override
    public void onError(Throwable t) {
        logger.info("On Error", t);
        // Set auth in this thread to whoever invoked the observer
        SecurityContextHolder.getContext().setAuthentication(auth);
        operationContext.setId(opId);
        final Task task = taskService.get(taskId);
        task.setState(Task.State.FAILED);
        task.setMessage(t.getMessage());
        taskService.merge(task, m -> {
            m.setState(task.getState());
            m.setMessage(task.getMessage());
        });
        SecurityContextHolder.getContext().setAuthentication(null);
        operationContext.removeId();
    }

    @Override
    public void onCompleted() {
        // Set auth in this thread to whoever invoked the observer
        SecurityContextHolder.getContext().setAuthentication(auth);
        operationContext.setId(opId);
        // Just log this.  Looking to see how often this happens.
        logger.info("Task {} completed, status {}", taskId, status);
        // We need to evict the auth token from the cache, since the available blockchains has just changed
        authHelper.evictToken();
        final Task task = taskService.get(taskId);
        task.setMessage("Operation finished");

        if (status == DeploymentSession.Status.SUCCESS) {
            if (replicaType == ReplicaType.DAML_PARTICIPANT) {
                task.setResourceId(blockchainId);
                task.setResourceLink(String.format("/api/blockchains/%s/clients", blockchainId));
            } else {
                // Create blockchain entity based on collected information.
                Blockchain blockchain = blockchainService.create(clusterId, consortiumId, type, nodeList);
                task.setResourceId(blockchain.getId());
                task.setResourceLink(String.format("/api/blockchains/%s", blockchain.getId()));
            }
            task.setState(Task.State.SUCCEEDED);
        } else {
            task.setState(Task.State.FAILED);
        }

        // Persist the finality of the task, success or failure.
        taskService.merge(task, m -> {
            // if the latest entry is in completed, don't change anything
            if (m.getState() != Task.State.SUCCEEDED && m.getState() != Task.State.FAILED) {
                // Otherwise, set the fields
                m.setMessage(task.getMessage());
                m.setResourceId(task.getResourceId());
                m.setResourceLink(task.getResourceLink());
                m.setState(task.getState());
            }
        });
        //
        logger.info("Updated task {}", task);
        SecurityContextHolder.getContext().setAuthentication(null);
        operationContext.removeId();
    }

    /**
     * Convert a {@link ConcordNode} instance to a {@link NodeEntry} instance with applicable
     * property values transferred.
     *
     * @param node {@code ConcordNode} instance to convert from.
     *
     * @return     a new instance of {@code NodeEntry}.
     */
    private static NodeEntry toNodeEntry(ConcordNode node) {
        // Fetch the first IP address in the data payload, or return 0.
        var ip = toCanonicalIpAddress(node.getHostInfo().getIpv4AddressMap().keySet().stream()
                                              .findFirst().orElse(0));

        ConcordNodeEndpoint endpoint = getConcordNodeEndpoint(node);

        // For now, use the orchestration site ID as region name. Eventually there should be some
        // human-readable display name to go with the site ID.
        var site = node.getHostInfo().getSite();
        var region = FleetUtils.toUuid(site);

        return new NodeEntry(
                FleetUtils.toUuid(node.getId()),
                ip,
                endpoint.getUrl(),
                endpoint.getCertificate(),
                region
        );
    }

    private static ConcordNodeEndpoint getConcordNodeEndpoint(ConcordNode node) {
        ConcordNodeEndpoint endpoint = null;
        switch (node.getInfo().getBlockchainType()) {
            case DAML:
                endpoint = node.getHostInfo().getEndpoints().getOrDefault("daml-ledger-api", null);
                break;
            case HLF:
                endpoint = node.getHostInfo().getEndpoints().getOrDefault("concord-hlf", null);
                break;
            default:
                endpoint = node.getHostInfo().getEndpoints().getOrDefault("ethereum-rpc", null);
        }
        return endpoint;
    }

    private static Replica toReplica(UUID blockchainId, ConcordNode node, ReplicaType replicaType,
                                     ClientType clientType) {
        // Fetch the first IP address in the data payload, or return 0.
        UUID replicaId = FleetUtils.toUuid(node.getId());
        String name = node.getInfo().getIpv4Addresses().keySet().stream().findFirst().orElse("replica");
        int publicIp = node.getHostInfo().getIpv4AddressMap().keySet().stream()
                                              .findFirst().orElse(0);
        int privateIp = node.getHostInfo().getIpv4AddressMap().getOrDefault(publicIp, 0);
        ConcordNodeEndpoint endpoint = getConcordNodeEndpoint(node);

        // For now, use the orchestration site ID as region name. Eventually there should be some
        // human-readable display name to go with the site ID.
        OrchestrationSiteIdentifier site = node.getHostInfo().getSite();
        UUID zoneId = FleetUtils.toUuid(site);
        Replica replica = new Replica(toCanonicalIpAddress(publicIp), toCanonicalIpAddress(privateIp),
                name, endpoint.getUrl(), endpoint.getCertificate(), zoneId, replicaType, blockchainId);
        replica.setId(replicaId);
        return replica;
    }


    /**
     * Simple conversion of a 4-byte value (expressed as an integer) to a canonical IPv4 address
     * format.
     *
     * @param value bytes to convert from.
     *
     * @return      canonical IP address format, as a {@link String} instance.
     */
    private static String toCanonicalIpAddress(int value) {
        var first = (value >>> 24);
        var second = (value & 0x00FF0000) >>> 16;
        var third = (value & 0x0000FF00) >>> 8;
        var fourth = (value & 0x000000FF);

        return String.format("%d.%d.%d.%d", first, second, third, fourth);
    }


}
