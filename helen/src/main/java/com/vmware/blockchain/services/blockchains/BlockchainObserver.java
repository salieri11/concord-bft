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

import com.vmware.blockchain.deployment.model.ConcordCluster;
import com.vmware.blockchain.deployment.model.ConcordNode;
import com.vmware.blockchain.deployment.model.DeploymentSession;
import com.vmware.blockchain.deployment.model.DeploymentSessionEvent;
import com.vmware.blockchain.security.AuthHelper;
import com.vmware.blockchain.services.blockchains.Blockchain.NodeEntry;
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
    private BlockchainService blockchainService;
    private TaskService taskService;
    private UUID taskId;
    private final List<NodeEntry> nodeList = new ArrayList<>();
    private DeploymentSession.Status status = DeploymentSession.Status.UNKNOWN;
    private UUID clusterId;

    /**
     * Create a new Blockchain Observer.  This handles the callbacks from the deployment
     * process.
     * @param blockchainService Blockchain service
     * @param taskService       Task service
     * @param taskId            The task ID we are reporting this on.
     */
    public BlockchainObserver(
            AuthHelper authHelper,
            BlockchainService blockchainService,
            TaskService taskService,
            UUID taskId) {
        this.authHelper = authHelper;
        this.blockchainService = blockchainService;
        this.taskService = taskService;
        this.taskId = taskId;
        auth = SecurityContextHolder.getContext().getAuthentication();
    }

    @Override
    public void onNext(DeploymentSessionEvent value) {
        logger.info("On Next: {}", value.getType());
        // Set auth in this thread to whoever invoked the observer
        SecurityContextHolder.getContext().setAuthentication(auth);

        switch (value.getType()) {
            case CLUSTER_DEPLOYED:
                ConcordCluster cluster = value.getCluster();
                // force the blockchain id to be the same as the cluster id
                clusterId = new UUID(cluster.getId().getHigh(), cluster.getId().getLow());
                logger.info("Blockchain ID: {}", clusterId);

                cluster.getInfo().getMembers().stream()
                        .map(BlockchainObserver::toNodeEntry)
                        .peek(node -> logger.info("Node entry, id {}", node.getNodeId()))
                        .forEach(nodeList::add);
                break;
            case COMPLETED:
                status = value.getStatus();
                logger.info("On Next(COMPLETED): status({})", status);
                break;
            default:
                break;
        }

        // Persist the current state of the task.
        final Task task = taskService.get(taskId);
        // Not clear if we need the merge here,
        taskService.merge(task, m -> {
            // if the latest entry is in completed, don't change anything
            if (m.getState() != Task.State.SUCCEEDED && m.getState() != Task.State.FAILED) {
                // Otherwise, set the fields
                m.setMessage(value.getType().name());
            }
        });
        SecurityContextHolder.getContext().setAuthentication(null);
    }

    @Override
    public void onError(Throwable t) {
        logger.info("On Error", t);
        // Set auth in this thread to whoever invoked the observer
        SecurityContextHolder.getContext().setAuthentication(auth);
        /* Check on this
        task.setState(Task.State.FAILED);
        task.setMessage(t.getMessage());
        task = taskService.put(task);
        */
        SecurityContextHolder.getContext().setAuthentication(null);
    }

    @Override
    public void onCompleted() {
        // Set auth in this thread to whoever invoked the observer
        SecurityContextHolder.getContext().setAuthentication(auth);
        // Just log this.  Looking to see how often this happens.
        logger.info("On Completed");
        final Task task = taskService.get(taskId);
        task.setMessage("Operation finished");

        if (status == DeploymentSession.Status.SUCCESS) {
            // Create blockchain entity based on collected information.
            Blockchain blockchain = blockchainService.create(clusterId, authHelper.getOrganizationId(), nodeList);
            task.setResourceId(blockchain.getId());
            task.setResourceLink(String.format("/api/blockchains/%s", blockchain.getId()));
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
        var endpoint = node.getHostInfo().getEndpoints().getOrDefault("ethereum-rpc", null);

        // For now, use the orchestration site ID as region name. Eventually there should be some
        // human-readable display name to go with the site ID.
        var site = node.getHostInfo().getSite();
        var region = new UUID(site.getHigh(), site.getLow()).toString();

        return new NodeEntry(
              new UUID(node.getId().getHigh(), node.getId().getLow()),
              ip,
              endpoint.getUrl(),
              endpoint.getCertificate(),
              region
        );
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
