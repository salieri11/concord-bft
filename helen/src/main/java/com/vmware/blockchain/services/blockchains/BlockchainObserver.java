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
import com.vmware.blockchain.deployment.model.DeploymentSessionEvent;
import com.vmware.blockchain.deployment.model.DeploymentSessionEvent.Type;
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
    private Task task;
    private DeploymentSessionEvent value;

    /**
     * Create a new Blockchain Observer.  This handles the callbacks from the deployment
     * process.
     * @param blockchainService Blockchain service
     * @param taskService       Task service
     * @param task              The task we are reporting this on.
     */
    public BlockchainObserver(
            AuthHelper authHelper,
            BlockchainService blockchainService,
            TaskService taskService,
            Task task) {
        this.authHelper = authHelper;
        this.blockchainService = blockchainService;
        this.taskService = taskService;
        this.task = task;
        auth = SecurityContextHolder.getContext().getAuthentication();
    }

    @Override
    public void onNext(DeploymentSessionEvent value) {
        this.value = value;
        logger.info("On Next: {}", value.getType());
        // Set auth in this thread to whoever invoked the observer
        SecurityContextHolder.getContext().setAuthentication(auth);
        task.setMessage(value.getType().name());
        if (value.getType() == DeploymentSessionEvent.Type.COMPLETED) {
            ConcordCluster cluster = value.getCluster();
            // force the blockchain id to be the same as the cluster id
            UUID bcId = new UUID(cluster.getId().getHigh(), cluster.getId().getLow());
            logger.info("BC ID: {}", bcId);
            List<NodeEntry> nodeList = new ArrayList<>();

            for (ConcordNode cn : cluster.getInfo().getMembers()) {
                NodeEntry n = new NodeEntry();
                // force the new node entry to have the same id as the concord node
                n.setNodeId(new UUID(cn.getId().getHigh(), cn.getId().getLow()));
                logger.info("Node entry, id {}", n.getNodeId());
                // When the ip, rpcurl and rpc cert info become available, insert them here
                nodeList.add(n);
            }
            Blockchain blockchain = blockchainService.create(bcId, authHelper.getConsortiumId(), nodeList);
            task.setResourceId(blockchain.getId());
            task.setResourceLink(String.format("/api/blockchains/%s", blockchain.getId()));
            task.setState(Task.State.SUCCEEDED);
        }
        task = taskService.merge(task, m -> {
            // if the latest entry is in completed, don't change anything
            if (!m.getMessage().equals(Type.COMPLETED.name())) {
                // Otherwise, set the messaage
                m.setMessage(task.getMessage());
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
        logger.info("On Complete");
        SecurityContextHolder.getContext().setAuthentication(null);
    }

}
